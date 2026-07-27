#!/usr/bin/env node
/**
 * Feishu MCP Server
 *
 * Uses @larksuiteoapi/node-sdk to expose Feishu APIs as MCP tools.
 * API 路径按需惰性解析，不预建索引。
 *
 * Tools:
 *   feishu_call_api   – 调用任意飞书 Open API（1248+ 端点）
 *   feishu_list_apis  – 搜索可用的 API 路径
 *   feishu_send_message – 快捷发文本消息
 *   feishu_get_user     – 查用户信息
 *   feishu_search_user  – 按手机号搜用户
 *
 * 配置：
 *   ~/.authinfo.gpg:  machine feishu-mcp login <appId> password <appSecret>
 *   或环境变量 FEISHU_APP_ID / FEISHU_APP_SECRET
 */

import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";

import { execSync } from "child_process";
import path from "path";
import fs from "fs";
import { createRequire } from "module";
const require = createRequire(import.meta.url);

// ======================== Auth ========================

function loadConfig() {
  try {
    const output = execSync(
      "gpg -q --pinentry-mode ask --for-your-eyes-only --no-tty -d ~/.authinfo.gpg 2>/dev/null",
      { encoding: "utf-8", timeout: 10000 }
    );
    for (const line of output.split("\n")) {
      const parts = line.trim().split(/\s+/);
      for (let i = 0; i < parts.length - 1; i++) {
        if (parts[i] === "machine" && parts[i + 1] === "feishu-mcp") {
          let appId = "", appSecret = "";
          for (let j = i + 2; j < parts.length; j += 2) {
            if (j + 1 < parts.length) {
              if (parts[j] === "login") appId = parts[j + 1];
              if (parts[j] === "password") appSecret = parts[j + 1];
            }
          }
          if (appId && appSecret) return { appId, appSecret };
        }
      }
    }
  } catch {}
  if (process.env.FEISHU_APP_ID && process.env.FEISHU_APP_SECRET)
    return { appId: process.env.FEISHU_APP_ID, appSecret: process.env.FEISHU_APP_SECRET };
  throw new Error("请在 ~/.authinfo.gpg 配置 machine feishu-mcp login <appId> password <appSecret>");
}

const CONFIG = loadConfig();

// ======================== SDK Client ========================

const lark = require("@larksuiteoapi/node-sdk");
const sdkClient = new lark.Client({
  appId: CONFIG.appId,
  appSecret: CONFIG.appSecret,
  appType: lark.AppType.SelfBuild,
  domain: lark.Domain.Feishu,
});

// Suppress SDK noise "client ready"
console.error = (() => {
  const orig = console.error;
  return (...args) => {
    if (args[0] === "[info]:") return;
    orig.apply(console, args);
  };
})();

// ======================== Lazy API Resolution ========================

const SKIP_KEYS = new Set([
  "tokenManager", "logger", "httpInstance", "cache",
  "accessToken", "userAccessToken",
]);

/**
 * 惰性遍历 client 对象，收集所有 {service, resource, method} 三元组。
 * 只在 feishu_list_apis 被调用时执行一次、缓存结果。
 */
let cachedEntries = null;
function collectEntries() {
  if (cachedEntries) return cachedEntries;
  const entries = [];
  function walk(obj, prefix) {
    if (!obj || typeof obj !== "object") return;
    for (const key of Object.keys(obj)) {
      if (SKIP_KEYS.has(key)) continue;
      const val = obj[key];
      if (typeof val === "function") {
        const parts = prefix.split(".");
        if (parts.length >= 2)
          entries.push({ path: `${parts[0]}.${parts.slice(1).join(".")}.${key}`, fn: val });
      } else if (typeof val === "object" && val !== null && !Array.isArray(val)) {
        walk(val, prefix ? `${prefix}.${key}` : key);
      }
    }
  }
  walk(sdkClient, "");
  // Remove non-service top-level keys
  const filtered = entries.filter((e) => !SKIP_KEYS.has(e.path.split(".")[0]));
  cachedEntries = filtered;
  return filtered;
}

/**
 * 按点号路径惰性查找 SDK client 上的方法。
 * 不做预建索引，每次从 client 对象树实时遍历。
 */
function resolveApi(apiPath) {
  const parts = apiPath.split(".");
  if (parts.length < 3)
    throw new Error(`API 路径需要至少 service.resource.method，收到: "${apiPath}"`);

  let current = sdkClient;
  for (let i = 0; i < parts.length; i++) {
    const key = parts[i];
    if (!current || typeof current[key] === "undefined") {
      // Try fuzzy match for error hint
      const candidates = Object.keys(current || {})
        .filter((k) => !SKIP_KEYS.has(k))
        .filter((k) => k.toLowerCase().includes(key.toLowerCase()) || key.toLowerCase().includes(k.toLowerCase()))
        .slice(0, 5);
      const hint = candidates.length ? `，相近: ${candidates.join(", ")}` : "";
      throw new Error(`路径 "${apiPath}" 在 "${parts.slice(0, i).join(".")}" 处找不到 "${key}"${hint}`);
    }
    current = current[key];
  }

  if (typeof current !== "function")
    throw new Error(`路径 "${apiPath}" 指向的不是一个函数`);
  return current;
}

// ======================== MCP Server ========================

const server = new Server(
  { name: "feishu-mcp", version: "1.1.0" },
  { capabilities: { tools: {} } }
);

// ======================== Tools ========================

const TOOLS = [
  {
    name: "feishu_call_api",
    description:
      "调用任意飞书 Open API。路径格式为 service.resource.method（小驼峰）—— 用 feishu_list_apis 搜索可用路径。",
    inputSchema: {
      type: "object",
      properties: {
        api: {
          type: "string",
          description:
            "API 路径，如 im.message.create（发消息）、docx.document.rawContent（读文档纯文本）、" +
            "calendar.calendarEvent.list（列日程）",
        },
        params: { type: "object", description: "URL query 参数 JSON" },
        data: { type: "object", description: "请求体 JSON" },
        path: { type: "object", description: "URL 路径参数 JSON" },
      },
      required: ["api"],
    },
  },
  {
    name: "feishu_list_apis",
    description: "搜索可用的飞书 API 路径。不传 query 则全量列出。",
    inputSchema: {
      type: "object",
      properties: {
        query: { type: "string", description: "搜索关键词，如 docx、message、calendar" },
        page: { type: "number", description: "页码（从 1 开始）" },
        page_size: { type: "number", description: "每页条数（默认 20，最大 100）" },
      },
    },
  },
  {
    name: "feishu_send_message",
    description: "发送飞书文本消息（快捷方式，等同 feishu_call_api im.message.create）",
    inputSchema: {
      type: "object",
      properties: {
        receive_id: { type: "string", description: "接收者 ID（open_id / chat_id）" },
        receive_id_type: {
          type: "string", enum: ["open_id", "user_id", "email", "chat_id"],
          description: "ID 类型（默认 open_id）",
        },
        content: { type: "string", description: "消息文本" },
      },
      required: ["receive_id", "content"],
    },
  },
  {
    name: "feishu_get_user",
    description: "通过 open_id 查询飞书用户（快捷方式）",
    inputSchema: {
      type: "object",
      properties: {
        open_id: { type: "string", description: "用户的 open_id（如 ou_xxx）" },
      },
      required: ["open_id"],
    },
  },
  {
    name: "feishu_search_user",
    description: "通过手机号搜索飞书用户",
    inputSchema: {
      type: "object",
      properties: {
        phone: { type: "string", description: "手机号" },
      },
      required: ["phone"],
    },
  },
  {
    name: "feishu_describe_api",
    description:
      "查询飞书 API 的签名信息（参数位置、必填/可选字段、文档链接）。调用 feishu_call_api 前先用此工具确认参数格式。",
    inputSchema: {
      type: "object",
      properties: {
        api: {
          type: "string",
          description:
            "API 路径，如 wiki.space.getNode、docx.document.rawContent",
        },
      },
      required: ["api"],
    },
  },
];

// ======================== Type Definition Parser ========================

let _typeContent = null;
function getTypeContent() {
  if (_typeContent) return _typeContent;
  const sdkPkgPath = require.resolve("@larksuiteoapi/node-sdk/package.json");
  _typeContent = fs.readFileSync(
    path.join(path.dirname(sdkPkgPath), "types/index.d.ts"),
    "utf-8"
  );
  return _typeContent;
}

/**
 * Scan backwards from a line to find the nesting context path.
 * Uses indentation + "key: {" pattern (without "payload?") to identify parent namespaces.
 */
function buildContextPath(lines, lineIdx) {
  const baseIndent = getIndent(lines[lineIdx]);
  const parents = [];
  let currentIndent = baseIndent;

  for (let i = lineIdx - 1; i >= 0; i--) {
    const line = lines[i];
    const indent = getIndent(line);
    const trimmed = line.trim();

    // Only consider lines with less or equal indentation (going up the tree)
    if (indent > currentIndent) continue;
    if (indent < currentIndent || parents.length === 0) {
      // Check for namespace pattern: keyName: { or keyName?: { (without "payload?")
      // Must be end of line or followed only by comment
      const nsMatch = trimmed.match(
        /^(\w+)\??\s*:\s*\{(\s*\/\/.*)?$/
      );
      if (nsMatch) {
        parents.unshift(nsMatch[1]);
        currentIndent = indent;
      }
    }
  }

  return parents.join(".");
}

function getIndent(line) {
  const match = line.match(/^\s*/);
  return match ? match[0].length : 0;
}

/**
 * Starting from `startLineIdx`, find the closing of the payload/return block
 * by tracking brace depth. Returns all content from opening brace to its match.
 */
function extractBlock(lines, startLineIdx, startCol) {
  let depth = 0;
  const result = [];
  let started = false;

  for (let i = startLineIdx; i < lines.length; i++) {
    const line = lines[i];
    const part = started ? line : line.substring(startCol);
    let accumulated = "";

    for (let j = 0; j < part.length; j++) {
      const ch = part[j];
      accumulated += ch;
      if (ch === "{") depth++;
      else if (ch === "}") depth--;

      // Stop immediately when the outer block closes
      if (depth === 0 && (started || j > 0)) {
        result.push(accumulated);
        return result.join("\n");
      }
    }

    result.push(accumulated);
    started = true;
  }

  return result.join("\n");
}

// Cache parsed signatures for the session
const _signatureCache = new Map();

/**
 * Parse a payload block to extract params/data/path sections with field info.
 */
function parsePayloadBlock(blockText) {
  // Quick and dirty parsing: find sections like "params:", "data:", "path:"
  // at the top level (not nested)
  const sections = {};
  let depth = 0;
  let currentSection = null;
  let sectionStart = -1;

  const lines = blockText.split("\n");
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Track depth changes
    // Check for section headers BEFORE counting braces on this line
    let isSectionHeader = false;
    if (depth === 1) {
      const trimmed = line.trim();
      const sectionMatch = trimmed.match(
        /^(params|data|path)\??\s*:\s*\{/
      );
      if (sectionMatch) {
        currentSection = sectionMatch[1];
        sections[currentSection] = { required: [], optional: [] };
        sectionStart = i;
        isSectionHeader = true;
      }
    }

    for (const ch of line) {
      if (ch === "{") depth++;
      else if (ch === "}") depth--;
    }

    // Extract field names inside a section (skip the section header line itself)
    if (!isSectionHeader && depth === 2 && currentSection) {
      const trimmed = line.trim();
      if (!trimmed.startsWith("//")) {
        const fieldMatch = trimmed.match(/^(\w+)(\??)\s*:/);
        if (fieldMatch) {
          const fieldName = fieldMatch[1];
          const isOptional = fieldMatch[2] === "?";
          if (isOptional) {
            sections[currentSection].optional.push(fieldName);
          } else {
            sections[currentSection].required.push(fieldName);
          }
        }
      }
    }
  }

  return sections;
}

/**
 * Get the full API signature from the type definition file.
 */
function getApiSignature(apiPath) {
  // Check cache
  if (_signatureCache.has(apiPath)) return _signatureCache.get(apiPath);

  const content = getTypeContent();
  const lines = content.split("\n");
  const parts = apiPath.split(".");
  const methodName = parts[parts.length - 1];
  const searchStr = `${methodName}: (payload?: {`;

  // First pass: find all matching lines
  const candidateLines = [];
  for (let i = 0; i < lines.length; i++) {
    if (lines[i].includes(searchStr)) {
      candidateLines.push(i);
    }
  }

  if (candidateLines.length === 0) {
    throw new Error(`未在类型定义中找到 API: ${apiPath}`);
  }

  // Try each candidate, match by context path
  for (const lineIdx of candidateLines) {
    const line = lines[lineIdx];
    const context = buildContextPath(lines, lineIdx);

    // Check if context starts with our path prefix (e.g., "wiki.space" for "wiki.space.getNode")
    const prefix = parts.slice(0, -1).join(".");
    if (context === prefix || context.endsWith(`.${prefix}`)) {
      // Found the right method
      const col = line.indexOf(searchStr) + searchStr.length - 1;
      const payloadBlock = extractBlock(lines, lineIdx, col);
      const params = parsePayloadBlock(payloadBlock);

      // Also extract the doc link from JSDoc comment
      let docLink = "";
      for (let j = lineIdx - 1; j >= 0; j--) {
        const prevLine = lines[j].trim();
        if (prevLine.startsWith("*/")) break;
        const linkMatch = prevLine.match(/(https:\/\/open\.feishu\.cn\/document\/[^\s]+)/);
        if (linkMatch) {
          docLink = linkMatch[1];
          break;
        }
      }

      const result = { api: apiPath, docLink, params };
      _signatureCache.set(apiPath, result);
      return result;
    }
  }

  // Fallback: if no context match but we have candidates, return the first one
  // with a note about the ambiguity
  const lineIdx = candidateLines[0];
  const line = lines[lineIdx];
  const context = buildContextPath(lines, candidateLines[0]);
  const col = line.indexOf(searchStr) + searchStr.length - 1;
  const payloadBlock = extractBlock(lines, lineIdx, col);
  const params = parsePayloadBlock(payloadBlock);

  let docLink = "";
  for (let j = lineIdx - 1; j >= 0; j--) {
    const prevLine = lines[j].trim();
    if (prevLine.startsWith("*/")) break;
    const linkMatch = prevLine.match(/(https:\/\/open\.feishu\.cn\/document\/[^\s]+)/);
    if (linkMatch) {
      docLink = linkMatch[1];
      break;
    }
  }

  const result = { api: apiPath, docLink, params, _note: `上下文路径为 "${context}"，可能与请求路径 "${prefix}" 不完全匹配` };
  _signatureCache.set(apiPath, result);
  return result;
}

const toolNames = new Set(TOOLS.map((t) => t.name));

// ======================== Handlers ========================

const HANDLERS = {
  feishu_call_api: async (args) => {
    const fn = resolveApi(args.api);
    const payload = {};
    if (args.params) payload.params = args.params;
    if (args.data) payload.data = args.data;
    if (args.path) payload.path = args.path;
    const result = await fn(payload);
    return { content: [{ type: "text", text: JSON.stringify(result, null, 2) }] };
  },

  feishu_list_apis: async (args) => {
    const all = collectEntries();
    const query = (args.query || "").toLowerCase();
    const filtered = query ? all.filter((e) => e.path.toLowerCase().includes(query)) : all;
    const page = Math.max(1, args.page || 1);
    const pageSize = Math.min(100, Math.max(1, args.page_size || 20));
    const start = (page - 1) * pageSize;
    const items = filtered.slice(start, start + pageSize).map((e) => e.path);
    return {
      content: [
        {
          type: "text",
          text: JSON.stringify(
            {
              total: filtered.length,
              page,
              page_size: pageSize,
              has_more: start + pageSize < filtered.length,
              items,
              hint: "用 feishu_call_api 调用，参数 api 填路径",
            },
            null, 2
          ),
        },
      ],
    };
  },

  feishu_send_message: async (args) => {
    const fn = resolveApi("im.message.create");
    const result = await fn({
      params: { receive_id_type: args.receive_id_type || "open_id" },
      data: {
        receive_id: args.receive_id,
        msg_type: "text",
        content: JSON.stringify({ text: args.content }),
      },
    });
    const msgId = result?.data?.message_id || "unknown";
    return {
      content: [{ type: "text", text: `✅ 消息发送成功\n消息 ID: ${msgId}\n${JSON.stringify(result, null, 2)}` }],
    };
  },

  feishu_get_user: async (args) => {
    const fn = resolveApi("contact.user.get");
    const result = await fn({ path: { user_id: args.open_id }, params: { user_id_type: "open_id" } });
    return { content: [{ type: "text", text: JSON.stringify(result, null, 2) }] };
  },

  feishu_search_user: async (args) => {
    // contact/v3/users/search 不在 SDK 中，用 raw HTTP
    const result = await sdkClient.contact.user.batchGetId({ data: { mobiles: [args.phone] } });
    return { content: [{ type: "text", text: JSON.stringify(result, null, 2) }] };
  },

  feishu_describe_api: async (args) => {
    // First verify the API path is valid at runtime
    resolveApi(args.api);

    const sig = getApiSignature(args.api);
    const lines = [`📖 **${sig.api}**`];

    if (sig.docLink) {
      lines.push(`📎 文档: ${sig.docLink}`);
    }

    if (sig._note) {
      lines.push(`⚠️  ${sig._note}`);
    }

    const sections = sig.params || {};
    const sectionKeys = Object.keys(sections);

    if (sectionKeys.length === 0) {
      lines.push("\n此 API 无需额外参数（payload 为空）。");
    } else {
      for (const sectionName of sectionKeys) {
        const section = sections[sectionName];
        const paramLoc = sectionName; // "params" | "data" | "path"
        const hint = {
          params: "→ 传给 feishu_call_api 的 \`params\` 参数（URL query）",
          data: "→ 传给 feishu_call_api 的 \`data\` 参数（请求体）",
          path: "→ 传给 feishu_call_api 的 \`path\` 参数（URL 路径参数）",
        }[paramLoc] || "";

        lines.push(`\n**\`${paramLoc}\`** ${hint}`);

        if (section.required && section.required.length > 0) {
          lines.push(`  必填: \`${section.required.join("\`, \`")}\``);
        }
        if (section.optional && section.optional.length > 0) {
          lines.push(`  可选: \`${section.optional.join("\`, \`")}\``);
        }
        if (
          (!section.required || section.required.length === 0) &&
          (!section.optional || section.optional.length === 0)
        ) {
          lines.push(`  (空对象，无需传参)`);
        }
      }
    }

    // Generate a compact JSON example
    const examplePayload = {};
    for (const sectionName of sectionKeys) {
      const section = sections[sectionName];
      const exampleFields = {};
      for (const f of section.required || []) {
        exampleFields[f] = "<必填>";
      }
      for (const f of section.optional || []) {
        exampleFields[f] = "<可选>";
      }
      if (Object.keys(exampleFields).length > 0) {
        examplePayload[sectionName] = exampleFields;
      }
    }

    if (Object.keys(examplePayload).length > 0) {
      lines.push(
        `\n📋 调用示例:\n\`\`\`json\n${JSON.stringify(
          { api: args.api, ...examplePayload },
          null, 2
        )}\n\`\`\``
      );
    }

    return { content: [{ type: "text", text: lines.join("\n") }] };
  },
};

// ======================== MCP Protocol ========================

server.setRequestHandler(ListToolsRequestSchema, async () => ({ tools: TOOLS }));

server.setRequestHandler(CallToolRequestSchema, async (request) => {
  const { name, arguments: args } = request.params;
  if (!toolNames.has(name))
    return { content: [{ type: "text", text: `未知工具: ${name}` }], isError: true };

  const handler = HANDLERS[name];
  if (!handler)
    return { content: [{ type: "text", text: `工具 "${name}" 未实现` }], isError: true };

  try {
    return await handler(args || {});
  } catch (err) {
    let detail = err.message;
    // SDK errors often have code/msg from the Feishu API response
    if (err.code !== undefined || err.msg) {
      detail = `${err.message}`;
      if (err.code !== undefined) detail += `\ncode: ${err.code}`;
      if (err.msg) detail += `\nmsg: ${err.msg}`;
    }
    return {
      content: [{ type: "text", text: `❌ 错误: ${detail}` }],
      isError: true,
    };
  }
});

// ======================== Main ========================

async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("✅ Feishu MCP Server started (lazy resolve, no pre-built index)");
  console.error(`   Tools: ${TOOLS.map((t) => t.name).join(", ")}`);
  console.error(`   Tip: use feishu_list_apis → feishu_call_api`);
}

main().catch((err) => {
  console.error("Fatal error:", err);
  process.exit(1);
});
