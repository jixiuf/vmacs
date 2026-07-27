#!/usr/bin/env node
/**
 * Feishu MCP Server
 *
 * Exposes Feishu (飞书) capabilities as MCP tools.
 * PI / Cursor / Codex 等 AI 编码 Agent 可以通过 MCP 直接调用飞书能力。
 *
 * 使用方式：
 *   node server.js
 *
 * PI 配置（pi.json / 项目 .pi/config）：
 *   {
 *     "mcpServers": {
 *       "feishu": {
 *         "command": "node",
 *         "args": ["/path/to/feishu-mcp/server.js"]
 *       }
 *     }
 *   }
 */

import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";

import { execSync } from "child_process";

// ======================== Config ========================

function loadConfig() {
  // 1. 从 .authinfo.gpg 读取（格式：machine feishu-mcp login <appId> password <appSecret>）
  try {
    const output = execSync(
      "gpg -q --pinentry-mode loopback --for-your-eyes-only --no-tty -d ~/.authinfo.gpg 2>/dev/null",
      { encoding: "utf-8", timeout: 10000 }
    );
    const lines = output.split("\n");
    for (const line of lines) {
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

  // 2. 环境变量回退
  if (process.env.FEISHU_APP_ID && process.env.FEISHU_APP_SECRET) {
    return {
      appId: process.env.FEISHU_APP_ID,
      appSecret: process.env.FEISHU_APP_SECRET,
    };
  }

  throw new Error(
    "未找到飞书配置。请在 ~/.authinfo.gpg 中添加以下条目：\n" +
      "  machine feishu-mcp login <appId> password <appSecret>\n" +
      "或设置 FEISHU_APP_ID 和 FEISHU_APP_SECRET 环境变量。"
  );
}

const CONFIG = loadConfig();

// ======================== Feishu API Client ========================

class FeishuClient {
  constructor(appId, appSecret) {
    this.appId = appId;
    this.appSecret = appSecret;
    this.baseUrl = "https://open.feishu.cn/open-apis";
    this._token = null;
    this._tokenExpiresAt = 0;
  }

  async getToken() {
    if (Date.now() < this._tokenExpiresAt) return this._token;
    const resp = await fetch(`${this.baseUrl}/auth/v3/tenant_access_token/internal`, {
      method: "POST",
      headers: { "Content-Type": "application/json; charset=utf-8" },
      body: JSON.stringify({ app_id: this.appId, app_secret: this.appSecret }),
    });
    const data = await resp.json();
    if (data.code !== 0) throw new Error(`获取 token 失败: ${data.msg}`);
    this._token = data.tenant_access_token;
    this._tokenExpiresAt = Date.now() + (data.expire - 60) * 1000;
    return this._token;
  }

  async request(method, path, body) {
    const token = await this.getToken();
    const url = `${this.baseUrl}${path}`;
    const headers = {
      Authorization: `Bearer ${token}`,
      "Content-Type": "application/json; charset=utf-8",
    };
    const opts = { method, headers };
    if (body) opts.body = JSON.stringify(body);
    const resp = await fetch(url, opts);
    const data = await resp.json();
    if (data.code !== 0) {
      throw new Error(`飞书 API 错误 [${data.code}]: ${data.msg}`);
    }
    return data;
  }

  // --- 文档 (Docx) ---
  async readDoc(docToken) {
    return this.request("GET", `/docx/v1/documents/${docToken}/raw_content`);
  }

  // --- 消息 (IM) ---
  async sendMessage(receiveId, receiveIdType, content) {
    const body = {
      receive_id: receiveId,
      receive_id_type: receiveIdType || "open_id",
      msg_type: "text",
      content: JSON.stringify({ text: content }),
    };
    return this.request("POST", `/im/v1/messages?receive_id_type=${receiveIdType || "open_id"}`, body);
  }

  // --- 通讯录 (Contact) ---
  async getUser(openId) {
    return this.request("GET", `/contact/v3/users/${openId}`);
  }

  async searchUserByPhone(phone) {
    return this.request("POST", "/contact/v3/users/search", {
      mail: "", // empty to trigger phone search
      phone: phone,
    });
  }
}

const client = new FeishuClient(CONFIG.appId, CONFIG.appSecret);

// ======================== MCP Server ========================

const server = new Server(
  {
    name: "feishu-mcp",
    version: "1.0.0",
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

// ======================== Tool Definitions ========================

const TOOLS = [
  {
    name: "feishu_doc_read",
    description: "读取飞书文档的原始 Markdown 内容",
    inputSchema: {
      type: "object",
      properties: {
        doc_token: {
          type: "string",
          description: "文档 token（从飞书文档 URL 中提取，如 https://xxx.feishu.cn/docx/TOKEN_HERE）",
        },
      },
      required: ["doc_token"],
    },
  },
  {
    name: "feishu_send_message",
    description: "发送飞书消息（纯文本）到指定用户或群聊",
    inputSchema: {
      type: "object",
      properties: {
        receive_id: {
          type: "string",
          description: "接收者 ID。给用户发用 open_id（如 ou_xxx），给群聊发用 chat_id（如 oc_xxx）",
        },
        receive_id_type: {
          type: "string",
          enum: ["open_id", "user_id", "email", "chat_id"],
          description: "ID 类型（默认 open_id -> 给个人；chat_id -> 给群聊）",
        },
        content: {
          type: "string",
          description: "消息文本内容",
        },
      },
      required: ["receive_id", "content"],
    },
  },
  {
    name: "feishu_get_user",
    description: "查询飞书用户信息（通过 open_id）",
    inputSchema: {
      type: "object",
      properties: {
        open_id: {
          type: "string",
          description: "用户的 open_id（如 ou_xxx）",
        },
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
        phone: {
          type: "string",
          description: "手机号",
        },
      },
      required: ["phone"],
    },
  },
];

const toolNames = new Set(TOOLS.map((t) => t.name));

// ======================== Tool Handlers ========================

const HANDLERS = {
  feishu_doc_read: async (args) => {
    const data = await client.readDoc(args.doc_token);
    return { content: [{ type: "text", text: JSON.stringify(data, null, 2) }] };
  },

  feishu_send_message: async (args) => {
    const data = await client.sendMessage(
      args.receive_id,
      args.receive_id_type || "open_id",
      args.content
    );
    const msgId = data?.data?.message_id || "unknown";
    return {
      content: [
        {
          type: "text",
          text: `✅ 消息发送成功\n消息 ID: ${msgId}\n${JSON.stringify(data, null, 2)}`,
        },
      ],
    };
  },

  feishu_get_user: async (args) => {
    const data = await client.getUser(args.open_id);
    return { content: [{ type: "text", text: JSON.stringify(data, null, 2) }] };
  },

  feishu_search_user: async (args) => {
    const data = await client.searchUserByPhone(args.phone);
    return { content: [{ type: "text", text: JSON.stringify(data, null, 2) }] };
  },
};

// ======================== Register Handlers ========================

server.setRequestHandler(ListToolsRequestSchema, async () => ({
  tools: TOOLS,
}));

server.setRequestHandler(CallToolRequestSchema, async (request) => {
  const { name, arguments: args } = request.params;

  if (!toolNames.has(name)) {
    return {
      content: [{ type: "text", text: `未知工具: ${name}` }],
      isError: true,
    };
  }

  const handler = HANDLERS[name];
  if (!handler) {
    return {
      content: [{ type: "text", text: `工具 "${name}" 尚未实现（需要 user_access_token 授权的飞书 API，当前仅支持 app 级别 API）` }],
      isError: true,
    };
  }

  try {
    return await handler(args || {});
  } catch (err) {
    return {
      content: [{ type: "text", text: `❌ 错误: ${err.message}` }],
      isError: true,
    };
  }
});

// ======================== Main ========================

async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("✅ Feishu MCP Server started");
  console.error(`   Tools: ${TOOLS.map((t) => t.name).join(", ")}`);
}

main().catch((err) => {
  console.error("Fatal error:", err);
  process.exit(1);
});
