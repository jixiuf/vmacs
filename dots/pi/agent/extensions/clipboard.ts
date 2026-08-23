/**
 * /clipboard — Export the session (active branch) as Markdown and copy it to
 * the clipboard, with OSC 52 support so it also works through SSH / MOSH /
 * tmux: the *local* terminal emulator captures the escape sequence and writes
 * your local clipboard, even when pi runs on a remote machine.
 *
 * OSC 52 note: the whole payload travels in a single escape sequence. This
 * extension does NOT cap the size — it is sent as-is and the terminal / tmux
 * decides whether to accept it (tmux hard limit ~1MB base64, Alacritty
 * handles 1MB+). Use `tail` / `--no-tools` to shrink very large exports.
 *
 * Usage:
 *   /clipboard               Copy session as Markdown (auto: system clipboard
 *                            first, OSC 52 for remote sessions)
 *   /clipboard osc52         Force OSC 52 only (via the terminal)
 *   /clipboard osc52 tail 30 Only the last 30 entries
 *   /clipboard --no-tools    Skip tool execution details (bash outputs, results)
 *   /clipboard test          Send a tiny OSC 52 test token (paste locally to verify the chain)
 *   /clipboard diag          Print environment + size diagnostics (no copy)
 *
 * Drop into ~/.pi/agent/extensions/ and run /reload.
 */

import type { ExtensionAPI, ExtensionCommandContext, SessionMessageEntry } from "@earendil-works/pi-coding-agent";
import { copyToClipboard } from "@earendil-works/pi-coding-agent";
import type { ImageContent, TextContent, ThinkingContent, ToolCall } from "@earendil-works/pi-ai";
import { platform } from "node:os";
import { closeSync, openSync, writeSync } from "node:fs";

// ── Clipboard helpers ────────────────────────────────────────────────────────

function isRemoteSession(env: NodeJS.ProcessEnv = process.env): boolean {
	return Boolean(env.SSH_CONNECTION || env.SSH_CLIENT || env.MOSH_CONNECTION);
}

/**
 * Write the OSC 52 sequence to a terminal so the terminal emulator puts it on
 * the local clipboard. Tries in order: stdout (normal interactive pi), the
 * controlling terminal /dev/tty (covers daemon/RPC modes where stdout/stderr
 * are redirected), then stderr. No size cap — the payload is sent as-is.
 */
function writeOsc52(seq: string): boolean {
	if (process.stdout.isTTY) {
		process.stdout.write(seq);
		return true;
	}
	try {
		const fd = openSync("/dev/tty", "w");
		try {
			writeSync(fd, seq);
		} finally {
			closeSync(fd);
		}
		return true;
	} catch {
		/* not attached to a controlling terminal */
	}
	if (process.stderr.isTTY) {
		process.stderr.write(seq);
		return true;
	}
	return false;
}

function emitOsc52(text: string): boolean {
	try {
		const encoded = Buffer.from(text, "utf8").toString("base64");
		return writeOsc52(`\x1b]52;c;${encoded}\x07`);
	} catch {
		return false;
	}
}

function formatSize(bytes: number): string {
	if (bytes < 1024) return `${bytes} B`;
	if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
	return `${(bytes / 1024 / 1024).toFixed(2)} MB`;
}

// ── Markdown rendering ───────────────────────────────────────────────────────

type AgentMessage = SessionMessageEntry["message"];
type ContentBlock = TextContent | ImageContent;

function formatTime(iso: string): string {
	const d = new Date(iso);
	return Number.isNaN(d.getTime()) ? iso : d.toLocaleString();
}

/** Wrap content in a fenced code block, using a longer fence if the content contains ```. */
function fence(content: string, lang = "text"): string {
	const body = content.replace(/\s+$/, "");
	let f = "```";
	while (body.includes(f)) f += "`";
	return `${f}${lang}\n${body}\n${f}`;
}

function renderContent(content: string | ContentBlock[]): string {
	if (typeof content === "string") return content.trim();
	const parts: string[] = [];
	for (const block of content) {
		if (block.type === "text") {
			const t = block.text.trim();
			if (t) parts.push(t);
		} else if (block.type === "image") {
			// Inline base64 would blow up the clipboard; use a placeholder.
			parts.push(`_🖼 image: ${block.mimeType}_`);
		}
	}
	return parts.join("\n\n").trim();
}

function renderAssistant(msg: Extract<AgentMessage, { role: "assistant" }>, includeTools: boolean): string {
	const textParts: string[] = [];
	const thinkingParts: string[] = [];
	const toolParts: string[] = [];

	for (const block of msg.content) {
		if (block.type === "text") {
			const t = block.text.trim();
			if (t) textParts.push(t);
		} else if (block.type === "thinking") {
			const t = block.thinking.trim();
			if (t) thinkingParts.push(t);
		} else if (block.type === "toolCall") {
			const args = JSON.stringify(block.arguments ?? {}, null, 2);
			toolParts.push(`🔧 **Tool: \`${block.name}\`**\n\n${fence(args, "json")}`);
		}
	}

	const blocks: string[] = [];
	const body = textParts.join("\n\n");
	if (body) blocks.push(body);
	if (thinkingParts.length > 0) {
		blocks.push(`<details>\n<summary>🧠 thinking</summary>\n\n${thinkingParts.join("\n\n")}\n\n</details>`);
	}
	if (includeTools && toolParts.length > 0) {
		blocks.push(toolParts.join("\n\n"));
	}
	return blocks.join("\n\n");
}

/** Convert one session entry to Markdown, or null when it should be skipped. */
function entryToMarkdown(
	entry: ReturnType<ExtensionCommandContext["sessionManager"]["buildContextEntries"]>[number],
	includeTools: boolean,
): string | null {
	const time = formatTime(entry.timestamp);

	switch (entry.type) {
		case "message": {
			const msg = entry.message;
			switch (msg.role) {
				case "user":
					return `## 👤 User — ${time}\n\n${renderContent(msg.content)}`;
				case "assistant": {
					const model = `${msg.provider}/${msg.model}`;
					const body = renderAssistant(msg, includeTools);
					if (!body) return null;
					const errorNote = msg.stopReason === "error" && msg.errorMessage
						? `\n\n> ⚠️ **Error**: ${msg.errorMessage}`
						: "";
					return `## 🤖 ${model} — ${time}\n\n${body}${errorNote}`;
				}
				case "toolResult":
					if (!includeTools) return null;
					return (
						`### 🔧 Result of \`${msg.toolName}\` — ${time}\n\n${fence(renderContent(msg.content))}` +
						(msg.isError ? `\n\n> ⚠️ **Tool error**` : "")
					);
				case "bashExecution":
					if (!includeTools) return null;
					return (
						`### 🖥 Bash (!) — ${time}\n\n$ ${msg.command}\n\n${fence(msg.output)}` +
						(msg.exitCode ? `\n\n> exit code: ${msg.exitCode}` : "")
					);
				case "custom":
					return `### 📦 Extension (${msg.customType}) — ${time}\n\n${renderContent(msg.content)}`;
				case "branchSummary":
					return `> 🔀 **Branch summary** — ${time}\n\n${msg.summary}`;
				case "compactionSummary":
					return `> 📚 **Compaction** — ${time}\n\n${msg.summary}`;
			}
			return null;
		}
		case "model_change":
			return `> **Model changed** → \`${entry.provider}/${entry.modelId}\` — ${time}`;
		case "thinking_level_change":
			return `> **Thinking level** → \`${entry.thinkingLevel}\` — ${time}`;
		case "compaction":
			return `> 📚 **Compaction** — ${time}\n\n${entry.summary}`;
		case "branch_summary":
			return `> 🔀 **Branch summary** — ${time}\n\n${entry.summary}`;
		case "custom_message":
			return `### 📦 Extension (${entry.customType}) — ${time}\n\n${renderContent(entry.content)}`;
		default:
			// custom / label / session_info — not part of the visible conversation
			return null;
	}
}

function buildMarkdown(
	ctx: ExtensionCommandContext,
	includeTools: boolean,
	tailCount?: number,
): { markdown: string; count: number; totalEntries: number } {
	let entries = ctx.sessionManager.buildContextEntries();
	const totalEntries = entries.length;
	if (tailCount != null && tailCount > 0 && entries.length > tailCount) {
		entries = entries.slice(-tailCount);
	}
	const header = ctx.sessionManager.getHeader();
	const sessionName = ctx.sessionManager.getSessionName();

	let title = sessionName;
	if (!title) {
		const firstUser = entries.find(
			(e): e is SessionMessageEntry & { message: { role: "user" } } =>
				e.type === "message" && e.message.role === "user",
		);
		const content = firstUser?.message.content;
		const text = typeof content === "string" ? content.trim() : "";
		title = (text || "Pi Session").slice(0, 60);
	}

	const blocks: string[] = [`# ${title}`, ""];
	blocks.push(`- **Session**: \`${header?.id ?? "ephemeral"}\``);
	blocks.push(`- **Working dir**: \`${header?.cwd ?? ctx.cwd}\``);
	if (header?.timestamp) blocks.push(`- **Started**: ${formatTime(header.timestamp)}`);
	if (tailCount != null && entries.length < totalEntries) {
		blocks.push(`- **Entries**: last ${entries.length} of ${totalEntries}`);
	} else {
		blocks.push(`- **Entries**: ${entries.length}`);
	}
	blocks.push("");

	let count = 0;
	for (const entry of entries) {
		const md = entryToMarkdown(entry, includeTools);
		if (md == null) continue;
		count++;
		blocks.push("---", "", md, "");
	}

	return { markdown: blocks.join("\n"), count, totalEntries };
}

// ── Diagnostics ──────────────────────────────────────────────────────────────

const OSC52_CHAIN_CHECKLIST = [
	"1. 本地终端需支持 OSC 52（iTerm2 / kitty / Alacritty / WezTerm / Windows Terminal；macOS Terminal.app 不支持）",
	"2. 本地 tmux：set -g set-clipboard on（tmux≥3.2 可加 set -ga terminal-features ',*:clipboard'）",
	"3. 远程 tmux：同样配置",
	"4. iTerm2 需勾选 Preferences→General→Selection→“Applications in terminal may access clipboard”，",
	"   并在 Preferences→Advanced 调大剪贴板大小上限",
];

function buildDiag(
	ctx: ExtensionCommandContext,
	markdown: string,
	count: number,
	totalEntries: number,
): string {
	const env = process.env;
	const lines: string[] = [];
	const base64Size = Buffer.byteLength(markdown, "utf8") * 4 / 3;
	lines.push(`会话: ${count} 条可见消息 / ${totalEntries} 条 entry`);
	lines.push(`Markdown: ${formatSize(Buffer.byteLength(markdown, "utf8"))} (base64 ${formatSize(base64Size)})`);
	lines.push(`OSC 52 大小限制: 无（本扩展不限制，直接发送）`);
	lines.push(`  客观限制: tmux 硬限 1MB base64（≈750KB 原文，超出会被 tmux 丢弃）；`);
	lines.push(`  Alacritty 实测支持 1MB+ 原文；iTerm2 默认 100KB 可在设置中调大`);
	lines.push(`  当前内容是否超 tmux 1MB 硬限: ${base64Size > 1_000_000 ? "是（tmux 会丢弃）" : "否"}`);
	lines.push(`平台: ${platform()}`);
	lines.push(`TERM: ${env.TERM ?? "-"}`);
	lines.push(`stdout isTTY: ${process.stdout.isTTY ? "是（直达终端）" : "否（被重定向，走 /dev/tty fallback）"}`);
	lines.push(`stderr isTTY: ${process.stderr.isTTY ? "是" : "否"}`);
	lines.push(`TMUX: ${env.TMUX ? "set（在 tmux 中）" : "unset"}`);
	lines.push(`SSH_CONNECTION: ${env.SSH_CONNECTION ? "set（SSH 远程会话）" : "unset"}`);
	lines.push(`SSH_CLIENT: ${env.SSH_CLIENT ?? "unset"}`);
	lines.push(`MOSH_CONNECTION: ${env.MOSH_CONNECTION ?? "unset"}`);
	lines.push(`DISPLAY: ${env.DISPLAY ?? "unset"}`);
	lines.push(`WAYLAND_DISPLAY: ${env.WAYLAND_DISPLAY ?? "unset"}`);
	lines.push(`是否判定远程: ${isRemoteSession() ? "是" : "否"}`);
	lines.push("");
	lines.push("OSC 52 链路检查：");
	lines.push(...OSC52_CHAIN_CHECKLIST);
	return lines.join("\n");
}

// ── Extension ────────────────────────────────────────────────────────────────

export default function (pi: ExtensionAPI) {
	pi.registerCommand("clipboard", {
		description: "Copy the session as Markdown to the clipboard (OSC 52 aware)",
		handler: async (args, ctx) => {
			const parts = args.trim().split(/\s+/).filter(Boolean);
			const forceOsc52 = parts.includes("osc52");
			const diag = parts.includes("diag");
			const includeTools = !parts.includes("--no-tools");

			const tailIdx = parts.indexOf("tail");
			const tailCount = tailIdx >= 0 ? Number.parseInt(parts[tailIdx + 1] ?? "", 10) : undefined;

			const { markdown, count, totalEntries } = buildMarkdown(ctx, includeTools, tailCount);
			if (count === 0) {
				ctx.ui.notify("会话为空，没有可复制的内容", "error");
				return;
			}

			const size = formatSize(Buffer.byteLength(markdown, "utf8"));

			if (parts.includes("test")) {
				const token = `pi-osc52-test-${Date.now().toString(36)}`;
				if (emitOsc52(token)) {
					ctx.ui.notify(`OSC 52 测试序列已发送。请切换到本地终端粘贴，应得到：${token}`, "info");
				} else {
					ctx.ui.notify("OSC 52 写入失败", "error");
				}
				return;
			}

			if (diag) {
				ctx.ui.notify(buildDiag(ctx, markdown, count, totalEntries), "info");
				return;
			}

			if (forceOsc52) {
				if (!emitOsc52(markdown)) {
					ctx.ui.notify(`OSC 52 写入失败（${size}）`, "error");
					return;
				}
				ctx.ui.notify(`已复制 ${count} 条消息（${size}）到剪贴板（OSC 52）`, "info");
				return;
			}

			// 远程会话：系统剪贴板只会写到远程机器（本地拿不到），OSC 52 才是穿透本地的关键。
			// 直接用无限制的 emitOsc52（内置 copyToClipboard 的 OSC 52 有 100KB 限制且会静默失败）。
			if (isRemoteSession()) {
				if (emitOsc52(markdown)) {
					ctx.ui.notify(`已复制 ${count} 条消息（${size}）到本地剪贴板（OSC 52）`, "info");
				} else {
					ctx.ui.notify(
						`OSC 52 写入失败（${size}）。可能超过 tmux 1MB base64 硬限（≈750KB 原文）或终端不支持。\n` +
							`可尝试：/clipboard osc52 tail 30、/clipboard osc52 --no-tools、/clipboard diag 查看大小`,
						"error",
					);
				}
				return;
			}

			try {
				await copyToClipboard(markdown);
				ctx.ui.notify(`已复制 ${count} 条消息（${size}）到剪贴板（system clipboard）`, "info");
			} catch {
				if (emitOsc52(markdown)) {
					ctx.ui.notify(`已复制 ${count} 条消息（${size}）到剪贴板（OSC 52 fallback）`, "info");
				} else {
					ctx.ui.notify(`复制失败：剪贴板工具不可用，OSC 52 写入也失败`, "error");
				}
			}
		},
	});
}
