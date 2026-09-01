/**
 * Model Usage Monitor — Pi Extension
 *
 * Shows balance / coding-plan quota for the currently selected provider in the
 * footer status area. The channel is chosen automatically from the active model:
 *
 * - deepseek            → GET /user/balance                        (CNY/USD balance)
 * - zai / zhipuai / bigmodel
 *   (coding plan)       → GET /api/monitor/usage/quota/limit       (5h / 7d quota %)
 * - moonshot / kimi     → GET /v1/users/me/balance                 (CNY/USD balance)
 *
 * Unsupported providers clear the status. Balance refreshes after each agent
 * turn; switching models invalidates the cache immediately.
 *
 * Derived from deepseek-usage.ts. Deploy to ~/.pi/agent/extensions/.
 */

import type { ExtensionAPI, ExtensionContext, Model } from "@earendil-works/pi-coding-agent";

// ── Constants ──────────────────────────────────────────────────────────────

const CACHE_TTL_MS = 30_000;
const STATUS_KEY = "model-usage";
const FETCH_TIMEOUT_MS = 10_000;

/** CNY balance thresholds for color coding */
const BALANCE_HIGH = 100; // green  (>= 100)
const BALANCE_LOW = 10; // red    (<= 10)
// yellow between 10 and 100

/** Quota usage percentage thresholds (higher usage = worse color) */
const QUOTA_WARN = 70; // yellow (>= 70)
const QUOTA_ERROR = 90; // red    (>= 90)

// ── Types ───────────────────────────────────────────────────────────────────

interface DeepSeekBalanceInfo {
  currency: string;
  total_balance: string;
}

interface DeepSeekBalanceResponse {
  is_available: boolean;
  balance_infos: DeepSeekBalanceInfo[];
}

interface ZaiLimitEntry {
  type: string;
  unit?: number;
  number?: number;
  percentage?: number;
}

interface ZaiQuotaResponse {
  code: number;
  success: boolean;
  msg?: string;
  data?: {
    limits?: ZaiLimitEntry[];
    level?: string;
  };
}

interface ZaiAccountReportResponse {
  code: number;
  success: boolean;
  msg?: string;
  data?: {
    balance?: number | null;
    availableBalance?: number | null;
    availableCreditBalance?: number | null;
    creditStatus?: string;
  };
}

interface MoonshotBalanceResponse {
  code: number;
  status?: boolean;
  data?: {
    available_balance: number;
    voucher_balance: number;
    cash_balance: number;
  };
}

/**
 * Parsed channel data. Either a monetary balance or coding-plan quota windows.
 */
type UsageInfo =
  | {
      kind: "balance";
      currency: string;
      amount: number;
      /** Credit-line accounts: used/total rendered instead of amount */
      used?: number;
      total?: number;
    }
  | { kind: "quota"; plan?: string; windows: { label: string; pct: number }[] };

interface Channel {
  /** Cache key */
  id: string;
  /** Human label shown before the value */
  label: string;
  matches(provider: string): boolean;
  fetch(ctx: ExtensionContext, model: Model): Promise<UsageInfo | null>;
}

// ── Helpers ─────────────────────────────────────────────────────────────────

function trimBase(url: string): string {
  return url.replace(/\/+$/, "");
}

function currencySymbol(currency: string): string {
  if (currency === "CNY") return "¥";
  if (currency === "USD") return "$";
  return `${currency} `;
}

/** Large CNY amounts (credit lines) render compactly as 万 */
function formatMoney(amount: number, currency: string): string {
  const symbol = currencySymbol(currency);
  if (amount >= 100_000) return `${symbol}${(amount / 10_000).toFixed(1)}万`;
  return `${symbol}${amount.toFixed(2)}`;
}

function balanceColor(amount: number): "error" | "warning" | "accent" {
  if (amount <= BALANCE_LOW) return "error";
  if (amount < BALANCE_HIGH) return "warning";
  return "accent";
}

function quotaColor(pct: number): "error" | "warning" | "accent" {
  if (pct >= QUOTA_ERROR) return "error";
  if (pct >= QUOTA_WARN) return "warning";
  return "accent";
}

/** Resolve the effective base URL: auth override, else provider config. */
function resolveBaseUrl(ctx: ExtensionContext, model: Model, authBaseUrl?: string): string | undefined {
  if (authBaseUrl) return trimBase(authBaseUrl);
  const provider = ctx.modelRegistry.getProvider(model.provider);
  return provider?.baseUrl ? trimBase(provider.baseUrl) : undefined;
}

// ── Channels ────────────────────────────────────────────────────────────────

const deepseekChannel: Channel = {
  id: "deepseek",
  label: "DeepSeek",
  matches: (provider) => provider.toLowerCase().includes("deepseek"),

  async fetch(ctx, model) {
    const auth = await ctx.modelRegistry.getApiKeyAndHeaders(model);
    if (!auth.ok || !auth.apiKey) return null;

    try {
      // Balance endpoint lives at the API root, not under /v1
      const base = resolveBaseUrl(ctx, model, auth.baseUrl) ?? "https://api.deepseek.com";
      const resp = await fetch(`${base}/user/balance`, {
        headers: {
          "Authorization": `Bearer ${auth.apiKey}`,
          "Accept-Encoding": "identity",
          ...auth.headers,
        },
        signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
      });

      if (!resp.ok) return null;

      const data = (await resp.json()) as DeepSeekBalanceResponse;
      if (!data.is_available || data.balance_infos.length === 0) return null;

      // Prefer CNY, fall back to first available
      const target = data.balance_infos.find((b) => b.currency === "CNY") ?? data.balance_infos[0];
      return { kind: "balance", currency: target.currency, amount: parseFloat(target.total_balance) };
    } catch {
      return null;
    }
  },
};

const zaiChannel: Channel = {
  id: "zai",
  label: "zai",
  matches: (provider) => /zai|zhipu|bigmodel/i.test(provider),

  async fetch(ctx, model) {
    const auth = await ctx.modelRegistry.getApiKeyAndHeaders(model);
    if (!auth.ok || !auth.apiKey) return null;

    // Coding-plan monitor host: CN (bigmodel.cn) vs global (api.z.ai)
    const base = resolveBaseUrl(ctx, model, auth.baseUrl) ?? "";
    const isCn = base.includes("bigmodel.cn") || /cn|zhipu|bigmodel/i.test(model.provider);
    const host = isCn ? "https://open.bigmodel.cn" : "https://api.z.ai";

    // Coding-plan accounts: quota windows. Non-plan (prepaid) CN accounts:
    // account balance report. Try quota first, fall back to balance.
    const quota = await fetchZaiQuota(host, auth);
    if (quota) return quota;

    if (isCn) {
      const report = await fetchZaiBalanceCn(auth);
      if (report) {
        const availableCredit = report.creditTotal - report.creditUsed;
        const amount = report.cash + Math.max(availableCredit, 0);
        return report.creditTotal > 0
          ? { kind: "balance", currency: "CNY", amount, used: report.creditUsed, total: report.creditTotal }
          : { kind: "balance", currency: "CNY", amount };
      }
    }

    return null;
  },
};

interface ZaiAuth {
  apiKey: string;
  headers?: Record<string, string>;
}

async function zaiFetchJson<T>(url: string, auth: ZaiAuth): Promise<{ status: number; body: T | null } | null> {
  try {
    // The dashboard XHR sends the raw key without a Bearer prefix;
    // retry with Bearer for endpoints that require it.
    let resp = await fetch(url, {
      headers: { "Authorization": auth.apiKey, ...auth.headers },
      signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
    });
    if (resp.status === 401 || resp.status === 403) {
      resp = await fetch(url, {
        headers: { "Authorization": `Bearer ${auth.apiKey}`, ...auth.headers },
        signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
      });
    }
    const raw = await resp.text();
    let body: T | null = null;
    try {
      body = JSON.parse(raw) as T;
    } catch {
      // non-JSON body
    }
    return { status: resp.status, body };
  } catch {
    return null;
  }
}

async function fetchZaiQuota(host: string, auth: ZaiAuth): Promise<UsageInfo | null> {
  const resp = await zaiFetchJson<ZaiQuotaResponse>(`${host}/api/monitor/usage/quota/limit`, auth);
  if (!resp || resp.status !== 200 || !resp.body?.success || !resp.body.data?.limits) return null;

  // unit 3 = 5h rolling window (session), unit 6 = 7d (weekly)
  const windows: { label: string; pct: number }[] = [];
  for (const limit of resp.body.data.limits) {
    if (limit.type !== "TOKENS_LIMIT" || typeof limit.percentage !== "number") continue;
    if (limit.unit === 3) windows.push({ label: "5h", pct: limit.percentage });
    else if (limit.unit === 6) windows.push({ label: "7d", pct: limit.percentage });
  }
  windows.sort((a, b) => (a.label === "5h" ? -1 : b.label === "5h" ? 1 : 0));
  if (windows.length === 0) return null;

  return { kind: "quota", plan: resp.body.data.level, windows };
}

/**
 * CN prepaid/resource-package accounts have no coding plan (the quota API
 * reports an error for them). The console account-report API returns the
 * balance instead. Undocumented endpoint, verified against live console.
 */
interface ZaiBalanceReport {
  cash: number;
  creditUsed: number;
  creditTotal: number;
}

async function fetchZaiBalanceCn(auth: ZaiAuth): Promise<ZaiBalanceReport | null> {
  const url = "https://www.bigmodel.cn/api/biz/account/query-customer-account-report";
  const resp = await zaiFetchJson<ZaiAccountReportResponse>(url, auth);
  if (!resp || resp.status !== 200 || !resp.body?.success || !resp.body.data) return null;

  const d = resp.body.data;
  const num = (v: number | null | undefined): number => (typeof v === "number" && Number.isFinite(v) ? v : 0);
  const cash = num(d.availableBalance ?? d.balance);
  const creditEnabled = d.creditStatus === "ENABLE";
  const creditTotal = creditEnabled ? num(d.creditBalance) : 0;
  const availableCredit = creditEnabled ? num(d.availableCreditBalance) : 0;
  return { cash, creditTotal, creditUsed: Math.max(creditTotal - availableCredit, 0) };
}

const moonshotChannel: Channel = {
  id: "moonshot",
  label: "Kimi",
  matches: (provider) => /moonshot|kimi/i.test(provider),

  async fetch(ctx, model) {
    const auth = await ctx.modelRegistry.getApiKeyAndHeaders(model);
    if (!auth.ok || !auth.apiKey) return null;

    try {
      const base = resolveBaseUrl(ctx, model, auth.baseUrl) ?? "https://api.moonshot.cn/v1";
      // International accounts (.ai) bill in USD, CN (.cn) in CNY
      const currency = base.includes("moonshot.ai") ? "USD" : "CNY";
      const resp = await fetch(`${base}/users/me/balance`, {
        headers: {
          "Authorization": `Bearer ${auth.apiKey}`,
          ...auth.headers,
        },
        signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
      });

      if (!resp.ok) return null;

      const data = (await resp.json()) as MoonshotBalanceResponse;
      if (data.code !== 0 || !data.data) return null;
      return { kind: "balance", currency, amount: data.data.available_balance };
    } catch {
      return null;
    }
  },
};

const CHANNELS: Channel[] = [deepseekChannel, zaiChannel, moonshotChannel];

// ── Rendering ───────────────────────────────────────────────────────────────

function renderInfo(info: UsageInfo, label: string, ctx: ExtensionContext): string {
  const theme = ctx.ui.theme;
  const head = theme.fg("muted", `${label}:`);

  if (info.kind === "balance") {
    if (info.used !== undefined && info.total !== undefined) {
      // Credit-line account: render as used/total, colored by usage ratio
      const pct = info.total > 0 ? (info.used / info.total) * 100 : 0;
      const value = `已用${formatMoney(info.used, info.currency)}/${formatMoney(info.total, info.currency)}`;
      return head + theme.fg(quotaColor(pct), value);
    }
    const value = formatMoney(info.amount, info.currency);
    return head + theme.fg(balanceColor(info.amount), value);
  }

  const plan = info.plan ? `(${info.plan})` : "";
  const parts = info.windows.map((w) => theme.fg(quotaColor(w.pct), `${w.label} ${Math.round(w.pct)}%`));
  const body = parts.length > 0 ? parts.join(" ") : theme.fg("dim", "n/a");
  return head + theme.fg("muted", plan) + body;
}

// ── State ───────────────────────────────────────────────────────────────────

interface CacheEntry {
  data: UsageInfo;
  time: number;
}

const cache = new Map<string, CacheEntry>();
const inFlight = new Map<string, Promise<UsageInfo | null>>();

// ── UI update ───────────────────────────────────────────────────────────────

async function updateFooter(ctx: ExtensionContext, model?: Model): Promise<void> {
  const active = model ?? ctx.model;
  if (!active) {
    ctx.ui.setStatus(STATUS_KEY, undefined);
    return;
  }

  const channel = CHANNELS.find((c) => c.matches(active.provider));
  if (!channel) {
    ctx.ui.setStatus(STATUS_KEY, undefined);
    return;
  }

  let info: UsageInfo | null;
  const cached = cache.get(channel.id);
  if (cached && Date.now() - cached.time < CACHE_TTL_MS) {
    info = cached.data;
  } else {
    // Deduplicate concurrent fetches for the same channel
    let pending = inFlight.get(channel.id);
    if (!pending) {
      pending = channel.fetch(ctx, active).finally(() => inFlight.delete(channel.id));
      inFlight.set(channel.id, pending);
    }
    info = await pending;
    if (info) cache.set(channel.id, { data: info, time: Date.now() });
  }

  ctx.ui.setStatus(STATUS_KEY, info ? renderInfo(info, channel.label, ctx) : undefined);
}

function invalidateCache(): void {
  cache.clear();
}

// ── Extension ───────────────────────────────────────────────────────────────

export default function (pi: ExtensionAPI) {
  // Initialize on session start — handles fresh start and session restore.
  pi.on("session_start", async (_event, ctx) => {
    await updateFooter(ctx);
  });

  // React to model changes — invalidate and refresh on switch
  pi.on("model_select", async (event, ctx) => {
    invalidateCache();
    await updateFooter(ctx, event.model as Model);
  });

  // Refresh after each agent turn
  pi.on("agent_end", async (_event, ctx) => {
    await updateFooter(ctx);
  });
}
