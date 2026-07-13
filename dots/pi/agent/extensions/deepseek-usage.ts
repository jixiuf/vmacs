/**
 * DeepSeek Usage Monitor — Pi Extension
 *
 * Monitors DeepSeek API balance and displays it in the footer.
 * Always shows CNY (RMB) balance with ¥ symbol.
 *
 * Usage: drop this file into ~/.pi/agent/extensions/
 */

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

// ── Constants ──────────────────────────────────────────────────────────────

const BALANCE_API = "https://api.deepseek.com/user/balance";
const CACHE_TTL_MS = 30_000;
const STATUS_KEY = "deepseek-usage";

/** RMB thresholds for color coding */
const THRESHOLD_HIGH = 100; // green  (>= 100)
const THRESHOLD_LOW = 10; // red    (<= 10)
// yellow between 10 and 100

// ── Types ───────────────────────────────────────────────────────────────────

interface BalanceInfo {
  currency: string;
  total_balance: string;
  granted_balance: string;
  topped_up_balance: string;
}

interface BalanceResponse {
  is_available: boolean;
  balance_infos: BalanceInfo[];
}

interface ParsedBalance {
  currency: string;
  totalBalance: number;
}

// ── State ───────────────────────────────────────────────────────────────────

let cachedBalance: ParsedBalance | null = null;
let lastFetchTime = 0;

// ── Helpers ─────────────────────────────────────────────────────────────────

function isDeepSeekProvider(provider: string): boolean {
  return provider.toLowerCase() === "deepseek";
}

/**
 * Resolve the preferred balance: CNY first, fallback to first available.
 * This differs from the upstream pi-deepseek-usage which prefers USD.
 */
function resolveBalance(balances: BalanceInfo[]): ParsedBalance | null {
  if (balances.length === 0) return null;
  const cny = balances.find((b) => b.currency === "CNY");
  const target = cny ?? balances[0];
  return {
    currency: target.currency,
    totalBalance: parseFloat(target.total_balance),
  };
}

function currencySymbol(currency: string): string {
  if (currency === "CNY") return "¥";
  if (currency === "USD") return "$";
  return `${currency} `;
}

function formatMoney(amount: number, currency: string): string {
  const symbol = currencySymbol(currency);
  return `${symbol}${amount.toFixed(2)}`;
}

/**
 * Determine color based on RMB balance thresholds.
 * Treats non-CNY balances as-is for color (fallback).
 */
function balanceColor(balance: ParsedBalance, ctx: ExtensionContext): string {
  const amount = balance.totalBalance;
  if (amount <= THRESHOLD_LOW) return "error";
  if (amount < THRESHOLD_HIGH) return "warning";
  return "accent";
}

function renderStatus(balance: ParsedBalance, ctx: ExtensionContext): string {
  const colored = ctx.ui.theme.fg(balanceColor(balance, ctx), formatMoney(balance.totalBalance, balance.currency));
  return ctx.ui.theme.fg("muted", "DeepSeek:") + colored;
}

// ── API call ────────────────────────────────────────────────────────────────

async function fetchBalance(ctx: ExtensionContext): Promise<ParsedBalance | null> {
  const model = ctx.modelRegistry.find("deepseek", "deepseek-v4-flash")
    ?? ctx.modelRegistry.find("deepseek", "deepseek-v4-pro")
    ?? ctx.modelRegistry.find("deepseek", "deepseek-v4");

  if (!model) return null;

  const auth = await ctx.modelRegistry.getApiKeyAndHeaders(model);
  if (!auth.ok || !auth.apiKey) return null;

  try {
    const resp = await fetch(BALANCE_API, {
      headers: {
        "Authorization": `Bearer ${auth.apiKey}`,
        "Accept-Encoding": "identity",
        ...auth.headers,
      },
      signal: AbortSignal.timeout(10_000),
    });

    if (!resp.ok) return null;

    const data = (await resp.json()) as BalanceResponse;
    if (!data.is_available) return null;

    return resolveBalance(data.balance_infos);
  } catch {
    return null;
  }
}

async function getBalance(ctx: ExtensionContext): Promise<ParsedBalance | null> {
  const now = Date.now();
  if (cachedBalance && now - lastFetchTime < CACHE_TTL_MS) {
    return cachedBalance;
  }

  const balance = await fetchBalance(ctx);
  if (balance) {
    cachedBalance = balance;
    lastFetchTime = now;
  }
  return balance;
}

// ── UI update ───────────────────────────────────────────────────────────────

async function updateFooter(ctx: ExtensionContext): Promise<void> {
  const balance = await getBalance(ctx);
  if (balance) {
    ctx.ui.setStatus(STATUS_KEY, renderStatus(balance, ctx));
  } else {
    ctx.ui.setStatus(STATUS_KEY, undefined);
  }
}

// ── Extension ───────────────────────────────────────────────────────────────

export default function (pi: ExtensionAPI) {
  // Initialize on session start — handles fresh start and session restore.
  // This ensures the footer is populated even if model_select fired before
  // the extension was registered.
  pi.on("session_start", async (_event, ctx) => {
    await updateFooter(ctx);
  });

  // React to model changes — invalidate and refresh on switch
  pi.on("model_select", async (event, ctx) => {
    if (isDeepSeekProvider(event.model.provider)) {
      cachedBalance = null;
      lastFetchTime = 0;
      await updateFooter(ctx);
    } else {
      ctx.ui.setStatus(STATUS_KEY, undefined);
    }
  });

  // Refresh balance after each agent turn
  pi.on("agent_end", async (_event, ctx) => {
    await updateFooter(ctx);
  });
}
