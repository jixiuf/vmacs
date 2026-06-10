/**
 * ask-user-question-rpc — pi.el-compatible structured questionnaire.
 *
 * Same tool schema as @juicesharp/rpiv-ask-user-question but uses only the
 * dialog API (ctx.ui.select / ctx.ui.input) so it works in both TUI mode
 * and RPC mode (Emacs pi.el).
 *
 * ⚠ CONFLICT NOTE: If @juicesharp/rpiv-ask-user-question is also installed,
 * both register the same "ask_user_question" tool. Only one will succeed.
 * For pi.el use, uninstall the TUI version:
 *   pi uninstall npm:@juicesharp/rpiv-ask-user-question
 *
 * Limitations vs the full TUI version:
 * - No side-by-side preview pane (previews are echoed inline in the select title)
 * - No tab bar — questions are asked sequentially
 * - No per-option notes
 * - No Submit review tab
 * - "Type something." and "Chat about this" are appended as regular options
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

// ── Schema (same as original) ──────────────────────────────────────────────

const MAX_QUESTIONS = 4;
const MIN_OPTIONS = 2;
const MAX_OPTIONS = 4;
const MAX_HEADER_LENGTH = 16;
const MAX_LABEL_LENGTH = 60;

const RESERVED_LABELS = [
  "Other",
  "Type something.",
  "Chat about this",
  "Next →",
] as const;

const OptionSchema = Type.Object({
  label: Type.String({
    maxLength: MAX_LABEL_LENGTH,
    description: `MAX ${MAX_LABEL_LENGTH} CHARACTERS — hard limit. The display text for this option. Should be concise (1-5 words).`,
  }),
  description: Type.String({
    description: "Explanation of what this option means or its trade-offs.",
  }),
  preview: Type.Optional(
    Type.String({
      description: "Optional preview content (markdown). In RPC mode, echoed inline in the select prompt.",
    }),
  ),
});

const QuestionSchema = Type.Object({
  question: Type.String({
    description: 'The complete question. Should end with a question mark. Example: "Which library should we use?"',
  }),
  header: Type.String({
    maxLength: MAX_HEADER_LENGTH,
    description: `MAX ${MAX_HEADER_LENGTH} CHARACTERS — short chip/tag. Examples: "Library", "Approach".`,
  }),
  options: Type.Array(OptionSchema, {
    minItems: MIN_OPTIONS,
    maxItems: MAX_OPTIONS,
    description: "2-4 options. The 'Type something.' and 'Chat about this' rows are appended automatically.",
  }),
  multiSelect: Type.Optional(
    Type.Boolean({
      default: false,
      description: "Set to true to allow multiple answers.",
    }),
  ),
});

const QuestionParamsSchema = Type.Object({
  questions: Type.Array(QuestionSchema, {
    minItems: 1,
    maxItems: MAX_QUESTIONS,
    description: "1-4 questions.",
  }),
});

type QuestionAnswer = {
  questionIndex: number;
  question: string;
  kind: "option" | "custom" | "chat" | "multi";
  answer: string | null;
  selected?: string[];
  preview?: string;
};

type QuestionnaireResult = {
  answers: QuestionAnswer[];
  cancelled: boolean;
  error?: string;
};

// ── Validation ─────────────────────────────────────────────────────────────

function validateQuestionnaire(params: {
  questions: Array<{
    question: string;
    header: string;
    options: Array<{ label: string }>;
    multiSelect?: boolean;
  }>;
}): { ok: true } | { ok: false; error: string } {
  if (!params.questions || params.questions.length === 0) {
    return { ok: false, error: "At least one question is required" };
  }
  if (params.questions.length > MAX_QUESTIONS) {
    return { ok: false, error: `At most ${MAX_QUESTIONS} questions allowed` };
  }

  const seenQuestions = new Set<string>();
  for (const q of params.questions) {
    if (seenQuestions.has(q.question)) {
      return { ok: false, error: "Question text must be unique" };
    }
    seenQuestions.add(q.question);

    if (q.options.length < MIN_OPTIONS) {
      return { ok: false, error: `Each question needs at least ${MIN_OPTIONS} options` };
    }
    if (q.options.length > MAX_OPTIONS) {
      return { ok: false, error: `Each question can have at most ${MAX_OPTIONS} options` };
    }

    const seenLabels = new Set<string>();
    for (const o of q.options) {
      if ((RESERVED_LABELS as readonly string[]).includes(o.label)) {
        return { ok: false, error: `Option label "${o.label}" is reserved` };
      }
      if (seenLabels.has(o.label)) {
        return { ok: false, error: "Option labels must be unique within a question" };
      }
      seenLabels.add(o.label);
    }
  }

  return { ok: true };
}

// ── Response formatting (same envelope shape) ───────────────────────────────

const DECLINE_MESSAGE = "User declined to answer questions";
const ENVELOPE_PREFIX = "User has answered your questions:";
const ENVELOPE_SUFFIX = "You can now continue with the user's answers in mind.";

function buildToolResult(text: string, details: QuestionnaireResult) {
  return {
    content: [{ type: "text" as const, text }],
    details,
  };
}

function buildQuestionnaireResponse(result: QuestionnaireResult | null, params: { questions: unknown[] }) {
  if (!result || result.cancelled) {
    return buildToolResult(DECLINE_MESSAGE, {
      answers: result?.answers ?? [],
      cancelled: true,
    });
  }

  const segments: string[] = [];
  for (let i = 0; i < params.questions.length; i++) {
    const a = result.answers.find((x) => x.questionIndex === i);
    if (a) {
      const parts: string[] = [`"${a.question}"="${formatAnswer(a)}"`];
      if (a.preview) parts.push(`selected preview: ${a.preview}`);
      segments.push(parts.join(". ") + ".");
    }
  }

  if (segments.length === 0) {
    return buildToolResult(DECLINE_MESSAGE, { answers: result.answers, cancelled: true });
  }

  return buildToolResult(
    `${ENVELOPE_PREFIX} ${segments.join(" ")} ${ENVELOPE_SUFFIX}`,
    result,
  );
}

function formatAnswer(a: QuestionAnswer): string {
  switch (a.kind) {
    case "chat":
      return "User wants to chat about this. Continue the conversation to help them decide.";
    case "multi":
      return a.selected && a.selected.length > 0 ? a.selected.join(", ") : "(no input)";
    case "custom":
      return a.answer && a.answer.length > 0 ? a.answer : "(no input)";
    case "option":
      return a.answer ?? "(no input)";
  }
}

// ── Dialog-API-based questionnaire UI ───────────────────────────────────────

const SENTINEL_CUSTOM = "✏️ Type something…";
const SENTINEL_CHAT = "💬 Chat about this";
const SENTINEL_DONE = "✅ Done selecting";

async function askSingleSelectQuestion(
  ctx: { ui: { select: (title: string, options: string[]) => Promise<string | undefined>; input: (title: string, placeholder?: string) => Promise<string | undefined> } },
  q: { question: string; header: string; options: Array<{ label: string; description: string; preview?: string }> },
  questionIndex: number,
): Promise<QuestionAnswer> {
  // Build select options: the real options + sentinels
  const optionLabels = q.options.map((o) => {
    const label = o.label;
    const desc = o.description ? ` — ${o.description}` : "";
    const previewHint = o.preview ? " [has preview]" : "";
    return `${label}${previewHint}${desc}`;
  });

  // Determine if we should add the "Type something." sentinel.
  // Original: suppressed when any option has a preview (no room in side-by-side layout).
  const hasAnyPreview = q.options.some((o) => o.preview && o.preview.length > 0);
  const sentinelOptions = hasAnyPreview
    ? [SENTINEL_CHAT]
    : [SENTINEL_CUSTOM, SENTINEL_CHAT];

  const allDisplayOptions = [...optionLabels, ...sentinelOptions];

  const title = `${q.header}: ${q.question}`;
  const choice = await ctx.ui.select(title, allDisplayOptions);

  if (!choice) {
    // User cancelled (Esc)
    return { questionIndex, question: q.question, kind: "chat", answer: null };
  }

  // Check sentinels
  if (choice === SENTINEL_CUSTOM) {
    const customText = await ctx.ui.input(q.question, "Type your answer…");
    if (!customText || customText.length === 0) {
      return { questionIndex, question: q.question, kind: "chat", answer: null };
    }
    return { questionIndex, question: q.question, kind: "custom", answer: customText };
  }

  if (choice === SENTINEL_CHAT) {
    return { questionIndex, question: q.question, kind: "chat", answer: null };
  }

  // Find the matching option by checking prefixes
  for (let i = 0; i < q.options.length; i++) {
    if (choice.startsWith(q.options[i].label)) {
      const opt = q.options[i];
      return {
        questionIndex,
        question: q.question,
        kind: "option",
        answer: opt.label,
        preview: opt.preview,
      };
    }
  }

  // Fallback — treat as option
  return {
    questionIndex,
    question: q.question,
    kind: "option",
    answer: choice,
  };
}

async function askMultiSelectQuestion(
  ctx: { ui: { select: (title: string, options: string[]) => Promise<string | undefined>; input: (title: string, placeholder?: string) => Promise<string | undefined> } },
  q: { question: string; header: string; options: Array<{ label: string; description: string }> },
  questionIndex: number,
): Promise<QuestionAnswer> {
  const selected: string[] = [];
  let available = q.options.map((o) => o.label);

  while (available.length > 0) {
    const currentOptions: string[] = [...available];
    if (selected.length > 0) {
      currentOptions.push(SENTINEL_DONE);
    }
    currentOptions.push(SENTINEL_CHAT);

    const title = selected.length > 0
      ? `${q.header}: ${q.question} [selected: ${selected.join(", ")}]`
      : `${q.header}: ${q.question} (multi-select)`;

    const choice = await ctx.ui.select(title, currentOptions);

    if (!choice) {
      // Cancelled — return what we have or chat
      if (selected.length > 0) {
        return { questionIndex, question: q.question, kind: "multi", answer: null, selected };
      }
      return { questionIndex, question: q.question, kind: "chat", answer: null };
    }

    if (choice === SENTINEL_CHAT) {
      return { questionIndex, question: q.question, kind: "chat", answer: null };
    }

    if (choice === SENTINEL_DONE) {
      if (selected.length > 0) {
        return { questionIndex, question: q.question, kind: "multi", answer: null, selected };
      }
      continue; // shouldn't happen, but guard
    }

    // Toggle: add to selected, remove from available
    selected.push(choice);
    available = available.filter((l) => l !== choice);
  }

  // All options selected
  return { questionIndex, question: q.question, kind: "multi", answer: null, selected };
}

async function askQuestion(
  ctx: { ui: { select: (title: string, options: string[]) => Promise<string | undefined>; input: (title: string, placeholder?: string) => Promise<string | undefined> } },
  q: { question: string; header: string; options: Array<{ label: string; description: string; preview?: string }>; multiSelect?: boolean },
  questionIndex: number,
): Promise<QuestionAnswer> {
  if (q.multiSelect) {
    return askMultiSelectQuestion(ctx, q, questionIndex);
  }
  return askSingleSelectQuestion(ctx, q, questionIndex);
}

// ── Tool prompt guidelines (same as original) ──────────────────────────────

const PROMPT_SNIPPET = `Ask the user up to ${MAX_QUESTIONS} structured questions (${MIN_OPTIONS}-${MAX_OPTIONS} options each) when requirements are ambiguous`;

const PROMPT_GUIDELINES = [
  `Use ask_user_question whenever the user's request is underspecified and you cannot proceed without concrete decisions — you can ask up to ${MAX_QUESTIONS} questions per invocation.`,
  `Each question MUST have ${MIN_OPTIONS}-${MAX_OPTIONS} options. Every option requires a concise label (1-5 words) and a description explaining what the choice means or its trade-offs. The user can additionally type a custom answer (the "Type something." row is appended automatically to single-select questions) or pick "Chat about this" to abandon the questionnaire.`,
  `Set multiSelect: true when multiple answers are valid; this suppresses the "Type something." row. Provide an options[].preview markdown string when an option benefits from richer side-by-side context (mockups, code snippets, diagrams, configs) — single-select only. NOTE: any non-empty preview on a single-select question ALSO suppresses the "Type something." row (no room in the side-by-side layout); "Chat about this" remains the escape hatch. If you recommend a specific option, make it the first option and append "(Recommended)" to its label.`,
  "Do not stack multiple ask_user_question calls back-to-back — group all clarifying questions into one invocation.",
];

// ── Extension entry point ──────────────────────────────────────────────────

export default function (pi: ExtensionAPI) {
  // Startup notification — visible in both TUI and RPC mode
  pi.on("session_start", async (_event, ctx) => {
    ctx.ui.setStatus("ask-user-question", "✅ ask_user_question ready");
    ctx.ui.notify("ask_user_question tool loaded — I can ask clarifying questions!", "info");
  });

  pi.registerTool({
    name: "ask_user_question",
    label: "Ask User Question",
    description: `Ask the user one or more structured questions during execution. Use when you need to:
1. Gather user preferences or requirements
2. Clarify ambiguous instructions
3. Get decisions on implementation choices as you work
4. Offer choices to the user about what direction to take

Usage notes:
- Users will always be able to type a custom answer (the "Type something." row is appended automatically to every single-select question) or pick "Chat about this" to abandon the questionnaire.
- Use multiSelect: true to allow multiple answers to be selected for a question. The "Type something." row is suppressed on multi-select questions.
- Previews are echoed inline in the select title in RPC mode (no side-by-side layout).`,
    promptSnippet: PROMPT_SNIPPET,
    promptGuidelines: PROMPT_GUIDELINES,
    parameters: QuestionParamsSchema,

    async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
      const typed = params as {
        questions: Array<{
          question: string;
          header: string;
          options: Array<{ label: string; description: string; preview?: string }>;
          multiSelect?: boolean;
        }>;
      };

      if (!ctx.hasUI) {
        return buildToolResult("Error: UI not available (running in non-interactive mode)", {
          answers: [],
          cancelled: true,
          error: "no_ui",
        });
      }

      const validation = validateQuestionnaire(typed);
      if (!validation.ok) {
        return buildToolResult(validation.error, {
          answers: [],
          cancelled: true,
          error: validation.error,
        });
      }

      const answers: QuestionAnswer[] = [];
      let cancelled = false;

      // Ask questions sequentially
      for (let i = 0; i < typed.questions.length; i++) {
        const answer = await askQuestion(ctx, typed.questions[i], i);

        if (answer.kind === "chat") {
          // User wants to chat — stop asking further questions, return what we have
          answers.push(answer);
          cancelled = true;
          break;
        }

        answers.push(answer);
      }

      return buildQuestionnaireResponse(
        { answers, cancelled },
        typed,
      );
    },
  });
}
