# Rime 引擎架构

> 源码依据：`~/repos/librime/src/rime/`（1.17.0 fork）。本文描述一次按键从进入到
> 上屏的完整数据流，以及支撑它的组件模型。读码前先看本文。

## 1. 组件模型（一切的基础）

Rime 的核心是**组件注册表 + Ticket**模式，schema 中的每一行配置最终都通过
"klass@namespace" 映射到一个 C++ 组件实例。

- `registry.h/.cc`：全局单例 `Registry`，按字符串名字注册组件类。
  各模块初始化时把自己提供的组件登记进去（见 `gears_module.cc`、`core_module.cc`）。
- `ticket.h/.cc`：`Ticket{engine, klass, name_space}`。**klass** 是组件类名
  （如 `table_translator`），**name_space** 是配置命名空间（如 `wubi_py_wubi`）。
- 组件工厂签名形如 `Class<T, Ticket&>`，`Create(ticket)` 内部用
  `ticket.schema->config()` 读取 **`<name_space>/` 前缀下**的配置键。

**关键推论（本项目踩过坑）**：`engine.translators: [table_translator@wubi]` 中，
`wubi` 就是 name_space；该 translator 实例只会读 `wubi:/dictionary`、
`wubi:/prism`、`wubi:/enable_sentence` 这些键。配置块键与 @name_space 不一致 →
读不到配置 → 静默回退到默认行为（用 schema id 作词典名）。排查 translator 异常
时第一个要核对的就是这一点。

模块注册的核心组件一览（`gears_module.cc`，已核对源码）：

- processors: `ascii_composer` `chord_composer` `express_editor` `fluid_editor`
  `key_binder` `navigator` `punctuator` `recognizer` `selector` `speller`
  `shape_processor` `streaming_chord_processor`
- segmentors: `abc_segmentor` `affix_segmentor` `ascii_segmentor` `matcher`
  `punct_segmentor` `fallback_segmentor`
- translators: `echo_translator` `punct_translator` `table_translator`
  `script_translator`(别名 `r10n_translator`) `reverse_lookup_translator`
  `schema_list_translator` `switch_translator` `history_translator`
- filters: `simplifier` `uniquifier` `charset_filter`(别名 `cjk_minifier`)
  `reverse_lookup_filter` `single_char_filter`
- formatters: `shape_formatter`

自定义词典组件 `Dictionary` 由 `DictionaryComponent` 提供，读取
`<name_space>/dictionary`、`<name_space>/prism`（默认与 dictionary 同名）、
`<name_space>/packs`（可选附加表）。

## 2. 一次按键的生命周期

`ConcreteEngine::ProcessKey`（`engine.cc`）：

```
KeyEvent
  → processors_ 逐个 ProcessKeyEvent
      kAccepted → 结束（键已被消化）
      kRejected → 直接跳出（交给后续 post_processors / 上抛）
      kNoop     → 继续下一个 processor
  → 未消化的键推入 context->commit_history()
  → post_processors_ 再走一轮
  → 仍未消化则上抛给前端（Squirrel/weasel 等）
```

典型 processor 分工：
- `speller`：把普通字母键追加到 `context->input()`（组合输入串），这是"打字"的入口。
- `punctuator`：处理标点键（查 punctuation.yaml 映射表）。
- `key_binder`：按键重绑定（如 Caps Lock、分号选重）。
- `selector` / `navigator`：数字选重、翻页、前后移动光标。
- `ascii_composer`：中英切换状态机（temporary/inline ASCII 模式）。
- `express_editor`/`fluid_editor`：决定回车/空格何时上屏（整句 vs 即时）。

`speller` 追加字符后会触发 `context->update_notifier()` → `Engine::Compose()`。

## 3. Compose：切分与翻译

`Engine::Compose(ctx)`（engine.cc，已核对）：

```
CalculateSegmentation(&comp)   // 1. 切分
TranslateSegments(&comp)       // 2. 翻译
```

### 3.1 切分 CalculateSegmentation

- 输入串 `input = ctx->input()`，只处理光标 `caret_pos` 之前的部分；
  **光标之后最多保留一个 segment**。
- 对当前未确认区域，**依次调用各 segmentor 的 `Proceed(segments)`**，
  返回 false 则中断链。segmentor 在 `Segmentation` 上添加 vertex（切分点）
  并给新 segment 打 tag。
- 常用 segmentor：
  - `ascii_segmentor`：ASCII 模式下整段标记 raw input。
  - `abc_segmentor`：把字母串切成标准 segment，tag `abc`（可配置 `extra_tags`）。
    translator 只处理 tag 匹配的 segment（`xlit_tag`，即 translator 的 `tag:` 配置，
    默认 `abc`）。
  - `matcher`：按 `recognizer/patterns` 给特定前缀（如 大写开头、反查 `` ` ``、
    自定义 tag 如 `/`）打独立 tag 的 segment。
  - `affix_segmentor`：处理带前后缀的反查段（tag + 前缀/后缀剥离）。
  - `punct_segmentor`/`fallback_segmentor`：标点与兜底段。

### 3.2 翻译 TranslateSegments

对每个 `status < kGuess` 的 segment：

```
for translator in translators_:          # 顺序即优先级无关，见下
    translation = translator->Query(input, segment)
    if translation && !translation->exhausted():
        menu->AddTranslation(translation)
for filter in filters_:                  # filter 可声明 AppliesToSegment
    if filter->AppliesToSegment(&segment):
        menu->AddFilter(filter.get())
segment.status = kGuess
```

- **多个 translator 的候选会并入同一个 Menu**；Menu 内部按候选的 quality 排序
  （`Menu::AddTranslation` 把各 translation 串成流水，逐个 peek 比较）。
  同权下先来的 translator 先出。因此"简码置顶"这类需求靠**候选权重或
  translator 顺序 + 权重**实现，而不是简单谁在前谁就在前。
- Filter（`filter.h`）：包装 translation 流，可重排/去重/改 comment
  （如 `simplifier` 繁转简、`uniquifier` 去重、`single_char_filter` 过滤单字）。

### 3.3 选中与上屏 OnSelect / OnCommit

- 用户选重 → `OnSelect`：当前 segment `Close()`；若 segment 到达输入串末尾且
  `_auto_commit` 开 → 立即 `Commit()`，否则 `Forward()` 等下一段输入
  （这就是"整句继续输入"的机制）。
- 上屏 → `OnCommit`：`ctx->GetCommitText()` → `FormatText`（formatters）→ sink
  给前端；同时把 composition 记入 `commit_history`（供 `history_translator`
  和 `encode_commit_history` 造词使用）。

## 4. Translator 内部通用结构

### 4.1 Translation / Menu / Peek 流

`translation.h`：`Translation` 是惰性迭代器，`Next()` 取下一个候选，
`Peek()` 看当前。filter/translation 可以叠层（装饰器模式）：
`TableTranslation` → 被 `Filter` 包装 → 菜单合并。

### 4.2 TranslatorOptions（translator 公共配置）

`gear/translator_commons.cc`（已核对读取代码）：

| 键 | 说明 |
|----|------|
| `tag` | 该 translator 处理的 segment tag，默认 `abc` |
| `delimiter` | 音节分隔符，默认 " "（继承 `speller/delimiter`） |
| `enable_completion` | 允许不完整输入补全（table 类常用；script 类默认开） |
| `strict_spelling` | 严格拼写（拒绝 fuzz 拼写） |
| `contextual_suggestions` | 开启上下文建议（配合 grammar 二次调权） |
| `max_sentences` | 最多生成多少个整句候选 |
| `preedit_format` | 候选区/预编辑串的显示变换（xform 列表） |
| `comment_format` | 注释（提示码）的显示变换 |
| `initial_quality` | 该 translator 候选的初始权重加成（Phrase quality 计算 `exp(weight)+initial_quality`） |

### 4.3 table_translator（`gear/table_translator.cc`）

- 基于静态 table（.table.bin）的**码表式**翻译器：输入码直接查表，天然精确。
- 内部持有 `Dictionary` + 可选 `UserDictionary`（`Memory` 基类管理生命周期）。
- 专属选项（已核对）：

| 键 | 说明 |
|----|------|
| `enable_charset_filter` | 字符集过滤（如只出 8105 字） |
| `enable_sentence` | 允许把多个词条拼成句子（用 poet，见 lm-grammar.md） |
| `sentence_over_completion` | 整句候选排在补全候选之前 |
| `enable_encoder` | 自动造词（用户词典） |
| `encode_commit_history` | 用最近上屏历史造词 |
| `max_phrase_length` | 造词最大长度 |
| `max_homographs` | 同码词最大数量 |
| `enable_user_dict` / `user_dict` / `db_class` | 用户词典开关/名字/存储类型（必须 `userdb`，见 dict-reference.md） |
| `enable_correction` | 纠错（配合 corrector） |

- 优先级：用户词（user phrase）> 静态表内词。补全候选（`enable_completion`）
  的 `matching_code_size < code.size()`，会有一定权重惩罚。
- `enable_sentence` 时走 `Poet::MakeSentence`（WordGraph 上的组句）。

### 4.4 script_translator（`gear/script_translator.cc`）

- 基于音节拼写（prism）+ 词典的**音节式**翻译器（拼音/注音类），核心是：
  1. `Syllabifier::BuildSyllableGraph`（algo/syllabifier.cc）：用 prism（DARTS trie）
     把输入串切成**音节 DAG**（vertices/edges，含 fuzz 拼写、缩写补全）。
  2. `Dictionary::Lookup` 按 DAG 做词图（WordGraph）。
  3. `Poet::MakeSentence` 用 grammar(LM) 动态规划组句。
- 专属选项（已核对）：

| 键 | 说明 |
|----|------|
| `spelling_hints` | 候选注释里显示几个音节的拼写（提示码） |
| `max_word_length` / `core_word_length` | 词条长度上限 / 优先组句的"核心词"长度 |
| `always_show_comments` | 选中后仍显示注释 |
| `enable_correction` | 模糊音纠错 |
| `enable_word_completion` | 词级补全 |
| `max_homophones` | 每音节最多同音字数 |
| `canonicalize` | 拼写规范化规则列表（Projection，作用于输入而非词库） |

- **混合库的关键**：prism 的 alphabet 里既有五笔码字母也有拼音音节时，
  一个拼写串可以被解析成多种音节切分（DAG 多路径），首候选取决于 prism
  内部排序（非确定性！见 deployment-debugging.md）。词库层保证"一个简码拼写
  只对应唯一音节"（ownership 设计）是消歧手段。

## 5. 拼写代数（Spelling Algebra）

`algo/algebra.h` + `algebra/calculus.cc`：schema `speller/algebra` 下的规则列表，
在**词库编译时**作用于每个词条的 code，生成 prism 中的多种拼写（含 fuzz）：

- 常用算子：`derive`（派生变体，不删除原拼写）、`abbrev`（缩写）、
  `xform`（替换）、`erase`（删除）、`fuzz`（模糊音标记，`strict_spelling` 时被拒）。
- 投影 `Projection` 也用于 `preedit_format`/`comment_format`/`canonicalize`。

## 6. Context / Composition / Segment 核心对象

- `Context`（context.h）：持有 `input` 串、`caret_pos`、`composition`、
  options（开关，对应 schema `switches`）、properties、`commit_history`。
  通知器：commit/select/update/option_update/property_update。
- `Composition`（composition.cc）：segment 序列；`Forward()` 确认下一段。
- `Segment`：`start/end`、`tag`、`status`（kVoid→kGuess→kSelected→kConfirmed）、
  `menu`（候选菜单）、`prompts`。
- `Candidate`（candidate.h）：`type/text/comment/preedit/start/end/quality`。
  `Phrase` 包 `DictEntry`；`Sentence` 是组合候选（components + word_lengths）。

## 7. Switch（开关）与 option

schema `switches:` 定义命名开关（`switches.cc`），如 `full_shape`/`simplification`。
`punctuator`、`simplifier`、`charset_filter` 等组件读取对应 option 决定行为；
`switch_translator` 把开关做成候选（菜单里切换）。user_config（如
`squirrel.custom.yaml` 之外的用户设置）会保存开关状态（RestoreSavedOptions）。

## 8. 部署产物与运行时资源

- 运行时只读 **build/** 下产物：`*.table.bin`、`*.prism.bin`、`*.reverse.bin`、
  schema 的编译版 yaml（`config_compiler` 处理 `__include/__patch` 后的成品）、
  `*.userdb/`（LevelDB 用户词典）。
- 资源解析：`resource.h/cc` 的 `ResourceResolver` 按 ResourceType
  （`table`/`prism`/`reverse_db`/`gram_db`/…）在用户目录与 SharedData 目录间
  解析路径。gram 文件的 ResourceType 是 `{"gram_db", "", ".gram"}`
  （librime-octagram 定义）。

详见 [deployment-debugging.md](deployment-debugging.md)。
