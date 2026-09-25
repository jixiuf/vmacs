# Schema 配置参考

> 配置键均以源码为据（标注了读取位置）。示例取自本项目 `egret_wubi_py.schema.yaml`。

## 1. schema 骨架

```yaml
# schema 文件: <schema_id>.schema.yaml，第一行注释是显示名
schema:
  schema_id: egret_wubi_py        # 必须与文件名一致；切换/编译都靠它
  name: 五笔·拼音混输
  version: "6"
  author / description / dependencies: [...]

# 每个键的挂载点：
#   schema:            Schema 元信息
#   switchers:         F4 菜单（少见直接配）
#   engine:            组件流水线（见 §2）
#   speller:           拼写代数 + 字母表（见 §3）
#   translator:        默认 translator 的配置块（name_space = "translator"）
#   <x>_translator:    命名 translator 配置块（name_space = "x"）
#   punctuator / key_binder / recognizer / ascii_composer / menu / style ...
```

**铁律（engine.cc + ticket 机制）**：`engine.translators: [table_translator@abc]`
的 `abc` 是 name_space，该 translator 的全部配置从 `abc:` 块读。配置块键名与
@name_space 不一致 = 配置全部丢失，静默回退（translator 用 schema_id 当词典名）。

## 2. engine

```yaml
engine:
  processors:        # 键盘按键处理（顺序敏感）
    - ascii_composer
    - recognizer
    - key_binder
    - speller
    - punctuator
    - selector
    - navigator
    - express_editor        # 或 fluid_editor（整句）
    - punctuator
    - chord_composer        # 并击才用
  segmentors:        # 输入串切分
    - ascii_segmentor
    - matcher               # 依赖 recognizer/patterns
    - abc_segmentor
    - punct_segmentor
    - fallback_segmentor
  translators:       # 候选生成（多 translator 候选按权重合并进同一菜单）
    - punct_translator
    - table_translator@wubi_py_wubi      # @后为 name_space，读 wubi_py_wubi: 块
    - script_translator@translator       # 读 translator: 块
    - reverse_lookup_translator@reverse_lookup
  filters:
    - simplifier@zh_simplification
    - uniquifier
  formatters:        # 上屏文本格式化（少用）
```

要点：
- processors 中 `speller` 必须在 `punctuator`/`selector` 之前，否则字母会被当标点处理。
- translator 顺序影响**同权重时**的候选先手；真正的置顶靠权重。
- filter 顺序即应用顺序；`simplifier` 一般放最前，`uniquifier` 收尾。
- 组件后可加 `@name_space` 定制配置块；同一组件可挂多个实例。

## 3. speller（拼写代数 + 字母表）

```yaml
speller:
  alphabet: zyxwvutsrqponmlkjihgfedcba   # 参与组码的字符集（prism 的 alphabet）
  delimiter: " "                          # 音节分隔符（显示用）
  algebra:                                # 编译词库时对每个词条 code 变换
    - erase/^xx$/                         # 删除匹配码
    - abbrev/^([a-z]).+$/$1/              # 缩写（首字母）
    - abbrev/^([a-z])[a-z]([a-z])/$1$2/   # 双缩
    - derive/^(..).+$/$1/                 # 派生变体（不删原码）→ 补全来源
    - fuzz/^([zcs])h/$1/                  # 模糊音（strict_spelling 时禁用）
    - xform/^([nl])v/$1ü/                 # 替换
```

算子语义（algo/calculus.cc）：
- `erase`：整个 code 匹配则删除词条拼写。
- `abbrev`：生成缩写，标记 abbreviation（可 `enable_completion` 才出）。
- `derive`：生成变体，原拼写保留。
- `fuzz`：生成模糊拼写（标记 fuzz；`strict_spelling: true` 的 translator 会拒绝）。
- `xform`：原地替换。
- 顺序敏感：先 erase 后 abbrev 等。

## 4. translator 配置块

### 4.1 公共选项（gear/translator_commons.cc，所有 translator 通用）

```yaml
translator:
  dictionary: egret_wubi_py      # 词典名（.dict.yaml 同名；不支持子目录路径！）
  prism: egret_wubi_py           # prism 名（默认同 dictionary；不支持子目录！）
  packs: [extra_dict]            # 可选：附加编译表（多表查询）
  tag: abc                       # 处理哪个 segment tag
  enable_completion: true        # 不完整输入出补全候选
  strict_spelling: false         # 拒绝 fuzz 拼写
  delimiter: " "
  contextual_suggestions: true   # 上下文建议（配合 grammar 二次调权）
  max_sentences: 65536           # 整句候选上限
  initial_quality: 1.0           # 本 translator 候选权重加成
  preedit_format:                # 预编辑显示变换（xform 列表）
    - xform/([aeiou])ng/$1ŋ/
  comment_format:                # 注释显示变换
    - xform/~/ /
```

### 4.2 table_translator 专属（五笔/码表类）

```yaml
wubi_py_wubi:
  dictionary: egret_wubi_py_wubi
  prism: egret_wubi_py_wubi
  enable_charset_filter: true     # 只出限定字符集字（配合 charset filter）
  enable_sentence: true           # 词组拼句（走 poet + grammar）
  sentence_over_completion: false # 整句优先于补全
  enable_encoder: true            # 自动造词
  encode_commit_history: true     # 上屏历史造词
  max_phrase_length: 4            # 造词最长词长
  max_homographs: 1               # 同码词去重阈值
  enable_user_dict: true
  user_dict: egret_wubi_py_wubi.user.dict
  db_class: userdb                # 必须 userdb；tabledb 只读、调频失效
  enable_correction: false
```

### 4.3 script_translator 专属（拼音/音节类）

```yaml
translator:
  dictionary: egret_wubi_py        # 混合库
  spelling_hints: 8                # 注释显示 n 个音节的反查拼写
  max_word_length: 6
  core_word_length: 4              # 优先参与组句的词长
  always_show_comments: false      # 选中后保留注释
  enable_correction: false         # 模糊纠错
  enable_word_completion: false
  max_homophones: 1                # 每个音节的同音字上限
  canonicalize:                    # 输入串规范化（编译期外、运行期）
    - xform/.../.../
```

### 4.4 reverse_lookup_translator

```yaml
reverse_lookup:
  dictionary: pinyin_simp           # 被反查的词典（如拼音查五笔码）
  prefix: "`"
  suffix: "'"
  tag: reverse_lookup               # 需要 matcher/recognizer 配合打 tag
  tips: 〔拼音反查〕
  preedit_format / comment_format: ...
```

## 5. recognizer / matcher

```yaml
recognizer:
  import_preset: default          # 继承 default.yaml 的 patterns（大写、/数字等）
  patterns:
    reverse_lookup: "`[a-z]*'?$"  # 命名与 segment tag 对应
```

## 6. menu / selector / key_binder / punctuator / ascii_composer

```yaml
menu:
  page_size: 9

key_binder:
  bindings:
    - { when: composing, accept: Return, send: Escape }   # 回车清空
    - { when: has_menu, accept: semicolon, send: 2 }       # 分号选二重
    - { when: paging, accept: comma, send: Page_Up }

punctuator:
  import_preset: default
  full_shape: ...        # 全角映射
  half_shape: ","

ascii_composer:
  good_old_caps_lock: true
  switch_key:
    Caps_Lock: commit_code
    Shift_L: inline_ascii     # commit_code / commit_text / inline_ascii / clear
```

`editor` 选型：`express_editor`（回车=清空，适合码表）；
`fluid_editor`（流式整句，回车上屏已有部分）。

## 7. switches（开关）

```yaml
switches:
  - name: full_shape            # 与组件约定 option 名对应
    states: [ 半角, 全角 ]
  - name: zh_simplification     # simplifier 组件读这个 option
    states: [ 汉字, 汉字→简 ]
    reset: 0                    # 每次会话重置为状态0；不写则记住
```

## 8. simplifier（繁简）

```yaml
zh_simplification:
  opencc_config: s2tw.json       # opencc 数据在用户目录 opencc/
  option_name: zh_simplification
  tips: none                     # 或 char / all
  excluded_types: [ reverse_lookup ]
```

## 9. grammar（语言模型，octagram 插件读取）

```yaml
grammar:
  language: wanxiang-lts-zh-hans       # → build/ 下找 <language>.gram
  collocation_max_length: 4            # n-gram 最长上下文（源码默认 4）
  collocation_min_length: 3            # 低于此长度视为弱搭配
  collocation_penalty: -12             # 正常搭配加分项
  non_collocation_penalty: -12         # 无证据时的罚分
  weak_collocation_penalty: -24        # 弱搭配罚分
  rear_penalty: -18                    # 句尾惩罚
```

详见 [lm-grammar.md](lm-grammar.md)。

## 10. 配置组合：`__include` / `__patch` / custom 机制

部署时 config_compiler（config/config_compiler.cc）处理：

```yaml
# 列表/节点引用
__include: egret.common            # 并入另一个文件（yaml 同目录，不带 .yaml）
__include: egret.common:/engine    # 并入某节点

# 打补丁（覆盖性合并，常在 *.custom.yaml 的 patch: 下）
__patch: default:/patch/my_style

patch:                             # default.custom.yaml 顶层固定写法
  menu/page_size: 9
  switches/@next: { name: foo, ... }   # 路径语法 @next/@before/@0..n
  switches/@2/reset: 1
```

- `*.custom.yaml` 由 AutoPatchConfigPlugin 自动应用到同名 yaml（`default.custom.yaml`
  → `default.yaml`，`<schema>.custom.yaml` → `<schema>.schema.yaml`）。
- 列表操作路径：`@next`（追加）、`@before 0`、`@last`、`/@index`。
- 编译结果在 `build/<name>.yaml`；**运行时只读 build 产物**。调试配置时
  `grep` build 产物确认 patch 是否生效，是排除"改了没生效"的最快手段。
