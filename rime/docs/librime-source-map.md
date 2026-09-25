# librime 源码地图

> 以 `~/repos/librime` 为准（fork 1.17.0 + 自编译 + plugins/librime-lua +
> plugins/librime-octagram + 未提交的 Sort patch）。读码顺序建议：
> `rime_api_impl.h` → `engine.cc` → `gears/*translator*.cc` → `dict/*`。

## 顶层

```
src/rime_api.{h,cc}     # C ABI：前端(Squirrel/weasel/liberime)调用的全部入口
src/rime_api_impl.h     # C API → C++ Service/Session 的胶水层
src/rime/               # 核心库（下述）
plugins/librime-lua/    # lua 脚本插件
plugins/librime-octagram/ # 八股文语法(LM)插件 → 注册 "grammar" 组件
tools/rime_api_console.cc # 命令行测试器（我们回归测试的基础）
data/minimal/           # 最小可运行数据集（试编译 prism/table 用）
doc/                    # 官方文档（多为 doxygen + chording 设计文档）
test/                   # gtest 单测——查组件行为细节的好出处
```

## src/rime/ 顶层文件（引擎核心对象）

| 文件 | 职责 |
|------|------|
| `engine.{h,cc}` | ConcreteEngine：按键分发、Compose（切分+翻译）、上屏。**架构总纲** |
| `context.{h,cc}` | 输入状态：input 串、caret、composition、options、commit_history、各类 notifier |
| `composition.{h,cc}` / `segmentation.{h,cc}` | segment 序列与切分状态机 |
| `candidate.{h,cc}` / `translation.{h,cc}` / `menu.{h,cc}` | 候选对象、惰性候选流、候选菜单（多 translator 汇聚点） |
| `schema.{h,cc}` | schema 加载（config → 组件 prescription） |
| `config.{h,cc}` + `config/` | 配置树（yaml 读取/编译/查询） |
| `registry.{h,cc}` / `component.h` / `ticket.{h,cc}` | 组件注册表与 Ticket（klass@namespace） |
| `service.{h,cc}` | 全局服务：session 管理、资源解析器工厂 |
| `deployer.{h,cc}` | 部署任务执行器 |
| `switcher.{h,cc}` / `switches.{h,cc}` | F4 方案切换菜单与命名开关 |
| `key_event.{h,cc}` / `key_table.cc` | 按键表示与键名表（"Control+Alt+Delete" 这类） |
| `language.{h,cc}` | Language 对象（component + user dict + LM 的组合身份，Poet 用它取 grammar） |
| `memory.{h,cc}`（在 gear/） | Memory 基类：translator 与 dictionary/user_dictionary 的生命周期粘合 |

## src/rime/algo/ —— 算法层

| 文件 | 职责 |
|------|------|
| `syllabifier.{h,cc}` | `BuildSyllableGraph`：输入串 → 音节 DAG（依赖 prism）；`CheckOverlappedSpellings`/`Transpose` 处理歧义切分 |
| `algebra.{h,cc}` / `calculus.{h,cc}` | 拼写代数算子（`derive/abbrev/xform/erase/fuzz`），编译词库时展开拼写 |
| `encoder.{h,cc}` | 造词规则引擎：`TableEncoder`（formula 规则）与 `ScriptEncoder`（查音节表）；formula 语法 `length_equal`/`length_in_range`、坐标 a..z |
| `spelling.{h,cc}` | Spelling 类型（normal/completion/abbreviation/fuzz/correction） |
| `dynamics.h` | 词频动态调整策略接口（userdb 回写） |
| `utilities.{h,cc}` | Checksum 等工具 |

## src/rime/config/ —— 配置编译

| 文件 | 职责 |
|------|------|
| `config_compiler.{h,cc}` | **部署时**配置编译器：处理 `__include` / `__patch` / `__append` / `__merge` / `__flatten` 生成 build/ 下的成品 yaml |
| `config_component.cc` | 运行时 config 加载（只加载编译成品） |
| `config_types.cc` | ConfigItem/Value/List/Map 数据结构 |
| `default_config_plugin.cc` | 让 `default.custom.yaml` 的 patch 作用到 default.yaml |
| `auto_patch_config_plugin.cc` | 处理 `*.custom.yaml` 的 `patch:`（对任意 yaml） |
| `legacy_preset_config_plugin.cc` / `legacy_dictionary_config_plugin.cc` | 旧式 `/preset`、`/dict` 引用兼容 |

## src/rime/dict/ —— 词典与存储

| 文件 | 职责 |
|------|------|
| `dictionary.{h,cc}` | Dictionary 组件：`Lookup(SyllableGraph)` → WordGraph；多表（packs）查找；**注意 `Create(ticket)` 读 `<ns>/dictionary`、`<ns>/prism`、`<ns>/packs`** |
| `dict_compiler.{h,cc}` | 词库编译总控：checksum 判重 → EntryCollector 收词 → Table/Prism/Reverse 落盘 |
| `entry_collector.{h,cc}` | 解析 .dict.yaml 明文（列：text/code/weight/stem；`# no comment`；`sort` 排序；preset_vocabulary 注入 essay） |
| `dict_settings.{h,cc}` | 词库头部（`---...---` 之间的 yaml）：name/version/sort/columns/import_tables/encoder/vocabulary |
| `table.{h,cc}` | .table.bin 读写：head index → trunk index → entry 列表，权重 float 存储 |
| `prism.{h,cc}` | .prism.bin 读写：DARTS（libdarts）双数组 trie，拼写→SyllableId 映射 + SpellingMap + alphabet + max_key_length |
| `reverse_lookup_dictionary.{h,cc}` | .reverse.bin：词 → 码 反查（reverse_lookup_filter/translator、UnityTableEncoder 造词都依赖它） |
| `user_dictionary.{h,cc}` | 用户词典：查/调频/造词/learn；`LookupWords` 是 user phrase 排序来源 |
| `db.{h,cc}` / `level_db.{h,cc}` / `table_db.{h,cc}` / `text_db.{h,cc}` | 存储后端。**user_dict 必须 userdb（LevelDB）才能调频/学习**；tabledb 只读 |
| `user_db_recovery_task.cc` | userdb 损坏恢复 |
| `user_dict_manager.cc`（lever/） | 导出/导入/合并用户词典快照（.userdb.txt） |
| `preset_vocabulary.{h,cc}` | 八股文词频表（essay.txt）：编译时给无码词条补权重 |
| `corrector.{h,cc}` | 模糊音/纠错候选 |
| `mapped_file.{h,cc}` / `string_table.cc` / `vocabulary.{h,cc}` | mmap 文件格式底座 |

## src/rime/gear/ —— 组件实现（gears 模块）

注册表见 `gears_module.cc`（组件名 → 类的对照就在这里，改组件名先看它）。

**processors**
| 文件 | 职责 |
|------|------|
| `speller.cc` | 字母键 → input 追加；`auto_select` 与 `max_code_length` 截码 |
| `punctuator.cc` | 标点映射（punctuation.yaml / punctuator: 配置） |
| `key_binder.cc` | 按键重绑定（key_binder/bindings） |
| `selector.cc` / `navigator.cc` | 选重与移动 |
| `ascii_composer.cc` | 中英切换状态机（ascii_composer/good_old_caps_lock 等） |
| `editor.cc` | `express_editor`（有 composing 则回车清空）/`fluid_editor`（整句流式） |
| `recognizer.cc` | `recognizer/patterns` 正则触发 tag 切分 |
| `chord_composer.cc` / `streaming_chord_processor.cc` | 并击输入 |

**segmentors**：`abc_segmentor.cc`（tag: abc）、`affix_segmentor.cc`（反查前后缀）、
`ascii_segmentor.cc`、`matcher.cc`、`fallback_segmentor.cc`。

**translators**
| 文件 | 职责 |
|------|------|
| `table_translator.cc` | 码表翻译器（五笔类）。选项见 architecture 文档 §4.3 |
| `script_translator.cc` | 音节翻译器（拼音类，r10n_translator 别名）。选项见 §4.4 |
| `reverse_lookup_translator.cc` | 反查（附一个 tag 的输入用另一套词典查） |
| `echo_translator.cc` | 原样回显 raw input |
| `punct_translator.cc` | 标点候选 |
| `schema_list_translator.cc` | F4 方案列表 |
| `switch_translator.cc` | 开关候选 |
| `history_translator.cc` | 重复上屏（重复输入上一条 commit） |

**组句与调权**
| 文件 | 职责 |
|------|------|
| `poet.{h,cc}` | **组句器**：WordGraph → DP/beam search 选最优句；`CompareWeight`（纯权重）与 `LeftAssociateCompare`（左侧优先）两种策略 |
| `grammar.h` | Grammar 接口：`Query(context, word, is_rear)` 返回 log 概率加分；无 grammar 时 kPenalty=-13.81 |
| `contextual_translation.cc` | `contextual_suggestions`：用前文（commit_history）二次调权 |
| `translator_commons.{h,cc}` | Phrase/Sentence/TranslatorOptions/Patterns/Spans 公共设施 |
| `memory.{h,cc}` | translator ↔ dictionary/user_dictionary 绑定 |
| `unity_table_encoder.cc` | 运行时造词写入 userdb：**造词规则读自词库头 encoder/rules**，`\x7fenc\x1f` 前缀标记编码条目 |

**filters**：`simplifier.cc`（opencc 繁简）、`charset_filter.cc`（字符集）、
`uniquifier.cc`、`reverse_lookup_filter.cc`（注释里显示另一套编码）、
`single_char_filter.cc`。

## src/rime/lever/ —— 部署与开关

| 文件 | 职责 |
|------|------|
| `deployment_tasks.{h,cc}` | 全部部署任务：InstallationUpdate / WorkspaceUpdate（复制 shared data） / SchemaUpdate（编译单个 schema：config + dict） / ConfigFileUpdate / PrebuildAllSchemas / SymlinkingPrebuiltDictionaries / UserDict* / Backup / CleanupTrash |
| `custom_settings.{h,cc}` / `customizer.cc` | `*.custom.yaml` 应用逻辑 |
| `switcher_settings.cc` | switcher 配置 |
| `levers_api_impl.h` | rime_levers C API（导出配置/词库管理） |

## plugins/librime-octagram —— 语法插件

见 [lm-grammar.md](lm-grammar.md) §1-2。要点：注册名为 `grammar` 的组件，
`Poet` 通过 `Grammar::Require("grammar")` 拿到它；`grammar/language` 指定
`<name>.gram`；查询即 n-gram 后缀匹配打分。

## plugins/librime-lua

- `lua/` 下注册 processor/segmentor/translator/filter/kr（key 运行时）。
- 本项目 `lua/` 目录的脚本经由 `rime.lua` 或 `lua/` 模块加载。
- 常用 API：`candidate`、`env.engine.context`、`mem`（memory 简化接口）、
  `opencc`、`revd`。写 lua 组件时先查 `plugins/librime-lua/src/types.cc` 可用类型。

## 快速定位问题的 grep 姿势

```bash
# 某 schema 配置键到底被谁读了
grep -rn 'name_space_ + "/KEY"' ~/repos/librime/src/rime/
grep -rn 'name_space + "/KEY"' ~/repos/librime/src/rime/

# 某组件注册名是否存在 / 选项是什么
grep -rn 'Register("COMPONENT_NAME"' ~/repos/librime/src/rime/

# 看某选项默认值与语义 → 直接读对应 gear/*.cc 的 Initialize/构造函数
```
