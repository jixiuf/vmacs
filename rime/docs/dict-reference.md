# 词库（Dictionary）参考

> 源码依据：`src/rime/dict/`（entry_collector / dict_compiler / table / prism /
> reverse_lookup_dictionary / user_dictionary / level_db）。

## 1. .dict.yaml 明文格式

```yaml
# 头部：--- 到 --- 之间的 yaml（dict_settings.cc 解析）
---
name: egret_wubi_py_wubi_chars   # 表名（import_tables 引用它，不带 .dict.yaml）
version: "0.1"
sort: original                   # original=按文件序 by_weight=按权重 by_frequency
columns:                         # 可选：定义正文列序（默认 1 text 2 code 3 weight）
  - text
  - code
  - weight
  - stem
import_tables:
  - dicts/egret_wubi_py/egret_wubi_py_wubi_chars   # 支持子目录！只有这里支持
  - dicts/wb86_wangxiang_cn_dicts/base.small
vocabulary: essay                # 八股文词频表（给无 weight 词组补权）
use_preset_vocabulary: true      # 同上（旧写法）
max_phrase_length: 4
min_phrase_weight: 100
encoder:
  exclude_patterns:
    - '^z.*$'
  rules:
    - length_equal: 2            # 词长=2 时
      formula: "AaAbBaBb"        # 坐标语法见 §4
    - length_in_range: [3, 10]   # 词长 3-10 时
      formula: "AaBaCaZ"
...
```

正文（`---` 之后）每行一个词条：

```
text<TAB>code<TAB>weight<TAB>[stem]
```

- **weight 语义**：词库里的数字在编译时被 `log()` 化后存入 table.bin
  （dict_compiler.cc:257 `e->weight = log(r->weight)`；候选质量 = `exp(weight)`，
  见 translator_commons.cc Phrase 构造）。支持绝对值或百分号相对权重
  （`500%` = preset_vocabulary 权重的 5 倍，entry_collector.cc）。
  **不同来源的词库如果权重刻度不同（知频 0-1500万 vs 万象 0-1000），混在同一个
  translator 里会破坏 LM 组句与排序**——本项目已统一 log-norm 0-1000。
- stem 列：造词用词干（罕见使用）。
- `# no comment` 行之后不再解析注释列。
- 同码多词条自然按权重裁决（如 tffu: 等 780 > 徒增 502），**不要**在词库层
  硬过滤冲突词——本项目曾因过滤字全码导致 `tffu` 出不了"等"。
- 导入即合并：import_tables 的各表与主表正文合并；重复词条处理取决于
  `sort` 与编译器去重（entry_collector）。

## 2. 编译产物

`dict_compiler.cc` 总控（部署时自动调用，checksum 变化才重编译）：

| 产物 | 生成器 | 内容 |
|------|--------|------|
| `build/<dict>.table.bin` | table.cc | 码表：head→trunk→entries，权重 log float |
| `build/<prism>.prism.bin` | prism.cc | DARTS trie：拼写串 → SyllableId；含 SpellingMap（类型/可信度）、alphabet、max_key_length。**由 speller/algebra 对所有词条 code 展开后构建** |
| `build/<dict>.reverse.bin` | reverse_lookup_dictionary.cc | 词→码 反查表 |
| `build/<dict>.multiple.table.bin` | 多 packs 时 | 附加表 |

要点与陷阱：
- **prism 非确定性**：libdarts 对同 key 集合构建时，内部排序受遍历顺序影响；
  一个拼写映射多个音节时，prism 内的首选音节可能随目录/文件序而变 → 候选翻转。
  对策：词库层保证简码拼写唯一音节（ownership），或 patch librime 在 lookup
  处排序（`patches/librime-0001-dict-entry-iterator-sort-on-lookup.patch`，存档）。
- `translator/dictionary:` 与 `prism:` 键**不支持子目录路径**，静默失败。
  子目录只能通过词库头 `import_tables` 引入；根包装 .dict.yaml 必须在用户目录
  根（Rime 解析 import_tables 时以各文件自身位置解析相对路径）。
- 重编译判定：dict 文件 checksum（含 import 的子表与 vocabulary 文件）与
  schema 文件 checksum，任一变化即重建 table；prism 另受 schema checksum 影响。
  部署失败/静默回退时先核对 build/ 下产物的 md5 与文件时间。

## 3. 词典组件链路（运行时）

```
translator 配置块
  ├─ dictionary: <name>   → Dictionary(name, prism, packs)
  │     ├─ table:  <name>.table.bin    （主表）
  │     ├─ packs:  <n>.multiple.table.bin（附加表）
  │     └─ prism:  <prism>.prism.bin
  └─ user_dict: <name>.user.dict (db_class: userdb → LevelDB)
        ├─ user phrase 查询（优先于静态表）
        ├─ 调频（dynamics: dynamics 模块写入 commit 时间戳/频率）
        └─ UnityTableEncoder 运行时造词（\x7fenc\x1f 前缀条目）
```

- `Memory`（gear/memory.h）是 translator 与词典生命周期的粘合层。
- 用户词典三态：`enable_user_dict: false` 完全不建；`db_class: tabledb` 只读；
  默认 `userdb` 可读可写。**测试前清 userdb**（`rm -rf build/<name>.userdb/`
  或移走），否则历史上屏会以 user phrase 形式调频，压住词库修正——本项目第 4 号坑。

## 4. 造词规则（encoder/rules）

读取位置：**词库头部的 `encoder:`**（dict settings），不是 schema！
 UnityTableEncoder 在运行时造词时按此规则用 reverse.bin 查每个字的全码。

formula 坐标语法（algo/encoder.cc）：
- 大写字母 = 取第几个字（A=第1字，B=第2字，… Z=第26字）。
- 小写字母 = 该字编码的第几位（a=1码，b=2码，…）。
- `Z` = 最后一个字，`z` = 最后一码。
- 例（五笔 86 4 码词组）：
  - 二字词 `AaBaBbCc` → 各取前两码：`XxYy+ZzWw`。常用写法 `AaAbBaBb`
    （每字前两码）。
  - 三字词 `AaBaCa` → 前两字首码+第三字前两码。
  - 四字及以上 `AaBaCaZa` → 前三字首码+末字首码。
- `length_equal: 2` / `length_in_range: [3, 10]` 限定适用词长。
- `exclude_patterns`：正则排除（如不想为某类码造词）。
- **词库头没有 encoder/rules 则运行时造词静默失效**（本项目第 6 号坑）。
- 薄包装模式（本项目）：根包装 .dict.yaml 只写头部（import_tables + encoder），
  正文数据放子表。

## 5. preset vocabulary（八股文词频）

- `vocabulary: essay` + 编译时 `essay.txt`（官方预设 https://github.com/rime/essay ，
  在 shared data）注入词频；`min_phrase_weight` 过滤低频词组；`max_phrase_length` 限制注入词长。
- 本项目万象方案直接用带权重的词库，未走 essay。

## 6. 用户词典快照与调试

```bash
# 导出 userdb 快照（可读文本）；工具在 build/bin/ 下，构建后才有
~/repos/librime/build/bin/rime_dict_manager --help   # 查看当前支持的子命令
# 直接看内容：
cd <用户目录>/build && strings egret_wubi_py_wubi.user.dict.userdb/*.ldb | head

# 清空某方案用户词典（回归测试必做）
rm -rf build/egret_wubi_py_wubi.user.dict.userdb build/egret_wubi_py.user.dict.userdb
```

userdb 内键格式：`<text>\t<code>`，值为 `c=频次 d=最后使用日期 t=类型`。
以 `\x7fenc\x1f` 开头的 text 是自动造词条目。

## 7. user_dict 共享陷阱（2026-09-25 实测）

- librime `UserDictionaryComponent::Create(dict_name, db_class)` 用 **db_pool_
  按 user_dict 名共享 db 实例**：多个 translator 的 `user_dict` 同名时，实际
  后端由**创建顺序**决定（先创建者胜出），后创建者的 db_class 被忽略。
- 本项目 custom_phrase(tabledb) 与 translator(userdb) 同名
  `egret_wubi_py.user.dict` → 全部落到 tabledb txt。混流学习（选台→选湾→
  上屏）会写进手工维护的 txt，并顺带产生收益：script 组句能看到「湾 wan」
  user phrase，使 `ckwan` 组出「台湾」。
- 若改为不同名（分离 tabledb/userdb），script 看不到学习结果，
  `ckwan` 永远「台万」（已实测）。当前刻意保持同名共享，学习条目用
  rime_dict_manager / 手工清理 txt。

## 8. 跨词库造词学习限制（2026-09-25 实测）

- script translator 的 Memorize 对 CommitEntry 逐元素 UpdateEntry：
  元素的音节 id 属于**产生该候选的 prism**。混流句子「台(传统库 ck) + 湾(混合库
  wan)」中，台 的音节 id 是传统 prism 的，script 用户词典的音节表
  （混合 prism）无法翻译 → 台湾 整词学习失败，只有 湾(wan) 单字条目成功。
- 结论：跨 translator 组句的整词 user phrase 学习不可依赖；组合收益来自
  单字 user phrase 抬升组句，或词库层直接提供混合拼写词条（见 lm-grammar.md
  §6 同类问题修复）。

## 9. 本项目词库结构（现状速查）

```
egret_wubi_py.dict.yaml            # 混合库根（根目录；软链）→ import: 五笔字表+五笔词组+拼音字表+拼音词表
egret_wubi_py_wubi.dict.yaml       # 传统库根包装（手工维护）→ import: wubi_chars + wubi_words + user_phrases；含 encoder/rules
dicts/egret_wubi_py/               # 生成数据（gen_wubi_py_unified_dicts.py 产出）
  egret_wubi_py_chars.dict.yaml    #   混合库五笔字表（ownership: 二级简码 owner 保留 ',' token）
  egret_wubi_py_wubi_chars.dict.yaml # 传统库字表（简码+全码，不过滤 2+2 冲突）
  egret_wubi_py_wubi_words.dict.yaml # 传统库词组
  egret_wubi_py_user_phrases.dict.yaml # 手工维护用户词组（tgtj 生不逢时 900）
  egret_wubi_py_pinyin_8105.dict.yaml  # 拼音字表（无调化）
  egret_wubi_py_pinyin_base_small.dict.yaml # 拼音词表（无调化）
dicts/wb86_wangxiang_cn_dicts/base.small.dict.yaml # 五笔词组（万象权重，>450 过滤）
```

改动任何词库/gen 脚本后：`python3 scripts/regression_test.py --rebuild`
（17 条黄金用例），见 `scripts/regression_test.py`。
