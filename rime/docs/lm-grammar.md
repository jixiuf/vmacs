# 语言模型（Grammar / .gram / 组句）

> 源码依据：`~/repos/librime/src/rime/gear/poet.cc`、`gear/grammar.h`、
> `~/repos/librime/plugins/librime-octagram/src/`（lotem/librime-octagram）。

## 1. 组句总流程（Poet）

`table_translator`（enable_sentence）与 `script_translator` 共用 `Poet`：

```
输入音节 DAG (SyllableGraph)
  → Dictionary::Lookup → WordGraph = map<int, map<int, DictEntryList>>
      键：词的 [start, end) 顶点位置；值：该区间上所有候选词
  → Poet::MakeSentence(graph, total_length, preceding_text)
      在 WordGraph 上做动态规划（beam search），打分函数：
        score = Σ ( entry_weight + Grammar::Query(context, word, is_rear) )
  → 产出 Sentence 候选（components + word_lengths）
```

关键细节（poet.cc 已核对）：
- Grammar 缺席时每词罚 `kPenalty = log(1e-6) ≈ -13.81`——**这就是权重量级
  混排导致组句失效的机理**：entry_weight 刻度若与 LM 的 log 概率刻度相差
  数量级，LM 加分/罚分无法影响选择。
- 两种比较策略：`CompareWeight`（句总权重）与 `LeftAssociateCompare`
  （左侧优先，table_translator 默认用于保守组句）。
- `preceding_text` 来自 `commit_history`（contextual_suggestions 开启时用于
  跨段上下文打分）。

## 2. octagram 插件（八股文语法）

组件注册名 `grammar`（`grammar_module.cc`），`Poet` 经
`Grammar::Require("grammar")` 创建。配置块（schema 根级 `grammar:`）：

```yaml
grammar:
  language: wanxiang-lts-zh-hans    # 必填；无此项则 grammar 直接禁用
  collocation_max_length: 4         # 查询的最大上下文字数（源码默认 4）
  collocation_min_length: 3         # 搭配最短长度（≥ 此值才算正常搭配）
  collocation_penalty: -12          # 命中搭配时的附加分
  non_collocation_penalty: -12      # 查不到时的罚分
  weak_collocation_penalty: -24     # 搭配过短（弱搭配）罚分
  rear_penalty: -18                 # 句尾(is_rear)候选的额外裁决
```

查询逻辑（octagram.cc::Query，已通读）：
1. 取 context 末尾 `collocation_max_length-1` 个字，与 word 前
   `collocation_max_length-1` 个字分别 UTF-8 编码（自研 3-byte 编码，gram_encoding.cc）。
2. 对 context 的每个后缀做 `db_->Lookup(context_suffix, word_query)`，最多返回
   `kMaxResults=8` 个匹配。
3. 分数 = `match.value / kValueScale(10000)` + 搭配罚分（搭配长度
   `context_len+match_len >= collocation_min_length` 或整句全匹配 →
   collocation_penalty，否则 weak_collocation_penalty），取最大值。
4. `is_rear`（句尾）时额外查 `word + "$"`，命中加 rear_penalty。
5. 查不到任何证据 → `non_collocation_penalty`。

`.gram` 文件定位：ResourceType `{"gram_db", "", ".gram"}` →
`build/<language>.gram`（用户目录 build/ 下，或 shared data）。

`.gram` 二进制格式（gram_db.cc）：自研格式，`log(count)*10000` 取整存分值，
按 UTF-8 编码串排序的有序词表 + 索引。构建工具：
`plugins/librime-octagram/tools/build_grammar.cc`（编译产出 `build_grammar`），
从 stdin 读 `<ngram编码串> <value>` 行对，输出 `<language>.gram`。

## 3. 训练管线（自训 LM）

已验证可产出可用 gram（本项目 jixiuf-zh-hans.gram 83.6MB / 2.1亿 n-gram）：

```
语料（清洗 → 10-20 字短句 → 去重）
  → 字符 2-6gram 统计（JACKCHAN000/rime-corpus-processing，Rust，
    已编译于 /tmp/rime-corpus-processing）
  → kenlm 训练 arpa（gaboolic/rime-build-grammar）
  → arpa → gram（rime-build-grammar 的 build_grammar，librime-octagram 工具链）
  → wanxiang-lts-zh-hans.gram / jixiuf-zh-hans.gram
```

语料来源与配比经验（重要）：
- 中文维基 3 分卷（4690万字）+ 得到课程文稿（1.06亿字）+ epub 抽样。
- **语料配比 = LM 口味**：电子书网文语料会强化「才最」类搭配；
  想翻转特定 LM 裁决（如 才最 vs 都）需要该表达在语料中的证据量压过竞争证据，
  否则用全码强制消歧。
- wanxiang-lts-zh-hans.gram（200MB，官方）与自训 gram 在 `egret.common.yaml`
  的 `grammar/language` 切换。AB 评测：wanxiang 82 分 vs 自训 85/92，
  自训通用性略差但特定场景更准。

## 4. LM 调优操作面

1. **改 grammar 罚分**：调 `collocation_penalty` / `non_collocation_penalty` /
   `weak_collocation_penalty` / `rear_penalty`。典型问题「虚词裁决」：
   他们国有→他们了，可尝试加大 non_collocation_penalty 绝对值让 LM 证据更强势。
2. **改语料再训**：对特定裁决不满意时，在语料中补充正确表达的短句，
   重跑训练管线。min-count 阈值（30/15/8/5/3）控制 gram 体积与噪声。
3. **词库层兜底**：LM 调不动的搭配，用词库权重/全码/用户词组强制。
4. **评测**：gaboolic/rime-schema-compare 做系统评测；本地
   `scripts/regression_test.py` 做回归（17 条黄金用例，LM 级差异单列）。

## 5. 词级 vs 字级 gram（2026-09-26 关键实测）

混流「首字五笔码+次字拼音」逐字组句（如 ck wan→台+万/湾）暴露两类 gram 的本质差异：

| gram | 键结构 | Query(台,湾) | ckwan 结果 | 同批 15 组混流用例 |
|------|--------|--------------|-----------|---------------------|
| wanxiang-lts-zh-hans (词级) | 分词序列 | -12(无证据) | 台万 ✗ | 13/15(扩到21例: 15/21) |
| zh-hans-t-essay-bgc (字级, lotem 八股文, 3.9MB) | 字符 n-gram | **+6.31** | **台湾 ✓** | **16/21(21例批)** |
| fcitx-zh-hans (25MB, fcitx libime 200G语料转换) | 未见效 | -12 | 台万 ✗ | 10/21, 出现"和珅满怀"类坏句 |

21例批=1个 ckwan + 20个高频二字词混流用例(首字五笔2码+次字拼音)。
结论: fcitx 转换模型与本引擎查询模式不匹配, 不用; essay-bgc 当前最优;
理论上限是"字级+现代大语料"自训 gram(jixiuf gram 即此, 文件遗失待恢复)。

- 词级 gram 只在「拼接串恰好等于词序列」时有证据；逐字组合的二字词大多
  查不到 → LM 盲区，静态字重差(万850 vs 湾719 ≈ 0.17 log)决定结果。
- 字级 gram 对逐字组合直接给二元组证据，差距 6.2 ≫ 0.17 → LM 正常裁决。
- 两者互补失效面不同（词级对 生姜/可以 更稳，字级对逐字组合更稳）。
  自训 jixiuf-zh-hans.gram(字符级 2-6gram, 维基+得到语料)是字级+现代语料
  的最优解，文件遗失待恢复/重训。

## 6. 同类问题（ckwan→台湾）修复路线（已实测验证）

1. **gram 路线**：`grammar/language: zh-hans-t-essay-bgc`
   （rimeinn/octagram-data, models/essay/, Git LFS 下载）。20/20 回归通过。
2. **词库路线**：为高频二字词批量生成"混合拼写"词条——首字取混合字表
   二级码 + 次字取拼音（如 `台湾 ck wan 900`），词级 gram 下也能命中
   （词直接匹配，无需 LM）。应作为 gen_wubi_py_unified_dicts.py 的
   系统性生成项，而非手工个例。
3. 两者可叠加：词库条目兜底常用词，字级 gram 覆盖长尾组合。

### 失败类补充（2026-09-26 续）

- 官方新版 zh-hans-t-essay-bgc.gram(20260712, 27.7MB) 已替换 rimeinn 旧版
  (3.9MB)，批测结果相同(16/21)——剩余失败根因在词库/机制而非 gram:
  (a) 传统词组精确匹配置顶（形码确定性设计），grammar 无从裁决
      （如 skyi: 传统词"可就"605 vs 组合"可以" grammar+8.63）;
  (b) script_translator 有 exact match 时默认不组句 → librime 实验补丁
      `always_make_sentences`（config 门控默认关）。
  要让 LM 仲裁"五笔词组 vs 拼音组合"需统一 quality 尺度，属产品决策。

### 调试手段

- octagram Query 打分不可见（DLOG 在 Release 编译掉）——临时在
  plugins/librime-octagram/src/octagram.cc Query 尾部加 LOG(INFO) 增量重编
  (`cmake --build build --target rime`)，日志 grep "GRAMDBG"。
- tools: /tmp/rime_learn_test.c（ctypes 式 C 直调 librime：模拟选字流、
  全局候选遍历 select_candidate）——选字学习流与候选来源分析的工具。

## 7. 相关调试

- `rime_api_console` 打开 GLOG_v=1 可看到 `Lookup(ctx + word)` 的 DLOG 输出
  （Octagram::Query 每步打分）——确认 LM 是否命中、命中值多少。
- gram 未生效的常见原因：`grammar/language` 名字与 build/ 下 .gram 文件名
  不一致；或 build 目录里根本没有该 gram（部署不复制 gram，需手动放置/
  软链）。
