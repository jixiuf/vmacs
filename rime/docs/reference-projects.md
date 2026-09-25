# 参考项目分析与词库构建/优化方法论

> 本文分析 `~/repos/` 下的成熟方案与工具仓库，以及"如何借助它们构建、
> 优化词库/LM"的可操作管线。仓库位置遵循 README 约定：先查 `~/repos/`，
> 没有则 clone。各仓库 README/AGENTS.md 是第一手资料，先读再动手。

## 0. 项目总览

| 项目 | 性质 | 对本项目的价值 |
|------|------|----------------|
| rime-ice 雾凇拼音 | 拼音方案+词库组织范本 | 词库分层组织、lua 库、英文混输 |
| rime_wanxiang 万象拼音 | 带声调拼音方案（amzxyz） | 权重体系、多音字处理、lua 重度工程化 |
| RIME-LMDG | 词库/LM 构建工厂（amzxyz） | 语料清洗、打分清洗、编码转换脚本、gram 发布 |
| rime-wubi-sentence 墨奇五笔整句 | 五笔整句方案（gaboolic） | 五笔码标注、简码自动分配算法、词库源数据 |
| rime-frost 白霜拼音 | 拼音+辅助码方案（gaboolic） | 词库分层、essay.txt、辅助码 lua |
| rime-build-grammar / rime-schema-compare | 训练/评测（gaboolic） | gram 训练与量化评测 |

## 1. RIME-LMDG —— 词库构建工厂（核心参考）

`~/repos/RIME-LMDG`（https://github.com/amzxyz/RIME-LMDG）

- **定位**：32GB 多领域语料 → 带声调全拼词库 + 语法模型(gram) 的生产线。
  词库全部**带声调全拼**；词频按"词组+拼音"双键统计（区分多音字，
  如 那里/哪里 不混）；词频经**对数归一化**。
- **词库分层**（dicts/，拼音方案直接 import）：
  `zi`（43324 字读音库，CJK 基础区+扩展） / `jichu`（2-4字基础词）
  / `lianxiang`（5字以上长词） / `duoyin`（多音字兼容）
  / `cuoyin`（错音错字提示）/ 分类库 `wuzhong`(动植物) `diming` `huaxue`
  `yaopin` `yixue` `yiren` `mingren` `renming` `shici`（15M 诗词）
  / `abbrev`（缩写）。
- **构建脚本**（根目录，Python）：
  - `TXT清洗.py` / `维基中文语料解析.py` / `json语料解析.py`：语料清洗入口
  - `多线程分词.py`：分词
  - `滑动窗口验证给短语打分并清洗无用词条.py`：**短语打分清洗**（保留真实
    搭配、剔除统计噪声——词库瘦身的关键思路）
  - `模型排序.py`：模型排序
  - `未重复声调分类.py`：声调分类
  - `rime固定或用户词典刷新为带声调编码.py` / `刷新为带辅助码编码.py`：
    **把已有 Rime 词库（固定/用户）批量重标注成带声调/带辅助码编码**
  - `wanxiang-tools.py`（188K）：工具集总成
  - `pypinyin/`：拼音标注元数据（多音字修正库，汉典基础上手工维护）
- **gram 发布**：Releases 直接下载模型（wanxiang-lts-zh-hans.gram 就来自这里
  → 本地 `~/repos/RIME-LMDG` 亦可自己构建）；构建教程见
  https://github.com/amzxyz/rime-build-grammar-word-frequency/wiki

**可借鉴动作**：
1. 给我们的词库做"带声调→无调化"或反向重标注 → 改其刷新脚本。
2. 词库清洗：滑动窗口打分思路可直接移植到我们的 gen 脚本（碎片词过滤，
   我们现在只是 >450 权重一刀切）。
3. 多音字：`pypinyin/` 元数据 + `duoyin.dict` 分层兼容，比我们同音异调
   合并更细——优化 gen_pinyin_toneless 的参考。

## 2. rime_wanxiang 万象拼音 —— 带声调方案范本

`~/repos/rime_wanxiang`（https://github.com/amzxyz/rime_wanxiang）

- 消费 RIME-LMDG 词库的完整方案（39.9K 的 wanxiang.schema.yaml 极其完整）。
- `dicts/`：与 LMDG 词库一一对应的发布版（jichu 43M 等）。
- `lua/wanxiang/`：26 个模块，重度工程化（super_filter/super_lookup/
  input_statistics/auto_phrase/userdb/context_reorder…）——**lua 能力上限
  的活标本**；`lua/data/` 持数据。
- 根目录 `AGENTS.md`（10.8K）写了完整开发规范，改它仓库先读。
- 借鉴点：schema 按功能拆 yaml（algebra/english/mixedcode/reverse/symbols）、
  `custom/` 放自定义方案、release-please 自动发版管理词库版本。

## 3. rime-wubi-sentence 墨奇五笔整句 —— 五笔码生产线

`~/repos/rime-wubi-sentence`（https://github.com/gaboolic/rime-wubi-sentence）

- **cn_dicts_wb/**：`base.dict.yaml`(12M) + `ext.dict.yaml`(10M) ——
  现成的**五笔编码大词库**（词组→wb86 码），另有 `8105.dict.yaml`（单字）、
  `41448.dict.yaml`、tiger/yucloud/yustar/ziyuan 等多套码表源。
- **program/generate_dict_code/**：简码自动分配脚本，最有参考价值：
  - `deal_super_2jian.py`：二字词 2+2 码生成（读 base/ext，取前两字前两码）
  - `deal_super_3jian.py` / `deal_super_3jian_no_conflict.py` /
    `deal_super_4jian_no_conflict.py`：简码分配 + **无冲突消解**
  - `deal_super_Njian.py`：N 字词规则
  - `deal_moqi_single_dict.py` / `deal_ice_dict_to_wubi_1.py`：把雾凇拼音
    词库转成五笔编码（**拼音词库→五笔词库的转换管线**）
  - `2字词频表.txt`(8.6M)、`字源1.28版4码.txt`：词频与码表原始数据
- **cn_dicts_cell / cn_dicts_common**：与 frost 共用的"词库单元格"组织
  （cell=可复用分类小表，common=公共表）。
- 借鉴点：我们的传统库/混合库造词（encoder rules）之外，需要**程序化
  生成简码并消解冲突**时直接抄这里；转码脚本结构可复用。

## 4. rime-ice 雾凇拼音 / rime-frost 白霜拼音

`~/repos/rime-ice`（https://github.com/iDvel/rime-ice）
`~/repos/rime-frost`（https://github.com/gaboolic/rime-frost）

- **cn_dicts/** 分层：`8105`(单字) `base`(基础词) `ext`(扩展词)
  `tencent`(16M 腾讯词频) `others`（rime-ice）；frost 进一步拆
  `cn_dicts_cell/`（分类小表）+ `cn_dicts_common/`（公共定义）——
  **多方案共享词库单元格的组织方式**，我们 dicts/ 目录结构可参考。
- frost 带 `essay.txt`(5.6M preset_vocabulary)——不需要 LMDG 时给无码词
  注词频的最简方案。
- lua：两家都带 cold_word_drop（后置词屏蔽）、pin_cand_filter、search.lua
  （tab 反查）、corrector、aux_code（frost 辅助码）——**抄 lua 先看这里**。
- 均带 `recipe.yaml`（东风破 plum 配方）——打包分发词库的机制参考。

## 5. 训练与评测工具

- `rime-build-grammar`（gaboolic）：kenlm+arpa→gram 完整训练管线（已验证）。
- `rime-schema-compare`（gaboolic，也在 RIME-LMDG/rime-schema-compare/）：
  py 自动调 librime 内核批量解码统计——**句子正确率/文字正确率量化评测**，
  是我们 regression_test.py 之外的大规模评测工具；gram 放 vendor/ 对比测。
- `rime-corpus-processing`（JACKCHAN000）：Rust 字符 2-6gram 统计（已编译）。

## 6. 词库构建/优化工作流（面向本项目的操作手册）

### 6.1 从语料到词库（增量扩词）

```
语料(维基/得到/epub/社区)
  → 清洗(RIME-LMDG TXT清洗.py 思路: 去重/长度过滤/乱码剔除)
  → 分词(多线程分词.py 或 jieba)
  → 短语打分清洗(LMDG 滑动窗口脚本思路: 保留真实搭配)
  → 编码标注:
      拼音 → pypinyin 元数据(注意多音字, LMDG/pypinyin) 或汉字转拼音
      五笔 → 单字全码表(egret_wubi_py_wubi_chars) 按字拆词取码
             (deal_ice_dict_to_wubi_1.py / deal_super_2jian.py 逻辑)
  → 权重归一化(log-norm 0-1000, 统一刻度! 见 architecture 坑1)
  → import 到对应表 / 分类 cell 表
  → python3 scripts/regression_test.py --rebuild 回归
```

### 6.2 优化现有词库

1. **碎片词过滤**： 现在 base.small 用 >450 权重一刀切；可升级为
   LMDG 滑动窗口打分（词+上下文搭配证据）。
2. **多音字细分**：LMDG 按"词组+拼音"双键统计 → 拼音表无调化时同音异调
   合并会丢频次信息；优化方向是 gen_pinyin_toneless 保留主导读音加权。
3. **简码冲突消解**：墨奇 no_conflict 脚本的贪心分配（高频优先占位）
   可替代我们 gen_chars 的 ownership 逻辑做交叉验证。
4. **词库瘦身**：按 LMDG 分层原则——jichu 保基础、长词进 lianxiang、
   专业词进分类表；我们的混合库同理可拆 cell。
5. **评测闭环**：任何词库改动 → regression_test（黄金用例）+ 
   rime-schema-compare（大规模句子正确率）双验证。

### 6.3 LM 构建与切换

```
方案A(自训): 语料 → 清洗短句 → rime-corpus-processing(2-6gram)
             → rime-build-grammar(kenlm+arpa) → build_grammar → .gram
方案B(现成): RIME-LMDG Releases 下载 gram（wanxiang-lts-zh-hans 即此来源）
切换: egret.common.yaml grammar/language: <名字>（.gram 文件须在用户目录 build/）
评测: rime-schema-compare AB 对比（gram 放 vendor/ 方案目录）
```

### 6.4 lua 能力借用

- 需要现成功能先抄：日历/农历/大写数字(本项目已有)、反查 search.lua、
  长词优先 long_word_filter、冷词屏蔽 cold_word_drop、辅助码 aux_code。
- 重度功能参考 rime_wanxiang/lua/wanxiang/（userdb 读写、输入统计、
  auto_phrase）——接口用法见 [lua-scripting.md](lua-scripting.md)。
