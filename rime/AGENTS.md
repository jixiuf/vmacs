# egret_wubi_py 五笔拼音混输方案 — 项目状态与工作入口

写于 2026-09-24 调试会话。目的：五笔(形码)+拼音混流、整句 LM 组句的 Rime 方案。
新会话从这里继续。

## 先读 docs/（深度知识库，读码与原理问题以它为准）

本目录（`~/vmacs/rime/docs/`）下有系统性 Rime 知识库：

- `docs/README.md` —— 索引 + 跨机使用约定（先查 ~/repos，没有则 clone）+ 仓库索引
- `docs/rime-architecture.md` —— 引擎管线/组件模型/translator 选项原理
- `docs/schema-config-reference.md` —— schema 配置全解
- `docs/dict-reference.md` —— 词库格式/编译/造词/用户词典
- `docs/lm-grammar.md` —— octagram/poet/gram/训练管线
- `docs/lua-scripting.md` —— librime-lua API 与实战范式
- `docs/deployment-debugging.md` —— 部署流程/调试方法/踩坑决策树
- `docs/reference-projects.md` —— 雾凇/万象/墨奇/白霜/LMDG 分析与词库工作流

本文件只保留**项目当前状态**：架构现状、待办、验证基线、本项目特有陷阱。
跨机注意：Rime 用户目录各平台不同（macOS `~/Library/Rime`、Linux 见
`docs/deployment-debugging.md` §2 平台表）；本文中目录以 macOS 本机为准。

## 架构（v6）

```
engine.translators:
  - table_translator@wubi_py_wubi   ← 传统结构五笔库(简码/全码/词组显式码) — 简码置顶
  - script_translator@translator    ← 混合库(五笔字词+拼音音节) + octagram LM 组句 — 混流核心
  (punct/lua date/lunar/number/unicode 等)
```

- 传统库根包装 `egret_wubi_py_wubi.dict.yaml`：薄包装(import_tables) + encoder/rules(造词规则)
  - 数据: `dicts/egret_wubi_py/egret_wubi_py_wubi_chars.dict.yaml`(简码+全码), `dicts/egret_wubi_py/egret_wubi_py_wubi_words.dict.yaml`(词组码)
  - 用户词组: `dicts/egret_wubi_py/egret_wubi_py_user_phrases.dict.yaml`(手工维护, 生不逢时=tgtj 900)
- 混合库 `egret_wubi_py.dict.yaml`(根目录, import_tables 引 dicts/ 下各数据文件)
  - 五笔字表: `dicts/egret_wubi_py/egret_wubi_py_chars.dict.yaml`(ownership 版: 二级简码 owner 保留逗号 token, 其余降级 3码/全码)
  - 五笔词组: `dicts/wb86_wangxiang_cn_dicts/base.small.dict.yaml`(五笔码+万象权重, >450 过滤)
  - 拼音字表: `dicts/egret_wubi_py/egret_wubi_py_pinyin_8105.dict.yaml`(无调化副本, 同音异调合并)
  - 拼音词库: `dicts/egret_wubi_py/egret_wubi_py_pinyin_base_small.dict.yaml`(无调化副本)
- LM: `wanxiang-lts-zh-hans.gram`(官方 200MB) 与 `jixiuf-zh-hans.gram`(自训 83.6MB) 可切换
  (`egret.common.yaml` 的 grammar/language)。当前=wanxiang。

## 仓库与工具

- 方案与词库: `~/vmacs/rime`(git) — commit f679a4ef 之后有大量改动
- Rime 用户目录: **macOS 就是 `~/Library/Rime`**（Squirrel 标准）；本机当前用
  `~/rime-test` 作调试/回归目录（关键 yaml 软链回本仓库，
  user.yaml previously_selected_schema=egret_wubi_py）。Linux 目录见
  docs/deployment-debugging.md §2 平台表。
- librime 源码: `~/repos/librime`(fork 1.17.0, 含 plugins/librime-octagram;
  **build/ 不在 git 里，用前先 `make deps && make merged-plugins`**；
  含 Sort patch 未提交改动)
- librime-lua: `~/repos/librime/plugins/librime-lua`(工作区为空分支，需
  `git checkout master`；lua 开发见 docs/lua-scripting.md)
- liberime(Emacs): `~/repos/liberime`, 模块 src/liberime-core.dylib @rpath→repos/librime/build/lib
- rimel(用户的 rimel.el): `~/repos/rimel`; rimel-convert-string-at-point 走 liberime-process-key
- 其它仓库(雾凇/万象/墨奇/白霜/RIME-LMDG 等): 先查 `~/repos/`，没有则按
  docs/README.md 索引 clone
- 测试: `cd <Rime用户目录> && printf '<码>\n\n' | ~/repos/librime/build/bin/rime_api_console`
  (console 以 cwd 为用户目录; 务必先确认 user.yaml 的 previously_selected_schema，
  否则会话恢复成别的方案)
- **回归测试: `python3 scripts/regression_test.py`** —— 17 条黄金用例
  (简码/字全码/词组/混流/LM句/用户词组)自动比对首候选, 已知 LM 级差异
  (黄金句才最/都不可能)单独报告不计失败。`--rebuild` 先重新部署
  (含预热防维护竞态), `--clean` 清用户词典, `--dir` 测其他环境。
  改词库/gen脚本/schema 后必跑; 新增修复时把用例加进 CASES 表。
- 评测/训练工具: gaboolic/rime-schema-compare; gaboolic/rime-build-grammar
  (kenlm+arpa→gram); JACKCHAN000/rime-corpus-processing(Rust 字符 2-6gram;
  此前编译产物在 /tmp/**已可能丢失**, 重编译即可)。词库构建/优化工作流见
  docs/reference-projects.md §6。
- 训练管线详见 docs/lm-grammar.md §3。

## 生成脚本

`scripts/gen_wubi_py_unified_dicts.py`:
- gen_base_small(): wb86_wangxiang base.small(>450 过滤碎片词)
- gen_chars(): 混合库字表 chars_wubi_py_x035(知频 log-norm×0.35 + WEIGHT_KNEE=600 膝点
  + 二级简码 ownership: 传统 level2 表的 owner 保留逗号 token, 其余降级 3码/全码;
  与二字词 2+2 码冲突的全码不发条目; **一级简码 plain letter token 已移除**)
- gen_wubi_traditional(): 传统库 chars/words 两数据文件 + 不再生成根包装
  (**根包装 wubi_py_wubi.dict.yaml 为手工维护**, 含 encoder/rules 造词规则)
- gen_pinyin_toneless(): 拼音字/词表无调化副本(同音异调合并→权重稳定排序)
- 注意: gen_pinyin_merged/gen_jianma2 已废弃待删; reweight_words.py 的
  WUBI_DICT 路径要改成 dicts/egret_wubi_py_wubi_words.dict.yaml

## 训练管线（自训 LM，已验证可产出可用 gram）

语料(清洗→10-20字短句→去重) → 字符 2-6gram 统计(rime-char-gram-builder, Rust)
→ build_grammar(librime-octagram 自带, 需 rpath 指向自编译 librime) → .gram
语料来源: 中文维基 3 分卷(4690万字) + 得到课程文稿 51965 个 md(1.06亿字) + epub 抽样。
当前 gram: jixiuf-zh-hans.gram 83.6MB/2.1亿 n-gram, min-count 30/15/8/5/3。
经验: 语料配比=LM 口味; 电子书网文语料会强化「才最」类搭配; 想翻转特定 LM 裁决
需要该表达在语料中的证据量压过竞争证据, 否则用全码强制消歧。

## 2026-09-26 调试会话新增发现（详见 docs/lm-grammar.md §5-6, dict-reference.md §7-8）

- 拼音污染修复: 传统库补全候选(type=completion)经 dynamic_translator 分组
  挪到流尾, 精确码>拼音>补全 三层就位, 补全能力保留(y 列 y 开头字)。
  **关键认知: dynamic_translator 是单条翻译流, Menu 不在流内按 quality 重排;
  Sentence 的 quality 是 log 尺度与 phrase 的 exp 尺度不可比, 不可全局排序。**
- user_dict 同名共享: custom_phrase(tabledb)与 translator 同名 → db_pool 按
  名共享, 后端由创建顺序决定; 分离名会失去 ckwan→台湾 的学习收益(已实测)。
- ckwan→台万 根因: wanxiang gram 是词级 n-gram, 逐字组合查不到二元组
  (Query(台,湾)=-12); 字级 gram(essay-bgc)下 +6.31 → 台湾 ✓。
  修复路线: 换字级 gram(已验证 20/20) 或 gen 脚本批量生成混合拼写词条
  (台湾 ck wan, 已原型验证)。

### 2026-09-26 gram 切换与失败类分析（续）

- 默认 grammar 已切 `zh-hans-t-essay-bgc`(lotem 20260712, 27.7MB, 官方 release),
  20/20 回归通过, ckwan→台湾 ✓。rimeinn 寄存的 3.9MB 是旧版, 勿用。
- fcitx-zh-hans.gram(25MB) 实测 10/21 且出坏句, 不用。
- 剩余混流失败(可就/生姜 类)根因不是 gram 弱, 而是两类机制:
  (a) **传统词组精确匹配压过 grammar 组合**(可就 skyi 605 在传统词库 →
      first_early 置顶, 设计使然=形码确定性; grammar 无从参与);
  (b) script_translator 有 exact match 时默认不组句 → 已加 librime 实验补丁
      `translator/always_make_sentences`(config 门控, 默认关, 未提交),
      配合 dynamic_translator 新分组(first > script > custom_phrase > 补全)
      可让混合库组合与 grammar 裁决, 但传统词组置顶仍是产品级权衡,
      待决策是否让 LM 仲裁。

## 已确认根因（勿重复踩坑；原理细节见 docs/）

1. 权重量级冲突: 知频(0-1500万)与万象(0-1000)混排→LM 失效。统一 log-norm 0-1000。
2. translator/dictionary 不支持子目录路径(静默失败)——只有 import_tables 支持。
   传统五笔库因此必须在用户目录根。
3. 一字多音节拼写(prism 内)的首候选取决于 prism 构建顺序(非确定性, 目录不同结果不同)。
   解法=词库层让每个简码拼写只对应唯一音节(ownership), 或 translator 顺序+权重置顶。
4. 用户词典自学习: 选中过的句子会成为高优先级 user phrase 并给句中词调频,
   会压制后续修正——测前清 userdb(egret_wubi_py.user.dict.userdb 等目录)。
5. db_class: user_dict 必须 userdb(LevelDB); tabledb 只读(调频/造词全失效)。
6. UnityTableEncoder 的造词规则读自词库文件头的 encoder/rules(dict settings),
   不是 schema——词库头没有 rules 则造词静默失效。
7. import_tables 引用名不可带 .dict.yaml 后缀; translator/dictionary 同理不支持子目录。
8. 调试时 console 的 maintenance 会因缺 symbols/punctuation 等失败并回退旧产物,
   导致"改了没生效"假象——把 SharedSupport 的 symbols/key_bindings/punctuation.yaml
   拷入用户目录可让 maintenance 成功(注意: 拷 symbols.yaml 会触发 deployer abort,
   原因未查明; 不拷而靠 fallback 也能工作)。
9. librime prism 构建非确定性(同输入不同目录→不同 prism), 会翻转接近的 LM 裁决;
   Sort-on-lookup patch 可缓解(见 ~/repos/librime 工作区 git diff; **本仓库已无
   patches/ 目录, patch 文件未存档**), 用户暂不接受改 librime。
10. **translator 的配置块键必须与 @name_space 完全一致**, 否则静默回退默认词典
    (schema id); 且 translator/dictionary 不支持子目录路径。两者叠加导致
    first_translator 长期实际加载的是混合库而非传统库(简码失效的真正根因)。
11. **传统库不要按 2+2 词码冲突过滤字全码**: 同码条目按权重自然裁决
    (等tffu 780>徒增502、中国900>䟧≈1), 硬过滤会杀死高频字全码
    (曾致 tffu 无法输出 等, 等 只能从混合库 tff 补全路径出来 → 选字残留 u、
    需二次空格)。混合库的同类过滤是为 LM 分段设计, 保留。
    注意: gen 脚本曾有两处相同的 word_codes 定义(gen_chars/gen_wubi_traditional
    各一), 改其中一处时极易删错——已改为各自独立且语义不同的两段。

## 未完成 / 下一步

1. ~~一级/二级简码首候选在真实环境仍缺~~ **已解决(2026-09-24)**。根因有二：
   (a) 配置块键与 @tag 不匹配：`first_translator: table_translator@egret_wubi_py_wubi`
   找的是 `egret_wubi_py_wubi:` 配置块，但配置写在 `wubi_py_wubi:` 下 → 翻译器拿不到
   配置回退到默认词典(混合库)；(b) `dictionary: dicts/...` 子目录路径静默失败。
   修复：根包装改名 `egret_wubi_py_wubi.dict.yaml`(在用户目录根, gen 脚本生成)，
   配置块键改为 `egret_wubi_py_wubi:`，`dictionary/prism: egret_wubi_py_wubi`(无子目录)，
   dependencies 去重，user_dict 改名 `egret_wubi_py_wubi.user.dict`。
2. LM 级残留(虚词/分词): 他们国有→他们了, 时间/事件, 发生 分词, 警惕/晶体 ——
   语料配比与 grammar penalty 微调空间; rime-schema-compare 可做系统评测。
3. ~~gen 脚本清理~~ **已完成(2026-09-24)**: gen_jianma2 已删；重复 strip_tone 已删；
   gen_chars/gen_pinyin_toneless 输出路径与 out_name 全部改为 egret_ 新名(此前重跑
   会在 dicts/ 重新生成 4 个旧名重复文件)；reweight_words.py WUBI_DICT 路径已修；
   过时 print 标签已修。gen 现在完全自洽：重跑输出与现库 byte-identical(仅头注释差异)。
4. librime Sort patch: ~/repos/librime 工作区已应用(未提交)，patch 文件需从
   `git -C ~/repos/librime diff` 导出存档后放入本仓库，用户未决定去留。
5. 全部改动待 commit(建议拆分: 词库生成/训练管线/schema 架构/docs 各一个 commit)。

## 验证基线（当前真实环境实测）

e→有✓ q→我✓ yi→就✓ yl→为✓ kl→另✓ de→胡✓ (简码修复后全部就位)
tffu→等✓(字全码不再被词码冲突过滤, 修复前等在第5位且选字残留u)
khlg→中国✓ xciy→经济✓ kdyc→顺序✓ trwu→我们✓ trwuchifan→我们吃饭✓(混流)
jfwaguangfnrt→时代广场✓ tgtj→生不逢时(造词✓, 用户词组 tgtj 900)
jintiandetianqizenmeyang→今天的天气怎么样✓ tamenlailehenduoren✓ wojintianmeiyoukong✓
wanxiang 模型下黄金句=才最✗(自训模型=都✓, 但通用性略差, 见 AB: 82 vs 85/92)

## 陷阱清单
- Rime 用户目录（macOS 即 `~/Library/Rime`；本机调试目录 `~/rime-test`）下根包装
  文件若为拷贝(非软链), 仓库重新生成后必须同步过去, 否则部署用的是旧
  import_tables(曾因 dicts/ 子目录化后包装未同步 → 传统库编译失败,
  简码/造词全丢)。egret_wubi_py.dict.yaml 为软链, egret_wubi_py_wubi.dict.yaml
  已改软链。

- import_tables 引用名不可带 .dict.yaml 后缀
- user.yaml previously_selected_schema 会被 console 运行改写, 测前重置
- 软链目录内 ln -sf 会自引用覆盖源文件(已毁过 dynamic_translator.lua, 靠上下文恢复)
- rime_deployer 偶发静默; 验证产物用文件数/md5
- rm -rf build 后首次 console 运行可能异常(维护竞态), 跑两次再采信
- 编辑含中文行尾注释的文件后必须 git diff 复核
