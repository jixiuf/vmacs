# Rime 知识库（docs/）

本目录是对 librime 源码（`~/repos/librime`，fork 自 rime/librime 1.17.0）与 Rime
输入法体系的系统性总结，供 AI agent 在修改 schema / 词库 / 语言模型时快速定位。

## 文档索引

| 文件 | 内容 |
|------|------|
| [rime-architecture.md](rime-architecture.md) | 引擎全景：按键处理管线、组件模型（Ticket/Registry）、Segment/Translation 数据流 |
| [librime-source-map.md](librime-source-map.md) | librime 源码地图：目录/文件职责、读码入口、插件（lua / octagram） |
| [schema-config-reference.md](schema-config-reference.md) | schema 配置全解：engine 各组件、translator 常用/专用选项、__include/__patch |
| [dict-reference.md](dict-reference.md) | 词库：.dict.yaml 格式、编译产物（table/prism/reverse）、import_tables、造词规则、用户词典 |
| [lm-grammar.md](lm-grammar.md) | 语言模型：octagram 插件、poet 组句算法、grammar 配置、gram 文件、训练管线 |
| [deployment-debugging.md](deployment-debugging.md) | 部署流程、build 产物、调试方法（rime_api_console 等）、本项目踩坑清单 |
| [lua-scripting.md](lua-scripting.md) | librime-lua：挂载方式、API 速查、实战范式（取自雾凇/万象/墨奇）、陷阱 |
| [reference-projects.md](reference-projects.md) | 参考项目分析（雾凇/万象/RIME-LMDG/墨奇/白霜）及词库构建、优化、评测工作流 |

## 本机环境速查（跨机注意事项）

> 本文档可能在多台机器上使用（macOS / Linux 混用）。所有 `~/repos/...` 路径
> 按「先查、没有则 clone」约定处理（见下方索引）；Rime 用户目录**各平台不同**
> （macOS `~/Library/Rime`、Linux ibus `~/.config/ibus/rime`、
> fcitx5 `~/.local/share/fcitx5/rime`，详见 deployment-debugging.md §2 平台表）。

- librime 源码: `~/repos/librime`（自编译 fork；**build/ 产物目录不在 git 里，
  用前先 `make deps && make merged-plugins`**；含 Sort patch 未提交改动）
- octagram 插件源码: `~/repos/librime/plugins/librime-octagram`（已并入 plugins 构建体系）
- liberime (Emacs): `~/repos/liberime`
- rimel: `~/repos/rimel`
- 本项目方案仓库: `~/vmacs/rime`（关键 yaml 软链到 Rime 用户目录）
- Rime 用户目录/测试目录：macOS 下为 `~/Library/Rime`（正式）与 `~/rime-test`
  （调试）；Linux 下按平台表查找对应目录，方案仓库同理软链过去
- 调试工具: `~/repos/librime/build/bin/rime_api_console`（构建后才有，见
  deployment-debugging.md §2/§6）
- 其它仓库查找约定：见下方“相关仓库索引”——先查 `~/repos/`，没有则 clone。

## 相关仓库索引（先查 ~/repos，没有则 clone 到 ~/repos）

**约定**：所有相关源码/数据仓库统一放在 `~/repos/`。下表“本地路径”是首选查找
位置；本机没有时，从“远程”栏 clone 到该路径再研究：

```bash
git clone --depth 1 <远程URL> ~/repos/<名字>
```

| 仓库 | 远程 | 本地路径（查找顺序第一） | 用途 |
|------|------|--------------------------|------|
| librime | https://github.com/rime/librime | `~/repos/librime` | 核心引擎（本项目已 fork 定制，含 plugins/librime-lua、librime-octagram） |
| librime-octagram | https://github.com/lotem/librime-octagram | `~/repos/librime/plugins/librime-octagram` | 八股文语法(LM)插件 |
| librime-lua | https://github.com/rime/librime-lua | `~/repos/librime/plugins/librime-lua` | lua 脚本插件（**本机工作区为空分支，需 `git checkout master`**；API 见 lua-scripting.md） |
| rime-essay | https://github.com/rime/essay | `~/repos/rime-essay` | essay.txt 八股文词频表（preset_vocabulary） |
| rime-prelude | https://github.com/rime/prelude | `~/repos/rime-prelude` | default.yaml / punctuation.yaml / symbols.yaml 官方预设 |
| plum | https://github.com/rime/plum | `~/repos/plum` | 东风破：方案包下载管理 |
| OpenCC | https://github.com/BYVoid/OpenCC | `~/repos/librime/deps/opencc` | 繁简转换（librime deps 子模块） |
| marisa-trie | https://github.com/s-yata/marisa-trie | `~/repos/librime/deps/marisa-trie` | marisa trie（librime deps 子模块） |
| darts-clone | https://github.com/s-yata/darts-clone | `~/repos/librime/include/darts.h` | DARTS trie（prism 用，头文件已随 librime 附带） |
| squirrel | https://github.com/rime/squirrel | `~/repos/squirrel` | macOS 前端 |
| weasel | https://github.com/rime/weasel | `~/repos/weasel` | Windows 前端 |
| fcitx5-rime | https://github.com/fcitx/fcitx5-rime | `~/repos/fcitx5-rime` | Linux fcitx5 前端 |
| liberime | https://github.com/m2ym/liberime | `~/repos/liberime` | Emacs 动态模块前端 |
| emacs-rime | https://github.com/tumashu/emacs-rime | `~/repos/emacs-rime` | Emacs rime 前端（emacs 包） |
| rime-ice 雾凇拼音 | https://github.com/iDvel/rime-ice | `~/repos/rime-ice` | 拼音方案+词库分层组织范本、lua 库（分析见 reference-projects.md §4） |
| rime_wanxiang 万象拼音 | https://github.com/amzxyz/rime_wanxiang | `~/repos/rime_wanxiang` | 带声调拼音方案（LMDG 词库消费方，lua 重度工程化） |
| rime-wubi-sentence 墨奇五笔整句 | https://github.com/gaboolic/rime-wubi-sentence | `~/repos/rime-wubi-sentence` | 五笔码标注/简码自动分配脚本（reference-projects.md §3） |
| rime-frost 白霜拼音 | https://github.com/gaboolic/rime-frost | `~/repos/rime-frost` | 词库 cell 组织、essay.txt、辅助码 lua |
| RIME-LMDG | https://github.com/amzxyz/RIME-LMDG | `~/repos/RIME-LMDG` | 词库/LM 构建工厂：语料清洗、打分清洗、编码转换脚本、gram 发布（reference-projects.md §1） |
| rime-build-grammar | https://github.com/gaboolic/rime-build-grammar | `~/repos/rime-build-grammar` | 训练管线：kenlm + arpa → .gram |
| rime-schema-compare | https://github.com/gaboolic/rime-schema-compare | `~/repos/rime-schema-compare` | 方案系统评测工具（句子/文字正确率） |
| rime-corpus-processing | https://github.com/JACKCHAN000/rime-corpus-processing | `~/repos/rime-corpus-processing` | Rust 字符 2-6gram 语料统计 |
| rime-octagram-data | https://github.com/lotem/rime-octagram-data | `~/repos/rime-octagram-data` | 官方八股文语料/gram 产物 |

## 与本项目相关的关键结论（勿重复踩坑）

1. translator 的配置块键必须与 `@namespace` 完全一致，否则静默回退默认词典。
2. `translator/dictionary`、`prism` 不支持子目录路径（静默失败）；只有词库头部的
   `import_tables` 可以引用子目录下的词表。
3. 知频(0-1500万)与万象(0-1000)权重量级不可混排 → 会使 LM 组句失效。
4. 用户词典自学习会以高优先级 user phrase 调频，测试前必须清 userdb。
5. prism 构建非确定性（同输入不同目录产物不同），接近的 LM 裁决可能被翻转。
