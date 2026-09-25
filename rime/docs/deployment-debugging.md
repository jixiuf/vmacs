# 部署流程与调试方法

> 源码依据：`src/rime/deployer.cc`、`src/rime/lever/deployment_tasks.cc`、
> `tools/rime_api_console.cc`、`src/rime_api_impl.h`。

## 1. 部署任务序列

`Deployer::Run`（deployer.cc）按序执行任务（lever/deployment_tasks.h）：

```
InstallationUpdate          # installation.yaml（distribution_code_name 等）
WorkspaceUpdate             # 同步 shared data → 用户目录（preference 复制策略）
SchemaUpdate (per schema)   # 编译单个 schema：
                              1. config 编译（__include/__patch/custom）→ build/<id>.yaml
                              2. dict 编译（DictCompiler）→ table/prism/reverse
ConfigFileUpdate            # default.yaml、punctuation.yaml 等独立编译
PrebuildAllSchemas
SymlinkingPrebuiltDictionaries
UserDictUpgrade / UserDictSync
BackupConfigFiles / CleanupTrash
```

要点：
- **增量判定**：config 与 dict 都有 checksum；但「产物损坏、失败半途、
  拷贝旧文件」不会被 checksum 察觉。怀疑产物不对时直接
  `rm -rf build/<相关产物>` 强制重建。
- rime_deployer 偶发**静默失败**：验证产物用文件数 + md5，不要只看退出码。
- 维护竞态：`rm -rf build` 后首次 console 运行可能异常（maintenance 与
  session 初始化竞态），**跑两次再采信**。
- symbols.yaml 拷入用户目录会让 deployer abort（原因未查明）；不拷而靠
  fallback 也能工作——调试缺 symbols 时别直接拷。

## 2. 调试工具

### Rime 用户目录（各平台不同！）

librime 本身不定义用户目录：由**前端**通过 `RimeTraits` 传入（setup.cc
`SetupDeployer`）。独立工具（rime_api_console / rime_deployer 不带参数时）
默认用**当前工作目录**（deployer.cc `user_data_dir(".")`）——所以调试时必须
先 `cd` 到目标用户目录。

| 平台/前端 | 用户目录 | 共享数据目录 |
|-----------|----------|--------------|
| macOS Squirrel | `~/Library/Rime` | `/Library/Input Methods/Squirrel.app/Contents/SharedSupport` |
| Windows Weasel | `%APPDATA%\Rime` | Weasel 安装目录 `\data` |
| Linux ibus-rime | `~/.config/ibus/rime` | `/usr/share/rime-data` |
| Linux fcitx5-rime | `~/.local/share/fcitx5/rime` | `/usr/share/fcitx5/rime-data`（发行版可能为 `/usr/share/rime-data`） |
| Emacs liberime | 由 elisp 侧指定（rimel 配置） | 同上 |

本项目在 macOS 上的两个工作目录（其他机器可能不同，以实际为准）：
- `~/rime-test`：调试/回归测试目录（console 与 regression_test 默认在这里跑）
- `~/Library/Rime`：正式目录（Squirrel），关键 yaml 软链回 `~/vmacs/rime`

### rime_api_console（最常用）

前提：`~/repos/librime/build/` 可能不存在（不在 git 里，换机器常见），先用
`cd ~/repos/librime && make deps && make merged-plugins` 构建（§6）。

```bash
cd ~/rime-test    # cd 到目标用户目录（console 以 cwd 为用户目录，见上表说明）
# 先重置 user.yaml，防止会话恢复成别的 schema：
#   previously_selected_schema: egret_wubi_py
printf 'tffu\n\n' | ~/repos/librime/build/bin/rime_api_console
```

- 输出即按键后的候选流（每一屏候选 + commit）。`Ctrl+g`/`Ctrl+e` 等控制键
  可模拟选重翻页；脚本测试用 `printf '<码>\n\n'`（空行=回车上屏）。
- 加日志：`GLOG_v=2`（debug log，含 BuildSyllableGraph、Lookup、Grammar 打分）：
  ```bash
  GLOG_v=2 printf 'trwuchifan\n\n' | rime_api_console 2>&1 | grep -E 'Lookup|grammar'
  ```
- console_test：本项目自制对比脚本（~/rime-test/console_test.cc）。

### 其他

- `rime_deployer --build <用户目录> [共享目录]`：全量部署；
  `rime_deployer --compile <schema.yaml> <用户目录> [共享目录]`：单 schema 编译
  （工具在 build/bin/ 下，构建后才有）。
- `rime_patch` / `rime_dict_manager`：配置补丁、用户词典导出合并（同上）。
- 回归测试：`python3 scripts/regression_test.py [--rebuild] [--clean] [--dir X]`
  （17 条黄金用例自动比对首候选；改词库/gen/schema 后必跑，新增修复加 CASES）。

## 3. 排障决策树（改了没生效？）

```
改的是 yaml 配置？
  → build/<name>.yaml 里 grep 新键是否存在
     ├─ 不存在 → patch/custom 没应用：查文件名（default.custom.yaml 命名）、
     │           __patch 路径语法、yaml 语法错误（deployer 日志）
     └─ 存在   → 运行时没读：查配置块键是否与 @name_space 一致（1号坑）
改的是词库？
  → md5 build/<dict>.table.bin 是否变化
     ├─ 没变 → import_tables 路径错误（静默跳过）/ checksum 未察觉 → rm build 产物
     └─ 变了 → prism 是否也重建；音节歧义（prism 非确定性，3号坑）
 translator 加载对了？
  → GLOG_v=2 看 "[ComponentTiming] prescription=..." 的 klass/name_space
     与词典名（dictionary 键读错会回退 schema_id 作词典名 → 缺词）
```

## 4. 本项目踩坑清单（完整版，血泪史）

1. **权重量级冲突**：知频(0-1500万)与万象(0-1000)混排 → LM 失效。
   统一 log-norm 0-1000。
2. **translator/dictionary 不支持子目录路径**（静默失败）——只有词库头
   import_tables 支持。传统五笔库根包装必须在用户目录根。
3. **一字多音节拼写的 prism 首候选非确定**：词库层 ownership 消歧
   （每个简码拼写只对应唯一音节）。
4. **用户词典自学习压制修正**：测前清 userdb（`egret_wubi_py.user.dict.userdb`
   等目录）。
5. **db_class 必须 userdb**：tabledb 只读，调频/造词全失效。
6. **造词规则读自词库头 encoder/rules**，不是 schema；词库头没有 rules
   则造词静默失效。
7. **import_tables 引用名不可带 .dict.yaml 后缀**；translator/dictionary 同理
   不支持子目录。
8. 维护竞态与 symbols.yaml abort（见 §1）。
9. **librime prism 构建非确定性**：patches/librime-0001-dict-entry-iterator-sort-on-lookup.patch
   （Sort chunks）可缓解，工作区已应用未提交，去留未定。
10. **配置块键必须与 @tag 完全一致**，否则静默回退默认词典（schema id）。
    曾致 first_translator 长期加载混合库、简码失效。
11. **传统库不要按 2+2 词码冲突过滤字全码**：同码条目按权重自然裁决，
    硬过滤会杀死高频字全码（tffu 等事件）。

## 5. 软链同步陷阱

- **Rime 用户目录**（macOS 即 `~/Library/Rime`，Linux 见 §2 平台表）下的文件若为
  拷贝（非软链），仓库重新生成后必须同步，否则部署用旧产物（曾因 dicts/
  子目录化后根包装未同步 → 传统库编译失败）。
- 在软链目录内 `ln -sf` 会自引用覆盖源文件（毁过 dynamic_translator.lua）。
  正确姿势：先 `rm` 再 `ln -s`。
- 用户目录下根包装 `egret_wubi_py.dict.yaml`、`egret_wubi_py_wubi.dict.yaml`
  均应为软链 → `~/vmacs/rime/`（其他平台同理，指到本仓库的检出路径）。

## 6. librime 本地构建

```bash
cd ~/repos/librime
make deps             # 首次构建前：拉取/编译 deps/ 子模块
make merged-plugins   # 或 make release（非合并插件）；产物在 build/
# 产物: build/lib/librime*.dylib + build/bin/{rime_api_console,rime_deployer,
#       rime_dict_manager,rime_patch,rime_table_decompiler}
# liberime 的 dylib rpath 指向 ~/repos/librime/build/lib → 重编译即对 Emacs 生效
```

- build/ 不在 git 里，**换机器/被清理后需要重新构建**。
- Makefile 目标：`deps` / `release`（BUILD_MERGED_PLUGINS=OFF）/
  `merged-plugins`（=ON，plugins/* 全部并入单库）/ `debug`（Debug + 日志）。
- 依赖子模块在 `deps/`（glog/leveldb/yaml-cpp/googletest/marisa-trie/opencc）。
- Sort patch（dict-entry-iterator-sort-on-lookup）当前已应用于工作区、未提交；
  去留未定，patch 存档于本仓库 `patches/`。
