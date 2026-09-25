# librime-lua 脚本开发指南

> 本插件源码：`~/repos/librime/plugins/librime-lua`。**注意**：本机该目录
> checkout 处于空分支状态，需要 `git checkout master`（或 `main`）才能看到
> 源码；权威 API 文档：https://github.com/hchunhui/librime-lua/wiki/Scripting
> （本项目 `egret_wubi_py.schema.yaml` 头部也注释了这个链接）。
> 实战范例优先看本项目 `lua/`、`~/repos/rime-ice/lua/`、
> `~/repos/rime_wanxiang/lua/wanxiang/`（super_* 系列是最重的实战代码）。

## 1. 挂载方式

```yaml
# schema 的 engine 里，lua 组件与原生组件混排：
engine:
  processors:
    - lua_processor@*select_character     # processor
    - lua_processor@*sbxlm.key_binder     # 模块路径（含子目录）也行
  segmentors:  [ ..., lua_segmentor@*custom_seg ]
  translators:
    - lua_translator@*date_translator     # 读配置块 date_translator: 下的参数
    - lua_translator@*wanxiang.lib        # 亦可不带 *（见下）
  filters:
    - lua_filter@*long_word_filter
    - lua_filter@*reduce_emoji_filter
```

- `@name`：从该组件的 `init(env)` 里用 `env.name_space` 读同名配置块。
  **name_space 同样决定配置位置，与原生 translator 规则一致**。
  万象风格在 init 里 `env.name_space:gsub('^*','')` 去掉 `*` 前缀。
- `@*name`：把 name_space 设为 `*name`；`@` 后直接写函数名（不写 `*`）则
  name_space 为空——惯例是全用 `*`。
- 组件注册：新版 librime-lua 自动扫描 `lua/` 目录；`rime.lua`（全局
  `xxx_translator = function(...)`）是旧机制，仅做兼容。模块可以用
  `require` 互相引用（如 `wanxiang.super_filter`）。

## 2. 组件接口

| 组件类型 | init(env) | 主函数 | 说明 |
|----------|-----------|--------|------|
| processor | 可选 | `func(key, env)` 返回 0/1/2（kNoop/kAccepted/kRejected） | 拦截/改写按键 |
| segmentor | 可选 | `func(seg, env)` 返回 bool，调用 `seg.tags` / `segmentation:add_segment(seg)` | 打 tag 切分 |
| translator | 可选 | `func(input, seg, env)` 用 `yield(cand)` 逐个产出候选 | 最常用 |
| filter | 可选 | `func(tr, env)` 迭代 `tr:peek()`/`tr:pop()`/`yield(cand)` | 改候选流 |

- `init(env)`：组件实例创建时调用；`fini(env)`：销毁时调用（清理句柄）。
- env 里常驻：`env.engine`、`env.name_space`、自己存的任意状态。
- 翻译器示例骨架（本项目 lua/date_translator.lua）：

```lua
local M = {}
function M.init(env)
    env.name_space = env.name_space:gsub('^*', '')
    M.date = env.engine.schema.config:get_string(env.name_space .. '/date') or 'rq'
end
function M.func(input, seg, env)
    if seg:has_tag(M.date_tag) then yield(cand) end
end
return M
```

## 3. 核心 API 速查

```
env.engine
  .schema            → Schema；.schema_id / .schema_name / .config
  .context           → Context
  .commit_history    → CommitHistory（前文，整句上下文用）
  .process_key(key)  → 回灌按键
  .commit_text(str)  → 直接上屏

env.engine.schema.config (Config)
  :get_string/get_int/get_bool/get_double("path/to/key")
  :get_list("path") → 遍历 item.type=="kString" 时 item.value
  :get_item(...)    → ConfigMap/ConfigList 节点

env.engine.context (Context)
  .input / .caret_pos / .composition / .commit_history
  :get_option(name) / :set_option(name, bool)   # 与 switches 联动
  :get_property / :set_property
  :refresh_non_confirmed_composition()

seg (Segment)
  .start / ._end / .length / .tags / .status
  :has_tag("abc")          # lua_translator 里决定是否响应

cand (Candidate) 构造与属性
  Candidate(type, start, end, text, comment)   -- 构造函数
  cand.quality = 1000000        -- 权重加成（排位）；type="self" 时可调
  cand.preedit / cand.comment / cand.text / cand.type
  SimpleCandidate / ShadowCandidate(遮盖文本)  / Phrase

yield(cand)     -- translator/filter 产出
tr:peek()/pop() -- filter 里消费上游翻译流（注意 exhausted）
```

常用扩展对象（按需查 wiki）：`Memory`（挂 user_dict 调频）、`Opencc`、
`ReverseDb`/`ReverseLookup`（查 .reverse.bin）、`Dictionary`/`UserDictionary`
（lua 直接查词，万象 input_statistics/auto_phrase 靠它）、`Db`（LevelDB），
`log.info/warning/error`、`os.time/io.open`（标准 lua）。

## 4. 实战范式（从参考项目提炼）

- **translator**：触发词模式（rq/sj/date、/js 计算器、R 大写数字）——
  `seg:has_tag()` 或 `input` 前缀匹配 → yield 自造候选，`cand.quality` 控制
  排位。参考：本项目 date/number/unicode/calculator.lua；rime-ice calc_translator。
- **filter**：后处理模式——长词优先 long_word_filter（按长度调 quality）、
  emoji 削减、英文候选降权 reduce_english_filter、以词定字 select_character。
  注意 filter 必须把上游候选完整透传，否则吞候选。
- **processor**：按键拦截——select_character（C-h/C-l 以词定字）、
  key_binder 复刻、并击补充。
- **重模块化（万象 super_*）**：wanxiang/librime.lua 一次注册多组件、
  lua 持有数据文件（lua/data/）、userdb 读写（userdb.lua）、
  input_statistics.lua（键忆统计）——复杂逻辑全下沉 lua 的样板。
- **辅助码（frost aux_code/）**：filter 读码表数据文件给候选加注释/过滤，
  是"五笔混流提示"类需求的现成参考。

## 5. 调试与陷阱

- **lua 目录软链自引用**：在软链目录里 `ln -sf` 会覆盖源文件（本项目
  dynamic_translator.lua 被毁过）。先 `rm` 再 `ln -s`。
- `yield` 出的候选 type 建议 `"self"` 或留空；type 会被 simplifier 等组件
  按 `excluded_types` 过滤。
- translator 的 `func` 会在**每次 Compose** 都被调用（高频），重逻辑要缓存
  在 `env` 里（init 建句柄，func 里复用）；大词表用 `local` 闭包持有。
- filter 里 `while(true)` 循环记得 `tr:exhausted()` 判空退出。
- 想看错误：`GLOG_v=2` 或前端日志；lua 报错会导致该组件静默失效，
  schema 里注释开关逐个排除是常用定位法。
- 配置热改：改 schema 配置块后需重新部署；纯 lua 逻辑改动也要部署
  （lua 文件被 deployer 校验复制）。
