-- lua/dynamic_translator.lua
-- https://ksqsf.moe/posts/2023-06-01-rime-double-pinyin/

--引入 table 翻译器后，script的造词机能会有较大的问题。

-- 假设我们要造词「世间」，输入 ui jm（为了说明问题，这里不用辅助码），然后手动选择「世」字，再选择
-- 「间」字。按理来说，我们期望输入法可以造出「世间」这个词。然而不论你如何尝试，使用这种方式输入，
-- 这个词都造不出来！

-- 这里假设"世" 在script 的词典中， 而“间” 在table中
-- 这是因为当我们选择了「世」字后，输入状态只剩下了 jm，而 jm 会进入 table 翻译器，所以「间」字是
-- table 翻译器输出的。script 翻译器根本看不到我们输入了「间」字，当然也无法造出「世间」这个词了。

-- 这个问题的彻底解决可能比较复杂，目前我采用的解决办法是：当用户已经选择了部分字后，就临时禁用掉
-- table 翻译器。

-- 虽然听起来简单，但是 librime 并不能直接实现这个想法。这里我通过 librime 的 Lua 插件支持写了一个自
-- 定义的顶层翻译器实现了这个想法。顶层翻译器根据当前状态依次调用 table 和 script 翻译器。根据文档，
-- 使用 Component 实现该功能。

local dynamic = {}
-- 可以通过 指定
-- dynamic_translator
--    first_translator: table_translator@wubi_jianma2   （可选：最先查询，如传统二级简码微型库）
--    table_translator: table_translator@custom_phrase
--    script_translator: script_translator@translator

function dynamic.init(env)
    -- 创建 translator 组件，供后续调用。
    -- 注意：组件必须挂在 env 上（每个 engine 实例独立），
    -- 不能存在模块级全局变量里：rimel-regexp / liberime-search 会频繁
    -- 创建并销毁临时 session（engine），全局引用会在临时 engine 销毁后
    -- 变成悬空指针，导致默认 session 查询时 SIGSEGV。
    env.name_space = env.name_space:gsub("^*", "")
    local config = env.engine.schema.config
    local table_translator_name = config:get_string(env.name_space .. "/table_translator")
        or "table_translator@custom_phrase"
    local script_translator_name = config:get_string(env.name_space .. "/script_translator")
        or "script_translator@translator"
    -- 可选：最先查询的 translator（如拼音冲突的传统二级简码微型库）
    local first_translator_name = config:get_string(env.name_space .. "/first_translator")
    env.first_translator = first_translator_name
        and Component.Translator(env.engine, "", first_translator_name) or nil
    env.table_translator = Component.Translator(env.engine, "", table_translator_name)
    env.script_translator = Component.Translator(env.engine, "", script_translator_name)
end

function dynamic.fini(env)
    env.first_translator = nil
    env.table_translator = nil
    env.script_translator = nil
end

function dynamic.func(input, seg, env)
    -- 分组而非排序(关键认知):
    -- 1) dynamic_translator 是单条翻译流, Menu 不在流内按 quality 重排;
    -- 2) script 流的原生顺序自带语义(句子 > user phrase > 词 > completion),
    --    且 Sentence 的 quality 是 log 尺度, 与 phrase 的 exp 尺度不可比;
    -- 3) 传统库 enable_completion 的补全候选(如 wan 输入时 佢/代收)会整体
    --    压在拼音候选前 → 挪到流尾; 精确码(简码/全码/词组)仍最前。
    -- 4) custom_phrase(混合库 table) 的精确词不能排在 script 句子前:
    --    混流输入 sk|yi 时, 五笔词"可就"精确匹配会压住组句"可以"
    --    (配合 translator/always_make_sentences 让 grammar 裁决)。
    -- 最终顺序: first(传统简码/词组) > script(句子/用户词/词) >
    --           custom_phrase(混合库词) > 补全(first+table 的 completion)。
    -- (选字后 table 禁用, 只剩 script 继续造词, 保持原逻辑)
    local first_early, script_cands, table_early, tail = {}, {}, {}, {}
    local function collect(res, early, to_tail)
        if res == nil then return end
        for cand in res:iter() do
            if to_tail and cand.type == "completion" then
                tail[#tail + 1] = cand
            elseif early then
                early[#early + 1] = cand
            end
        end
    end

    if (env.engine.context.input == input) then
        if env.first_translator ~= nil then
            collect(env.first_translator:query(input, seg), first_early, true)
        end
    end
    collect(env.script_translator:query(input, seg), script_cands, false)
    if (env.engine.context.input == input) then
        collect(env.table_translator:query(input, seg), table_early, true)
    end

    for _, cand in ipairs(first_early) do yield(cand) end
    for _, cand in ipairs(script_cands) do yield(cand) end
    for _, cand in ipairs(table_early) do yield(cand) end
    for _, cand in ipairs(tail) do yield(cand) end
end

return dynamic
