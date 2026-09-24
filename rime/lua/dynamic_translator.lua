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
    -- 未选字时顺序 = jianma2(简码) -> table(字/造词) -> script(词/句子)
    -- （1~2码首候选由 jianma2 保证；词库层已让二字词压过同码全码生僻字）
    -- 选字后 table 禁用，只剩 script 继续造词
    if (env.engine.context.input == input) then
        if env.first_translator ~= nil then
            local first_res = env.first_translator:query(input, seg)
            if first_res ~= nil then
                for cand in first_res:iter() do
                    yield(cand)
                end
            end
        end
        local table_res = env.table_translator:query(input, seg)
        if table_res ~= nil then
            for cand in table_res:iter() do
                yield(cand)
            end
        end
    end
    local script_res = env.script_translator:query(input, seg)
    if script_res ~= nil then
        for cand in script_res:iter() do
            yield(cand)
        end
    end
end

return dynamic
