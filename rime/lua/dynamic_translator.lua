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
--    table_translator: table_translator@custom_phrase
--    script_translator: script_translator@translator

function dynamic.init(env)
    -- 创建 translator 组件，供后续调用
    env.name_space = env.name_space:gsub("^*", "")
    local config = env.engine.schema.config
    local table_translator_name = config:get_string(env.name_space .. "/table_translator")
        or "table_translator@custom_phrase"
    local script_translator_name = config:get_string(env.name_space .. "/script_translator")
        or "script_translator@translator"
    dynamic.table_translator = Component.Translator(env.engine, "", table_translator_name)
    dynamic.script_translator = Component.Translator(env.engine, "", script_translator_name)
end

function dynamic.fini(env)
end

function dynamic.func(input, seg, env)
    -- print(env.engine.context.input .. " |" .. input)
    -- (env.engine.context.input == input) 判断用户是否已经选过字
    -- 比如 初始输入为 : nihaoma  当你先中 “你好” 后 就剩下 ma
    -- 此时 "env.engine.context.input"=="nihaoma", input="ma"
    if (env.engine.context.input == input) then
        local table_translator_res = dynamic.table_translator:query(input, seg)
        if  table_translator_res ~= nil then
            for cand in table_translator_res:iter() do
                yield(cand)
            end
        end
    end

    local script_translator_res = dynamic.script_translator:query(input, seg)
    if script_translator_res~=nil then
        for cand in script_translator_res:iter() do
            yield(cand)
        end
    end
end

return dynamic
