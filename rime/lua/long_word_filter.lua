-- 长词优先（提升「西安」「提案」「图案」「饥饿」等词汇的优先级）
-- 感谢&参考于： https://github.com/tumuyan/rime-melt
-- 不提升包含英文、数字的候选项
-- 不提升包含 emoji、假名的候选项（通过将此 Lua 放到 simplifier@emoji 前面来实现）
local function long_word_filter(input, env)
    -- 提升 count 个词语，插入到第 idx 个位置，默认 2、4。
    env.name_space = env.name_space:gsub("^*", "")
    local config = env.engine.schema.config
    local count = config:get_int(env.name_space .. "/count") or 2
    local idx = config:get_int(env.name_space .. "/idx") or 4

    local l = {}
    local firstWordLength = 0 -- 记录第一个候选词的长度，提前的候选词至少要比第一个候选词长
    -- local s1 = 0 -- 记录筛选了多少个英语词条(只提升 count 个词的权重，并且对 comment 长度过长的候选进行过滤)
    local s2 = 0 -- 记录筛选了多少个汉语词条(只提升 count 个词的权重)
    local i = 1
    for cand in input:iter() do
        leng = utf8.len(cand.text)
        if (firstWordLength < 1 or i < idx) then
            i = i + 1
            firstWordLength = leng
            yield(cand)
		-- 不知道这两行是干嘛用的，似乎注释掉也没有影响。
		-- elseif #table > 30 then
		--     table.insert(l, cand)
		-- 注释掉了英文的
		-- elseif ((leng > firstWordLength) and (s1 < 2)) and (string.find(cand.text, "^[%w%p%s]+$")) then
		--     s1 = s1 + 1
		--     if (string.len(cand.text) / string.len(cand.comment) > 1.5) then
		--         yield(cand)
		--     end
		-- 换了个正则，否则中英混输的也会被提升
		-- elseif ((leng > firstWordLength) and (s2 < count)) and (string.find(cand.text, "^[%w%p%s]+$")==nil) then
        elseif ((leng > firstWordLength) and (s2 < count)) and (string.find(cand.text, "[%w%p%s]+") == nil) then
            yield(cand)
            s2 = s2 + 1
        else
            table.insert(l, cand)
        end
    end
    for i, cand in ipairs(l) do
        yield(cand)
    end
end

return long_word_filter
