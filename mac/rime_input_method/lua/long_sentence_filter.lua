-- https://github.com/hchunhui/librime-lua/wiki/objects
-- 为五笔拼音混合输入法打长句而生的一个过滤器

-- ：理论上 拼音不该出现多个声母相临的情况（ 前提将en an in ing 等中的n g等声母替换后 ）
-- 若声母相临了，则断为wubi,否则为pinyin ，
-- 当然这个判断方法还有待改进

-- 若各选项也为sentence类型，则若其preedit 也匹配相应的wubi 或pinyin ,则会提升相应的排名

--
-- print( judge_input_type("wqvb"))
-- print( judge_input_type("nihao"))
-- print( judge_input_type("nihenhao"))
-- print( judge_input_type("yinhang"))
-- print( judge_input_type("lkpe"))
-- print( judge_input_type("shengyin"))
-- print( judge_input_type("haishihuijiaba"))
-- print( judge_input_type("tgmk"))
-- print( judge_input_type("gongyuan"))



-- 声母列表
local initials = {
    "b", "p", "m", "f", "d", "t", "n", "l",
    "g", "k", "h", "j", "q", "x", "zh", "ch",
    "sh", "r", "z", "c", "s", "y", "w"
}

-- 韵母中可能包含声母字符的特殊组合及其替换字符
local vowel_replacements = {
    ["ing"] = "X",
    ["in"] = "Y",
    ["en"] = "Z",
    ["ong"] = "A",
    ["eng"] = "B",
    ["ang"] = "C",
    ["an"] = "D",
    ["ai"] = "E",
    ["ei"] = "F",
    ["ao"] = "G",
    ["ou"] = "H"
}

-- 判断一个字符串是否是声母
local function is_initial(s)
    for _, initial in ipairs(initials) do
        if s == initial then
            return true
        end
    end
    return false
end

-- 替换韵母中可能出现的声母字符
local function replace_vowels(input)
    local result = input
    for vowel, replacement in pairs(vowel_replacements) do
        result = string.gsub(result, vowel, replacement)
    end
    return result
end

-- 判断输入的字符串是否是拼音
local function is_pinyin(input)
    local processed_input = replace_vowels(input)
    local i = 1
    local len = #processed_input
    while i <= len do
        local found_initial = false
        -- 先尝试匹配最长的声母
        for j = math.min(len, i + 2), i, -1 do
            local sub = string.sub(processed_input, i, j)
            if is_initial(sub) then
                i = j + 1
                found_initial = true
                break
            end
        end
        if not found_initial then
            -- 如果没找到声母，说明当前部分可能是韵母，继续往后检查
            i = i + 1
        else
            -- 若找到了声母，检查后续是否紧接着另一个声母
            if i <= len then
                local next_found_initial = false
                for k = math.min(len, i + 2), i, -1 do
                    local next_sub = string.sub(processed_input, i, k)
                    if is_initial(next_sub) then
                        next_found_initial = true
                        break
                    end
                end
                if next_found_initial then
                    return false
                end
            end
        end
    end
    return true
end

-- 主判断函数
local function judge_input_type(input)
    if is_pinyin(input) then
        return "pinyin"
    else
        return "wubi"
    end
end

local function long_sentence_filter(input, env)
    -- 提升 count 个词语，插入到第 idx 个位置，默认 2、4。
    -- env.name_space = env.name_space:gsub("^*", "")
    -- local config = env.engine.schema.config
    -- local count = config:get_int(env.name_space .. "/count") or 2
    -- local idx = config:get_int(env.name_space .. "/idx") or 4

    local l = {}
    local first = true
    local firstSentence = false -- 记录第一个候选词是否是句子类型
    local ctxInput = env.engine.context.input
    local inputType = judge_input_type(ctxInput) -- pinyin or wubi
    local firstSentenceLength

    for cand in input:iter() do
        if first and cand.type == "sentence" then
            firstSentence=true
            firstSentenceLength = utf8.len(cand.text)
            first=false
            -- print("first" .. cand.text .. cand.type .. " " .. cand.preedit .. " " .. inputType .. " ".. judge_input_type(cand.preedit))
        end
        if firstSentence and firstSentenceLength>4 and cand.type == "sentence"  then
            --cand.preedit: 得到当前候选词预处理后的输入编码（
            -- 如形码映射字根、音码分音节加变音符，如："ni hao"）(name_space/preedit_format)
            if inputType == judge_input_type(cand.preedit) then
                -- print(cand.text .. cand.type .. " " .. cand.preedit .. " " .. inputType .. " ".. judge_input_type(cand.preedit))
                yield(cand)
            else
                table.insert(l, cand)
            end
        else
            table.insert(l, cand)
        end
    end
    for i, cand in ipairs(l) do
        -- if i<4 then
        --     print(cand.text .. cand.type .. " " .. cand.preedit .. " " .. inputType .. " ".. judge_input_type(cand.preedit))
        -- end
        yield(cand)
    end
end

return long_sentence_filter
