local function modify_preedit_filter(input, env)
    local config = env.engine.schema.config
    local delimiter = config:get_string('speller/delimiter') or " '"  -- 默认分隔符
  -- - name: tone_display
  --   states: [ 调, 声 ]
  --   reset: 1

    env.settings = {tone_display = env.engine.context:get_option("tone_display")} or true
    local auto_delimiter = delimiter:sub(1, 1)
    local manual_delimiter = delimiter:sub(2, 2)

    local is_tone_display = env.settings.tone_display
    local context = env.engine.context

    -- **使用 seg:has_tag() 来检查是否处于反查模式**
    local seg = context.composition:back()
    env.is_radical_mode = seg and (
        seg:has_tag("radical_lookup") 
        or seg:has_tag("reverse_stroke") 
    ) or false

    for cand in input:iter() do
        --**如果处于反查模式，直接跳过不修改**
        if env.is_radical_mode then
            yield(cand)
            goto continue
        end

        local genuine_cand = cand:get_genuine()
        local preedit = genuine_cand.preedit or ""
        local comment = genuine_cand.comment

        -- 如果 comment 为空，直接跳过，不做任何处理
        if not comment or comment == "" then
            yield(cand)
            goto continue
        end

        if is_tone_display and #preedit >= 1 then
            local input_parts = {}
            local current_segment = ""

            for i = 1, #preedit do
                local char = preedit:sub(i, i)
                if char == auto_delimiter or char == manual_delimiter then
                    if #current_segment > 0 then
                        table.insert(input_parts, current_segment)
                        current_segment = ""
                    end
                    table.insert(input_parts, char)
                else
                    current_segment = current_segment .. char
                end
            end

            if #current_segment > 0 then
                table.insert(input_parts, current_segment)
            end

            -- 提取拼音片段
            local pinyin_segments = {}
            for segment in string.gmatch(comment, "[^" .. auto_delimiter .. manual_delimiter .. "]+") do
                local pinyin = string.match(segment, ".+")
                if pinyin then
                    table.insert(pinyin_segments, pinyin)
                end
            end

            -- 遍历拼音片段，将前面的片段替换为拼音
            local pinyin_index = 1
            for i, part in ipairs(input_parts) do
                if part ~= auto_delimiter and part ~= manual_delimiter then
                    if pinyin_index <= #pinyin_segments then
                        if i == #input_parts and #part == 1 then
                            local first_pinyin = pinyin_segments[pinyin_index]
                            if first_pinyin and (#first_pinyin >= 2) then
                                local prefix = first_pinyin:sub(1, 2)
                                if prefix == "zh" or prefix == "ch" or prefix == "sh" then
                                    input_parts[i] = prefix
                                else
                                    input_parts[i] = part
                                end
                            else
                                input_parts[i] = part
                            end
                        else
                            input_parts[i] = pinyin_segments[pinyin_index]
                            pinyin_index = pinyin_index + 1
                        end
                    end
                end
            end

            genuine_cand.preedit = table.concat(input_parts)
        end
        yield(genuine_cand)
        ::continue::
    end
end

return modify_preedit_filter
