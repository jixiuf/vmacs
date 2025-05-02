local function my_segmentor(segmentation, env)
    if(not segmentation:empty()) then
        local segment = segmentation:back()
        local str = table.concat(segment.tags, ", ")
        local input = env.engine.context.input
        -- print(str)
        -- print(input)
        -- print(commit)

    end
    -- true: 交由下一个分段器处理
    return true
end
return my_segmentor
