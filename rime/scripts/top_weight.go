package main

import (
	"fmt"
	"path/filepath"
	"scripts/rime"
	"strings"
)

// 只保留源dict中权重大于 minWeight 的词条

var dicts = map[string]float64{ // value=min_weight
	"../dicts/wangxiang_cn_dicts/base.dict.yaml":        450,
	"../dicts/wangxiang_cn_dicts/correlation.dict.yaml": 300,
	"../dicts/wb86_cn_dicts/base.dict.yaml":             300,
	"../dicts/wb86_cn_dicts/ext.dict.yaml":              30,
	// "../dicts/wubi86/wubi86.dict.yaml":      0,
	// "../dicts/wubi86/wubi86.base.dict.yaml": 0,
}

func main() {
	for dict, minWeight := range dicts {
		dir := filepath.Dir(dict)
		dictFileName := filepath.Base(dict)
		dictName := strings.TrimSuffix(dictFileName, ".dict.yaml") + ".small"
		outputFileName := filepath.Join(dir, fmt.Sprintf("%s.dict.yaml", dictName))

		// 有声调版的 万象拼音大 “字”库
		baseDict, err := rime.ParseDict(dict)
		if err != nil {
			fmt.Println("Error parsing dictionary:", err)
			return
		}
		dictOutput := rime.NewDict(dictName)
		dictOutput.FileComments = fmt.Sprintf(`
# 本字库使用 top_weight.go 生成
#只保留权重>%.0f 的
%s`, minWeight, baseDict.FileComments)
		baseDict.Entries.SortByWeight() // 按权重排序
		for _, e := range baseDict.Entries {
			if e.Weight >= minWeight {
				dictOutput.Entries = append(dictOutput.Entries, e)
			}
		}

		// 写回文件
		err = dictOutput.Write(outputFileName)
		if err != nil {
			fmt.Println("Error serializing dictionary:", err)
			return
		}

	}

}
