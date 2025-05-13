package main

import (
	"fmt"
	"path/filepath"
	"scripts/rime"
	"strings"
)

// 过滤掉不在 8105字库中的词条(只要词条中出现了不在8105字库里的词条就删除)
var dicts = map[string]string{
	// "../dicts/wubi86/wubi86.base.dict.yaml": "../dicts/wubi86/wubi86.small_base.dict.yaml",
	"../dicts/wubi86/wubi86.dict.yaml": "../dicts/wubi86/wubi.small.dict.yaml",
}

const dictFile8105 = "../dicts/frost_cn_dicts/8105.dict.yaml"

func main() {
	dict8105, err := rime.ParseDict(dictFile8105)
	if err != nil {
		fmt.Println("Error parsing dictionary:", err)
		return
	}
	for dictSrcFile, dictDestFile := range dicts {
		handle(dict8105, dictSrcFile, dictDestFile)
	}
}

func handle(dict8105 *rime.Dict, dictSrcFile, dictDestFile string) {
	srcDict, err := rime.ParseDict(dictSrcFile)
	if err != nil {
		fmt.Println("Error parsing dictionary:", err)
		return
	}
	destDict := rime.NewDictFrom(getDictName(dictDestFile), srcDict)
	destDict.FileComments = fmt.Sprintf(`# 本字库使用 keep_8105.go 生成
# 源字库: %s
# 仅保留 8105.dict.yaml中汉字组成的词条
%s
`, dictSrcFile, srcDict.FileComments)
	for _, entry := range srcDict.Entries {
		isAllCharsIn8105 := dict8105.Entries.ContainsAll(entry.Chars()...)
		if isAllCharsIn8105 {
			destDict.Entries.Add(entry.Clone())
		}
	}

	// 写回文件
	err = destDict.Write(dictDestFile)
	if err != nil {
		fmt.Println("Error serializing dictionary:", err)
		return
	}

}

func getDictName(dict string) string {
	dictFileName := filepath.Base(dict)
	return strings.TrimSuffix(dictFileName, ".dict.yaml")

}
