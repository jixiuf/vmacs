package main

import (
	"fmt"
	"path/filepath"
	"scripts/rime"
	"strings"
)

type Config struct {
	Src         string
	Dest        string
	MinChars    int // 仅包含 >=此值的词条
	MaxChars    int // 仅包含 <=此值的词条
	FileComment string
}

// 只保留在 8105字库中字构成的词条
var dicts = []Config{
	{
		Src:         "../dicts/wubi86/wubi86.dict.yaml",
		Dest:        "../dicts/wubi86/wubi86.small.dict.yaml",
		MinChars:    2,
		MaxChars:    30,
		FileComment: " >=2 字的 词条",
	},
	{
		Src:         "../dicts/wubi86/wubi86.chars.dict.yaml",
		Dest:        "../dicts/wubi86/wubi.chars.8105.dict.yaml",
		MinChars:    1,
		MaxChars:    1,
		FileComment: " 1字的8105.dict.yaml中的字",
	},
}

const dictFile8105 = "../dicts/frost_cn_dicts/8105.dict.yaml"

func main() {
	dict8105, err := rime.ParseDict(dictFile8105)
	if err != nil {
		fmt.Println("Error parsing dictionary:", err)
		return
	}
	for _, cfg := range dicts {
		handle(dict8105, cfg)
	}
}

func handle(dict8105 *rime.Dict, cfg Config) {
	srcDict, err := rime.ParseDict(cfg.Src)
	if err != nil {
		fmt.Println("Error parsing dictionary:", err)
		return
	}
	destDict := rime.NewDictFrom(getDictName(cfg.Dest), srcDict)
	destDict.FileComments = fmt.Sprintf(`# 本字库使用 keep_8105.go 生成
# 源字库: %s
# 仅保留 8105.dict.yaml中汉字组成的词条
# %s
%s
`, cfg.Src, cfg.FileComment, srcDict.FileComments)
	for _, entry := range srcDict.Entries {
		chars := entry.Chars()
		isAllCharsIn8105 := dict8105.Entries.ContainsAll(chars...)
		if isAllCharsIn8105 && len(chars) >= cfg.MinChars && len(chars) <= cfg.MaxChars {
			destDict.Entries.Add(entry.Clone())
		}
	}

	// 写回文件
	err = destDict.Write(cfg.Dest)
	if err != nil {
		fmt.Println("Error serializing dictionary:", err)
		return
	}

}

func getDictName(dict string) string {
	dictFileName := filepath.Base(dict)
	return strings.TrimSuffix(dictFileName, ".dict.yaml")

}
