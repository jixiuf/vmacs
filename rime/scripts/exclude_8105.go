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

// 仅保留不在 8105字库中的词条
var dicts = []Config{
	{
		Src:         "../dicts/wubi86/wubi86.dict.yaml",
		Dest:        "../dicts/wubi86/wubi86.ext.dict.yaml",
		MinChars:    2,
		MaxChars:    30,
		FileComment: "  >=2 字的 词条(仅保留 非 8105.dict.yaml中)",
	},
	{
		Src:         "../dicts/wubi86/wubi86.chars.dict.yaml",
		Dest:        "../dicts/wubi86/wubi.chars.ext.dict.yaml",
		MinChars:    1,
		MaxChars:    1,
		FileComment: " 1字的 词条(仅保留 非 8105.dict.yaml中)",
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
	destDict.FileComments = fmt.Sprintf(`# 本字库使用 exclude_8105.go 生成
# 源字库: %s
# %s
%s
`, cfg.Src, cfg.FileComment, srcDict.FileComments)
	for _, entry := range srcDict.Entries {
		chars := entry.Chars()
		isAllCharsIn8105 := dict8105.Entries.ContainsAll(chars...)
		if !isAllCharsIn8105 && len(chars) >= cfg.MinChars && len(chars) <= cfg.MaxChars {
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
