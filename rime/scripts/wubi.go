package main

import (
	"fmt"
	"scripts/rime"
)

func main() {
	// wubi86CharDict := loadWubiChars()
}
func loadWubiChars() (wubi86CharDict *rime.Dict) {
	wubi86CharDict, err := rime.ParseDict("../dicts/wubi86/wubi86.chars.dict.yaml")
	if err != nil {
		fmt.Println("Error parsing dictionary:", err)
		return
	}
	return
}

// 提取wubi 单字
func saveWubiChar() {
	wubi86, err := rime.ParseDict("../dicts/wubi86/wubi86.dict.yaml")
	if err != nil {
		fmt.Println("Error parsing dictionary:", err)
		return
	}
	destDict := rime.NewDictFrom("wubi86.chars", wubi86)
	for _, e := range wubi86.Entries {
		if len(e.Chars()) != 1 {
			continue
		}
		destDict.Entries.Add(e.Clone())
	}
	destDict.Write("../dicts/wubi86/wubi86.chars.dict.yaml")

}
func loadWubi() {
	wubi86, err := rime.ParseDict("../dicts/wubi86/wubi86.dict.yaml")
	if err != nil {
		fmt.Println("Error parsing dictionary:", err)
		return
	}
	wubi, err := rime.ParseDict("../dicts/wubi86/wubi.dict.yaml")
	if err != nil {
		fmt.Println("Error parsing dictionary:", err)
		return
	}
	for _, entry := range wubi.Entries {
		if len(entry.Chars()) != 1 {
			continue
		}
		if wubi86.Entries.ContainsAll(entry.Word) {
			continue
		}

		appended := wubi86.Entries.Add(entry.Clone())
		if appended > 0 {
			fmt.Println(entry.Word, entry.Code, entry.Weight)
		}

	}

}
