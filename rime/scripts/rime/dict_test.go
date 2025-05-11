package rime

import (
	"fmt"
	"testing"
)

func TestRead(t *testing.T) {
	dict, err := ParseDict("../wubi86.dict.yaml")
	if err != nil {
		fmt.Println("Error parsing dictionary:", err)
		return
	}

	// dict.SortByWeight() // 按权重排序

	// 序列化回文件
	err = dict.Write("/tmp/dict/wubi86_sorted.dict.yaml")
	if err != nil {
		fmt.Println("Error serializing dictionary:", err)
		return
	}

}
func TestRead2(t *testing.T) {
	dict, err := ParseDict("../../dicts/frost_cn_dicts/8105.dict.yaml")
	if err != nil {
		fmt.Println("Error parsing dictionary:", err)
		return
	}

	// dict.SortByWeight() // 按权重排序

	// 序列化回文件
	err = dict.Write("/tmp/dict/wubi86_sorted.dict.yaml")
	if err != nil {
		fmt.Println("Error serializing dictionary:", err)
		return
	}

}
