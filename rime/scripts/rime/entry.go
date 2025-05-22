package rime

import "strings"

// Entry 表示字典条目
type Entry struct {
	IsCommentedEntry bool // a commented entry line
	Word             string
	Code             string
	Weight           float64
	HasWeight        bool
	//cangjie: 個	owjr	246268	ow'jr
	//wubi: 工	a	99454797	aa
	Stem     string // #第四列造詞碼
	Comments []string
}

func (e *Entry) Clone() *Entry {
	if e == nil {
		return nil
	}

	e2 := &Entry{}
	*e2 = *e
	return e2
}

// 去除code 中的声调
func (e *Entry) RemoveTone() *Entry {
	e2 := e.Clone()
	for tone, char := range tones {
		e2.Code = strings.ReplaceAll(e2.Code, tone, char)
	}
	return e2
}

type Entries []*Entry

// 如果已经含相同code 的word,则忽略
func (es *Entries) Add(es2 ...*Entry) (cnt int64) {
	for _, e := range es2 {
		found := es.FindByWordAndCode(e.Word, e.Code)
		if len(found) == 0 {
			*es = append(*es, e)
			cnt++
		}
	}
	return
}
func (es Entries) Find(word string) (words Entries) {
	for _, e := range es {
		if e.Word == word && !e.IsCommentedEntry {
			words = append(words, e)
		}
	}
	return
}
func (es Entries) FindByWordAndCode(word, code string) (words Entries) {
	for _, e := range es {
		if e.Word == word && e.Code == code && !e.IsCommentedEntry {
			words = append(words, e)
		}
	}
	return
}
func (es Entries) Clone() (es2 Entries) {
	for _, e := range es {
		es2 = append(es2, e.Clone())
	}
	return
}
func (es Entry) Chars() []string {
	runes := []rune(es.Word)
	chars := make([]string, len(runes))
	for i, r := range runes {
		chars[i] = string(r)
	}
	return chars

}
func (es Entries) ContainsAll(words ...string) bool {
	for _, word := range words {
		if len(es.Find(word)) == 0 {
			return false
		}
	}
	return true
}

var tones = map[string]string{
	"ā": "a",
	"á": "a",
	"ǎ": "a",
	"à": "a",
	"ō": "o",
	"ó": "o",
	"ǒ": "o",
	"ò": "o",
	"ē": "e",
	"é": "e",
	"ě": "e",
	"è": "e",
	"ī": "i",
	"í": "i",
	"ǐ": "i",
	"ì": "i",
	"ū": "u",
	"ú": "u",
	"ǔ": "u",
	"ù": "u",
	"ǖ": "v",
	"ǘ": "v",
	"ǚ": "v",
	"ǜ": "v",
	"ü": "v",
	"ń": "en",
	"ň": "en",
	"ǹ": "en",
}
