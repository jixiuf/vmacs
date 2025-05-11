package rime

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

type Entries []*Entry

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
