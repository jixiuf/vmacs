package rime

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/goccy/go-yaml"
	// "gopkg.in/yaml.v3"
)

// Dict 表示 Rime 字典文件的结构
type Dict struct {
	FileName            string   `yaml:"-"`
	Name                string   `yaml:"name,omitempty"`
	Version             string   `yaml:"version,omitempty"`
	Sort                string   `yaml:"sort,omitempty"`
	UsePresetVocabulary bool     `yaml:"use_preset_vocabulary,omitempty"`
	Columns             []string `yaml:"columns,omitempty"`
	ImportTables        []string `yaml:"import_tables,omitempty"`
	Encoder             *struct {
		ExcludePatterns []string `yaml:"exclude_patterns"`
		Rules           []Rule   `yaml:"rules"`
	} `yaml:"encoder,omitempty"`
	Entries            Entries         `yaml:"-"` // 条目部分需要特殊处理
	FileComments       string          `yaml:"-"`
	Comments           yaml.CommentMap `yaml:"-"`
	ImportedTablesDict []*Dict         `yaml:"-"`
}

func NewDict(name string) *Dict {
	return &Dict{
		Name:     name,
		Version:  time.Now().Format(time.DateOnly),
		Sort:     "by_weight",
		Comments: yaml.CommentMap{},
	}
}
func (dict *Dict) SortByWeight() {
	sort.SliceStable(dict.Entries, func(i, j int) bool {
		return dict.Entries[i].Weight > dict.Entries[j].Weight
	})

}

// Rule 表示编码规则
type Rule struct {
	LengthEqual   int    `yaml:"length_equal,omitempty"`
	LengthInRange []int  `yaml:"length_in_range,omitempty,flow"`
	Formula       string `yaml:"formula"`
}

// ParseDict 解析 Rime 字典文件
func ParseDict(filename string) (*Dict, error) {
	file, err := os.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	var dict Dict
	dict.FileName = filename

	parts := strings.SplitN(string(file), "\n---\n", 2)
	yamlContent := string(file)
	if len(parts) == 2 {
		dict.FileComments = parts[0]
		yamlContent = parts[1]
	}
	// 将文件内容按 "---" 分割为头部和条目部分
	parts = strings.SplitN(yamlContent, "\n...\n", 2)
	if len(parts) != 2 {
		return nil, fmt.Errorf("invalid dictionary format")
	}

	// 解析头部信息
	dict.Comments = make(yaml.CommentMap)
	decode := yaml.NewDecoder(bytes.NewBuffer([]byte(parts[0])), yaml.CommentToMap(dict.Comments))
	err = decode.Decode(&dict)
	if err != nil {
		return nil, err
	}

	// 解析条目部分
	scanner := bufio.NewScanner(strings.NewReader(parts[1]))
	var comments []string
	for scanner.Scan() {
		line := scanner.Text()
		if strings.TrimSpace(line) == "" {
			continue // 跳过空行
		}
		isComment := false
		if strings.HasPrefix(line, "#") {
			if !strings.Contains(line, "\t") {
				comments = append(comments, line)
				continue // 跳过注释
			}
			isComment = true
			line = strings.TrimSpace(strings.TrimPrefix(line, "#"))
		}

		parts := strings.Split(line, "\t")
		var weight float64
		var hasWeight bool
		if len(parts) > 2 {
			hasWeight = true
			_, err = fmt.Sscanf(parts[2], "%f", &weight)
			if err != nil {
				comments = append(comments, scanner.Text())
				continue
			}
		}

		stem := ""
		if len(parts) > 3 {
			stem = parts[3]
		}

		dict.Entries = append(dict.Entries, &Entry{
			IsCommentedEntry: isComment,
			Word:             parts[0],
			Code:             parts[1],
			Weight:           weight,
			HasWeight:        hasWeight,
			Stem:             stem,
			Comments:         comments,
		})
		comments = nil
	}
	for _, tblFile := range dict.ImportTables {
		tblDict, err := ParseDict(tblFile)
		if err != nil {
			fmt.Printf("parse dict(%s) error:%v\n", tblFile, err)
			continue
		}
		dict.ImportedTablesDict = append(dict.ImportedTablesDict, tblDict)

	}

	return &dict, nil
}

// WriteDict 将字典序列化回文件
func (dict *Dict) Write(filename string) error {
	filePath, err := filepath.Abs(filename)
	if err != nil {
		return err
	}
	os.MkdirAll(filepath.Dir(filePath), 0755)

	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()
	file.WriteString(dict.FileComments + "\n")
	file.WriteString("---\n")

	// 写入头部信息
	encoder := yaml.NewEncoder(file, yaml.WithComment(dict.Comments))
	err = encoder.Encode(dict)
	if err != nil {
		return err
	}
	// 写入分隔符
	file.WriteString("...\n")

	// 写入条目部分
	for _, entry := range dict.Entries {
		for _, comment := range entry.Comments {
			file.WriteString(comment + "\n")
		}
		if entry.IsCommentedEntry {
			fmt.Fprintf(file, "# ")
		}

		fmt.Fprintf(file, "%s\t%s", entry.Word, entry.Code)
		if entry.HasWeight || entry.Weight != 0 || entry.Stem != "" {
			if entry.Weight-float64(int64(entry.Weight)) < 0.000001 {
				fmt.Fprintf(file, "\t%d", int64(entry.Weight))
			} else {
				fmt.Fprintf(file, "\t%.1f", entry.Weight)
			}

		}

		if entry.Stem != "" {
			fmt.Fprintf(file, "\t%s\n", entry.Stem)
		} else {
			fmt.Fprintf(file, "\n")
		}

	}

	return nil
}
