package main

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
)

var (
	params = []string{"-S", "--no-heading", "--line-number", "--color", "never"}
)

var first = true
var notMatchList []string
var matchList []string

func main() {

	args := []string{}
	for _, arg := range os.Args[1:] {
		arg = strings.Replace(arg, ".*?", " ", -1)
		arg = strings.Replace(arg, "\\(", "quote_prefix", -1)
		arg = strings.Replace(arg, "\\)", "quote_suffix", -1)
		arg = strings.Replace(arg, "(", "", -1)
		arg = strings.Replace(arg, ")", "", -1)
		arg = strings.Replace(arg, "quote_prefix", "\\(", -1)
		arg = strings.Replace(arg, "quote_suffix", "\\)", -1)
		tokens := strings.Split(arg, " ")
		for _, token := range tokens {
			args = append(args, token)
		}
	}

	for _, arg := range args {

		if strings.HasPrefix(arg, "!") {
			if arg != "!" {
				notMatchList = append(notMatchList, arg[1:])
			}

		} else if strings.HasPrefix(arg, "-") {
			params = append(params, arg)
		} else {
			if first {
				params = append(params, arg)
				params = append(params, ".")
				first = false
			} else {
				matchList = append(matchList, arg)
			}
		}
	}
	cmd := exec.Command("rg", params...)
	data, err := cmd.CombinedOutput()
	if err != nil {
		fmt.Println(string(data), err)
		return
	}

	lines := strings.Split(string(data), "\n")
	lines = filterNotMatch(filter(lines))
	for _, line := range lines {
		fmt.Println(line)
	}
}

func filter(lines []string) (result []string) {
	for _, line := range lines {
		skip := false
		for _, key := range matchList {
			if !strings.Contains(line, key) {
				skip = true
				continue
			}
		}
		if !skip {
			result = append(result, line)

		}

	}
	return
}

func filterNotMatch(lines []string) (result []string) {
	for _, line := range lines {
		skip := false
		for _, key := range notMatchList {
			if strings.Contains(line, key) {
				skip = true
				continue
			}
		}
		if !skip {
			result = append(result, line)

		}

	}
	return
}
