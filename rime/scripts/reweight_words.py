#!/usr/bin/env python3
"""用语料重扫的词频, 对数归一化后同步重写 拼音词库 与 五笔词库 的词组权重.
单字权重不动(知频权威). 用法: python3 reweight_words.py"""
import math, os, re, sys

DICTS = "/Users/jixiuf/vmacs/rime/dicts"
ROOT = "/Users/jixiuf/vmacs/rime"
REWEIGHTED = "/tmp/corpus/pinyin_reweighted.txt"      # rime-frequency 输出(含头)
PINYIN_DICT = os.path.join(DICTS, "wubi_py_pinyin_base_small.dict.yaml")
WUBI_DICT = os.path.join(ROOT, "dicts", "egret_wubi_py", "egret_wubi_py_wubi_words.dict.yaml")

def main():
    # 1) 读重扫结果: word -> new_raw_count
    new = {}
    started = False
    for line in open(REWEIGHTED, encoding="utf-8"):
        if line.startswith("..."):
            started = True
            continue
        if not started or line.startswith("#") or "\t" not in line:
            continue
        parts = line.rstrip("\n").split("\t")
        if len(parts) >= 3 and parts[2].isdigit():
            new[parts[0]] = int(parts[2])
    w_max = max(new.values()) if new else 1

    def norm(cnt):
        return max(1, int(round(1000.0 * math.log(1 + cnt) / math.log(1 + w_max))))

    def reweight(path, is_word_line):
        out, n, changed = [], 0, 0
        started = False
        for line in open(path, encoding="utf-8"):
            if not started:
                out.append(line)
                if line.rstrip() == "...":
                    started = True
                continue
            if line.startswith("#") or "\t" not in line:
                out.append(line)
                continue
            parts = line.rstrip("\n").split("\t")
            word = parts[0]
            if len(parts) >= 3 and is_word_line(word) and word in new:
                parts[2] = str(norm(new[word]))
                changed += 1
            out.append("\t".join(parts) + "\n")
            n += 1
        open(path, "w", encoding="utf-8").write("".join(out))
        print(f"{os.path.basename(path)}: {changed} weights updated, {n} entries")

    # 拼音词库: 全部条目都是词
    reweight(PINYIN_DICT, lambda w: len(w) >= 2)
    # 五笔库: 只动多字词条(单字权重=知频膝点, 不动)
    reweight(WUBI_DICT, lambda w: len(w) >= 2)

if __name__ == "__main__":
    main()
