#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""egret_wubi_py 回归测试。

对真实环境(~/Library/Rime)逐条跑 rime_api_console, 校验首候选。

用法:
  python3 scripts/regression_test.py            # 只跑用例
  python3 scripts/regression_test.py --rebuild  # 先 rm -rf build 重新部署再跑
  python3 scripts/regression_test.py --clean    # 先清用户词典(userdb)再跑
  python3 scripts/regression_test.py --dir /tmp/rime_final   # 测其他环境

注意(见 AGENTS.md 陷阱清单):
- user.yaml 的 previously_selected_schema 会被 console 运行改写, 每次运行前重置
- rm -rf build 后首次 console 可能有维护竞态, --rebuild 模式会先空跑一次预热
"""

import argparse
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
LIBRIME_CONSOLE = os.path.expanduser(
    "~/repos/librime/build/bin/rime_api_console")
LIBRIME_DEPLOYER = os.path.expanduser(
    "~/repos/librime/build/bin/rime_deployer")
SQUIRREL_SHARED = "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"

# (输入码, 期望首候选, 说明)
CASES = [
    # 一级/二级/三级简码 (传统库 first_translator, 肌肉记忆)
    ("e", "有", "一级简码"),
    ("q", "我", "一级简码"),
    ("yi", "就", "一级简码(易与拼音'以'冲突)"),
    ("yl", "为", "二级简码"),
    ("kl", "另", "二级简码"),
    ("de", "胡", "二级简码(易与拼音'的'冲突)"),
    # 字全码 (曾因 2+2 词码冲突被过滤, 根因#11)
    ("tffu", "等", "字全码>词组(等780>徒增502)"),
    # 词组 (传统库显式码)
    ("khlg", "中国", "二字词 2+2"),
    ("xciy", "经济", "二字词 2+2"),
    ("kdyc", "顺序", "二字词 2+2"),
    ("trwu", "我们", "二字词 2+2"),
    # 混流 + LM 组句
    ("trwuchifan", "我们吃饭", "五笔+拼音混流"),
    ("jfwaguangfnrt", "时代广场", "四字词 1+1+1+1"),
    ("jintiandetianqizenmeyang", "今天的天气怎么样", "纯拼音长句"),
    ("tamenlailehenduoren", "他们来了很多人", "LM 组句"),
    ("wojintianmeiyoukong", "我今天没有空", "LM 组句"),
    # 用户词组/造词
    ("tgtj", "生不逢时", "用户词组(手工维护)"),
]

# 已知 LM 级问题: wanxiang 官方模型=才最, 自训 jixiuf gram=都不可能。
# 切 egret.common.yaml 的 grammar/language 后期望值不同, 不作为失败项, 仅报告。
KNOWN_LM_CASES = [
    ("fqywvkwsniftjbgiskceybgazhelile",
     ["无论如何你都不可能离开这里了", "无论如何你才最不可能离开这里了"],
     "黄金句(gram 配比相关, 两个结果均记录)"),
]

USERDB_DIRS = [
    "egret_wubi_py.user.dict.userdb",
    "egret_wubi_py_wubi.user.dict.userdb",
]


def reset_selected_schema(user_dir):
    """console 运行会改写 previously_selected_schema, 测前强制重置。"""
    path = os.path.join(user_dir, "user.yaml")
    if not os.path.exists(path):
        return
    with open(path, encoding="utf-8") as f:
        src = f.read()
    fixed = re.sub(r"previously_selected_schema: \S+",
                   "previously_selected_schema: egret_wubi_py", src)
    if fixed != src:
        with open(path, "w", encoding="utf-8") as f:
            f.write(fixed)


def first_candidate(code, cwd):
    """跑一次 console, 返回首候选文本(如 '有')。"""
    p = subprocess.run([LIBRIME_CONSOLE], input=code + "\n\n",
                       capture_output=True, text=True, cwd=cwd, timeout=120)
    for line in p.stdout.splitlines():
        m = re.match(r"1\. \[([^\]]*)\]", line.strip())
        if m:
            return m.group(1)
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default=os.path.expanduser("~/Library/Rime"),
                    help="被测用户目录 (默认 ~/Library/Rime)")
    ap.add_argument("--rebuild", action="store_true",
                    help="先删除 build 并重新部署")
    ap.add_argument("--clean", action="store_true",
                    help="先清除用户词典 userdb(调频/自学习归零)")
    args = ap.parse_args()

    user_dir = args.dir
    if not os.path.isdir(user_dir):
        sys.exit(f"用户目录不存在: {user_dir}")

    if args.clean:
        for d in USERDB_DIRS:
            p = os.path.join(user_dir, d)
            if os.path.exists(p):
                import shutil
                shutil.rmtree(p)
                print(f"已清除 {d}")

    if args.rebuild:
        import shutil
        shutil.rmtree(os.path.join(user_dir, "build"),
                      ignore_errors=True)
        r = subprocess.run([LIBRIME_DEPLOYER, "--build", user_dir,
                            SQUIRREL_SHARED], capture_output=True)
        print("部署完成" if r.returncode == 0 else "部署异常(仍继续测试)")

    reset_selected_schema(user_dir)

    # rebuild 后首次 console 可能有维护竞态, 空跑一次预热
    if args.rebuild:
        first_candidate("khlg", user_dir)
        reset_selected_schema(user_dir)

    failed, passed = [], 0
    print(f"\n{'输入码':<28} 期望       实际       说明")
    print("-" * 78)
    for code, expect, note in CASES:
        got = first_candidate(code, user_dir)
        reset_selected_schema(user_dir)
        ok = (got == expect)
        passed += ok
        if not ok:
            failed.append((code, expect, got, note))
        print(f"{code:<28} {expect:<10} {(got or '?'):<10} "
              f"{'✓' if ok else '✗'} {note}")

    print("-" * 78)
    for code, accepts, note in KNOWN_LM_CASES:
        got = first_candidate(code, user_dir)
        reset_selected_schema(user_dir)
        tag = "✓(已知)" if got in accepts else "⚠ 新结果(记录到 AGENTS.md)"
        print(f"{code:<28} {'/'.join(accepts)[:18]:<20} {(got or '?'):<10} {tag} {note}")

    total = len(CASES)
    print(f"\n通过 {passed}/{total}")
    if failed:
        print("失败用例:")
        for code, expect, got, note in failed:
            print(f"  {code}  期望={expect}  实际={got}  ({note})")
        sys.exit(1)
    print("全部通过 ✓")


if __name__ == "__main__":
    main()
