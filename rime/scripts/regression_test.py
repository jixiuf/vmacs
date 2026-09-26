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

# 共享数据目录按平台探测: macOS=Squirrel SharedSupport;
# Linux=ibus-rime 的 /usr/share/rime-data 或 fcitx5 的数据目录。
# 均不可用时用 --shared-dir 指定(部署时 punctuator/recognizer 的
# import_preset: symbols/default 需要 symbols.yaml/default.yaml 源文件)。
SQUIRREL_SHARED = "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"
LINUX_SHARED_CANDIDATES = [
    "/usr/share/rime-data",
    "/usr/share/fcitx5/rime-data",
]

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
    # 2026-09-25 实测新增: 拼音音节首候选(传统库补全污染修复后)
    ("wan", "万", "拼音单音节(曾披传统库 wan* 补全污染→佢/代收)"),
    ("taiwan", "台湾", "拼音词组直查(混合库 tai wan 词条)"),
    ("ckiy", "台湾", "传统词组 2+2"),
    # 已知行为: ck+wan 组句因 万850>湾719 权重差 > LM 贡献(~±20), 出「台万」;
    # 逐字选台选湾一次后 userdb 记住 ck+wan=台湾, 之后 ckwan→台湾。不作硬性断言。
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


def detect_shared_dir(explicit=None):
    """按平台探测共享数据目录(需含 symbols.yaml/default.yaml 等源文件)。"""
    if explicit:
        return explicit
    import platform
    candidates = []
    if platform.system() == "Darwin":
        candidates = [SQUIRREL_SHARED]
    else:
        candidates = LINUX_SHARED_CANDIDATES
    for c in candidates:
        if os.path.isdir(c) and os.path.exists(os.path.join(c, "symbols.yaml")):
            return c
    return None


def deploy(user_dir, shared_dir):
    cmd = [LIBRIME_DEPLOYER, "--build", user_dir]
    if shared_dir:
        cmd.append(shared_dir)
    r = subprocess.run(cmd, capture_output=True)
    print("部署完成" if r.returncode == 0 else "部署异常(仍继续测试)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default=os.path.expanduser("~/Library/Rime"),
                    help="被测用户目录 (macOS 默认 ~/Library/Rime; "
                         "Linux 如 ~/.config/ibus/rime 或调试目录)")
    ap.add_argument("--rebuild", action="store_true",
                    help="先删除 build 并重新部署")
    ap.add_argument("--clean", action="store_true",
                    help="先清除用户词典 userdb(调频/自学习归零)")
    ap.add_argument("--shared-dir", default=None,
                    help="共享数据目录(默认按平台探测; Linux 无 rime-data 时"
                         "可指向含 symbols.yaml/default.yaml 的自备目录)")
    args = ap.parse_args()

    user_dir = args.dir
    if not os.path.isdir(user_dir):
        sys.exit(f"用户目录不存在: {user_dir}")
    shared_dir = detect_shared_dir(args.shared_dir)

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
        deploy(user_dir, shared_dir)
        print(f"共享数据目录: {shared_dir or '(未找到, 依赖已有 build 产物)'}")

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
