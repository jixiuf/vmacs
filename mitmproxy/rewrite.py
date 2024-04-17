#  -*- coding:utf-8 -*-
from mitmproxy import http
# mitmproxy -p 8888 -s ~/.mitmproxy/rewrite.py
# 定义一个映射字典，键是原始前缀，值是新前缀
URL_PREFIX_MAP = {
    "http://entree.dev.didatrip.com/badge/": "http://localhost:3831/badge/",
    "http://entree.dev.didatrip.com/label/": "http://localhost:3890/label/",
}

def request(flow: http.HTTPFlow) -> None:
    # 遍历映射字典
    for original_prefix, new_prefix in URL_PREFIX_MAP.items():
        # 检查请求的 URL 是否以原始前缀开始
        if flow.request.pretty_url.startswith(original_prefix):
            # 替换 URL 的主机、端口和路径
            new_url = flow.request.pretty_url.replace(original_prefix, new_prefix, 1)
            flow.request.url = new_url
            # flow.request.headers["Xi-av"] = "11.6.2"
            break  # 匹配到一个前缀后就不需要继续检查其他前缀
