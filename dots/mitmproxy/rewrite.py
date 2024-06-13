#  -*- coding:utf-8 -*-
from mitmproxy import http, ctx
import json
import requests
from urllib.parse import urlparse
# https://docs.mitmproxy.org/stable/addons-examples/#http-modify-form
# E(event) 查看 代码中打印的日志 

URL_PREFIX_MAP = {
    # "http://entree.dev.didatrip.com/badge/": "http://localhost:3831/badge/",
    # "http://entree.dev.didatrip.com/bauhinia/": "http://127.0.0.1:3880/bauhinia/",
    "http://entree.dev.didatrip.com/label/": "http://localhost:3890/label/",
    "https://entree-ali.igetget.com/label/": "http://localhost:3890/label/",
    "http://odobpkg.test.svc.luojilab.dc/odobpkg/":"http://127.0.0.1:17946/odobpkg/",
    # 填写你的映射字典
}

def replace_url_prefix(url):
    # 遍历映射字典
    for original_prefix, new_prefix in URL_PREFIX_MAP.items():
        # 检查请求的 URL 是否以原始前缀开始
        if url.startswith(original_prefix):
            # 替换 URL 的前缀
            return url.replace(original_prefix, new_prefix, 1)
    return url

def request(flow: http.HTTPFlow) -> None:
    if flow.request.pretty_host == "cp.iget.dev.didatrip.com" \
       and flow.request.path.startswith("/api/proxy"):
        try:
            body_data = json.loads(flow.request.get_text())
            url = replace_url_prefix(body_data.get("url"))
            method = body_data.get("method").lower()
            data = body_data.get("data")

            if not url or not method:
                ctx.log.error("请求缺少URL或方法")
                return

            # 替换URL前缀
            for original_prefix, new_prefix in URL_PREFIX_MAP.items():
                if url.startswith(original_prefix):
                    url = url.replace(original_prefix, new_prefix, 1)
                    break

            # 设置请求的新URL
            flow.request.url = url
            flow.request.method = method.upper()
            # 将数据转换为字符串并设置请求内容
            flow.request.content = json.dumps(data).encode()
            # 更新请求头
            flow.request.headers["Content-Type"] = "application/json"
            parsed_url = urlparse(url)
            host = parsed_url.hostname
            ctx.log.error("请求缺少URL或方法%s" % host)
            
            flow.request.headers["Host"] = host

        except json.JSONDecodeError as e:
            ctx.log.error(f"JSON解析错误: {e}")
            # 创建500错误响应
            flow.response = http.Response.make(
                500,
                b"Internal Server Error",
                {"Content-Type": "text/html"}
            )
        except Exception as e:
            ctx.log.error(f"处理请求时出现错误: {e}")
            # 创建500错误响应
            flow.response = http.Response.make(
                500,
                b"Internal Server Error",
                {"Content-Type": "text/html"}
            )
    else:
        for original_prefix, new_prefix in URL_PREFIX_MAP.items():
            # 检查请求的 URL 是否以原始前缀开始
            if flow.request.pretty_url.startswith(original_prefix):
                # 替换 URL 的主机、端口和路径
                new_url = flow.request.pretty_url.replace(original_prefix, new_prefix, 1)
                flow.request.url = new_url
                # flow.request.headers["Xi-av"] = "11.6.2"
                break  # 匹配到一个前缀后就不需要继续检查其他前缀
