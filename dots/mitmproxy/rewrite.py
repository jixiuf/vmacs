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
    "https://entree.igetget.com/hotfix/": "http://10.1.0.140:8080/hotfix/",
    "https://entree-ali.igetget.com/hotfix/": "http://10.1.0.140:8080/hotfix/",
    "http://odobpkg.test.svc.luojilab.dc/":"http://127.0.0.1:17946/",
    "http://label-default.test.svc.luojilab.dc/":"http://127.0.0.1:3890/",
    # 填写你的映射字典
}

# cms oms 调内部服务的时候 会走proxy,如 https://oms.luojilab.com/api/proxy
# 此处通过脚本 绕过代理 直接请求内部服务， 并且可以通过 上面的 URL_PREFIX_MAP  将请求打向本地的服务

CMS_OMS_API_PROXY=[ "cp.iget.dev.didatrip.com","oms.test.igetget.dc","cms.luojilab.com","oms.luojilab.com"]

def is_server_responding(url):
    try:
        # 解析URL并获取主机部分
        parsed_url = urlparse(url)
        base_url = f"{parsed_url.scheme}://{parsed_url.netloc}/ping"

        # 发送GET请求到 /ping
        response = requests.get(base_url)
        return response.status_code == 200
    except requests.exceptions.RequestException as e:
        return False

def replace_url_prefix(url):
    # 遍历映射字典
    for original_prefix, new_prefix in URL_PREFIX_MAP.items():
        # 检查请求的 URL 是否以原始前缀开始
        if url.startswith(original_prefix):
            # 替换 URL 的前缀
            return url.replace(original_prefix, new_prefix, 1)
    return url

def request(flow: http.HTTPFlow) -> None:
    if flow.request.pretty_host in CMS_OMS_API_PROXY  \
       and flow.request.path.startswith("/api/proxy"):
        try:
            body_data = json.loads(flow.request.get_text())
            url = body_data.get("url")
            method = body_data.get("method").lower()
            data = body_data.get("data")
            headers = body_data.get("headers")

            newURL = replace_url_prefix(url)
            if is_server_responding(newURL):
                url=newURL


            if not url or not method:
                return

            if headers != None and headers.get("Content-Type")=="application/x-www-form-urlencoded":
                flow.request.headers["Content-Type"] = "application/x-www-form-urlencoded"
                flow.request.urlencoded_form =data
            else:
                # 将数据转换为字符串并设置请求内容
                flow.request.headers["Content-Type"] =  "application/json"
                flow.request.content = json.dumps(data).encode()

            # 设置请求的新URL
            flow.request.url = url
            flow.request.method = method.upper()
            # 更新请求头
            parsed_url = urlparse(url)
            host = parsed_url.hostname

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
                flow.request.headers["Xi-av"] = "12.3.1"
                break  # 匹配到一个前缀后就不需要继续检查其他前缀
