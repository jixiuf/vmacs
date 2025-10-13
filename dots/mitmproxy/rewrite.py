#  -*- coding:utf-8 -*-
# pip install mitmproxy
# pip install requests        # for mitmproxy rewrite.py
from mitmproxy import http, ctx
import json
import requests
from urllib.parse import quote
from urllib.parse import urlparse
# https://docs.mitmproxy.org/stable/addons-examples/#http-modify-form
# E(event) 查看 代码中打印的日志

URL_PREFIX_MAP = {
    # "http://entree.dev.didatrip.com/badge/": "http://localhost:3831/badge/",
    # "http://entree.dev.didatrip.com/bauhinia/": "http://127.0.0.1:3880/bauhinia/",
    "http://entree.dev.didatrip.com/label/": "http://localhost:3890/label/",
    "http://entree.dev.didatrip.com/mustard/": "http://localhost:5669/mustard/",
    "http://entree.dev.didatrip.com/message-center/internal-message": "http://localhost:4000/message-center/internal-message",
    "http://entree.dev.didatrip.com/messagecenter/internal-message": "http://localhost:4000/messagecenter/internal-message",
    "https://entree-ali.igetget.com/label/": "http://localhost:3890/label/",
    "https://entree.igetget.com/hotfix/": "http://10.1.0.140:8080/hotfix/",
    "https://entree-ali.igetget.com/hotfix/": "http://10.1.0.140:8080/hotfix/",
    "http://odobpkg.test.svc.luojilab.dc/":"http://127.0.0.1:17946/",
    "http://label-default.test.svc.luojilab.dc/":"http://127.0.0.1:3890/",
    "http://token-default.test.svc.luojilab.dc/":"http://token-default.test.svc.luojilab.dc/",
    # "https://dbs.luojilab.com/bsai/":"http://127.0.0.1:28767/bsai/",
    "https://dbs.luojilab.com/bsai/":"http://10.1.0.140:8086/bsai/",
    "http://bschool.dev.didatrip.com/bslive/":"http://127.0.0.1:60071/bslive/",
    "http://bschool.dev.didatrip.com/scrm/":"http://127.0.0.1:2654/scrm/",
    "http://bschool.dev.didatrip.com/b-school/bgate/":"http://127.0.0.1:2654/b-school/bgate/",
    "http://bschool.dev.didatrip.com/curriculum/":"http://127.0.0.1:3880/curriculum/",
    "http://quiz-activity.test.svc.luojilab.dc/quiz_activity/":"http://127.0.0.1:58658/quiz_activity/",
    "http://entree.dev.didatrip.com/quiz_activity/":"http://localhost:58658/quiz_activity/",
    "http://entree.dev.didatrip.com/ddpush/":"http://localhost:23587/ddpush/",
    # 填写你的映射字典
}
proxy_hosts = {
    "iget-business-manage.test.svc.luojilab.dc":{
        # "bs_scrm":"http://bs-scrm.test.svc.luojilab.dc",
        "bs_scrm":"http://localhost:2654",
    },
}
# cms oms 调内部服务的时候 会走proxy,如 https://oms.luojilab.com/api/proxy
# 此处通过脚本 绕过代理 直接请求内部服务， 并且可以通过 上面的 URL_PREFIX_MAP  将请求打向本地的服务

# ,"cms.luojilab.com","oms.luojilab.com"
CMS_OMS_API_PROXY=[ "cp.iget.dev.didatrip.com","oms.test.igetget.dc"]
BSCHOOL_API_PROXY=["iget-business-manage.test.svc.luojilab.dc"]

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
    # flow.request.headers["Origin"] = "http://localhost:8080"
    # flow.request.headers["Referer"] = "http://localhost:8080"
    # flow.request.headers["Content-Type"] =  "application/json"
    if flow.request.pretty_host in BSCHOOL_API_PROXY  \
       and flow.request.path.startswith("/api/proxy"):
        try:
            body_data = json.loads(flow.request.get_text())
            host_key = body_data.get("host")
            target_host = proxy_hosts[flow.request.pretty_host].get(host_key)
            if target_host==None:
                return

            newurl = f"{target_host}{body_data['url']}"
            method = body_data.get("method", "get").lower()
            params = body_data.get("params", {})
            headers = body_data.get("headers", {})
            data = body_data.get("data", {})
            
            flow.request.headers.update(headers)  # 新增headers注入
            newURL = replace_url_prefix(newurl)
            if is_server_responding(newURL):
                url=newURL

                if not url or not method:
                    return

                if headers != None and headers.get("Content-Type")=="application/x-www-form-urlencoded":
                    flow.request.headers["Content-Type"] = "application/x-www-form-urlencoded"
                    flow.request.urlencoded_form =params
                elif method == "get":
                    # 设置请求参数
                    flow.request.query.update(params)
                    flow.request.headers.pop("Content-Type")
                    flow.request.headers.pop("Content-Length")
                    flow.request.content = None
                    url=f"{url}?a=a"
                    for key, value in params.items():
                        encoded_value = quote(str(value))
                        url = f"{url}&{key}={encoded_value}"

                    url=url.replace("?a=a", "?")
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
    elif flow.request.pretty_host in CMS_OMS_API_PROXY  \
         and flow.request.path.startswith("/api/proxy"):
        try:
            body_data = json.loads(flow.request.get_text())
            url = body_data.get("url")
            method = body_data.get("method").lower()
            params = body_data.get("params", {})
            headers = body_data.get("headers", {})
            data = body_data.get("data", {})

            newURL = replace_url_prefix(url)
            flow.request.headers.update(headers)  # 新增headers注入
            if is_server_responding(newURL):
                url=newURL
                if not url or not method:
                    return
                if headers != None and headers.get("Content-Type")=="application/x-www-form-urlencoded":
                    flow.request.headers["Content-Type"] = "application/x-www-form-urlencoded"
                    flow.request.urlencoded_form =params
                elif method == "get":
                    # 设置请求参数
                    flow.request.query.update(params)
                    flow.request.headers.pop("Content-Type")
                    flow.request.headers.pop("Content-Length")
                    flow.request.content = None
                    url=f"{url}?a=a"
                    for key, value in params.items():
                        encoded_value = quote(str(value))
                        url = f"{url}&{key}={encoded_value}"

                    url = url.replace("?a=a", "?")
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
                if is_server_responding(new_url):
                    flow.request.url = new_url
                    # flow.request.headers["Xi-av"] = "12.3.1"
                    break  # 匹配到一个前缀后就不需要继续检查其他前缀

# def response(flow):
#     if "text/event-stream" in flow.response.headers.get("content-type", ""):
#         flow.response.stream = True
# def response(flow: http.HTTPFlow) -> None:
#     # 添加 CORS 响应头
#     flow.response.headers["Access-Control-Allow-Origin"] = "*"
#     flow.response.headers["Access-Control-Allow-Methods"] = "GET, POST, PUT, DELETE, OPTIONS"
#     flow.response.headers["Access-Control-Allow-Headers"] = "*"

# flow.response.headers["Access-Control-Allow-Headers"] = "*"
