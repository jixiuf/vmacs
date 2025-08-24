另 ../bin/wechatocr ./libwcocr.so 为 https://github.com/jixiuf/wechat-ocr 编译出的文件
../lib/libwcocr.so 需要 copy 到 /usr/local/lib/下
以下命令  可实现 图下文字识别 (/opt/wechat 为4.0.x 版的wechat)
../bin/wechatocr  /opt/wechat/wxocr /opt/wechat  test.png

