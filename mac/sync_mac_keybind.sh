#!/bin/sh
# # @ cmd
# # ^ ctrl
# # $ shift
# # ~ option
# defaults write com.apple.finder NSUserKeyEquivalents  '{
# "向前" = "^l";
# "向后" = "^h";
# "前往文件夹..." = "^;";
# "移到废纸篓" = "^d";
# "打开" = "^m";
# }'
# defaults write -app Safari NSUserKeyEquivalents  '{
# "返回"="@h";
# "前进"="@l";
# "关闭标签页"="^w";
# "显示上一个标签页"="@p";
# "显示下一个标签页"="@n";
# "打开位置..."="^;";
# "查找..."="^s";
# "重新载入页面"="^r";
# }'
# echo "launched"
#defaults read com.apple.finder NSUserKeyEquivalents