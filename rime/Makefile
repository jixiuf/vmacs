# -*- coding:utf-8 -*-
# 日志在这里 $TMPDIR/rime.squirrel.*
# https://github.com/LEOYoon-Tsaw/Rime_collections/blob/master/Rime_description.md
# 五笔拼音中的拼音会用到此词库
# 拼音 词库扩展
#词典可以用 深蓝词库工具 导入，网上可下载
# wget -c https://github.com/amzxyz/rime_wanxiang_pro/raw/refs/heads/main/amz-v3n2m1-zh-hans.gram
# wget -c https://github.com/amzxyz/RIME-LMDG/releases/download/v2n3/amz-v2n3m1-zh-hans.gram;\

UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
    TARGET :=  $$HOME/.local/share/fcitx5/rime/
else
    TARGET := $$HOME/Library/Rime/
endif

.PHONY: deploy
deploy:

	mkdir -p  $(TARGET)
	ln -s -f "$(CURDIR)/user_dicts" "$(TARGET)"
	@if [ ! -f ~/Documents/jianguo/jianguo/rime/  ]; then \
		unlink "$(TARGET)sync" ;\
		ln -s -f ~/Documents/jianguo/jianguo/rime/sync "$(TARGET)sync";\
		unlink "$(TARGET)user_dicts" ;\
		ln -s -f ~/Documents/jianguo/jianguo/rime/user_dicts "$(TARGET)user_dicts";\
	fi
	ln -s -f "$(CURDIR)/dicts" "$(TARGET)"
	ln -s -f "$(CURDIR)/lua" "$(TARGET)"
	ln -s -f "$(CURDIR)/opencc" "$(TARGET)"
	ln -s -f "$(CURDIR)/"*.yaml "$(TARGET)"
	@if [ ! -f wanxiang-lts-zh-hans.gram  ]; then \
		wget -c https://github.com/amzxyz/RIME-LMDG/releases/download/LTS/wanxiang-lts-zh-hans.gram;\
	fi
	cp -f "$(CURDIR)/"*.gram "$(TARGET)"
