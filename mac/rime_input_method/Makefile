# -*- coding:utf-8 -*-
.PHONY: deploy
deploy:
# 日志在这里 $TMPDIR/rime.squirrel.*
# https://github.com/LEOYoon-Tsaw/Rime_collections/blob/master/Rime_description.md
# 五笔拼音中的拼音会用到此词库
# 拼音 词库扩展
#词典可以用 深蓝词库工具 导入，网上可下载
	rm -rf ~/Library/Rime/
	mkdir -p  ~/Library/Rime/
	ln -s -f "$(CURDIR)/en_dicts" ~/Library/Rime/en_dicts
	ln -s -f "$(CURDIR)/cn_dicts" ~/Library/Rime/cn_dicts
	ln -s -f "$(CURDIR)/squirrel.custom.yaml" ~/Library/Rime/squirrel.custom.yaml
	ln -s -f "$(CURDIR)/default.custom.yaml" ~/Library/Rime/default.custom.yaml
	ln -s -f "$(CURDIR)/pinyin_jixiuf.schema.yaml" ~/Library/Rime/
	ln -s -f "$(CURDIR)/wubi_pinyin_jixiuf.schema.yaml" ~/Library/Rime/
	ln -s -f "$(CURDIR)/double_pinyin_flypy_jixiuf.schema.yaml" ~/Library/Rime/
	ln -s -f "$(CURDIR)/"*.yaml ~/Library/Rime/
	ln -s -f "$(CURDIR)/"*.txt ~/Library/Rime/
	ln -s -f "$(CURDIR)/"*.lua ~/Library/Rime/
	ln -s -f "$(CURDIR)/zh-hans-t-essay-bgw.gram" ~/Library/Rime/
	ln -s -f "$(CURDIR)/opencc" ~/Library/Rime/


# ln -s -f $(CURDIR)/wubi_pinyin.schema.custom.yaml ~/Library/Rime/
# ln -s -f $(CURDIR)/wubi86.userdb ~/Library/Rime/wubi86.userdb
# ln -s -f $(CURDIR)/pinyin_simp.schema.custom.yaml ~/Library/Rime/


# ln -s -f $(CURDIR)/pinyin_jixiuf_qq_chengyu.dict.yaml ~/Library/Rime/
# ln -s -f $(CURDIR)/pinyin_jixiuf_qq_yanyu.dict.yaml ~/Library/Rime/

# ln -s -f "$(CURDIR)/luna_pinyin.userdb" ~/Library/Rime/luna_pinyin.userdb


# # cache 的已编译词库
# @if [ -d ~/Library/Mobile\ Documents/com~apple~CloudDocs/rime_input_method/wubi_pinyin_jixiuf.userdb  ]; then \
#	rm -rf ~/Library/Rime/wubi_pinyin_jixiuf.userdb;\
#	ln -s -f ~/Library/Mobile\ Documents/com~apple~CloudDocs/rime_input_method/wubi_pinyin_jixiuf.userdb ~/Library/Rime/wubi_pinyin_jixiuf.userdb ; \
#	rm -rf ~/Library/Rime/luna_pinyin.userdb;\
#	ln -s -f ~/Library/Mobile\ Documents/com~apple~CloudDocs/rime_input_method/luna_pinyin.userdb ~/Library/Rime/luna_pinyin.userdb;\
# fi
