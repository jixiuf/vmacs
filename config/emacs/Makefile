# -*- coding:utf-8 -*-
.PHONY: eshell lib rime
PWD := `pwd`
LINK_CMD := ln -s -f
EMACSCMD ?= emacs
LIBRARY_PATH=/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin19/10.2.0
BATCH  = $(EMACSCMD) -batch -Q $(LOAD_PATH)  -l ./early-init.el --eval "(package-initialize)" -l ./init.el
EMACS_BASE  = $(EMACSCMD)  -Q $(LOAD_PATH)  -l ./early-init.el --eval "(package-initialize)" -l ./init-base.el
all:clean  update-autoload-cookie deps
base:
	$(EMACS_BASE)  --debug-init
compile:lib
	@echo "delete *.elc 以避免有问题的elc文件影响编译"
	@rm -rf *.elc
	@rm -rf ./lazy/*.elc
	@rm -rf ./conf/*.elc
	$(BATCH) --eval '(byte-recompile-directory (expand-file-name "./lazy/" user-emacs-directory) 0)' \
             --eval '(byte-recompile-directory (expand-file-name "conf/" user-emacs-directory)  0)'

	@if [  -f ./elpa/.elpa_compiled_by_make ]; then \
	   echo "如果想编译elpa/目录下的文件 请手动删除 ./elpa/.elpa_compiled_by_make";\
    else\
	   make compile-elpa; \
	   touch ./elpa/.elpa_compiled_by_make;\
	fi
	make update-autoload-cookie
update-autoload-cookie:
	$(LINK_CMD) $(PWD)/gnus.el ~/.gnus.el

	@echo "生成 lisp/update-autoload-cookie.el"
	@-rm lazy-loaddefs.el lisp/lazy-loaddefs.el
	@$(BATCH) -l ./conf/conf-lazy-load.el
compile-elpa:
	$(BATCH) --eval '(byte-recompile-directory "./elpa/" 0)'
clean:
	@echo "delete *.elc"
	@rm -rf ./lazy/*.elc
	@rm -rf ./conf/*.elc
	@rm -rf *.elc
#	find elpa -name "*eln-x86_64-apple-darwin18.7.0-ecc4586396ba93a9" -exec rm  -rf {} \;
clean-elpa:
	find elpa -name "*.elc" -exec rm {} \;

lib:
	find $(PWD)/lib -name "*.dylib" -exec ln -fs {} /usr/local/lib/ \;
	find $(PWD)/lib -name "*.so" -exec ln -fs {} /usr/local/lib/ \;
	find $(PWD)/lib/include -exec ln -fs {} /usr/local/include \;
# make -C submodule/emacs-rime lib
# ln -fs ~/.emacs.d/submodule/emacs-rime/{librime-emacs.so,librime-emacs.dylib}

eshell:
	./bin/zsh-to-eshell-alias.sh
deps:
	git submodule init
	git submodule update --recursive --remote
# curl https://raw.githubusercontent.com/manateelazycat/awesome-tab/master/awesome-tab.el >./lazy/awesome-tab.el
# curl https://raw.githubusercontent.com/akermu/emacs-libvterm/master/vterm.el> ./lazy/vterm.el
# curl https://raw.githubusercontent.com/jixiuf/vterm-toggle/master/vterm-toggle.el> ./lazy/vterm-toggle.el
# curl https://raw.githubusercontent.com/yjwen/org-reveal/master/ox-reveal.el >./lazy/ox-reveal.el
# curl https://raw.githubusercontent.com/magit/libegit2/master/libgit.el > ./lazy/libgit.el

meow:
	emacs -q -L submodule/meow/ -l submodule/meow/meow.el -l init-meow.el --eval "(setq inhibit-startup-screen t)"
rime:
	rm -rf rime/
	mkdir -p rime
	ln -s -f "$$HOME/Library/Rime/squirrel.custom.yaml" rime/squirrel.custom.yaml
	ln -s -f "$$HOME/Library/Rime/default.custom.yaml" rime/default.custom.yaml
	ln -s -f "$$HOME/Library/Rime/pinyin_jixiuf.schema.yaml" rime
	ln -s -f "$$HOME/Library/Rime/wubi_pinyin_jixiuf.schema.yaml" rime
	ln -s -f "$$HOME/Library/Rime/double_pinyin_flypy_jixiuf.schema.yaml" rime
	ln -s -f "$$HOME/Library/Rime/"*.dict.yaml rime/
	cp  -rf "$$HOME/Library/Rime/"*.userdb rime/
