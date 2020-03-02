# -*- coding:utf-8 -*-
.PHONY: eshell lib
EMACSCMD ?= emacs
BATCH  = $(EMACSCMD) -batch -Q $(LOAD_PATH)  -l ./early-init.el --eval "(package-initialize)" -l ./init.el
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
	@echo "生成 lisp/update-autoload-cookie.el"
	@-rm lisp/lazy-loaddefs.el
	@$(BATCH) -l ./conf/conf-lazy-load.el
compile-elpa:
	$(BATCH) --eval '(byte-recompile-directory "./elpa/" 0)'
clean:
	@echo "delete *.elc"
	@rm -rf ./lazy/*.elc
	@rm -rf ./conf/*.elc
	@rm -rf *.elc
clean-elpa:
	find elpa -name "*.elc" -exec rm {} \;
dump: clean update-autoload-cookie deps
# @ln -sf  `pwd`/post-receive .git/hooks/
# @ln -sf  `pwd`/pre-push .git/hooks/ #
	@-pkill  -f dump-emacs-portable
	@mkdir -p ~/.emacs.d/cache/dump/
	$(EMACSCMD) -batch -Q $(LOAD_PATH)  -l ./early-init.el --eval "(package-initialize)"  -l ~/.emacs.d/dump-init.el  -eval '(dump-emacs-portable "~/.emacs.d/cache/dump/emacs_tmp.pdump")'
	@cp -f ~/.emacs.d/cache/dump/emacs_tmp.pdump ~/.emacs.d/cache/dump/emacs.pdump

lib:
	find $(PWD)/lib -exec ln -fs {} /usr/local/lib/ \;

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


rime:
	rm -rf ~/.emacs.d/cache/rime/
	mkdir -p ~/.emacs.d/cache/rime
	ln -s -f "$$HOME/Library/Rime/squirrel.custom.yaml" $$HOME/.emacs.d/cache/rime/squirrel.custom.yaml
	ln -s -f "$$HOME/Library/Rime/default.custom.yaml" $$HOME/.emacs.d/cache/rime/default.custom.yaml
	ln -s -f "$$HOME/Library/Rime/pinyin_jixiuf.schema.yaml" $$HOME/.emacs.d/cache/rime
	ln -s -f "$$HOME/Library/Rime/wubi_pinyin_jixiuf.schema.yaml" $$HOME/.emacs.d/cache/rime
	ln -s -f "$$HOME/Library/Rime/"*.dict.yaml $$HOME/.emacs.d/cache/rime/
	cp  -rf "$$HOME/Library/Rime/"*.userdb $$HOME/.emacs.d/cache/rime/
