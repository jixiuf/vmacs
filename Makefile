# -*- coding:utf-8 -*-
.PHONY: eshell
EMACSCMD ?= emacs
BATCH  = $(EMACSCMD) -batch -Q $(LOAD_PATH)  -l ./early-init.el --eval "(package-initialize)" -l ./init.el
compile:
	cd ./bin/&&go build ./rgwrapper.go
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
dump: clean update-autoload-cookie
# @ln -sf  `pwd`/post-receive .git/hooks/
# @ln -sf  `pwd`/pre-push .git/hooks/ #
	@-pkill  -f dump-emacs-portable
	@mkdir -p ~/.emacs.d/cache/dump/
	emacs --batch -l ~/.emacs.d/dump-init.el  -eval '(dump-emacs-portable "~/.emacs.d/cache/dump/emacs_tmp.pdump")'
	@cp -f ~/.emacs.d/cache/dump/emacs_tmp.pdump ~/.emacs.d/cache/dump/emacs.pdump


eshell:
	./bin/zsh-to-eshell-alias.sh
