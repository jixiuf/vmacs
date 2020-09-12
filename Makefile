# -*- coding:utf-8 -*-
.PHONY: eshell lib rime
EMACSCMD ?= emacs
EMACSGCCCMD ?= gccemacs
EMACSNATIVE ?= $(EMACSGCCCMD) --batch --quick -L . \
			--eval "(let ((default-directory \"~/.emacs.d/elpa/\")) (normal-top-level-add-subdirs-to-load-path))" \
			--eval "(add-to-list 'load-path \"~/.emacs.d/conf\")" \
			--eval "(add-to-list 'load-path \"~/.emacs.d/lazy\")" \
			--eval "(let ((default-directory \"~/.emacs.d/submodule/\")) (normal-top-level-add-subdirs-to-load-path))" \
            -f batch-native-compile

BATCH  = $(EMACSCMD) -batch -Q $(LOAD_PATH)  -l ./early-init.el --eval "(package-initialize)" -l ./init.el
EMACS_BASE  = $(EMACSCMD)  -Q $(LOAD_PATH)  -l ./early-init.el --eval "(package-initialize)" -l ./init-base.el
dump: clean update-autoload-cookie deps
# @ln -sf  `pwd`/post-receive .git/hooks/
# @ln -sf  `pwd`/pre-push .git/hooks/ #
	echo $(EMACSCMD)
	@-pkill  -f dump-emacs-portable
	@mkdir -p ~/.emacs.d/cache/dump/
	$(EMACSCMD) -batch -Q $(LOAD_PATH)  -l ./early-init.el --eval "(package-initialize)"  -l ~/.emacs.d/dump-init.el  -eval '(dump-emacs-portable "~/.emacs.d/cache/dump/emacs_tmp.pdump")'
	@cp -f ~/.emacs.d/cache/dump/emacs_tmp.pdump ~/.emacs.d/cache/dump/emacs.pdump
base:
	$(EMACS_BASE) --eval "(load-theme 'vmacs)" --debug-init
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
native:
# $(BATCH) --eval '(native-compile-async "~/.emacs.d/elpa/" 5)'
	@-rm -rf ~/.emacs.d/eln-cache
	for dir in $$HOME/.emacs.d/elpa/*; do \
		rm -f $$dir/*.elc ; \
		pushd $$dir; \
		$(EMACSNATIVE)   $$(printf '%s\n' $$dir/*.el | grep -v autoloads.el|grep -v pkg.el) || true; \
		popd ;\
	done
	$(EMACSNATIVE)   $$HOME/.emacs.d/lazy/*.el || true;
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
	find elpa -name "*eln-x86_64-apple-darwin18.7.0-ecc4586396ba93a9" -exec rm  -rf {} \;
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


rime:
	rm -rf ~/.emacs.d/rime/
	mkdir -p ~/.emacs.d/rime
	ln -s -f "$$HOME/Library/Rime/squirrel.custom.yaml" $$HOME/.emacs.d/rime/squirrel.custom.yaml
	ln -s -f "$$HOME/Library/Rime/default.custom.yaml" $$HOME/.emacs.d/rime/default.custom.yaml
	ln -s -f "$$HOME/Library/Rime/pinyin_jixiuf.schema.yaml" $$HOME/.emacs.d/rime
	ln -s -f "$$HOME/Library/Rime/wubi_pinyin_jixiuf.schema.yaml" $$HOME/.emacs.d/rime
	ln -s -f "$$HOME/Library/Rime/double_pinyin_flypy_jixiuf.schema.yaml" $$HOME/.emacs.d/rime
	ln -s -f "$$HOME/Library/Rime/"*.dict.yaml $$HOME/.emacs.d/rime/
	cp  -rf "$$HOME/Library/Rime/"*.userdb $$HOME/.emacs.d/rime/
