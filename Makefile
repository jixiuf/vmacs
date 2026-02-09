.PHONY: all emacs-compile elisp-compile deploy
# build-mac:
# 	 ./configure --disable-ns-self-contained  --with-ns  --with-modules;make -j 4;sudo make install

PWD := `pwd`
LINK_CMD := ln -s -f
LINK_CMD_HARD := ln -f
NORMAL_FILES_COMMON := `echo ssh password-store pam-gnupg gnupg mitmproxy authinfo.gpg gitconfig gitattributes gitignore  vimrc  zshenv zshrc   bashrc  fzf.zsh  mbsyncrc mailrc msmtprc`
default:
	make deploy
	make -C config/emacs
	sudo make sudo


deploy:
	@-unlink ~/.gnupg
	mkdir -p ~/.gnupg
	for file in ~/Documents/jianguo/jianguo/keepass/gnupg/*; do \
		name="$$(basename $$file)"; \
		if [ ! "$$name" = "private-keys-v1.d" ]; then \
			$(LINK_CMD) $$file ~/.gnupg/$$name; \
		fi; \
	done
	@if [ `uname -s` = "Linux" ] ; then \
		cd linux && $(MAKE) ; \
	fi
	@if [ `uname -s` = "Darwin" ] ; then \
		cd mac && $(MAKE) ; \
	fi
	make -C ~/Documents/jianguo/jianguo/keepass/gpg-backup restore
	gpg -d dots/notmuch-config.gpg > dots/notmuch-config 2>/dev/null
	@-for file in dots/*; do \
		link_name=$$(echo "$$file" | tr ':' '/'); \
		name="$${link_name#"dots/"}";\
		rm -rf ~/.$$name ;\
		$(LINK_CMD) $(PWD)/$$file ~/.$$name ;\
	done
	@-rm -f ~/.notmuch-config.gpg
	@-for file in doth/*; do \
		link_name=$$(echo "$$file" | tr ':' '/'); \
		name="$${link_name#"doth/"}";\
		rm -rf ~/.$$name ;\
		$(LINK_CMD_HARD) $(PWD)/$$file ~/.$$name ;\
	done


	gpg -d ~/.ssh/id_rsa.gpg > ~/.ssh/id_rsa 2>/dev/null
	gpg -d ~/.ssh/config.gpg > ~/.ssh/config 2>/dev/null
	gpg -d ~/.ssh/authorized_keys.gpg > ~/.ssh/authorized_keys 2>/dev/null
	chmod 600 ~/.ssh/id_rsa
	chmod 600 ~/.ssh/config
	chmod 600 ~/.ssh/authorized_keys
	@-mkdir -p ~/.config/
	@-for file in config/*; do \
		link_name=$$(echo "$$file" | tr ':' '/'); \
		mkdir -p "$$HOME/.$$(dirname "$$link_name")"; \
		if [ -h ~/.$$link_name ]; then \
			rm -rf ~/.$$link_name ;\
        fi;\
		$(LINK_CMD) $(PWD)/$$file ~/.$$link_name ;\
	done


	@if [ ! -d ~/.cache/.vimbackup ]; then\
		mkdir -p ~/.cache/.vimbackup;\
	fi

	make -C rime

sudo:

	@if [ `uname -s` = "Linux" ] ; then \
	  cd linux && $(MAKE)  sudo; \
	fi
	@if [ `uname -s` = "Darwin" ] ; then \
	  cd mac && $(MAKE)  sudo; \
	fi
	for file in bin/*; do \
		if [ -h /usr/local/$$file ]; then \
		rm -rf /usr/local/$$file ;\
		fi;\
		if	[ -f $$file ]; then \
			$(LINK_CMD) $(PWD)/$$file /usr/local/$$file ;\
		fi;\
	done;
meow:
	make -C config/emacs meow
