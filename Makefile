.PHONY: all emacs-compile elisp-compile deploy

PWD := `pwd`
LINK_CMD := ln -s -f
LINK_CMD_HARD := ln -f
NORMAL_FILES_COMMON := `echo ssh password-store pam-gnupg gnupg mitmproxy authinfo.gpg gitconfig gitattributes gitignore  vimrc  zshenv zshrc   bashrc  fzf.zsh  mbsyncrc mailrc msmtprc`
default:
	sudo make sudo
	make deploy
	make -C config/emacs

deploy:
	gpg -d dots/notmuch-config.gpg > dots/notmuch-config 2>/dev/null
	@-for file in dots/*; do \
		link_name=$$(echo "$$file" | tr ':' '/'); \
		name="$${link_name#"dots/"}";\
		rm -rf ~/.$$name ;\
		$(LINK_CMD) $(PWD)/$$file ~/.$$name ;\
	done
	@-rm -f ~/.notmuch-config.gpg

	@-rm -f ~/.gnupg
	$(LINK_CMD) ~/Documents/jianguo/jianguo/keepass/gnupg ~/.gnupg
	gpg -d ~/.ssh/id_rsa.gpg > ~/.ssh/id_rsa 2>/dev/null
	gpg -d ~/.ssh/config.gpg > ~/.ssh/config 2>/dev/null
	gpg -d ~/.ssh/authorized_keys.gpg > ~/.ssh/authorized_keys 2>/dev/null
	chmod 600 ~/.ssh/id_rsa
	sudo cp -f ~/.ssh/config /etc/ssh/ssh_config.d/1config.conf
	sudo chmod 644  /etc/ssh/ssh_config.d/1config.conf
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
	@if [ `uname -s` = "Linux" ] ; then \
		cd linux && $(MAKE) ; \
	fi

	@if [ `uname -s` = "Darwin" ] ; then \
	  cd mac && $(MAKE) ; \
	fi
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
