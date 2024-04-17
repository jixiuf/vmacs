# -*- coding:utf-8 -*-
.PHONY: deploy
PWD := `pwd`
DIR := `pwd`
DATE := `date +%Y-%m-%d_%H-%M-%S`
LINK_CMD := ln -s -f
default:
	sudo make sudo
	make deploy

deploy:
	gpg -d config/rbw/config.json.gpg > config/rbw/config.json
	@-mkdir -p ~/bin ~/.cargo/ ~/go/ /usr/local/bin/
	@-rm -rf ~/.cargo/bin ~/go/bin
	ln -fs /usr/local/bin/ ~/.cargo/bin
	ln -fs /usr/local/bin/  ~/go/bin
# @-for file in bin/*; do \
#	if [ -h ~/$$file ]; then \
#	unlink ~/$$file ;\
#     fi;\
#	if	[ -f $$file ]; then \
#		$(LINK_CMD) $(PWD)/$$file ~/$$file ;\
#	fi;\
# done
	@-for file in bin/*; do \
		if [ -h /usr/local/$$file ]; then \
		unlink /usr/local/$$file ;\
        fi;\
		if	[ -f $$file ]; then \
			$(LINK_CMD) $(PWD)/$$file /usr/local/$$file ;\
		fi;\
	done


	@-for file in local/*; do \
		link_name=$$(echo "$$file" | tr ':' '/'); \
		mkdir -p "$$HOME/.$$(dirname "$$link_name")"; \
		if [ -h ~/.$$link_name ]; then \
			unlink ~/.$$link_name ;\
        fi;\
		$(LINK_CMD) $(PWD)/$$file ~/.$$link_name ;\
	done
	@-mkdir -p ~/.config/
	@-for file in config/*; do \
		link_name=$$(echo "$$file" | tr ':' '/'); \
		mkdir -p "$$HOME/.$$(dirname "$$link_name")"; \
		if [ -h ~/.$$link_name ]; then \
			unlink ~/.$$link_name ;\
        fi;\
		$(LINK_CMD) $(PWD)/$$file ~/.$$link_name ;\
	done
#	@-mkdir -p ~/.local/share/gnome-shell/extensions
#	if [ ! -d ~/.local/share/gnome-shell/extensions/gnome-magic-window@adrienverge ]; then \
#		git clone git@github.com:jixiuf/gnome-magic-window.git  ~/.local/share/gnome-shell/extensions/gnome-magic-window@adrienverge ;\
#	fi
#	gnome-extensions enable gnome-magic-window@adrienverge

sudo:
# ln -sf udevmon_$$HOSTNAME.yaml etc/interception/udevmon.yaml
	@-for file in etc/*; do \
		link_name=$$(echo "$$file" | tr ':' '/'); \
		mkdir -p "/$$(dirname "$$link_name")"; \
		if [ -h /$$link_name ]; then \
			unlink /$$link_name ;\
        fi;\
		$(LINK_CMD) $(PWD)/$$file /$$link_name ;\
	done
	mkdir -p /usr/local/{bin,}
	chown jixiuf:users /usr/local  -R
	on-keyboard-update
	find /usr/local/bin/ -xtype l -delete
