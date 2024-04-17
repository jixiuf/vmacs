# -*- coding:utf-8 -*-
.PHONY: deploy
PWD := `pwd`
LINK_CMD := ln -s -f
default:
	make deploy
	sudo make sudo

deploy:
	@-mkdir -p  ~/Library/LaunchAgents/
	@-mkdir -p /usr/local/etc/

	@-$(LINK_CMD) $(PWD)/../proxychains.conf /usr/local/etc/
	@-$(LINK_CMD) $(PWD)/../tsocks.conf /usr/local/etc/
	@-$(LINK_CMD) $(PWD)/LaunchAgents/git_pull_master.sh ~/Library/LaunchAgents/git_pull_master.sh
	@-$(LINK_CMD) $(PWD)/LaunchAgents/git_pull_master.plist ~/Library/LaunchAgents/
	@-$(LINK_CMD) $(PWD)/LaunchAgents/midnight.sh ~/Library/LaunchAgents/midnight.sh
	@-$(LINK_CMD) $(PWD)/LaunchAgents/midnight.plist ~/Library/LaunchAgents/
	@-$(LINK_CMD) $(PWD)/LaunchAgents/autosshD_ssh_port.plist ~/Library/LaunchAgents/autosshD_ssh_port.plist
	@-$(LINK_CMD) $(PWD)/LaunchAgents/mbsync.plist ~/Library/LaunchAgents/mbsync.plist
	@-$(LINK_CMD) $(PWD)/LaunchAgents/emacs-daemon.plist ~/Library/LaunchAgents/emacs-daemon.plist
	defaults write -g InitialKeyRepeat -int 15 # normal minimum is 15 (225 ms)
	defaults write -g KeyRepeat -int 2 # normal minimum is 2 (30 ms)

	@-mkdir -p ~/.config/
	@-for file in config/*; do \
		link_name=$$(echo "$$file" | tr ':' '/'); \
		mkdir -p "$$HOME/.$$(dirname "$$link_name")"; \
		if [ -h ~/.$$link_name ]; then \
			unlink ~/.$$link_name ;\
        fi;\
		$(LINK_CMD) $(PWD)/$$file ~/.$$link_name ;\
	done
	@mkdir -p ~/bin
	@-for file in bin/*; do \
		if [ -h /$$file ]; then \
		unlink /$$file ;\
        fi;\
		if	[ -f $$file ]; then \
			$(LINK_CMD) $(PWD)/$$file ~/$$file ;\
		fi;\
	done

	@-rm -rf $$HOME/.hammerspoon
	@-$(LINK_CMD) $(PWD)/hammerspoon $$HOME/.hammerspoon

	@-mkdir -p ~/.cow
	@-$(LINK_CMD) $(PWD)/cowrc ~/.cow/rc
#emacs 中使用 term
# 如果你使用的是 mac 系统，发现 multi-term 每行出出了 4m，在 shell 里运
# 行下：tic -o ~/.terminfo
# /Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti
	@-rm -rf ~/.terminfo
	@-$(LINK_CMD) $(PWD)/terminfo ~/.terminfo

	@make -C rime_input_method/
# @-$(LINK_CMD) $(PWD)/squirrel_toggle_option $$HOME/bin

# Enable apps at startup
#
# osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Hammerspoon.app", hidden:true}' > /dev/null
# osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Karabiner-Elements.app", hidden:true}' > /dev/null

#调整 keyrepeat
# defaults write -g InitialKeyRepeat -int 15 # normal minimum is 15 (225 ms)
# defaults write -g KeyRepeat -int 1.5 # normal minimum is 2 (30 ms)

sudo:
#-$(LINK_CMD) $(PWD)/environment.conf /etc/environment
#	-$(LINK_CMD) $(PWD)/mac-yosemate-set-env.plist /Library/LaunchAgents/mac-yosemate-set-env.plist
	-$(LINK_CMD) $(PWD)/LaunchAgents/limit.plist /Library/LaunchDaemons/limit.plist
	-$(LINK_CMD) $(PWD)/LaunchAgents/limit_proc.plist /Library/LaunchDaemons/limit_proc.plist
	chown root:wheel /Library/LaunchDaemons/limit.plist
	chown root:wheel /Library/LaunchDaemons/limit_proc.plist
# @rm -rf $(PWD)/applescript/EmacsClient.app
# @tar -xf $(PWD)/applescript/EmacsClient.app.tar.gz -C $(PWD)/applescript/
# @-$(LINK_CMD) $(PWD)/applescript/EmacsClient.app /Applications/
# @rm -rf $(PWD)/applescript/EmcacClientOpenWith-InIterm2.app
# @tar -xf $(PWD)/applescript/EmcacClientOpenWith-InIterm2.app.tar.gz -C $(PWD)/applescript/
# @-$(LINK_CMD) $(PWD)/applescript/EmcacClientOpenWith-InIterm2.app /Applications/
# @-mkdir -p /Library/Application\ Support/org.pqrs/config
# @-$(LINK_CMD) $(PWD)/karabiner/karabiner.json /Library/Application\ Support/org.pqrs/config/karabiner.json
	launchctl limit maxfiles 100000 100000
	launchctl limit maxproc 20000 20000
	sudo rm -f  /etc/zprofile
