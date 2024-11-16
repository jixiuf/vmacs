;;; conf-erc.el --- Description -*- lexical-binding: t; -*-
(setq erc-nick "jixiuf")
;; (setq erc-server "irc.libera.chat")
;; (setq erc-port 6667)
(setq erc-prompt-for-password nil)

(setq erc-echo-notices-in-minibuffer-flag t
      ;; erc-server-coding-system '(utf-8 . utf-8)
      ;; erc-encoding-coding-alist '(("#linuxfire" . chinese-iso-8bit))
      erc-kill-buffer-on-part t)
;; (erc :server "irc.libera.chat" :port 6667 :nick "jixiuf")
;; 连接erc 后，自动打开某些channel
(setq erc-autojoin-channels-alist
      '(("irc.libera.chat"
         "#emacs"
         "#hyprland"
         )))
(require 'erc)
(erc-autojoin-mode 1)


;; 4. 过滤信息
;; 如果你对某些消息或者某个人说的话特别感兴趣，我们可以通过关键字匹配对相关信息进行高亮。例如：
(erc-match-mode 1)
(setq erc-keywords '("emacs"))
;; (setq erc-pals '("rms")) ;这个不懂，comment 之


;; 相反地，如果你对某些消息不感兴趣，比如有人进来啦，有人出去啦，如此这般一下就不会看到了：
(setq erc-ignore-list nil)
(setq erc-hide-list
      '("JOIN" "PART" "QUIT" "MODE"))


;; 5. 新信息提醒
;; 信息一般可分为三种：
;; 1) 某人悄悄跟你说话(即所谓的 private message)，这会打开一个新小窗，即 buffer.
;; ERC>/msg NICK how are you doing
;; 2) 某人公开地跟你说话，即别的在 channel 里的人也能看到。一般来说，习惯用 nick加 `:’ 表示。
;; (要输入某人 nick 的时候，首字母加 TAB 就能帮你补全，一次不行，多 TAB 几次可以选择)
;; <xwl> ahei: 你可以 match regexp,
;; 3) 别的情形。
;; ERC 会通过 erc-modified-channels-object 来设置 mode line，提示有新消息，类似：
;; [#o: 38, #emacs-cn: 5]
;; 为什么要区分以上三种情形呢? 因为我们可以对不同信息，
;; 用不同的颜色在mode line 来提示，这样方便你决定是不是要及时地去查阅这条消息。



(provide 'conf-erc)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-erc.el ends here.
