;; iterm2下实同一些 终端下本没有的按键
;;参见 这个链接中含中文  http://jixiuf.github.io/blog/emacs-在mac上的安装及一些相应配置/#orgheadline15
(defun iterm2-keybind-mapping()
  (global-set-key (kbd "C-[ [ a a") 'backward-kill-word) ;== "M-[ a a" iterm2 map to ctrl-backspace
  ;; (global-set-key (kbd "C-[ [ a c") 'hippie-expand)   ; iterm map to ctrl-return
  (global-set-key (kbd "C-[ [ a d") (key-binding (kbd "C-,") ))   ;iterm2 map to ctrl-,

  (global-set-key (kbd "C-[ [ a e") (key-binding (kbd "C-.") ))   ;iterm2 map to ctrl-.
  (global-set-key (kbd "C-[ [ a f") (key-binding (kbd "C-;") ))   ; iterm map to ctrl-;
  (global-set-key (kbd "C-[ [ a h") (key-binding (kbd "C-i") )) ; iterm map to C-i
  (global-set-key (kbd "C-[ [ a i") (key-binding (kbd "C-3") ))   ;iterm2 map to ctrl-3
  (global-set-key (kbd "C-[ [ a j") 'ignore) ; iterm map to C-4
  (global-set-key (kbd "C-[ [ a j") (key-binding (kbd "C-4") ))   ;iterm2 map to ctrl-f3
  ;; (global-set-key (kbd "C-w C-[ [ a h") 'goto-definition) ; C-wC-i

  (global-set-key (kbd "C-<f3>") 'cd-iterm2)
  (global-set-key (kbd "C-[ [ a l") (key-binding (kbd "C-<f3>") ))   ;iterm2 map to ctrl-f3


  (global-set-key (kbd "C-[ [ a b") (key-binding (kbd "C-<f2>") )) ; map to ctrl-f2
  )
(iterm2-keybind-mapping)

(provide 'conf-iterm2)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-iterm2.el ends here.
