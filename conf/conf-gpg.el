;;; conf-gpg.el --- gpg 加密相关

;;; Code:
;; 需要 安装 gnugpg
;; on mac
;; brew install gpg

;;; gpg 的命令用法
;;  对称加密
;; gpg -c message.txt # 对 message.txt 文件进行加密，会提示输入密码，加密后的内容存到 message.txt.gpg 文件中
;; 解密过程
;; gpg -d message.txt.gpg >d.txt 解密的文件存到 d.txt 中
;; gpg  message.txt.gpg  解密的文件存到 message.txt 中

;;非对称加密
;; 公钥的作用：别人用来给你发用你公钥加密的信息＆别人用你的公钥验证你的签名(这个签名用你的私钥生成的，任何人都可以用你的公钥来验证这个签名是不是你签的,以此能确定这个内容确实是你发的)
;; 私钥的作用：你用来创建签名(用你自己的私钥)＆解密别人发给你的信息的(这个信息是别人用你的公钥进行加密的，只有你的私钥能解开)
;; 具体而言，A要想发送加密信息到 B，则：
;; 　　A 有 A 的签名私钥和 B 的加密公匙
;; 　　B 有 A 的签名公匙和 B 的加密私钥

;; 生成公钥私钥
;; gpg --gen-key 其间会让输入用户名 邮箱等,可以用不同的邮箱来代表公钥私钥
;; gpg --export --full-gen-key # 生成可以用于签名、加密、认证的命令,如用于ssh

;; gpg --list-keys   #查看公钥 or -k
;; gpg --list-secret-keys # 查看私钥 or -K
;; gpg -K --with-keygrip
;; 导出公钥
;; gpg --export -a jixiuf >pub.key #-a 表示 ascii 码可打印形式 mailorname 为刚才输入的用户名与邮箱
;; 1 gpg --export-ssh-key <key id> > .ssh/id_rsa.pub
;; gpg --export-ssh-key jixiuf > .ssh/id_rsa.pub # 导出ssh 用公钥
;; 导出私钥
;; gpg --export-secret-keys -a mailorname>pri.key


;; 导入  公钥或私钥
;; gpg --import file.key

;; gpg -e -r jixiufeng .authinfo  # 生成 .authinfo.gpg
;; gpg -ea -r jixiuf message.txt
;; gpg -ear jixiuf message.txt
;; -e 表示加密，-a 表示加密后为 ascii 可打印文件 生成的文件后缀名会为 asc,否则为 gpg 二进制文件
;; -r 后跟接收者，加密的过程会用我的私钥和对方的公钥进行加密，加密后的文件只有对方的私钥可以解密

;; 解密 直接
;; gpg message.txt.asc
;; gpg message.txt.gpg
;; 或
;; gpg -d message.txt.gpg >d.txt 解密的文件存到 d.txt 中


;; 签名
;; 　　为避免别人宣称是你的风险，对所有你加密的东西签名是有用的。签名的意义在于两个方面：
;; Authenticity 和 Integrity。即数字签名可以证明数据是你发送的，并同时证明发送的内容未曾被别人修改过。
;; 签名后的内容包含了原内容与一个签名

;;签名用你的私钥签， 别人用你的公钥来验证签名
;;
;; gpg -s message.txt  会生成 message.txt.gpg 其中的内容为二进制
;; gpg --clearsign message.txt 会生成 message.txt.asc 其中的内容为 ascii
;; -s 与 --clearsign 区别只是是否可读，通常发一个签名的邮件会用 --clearsign 生成可读的文本
;; 上述签名后的内容与签名混成了一个文件 可以用 gpg -d  message.txt.asc >a.txt 来去除签名，只保留原内容
;; 也可以将签名与原内容分开 gpg --detach-sign message.txt 生成的签名单独放到一个文件中 message.txt.sig

;;验证签名 (验证签名之前需要先导入对方的公钥)  gpg --import file.key
;; gpg --verify message.txt.asc


;;  签名且加密
;; 将上面签名 与加密的命令合并即可
;; 如 gpg -s -e -a -r jixiuf message.txt
;; 如解密的时候会提示签名是否正确 gpg -d message.txt.asc>a message.txt
;;
;;         jixiuf@jxfhome ~ $ gpg -d fund.txt.asc >a                                                                             2
;;         gpg: encrypted with 2048-bit RSA key, ID EF1EF652, created 2017-02-01
;;               "jixiuf (jixiuf rsa keys) <jixiuf@qq.com>"
;;         gpg: Signature made Thu Feb  2 15:13:10 2017 CST using RSA key ID 8BA69D04
;;         gpg: Good signature from "jixiuf (jixiuf rsa keys) <jixiuf@qq.com>" [ultimate]
;;
;; 为什么要先签名后加密？
;; 答：
;; 　　看你发送的这个信息算不算秘密。算的话必须先签名后加密。
;; 如果不算的话都行。但签名后加密比较好。除非要大家都能验证信息来源。
;; 　　对于这个问题我们举个例子：
;; 　　对于先加密后签名
;; 　　ABC
;; 　　A 要把秘密给 B。
;; 　　那么
;; 　　A 有 A 的签名私钥和 B 的加密公匙
;; 　　B 有 A 的签名公匙和 B 的加密私钥

;; 　　现在 C 加入。欺骗 B 说他也知道这个秘密（其实他不知道）
;; 　　C 把自己的签名公匙给 B
;; 　　因为 C 知道 A 的签名公匙，C可以把 A 给 B 的信息签名去掉再加上自己的签名
;; 　　这个时候
;; 　　B 有用自己公匙加密的 2 条信息。明文是一样的。一个 A 签名一个 C 签名。B是无法知道谁真正知道秘密。
;; 　　
;; 　　如果是先签名后加密。
;; 　　因为 C 不可能知道加密密匙，所以不能解密后改签名。所以就不能欺骗 B 说他也知道秘密。
;; 　　
;; 　　所以先加密后签名的缺陷不是 C 能知道秘密，而是 B 可能被欺骗。
;; 　　因此签名都用在义务宣言上，表明自己说过的负责，不会不认帐。大家不会没事去冒名。（比如欠条）
;; 　　如果对于别人会冒名的宣言，比如（出售机密，勒索等）如果签名没有被加密，B就可能向不知道秘密的人买秘密。
;;; easypg，emacs 自带
(require 'epa-file)
(epa-file-enable)
(setf epg-pinentry-mode 'loopback)
;; ;; 总是使用对称加密
;; ;; 设置成不是 t 与 nil 的期他值 以使用对称加密（即提示用户输入密码以解密 而非使用公钥私钥的形式）
;; (setq-default epa-file-select-keys t)
;; (setq-default epa-file-encrypt-to nil) ;默认用哪个公钥私钥解密
;; ;; -*- epa-file-encrypt-to: ("your@email.address") -*-
;; ;; 允许缓存密码，否则编辑时每次保存都要输入密码
;; (setq-default epa-file-cache-passphrase-for-symmetric-encryption nil)

;; (setq-default epa-file-inhibit-auto-save nil)

(provide 'conf-gpg)

;; Local Variables:
;; coding: utf-8
;; End:

;;; conf-gpg.el ends here.
