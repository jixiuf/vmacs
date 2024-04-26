登录时 自动解锁gpg
1. emerge sys-auth/pam-gnupg #from guru
2. 需要保证user 登录时的密码与 gpg 的蜜码相同
3. /etc/pam.d/system-local-login 
   auth 最后一行添加
  auth     optional  pam_gnupg.so store-only
  session 最后一行添加
  session  optional  pam_gnupg.so
4. /etc/pam.d/swaylock 末尾添加
auth     optional  pam_gnupg.so

5 .gpg -K --with-keygrip
将需要自动解锁的  keygrip 加入到 $HOME/.pam-gnupg  一行一个