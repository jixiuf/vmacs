#!/bin/sh
prefix=/usr/local/emacs
sudo mkdir -p /usr/local/emacs
sudo chown ${USER}:admin  /usr/local/emacs
echo ${prefix}
export PATH=$PATH:/usr/local/opt/gnu-sed/libexec/gnubin:/usr/local/opt/gnu-sed/bin
./autogen.sh
./configure \
--disable-silent-rules \
--enable-locallisppath=/usr/local/share/emacs/site-lisp \
--prefix=${prefix} \
--with-gnutls \
--without-x \
--without-dbus \
--without-imagemagick \
--with-modules \
--with-ns \
--without-xml2 \
--disable-ns-self-contained
make
make install
rm -rf ${prefix}/Emacs.app
cp -rf nextstep/Emacs.app  ${prefix}/Emacs.app
mv -f ${prefix}/bin/emacs ${prefix}/bin/emacsbak
cat >${prefix}/bin/emacs << EOS
#!/bin/bash
exec ${prefix}/Emacs.app/Contents/MacOS/Emacs "\$@"
EOS

chmod 755 ${prefix}/bin/emacs
