#!/bin/sh
wordingdir=`dirname $0`
# instead usr/include
# sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /


prefix=/usr/local/emacs
if [ ! -d /usr/local/emacs ]; then
    sudo mkdir -p /usr/local/emacs ;
    sudo chown ${USER}:admin  /usr/local/emacs;
fi
echo ${prefix}
# echo ${wordingdir}/emacs-env
source ${wordingdir}/emacs-env
./autogen.sh
./configure \
--disable-silent-rules \
--enable-locallisppath=/usr/local/share/emacs/site-lisp \
--prefix=${prefix} \
--with-nativecomp \
--with-gnutls \
--without-x \
--without-dbus \
--with-xwidgets \
--without-imagemagick \
--with-modules \
--with-ns \
--with-xml2 \
--disable-ns-self-contained
# ./configure -C
# cd lisp;make autoload
# git clean -fdx
make -J4 || make bootstrap -J4||git clean -fdx&&make bootstrap -J4
 echo more info see INSTALL.REPO when compile error
function catch_errors() {
    echo "script aborted, because of errors";
    exit $?;
}

trap catch_errors ERR;

make install
rm -rf ${prefix}/Emacs.app
cp -rf nextstep/Emacs.app  ${prefix}/Emacs.app
mv -f ${prefix}/bin/emacs ${prefix}/bin/emacsbak
cat >${prefix}/bin/emacs << EOS
#!/bin/bash
exec ${prefix}/Emacs.app/Contents/MacOS/Emacs "\$@"
EOS

chmod 755 ${prefix}/bin/emacs

make -C ~/.emacs.d dump
