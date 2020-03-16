#!/bin/sh
workingdir=`dirname $0`
# instead usr/include
# sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /


# export PATH=/usr/local/opt/gcc/bin:$PATH
prefix=/usr/local/emacs
if [ ! -d /usr/local/emacs ]; then
    sudo mkdir -p /usr/local/emacs ;
    sudo chown ${USER}:admin  /usr/local/emacs;
fi
echo ${prefix}

libs=(
    /usr/local/opt/openssl@1.1
    /usr/local/opt/texinfo
    /usr/local/opt/gnu-sed
    /usr/local/opt/gcc
    /usr/local/opt/libxml2
    # /usr/local/opt/giflib
    /usr/local/opt/jpeg
    /usr/local/opt/libtiff
    /usr/local/opt/gnutls

    # Required by gnutls
    /usr/local/opt/nettle
    /usr/local/opt/libtasn1
    /usr/local/opt/p11-kit
)
CFLAGS=""
LDFLAGS=""
PKG_CONFIG_PATH=""
for dir in "${libs[@]}"; do
    CFLAGS="${CFLAGS}-I${dir}/include "
    LDFLAGS="${LDFLAGS}-L${dir}/lib "
    PKG_CONFIG_PATH="${PKG_CONFIG_PATH}${dir}/lib/pkgconfig:"
    PATH="${PATH}${dir}/bin:"
done
export CPPFLAGS="${CFLAGS}"
export CFLAGS
export LDFLAGS
export PKG_CONFIG_PATH
export PATH

export LDFLAGS="${LDFLAGS}-L/usr/local/lib/gcc/9 "
echo $LDFLAGS
export PATH=$PATH:/usr/local/opt/gnu-sed/libexec/gnubin

git clean -fdx
./autogen.sh
# # CC=/usr/local/opt/gcc/bin/gcc-9 \
# export CC="gcc-9"
# export CPP="cpp-9"
CC='clang' \
./configure \
--disable-silent-rules \
--enable-locallisppath=/usr/local/share/emacs/site-lisp \
--prefix=${prefix} \
--with-nativecomp \
--with-gnutls \
--without-x \
--without-dbus \
--without-imagemagick \
--with-modules \
--with-ns \
--with-xml2 \
--disable-ns-self-contained
# --with-xwidgets \
# ./configure -C
# cd lisp;make autoload
# git clean -fdx
# make bootstrap -j 4
make -j 12 BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq comp-speed 0)"'
# make bootstrap BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq comp-speed 0)"'
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
