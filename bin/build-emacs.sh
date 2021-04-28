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

export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:${PATH}"
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

export LDFLAGS="-L/usr/local/lib/gcc/10"

if [ $# -gt 0  ]; then
    git clean -fdx;
    ./autogen.sh;
    CC='clang' \
    ./configure \
    --disable-silent-rules \
    --with-native-compilation \
    --enable-locallisppath=/usr/local/share/emacs/site-lisp \
    --prefix=${prefix} \
    --with-modules \
    --with-ns \
    --disable-ns-self-contained;
fi
export INSTALL="/usr/local/opt/coreutils/libexec/gnubin/install -c"
make -j 12
# make bootstrap BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq comp-speed 0)"'
echo more info see INSTALL.REPO when compile error
function catch_errors() {
    echo "script aborted, because of errors";
    exit $?;
}

trap catch_errors ERR;

make install
# rm -rf ${prefix}/Emacs.app
# cp -rf nextstep/Emacs.app  ${prefix}/Emacs.app
# mv -f ${prefix}/bin/emacs ${prefix}/bin/emacsbak
# cat >${prefix}/bin/emacs << EOS
# #!/bin/bash
# find ~/.emacs.d/eln-cache -type f -size -1  -exec rm -f {} \;
# LIBRARY_PATH=/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin19/10.2.0 exec ${prefix}/bin/emacs "\$@"
# #exec ${prefix}/Emacs.app/Contents/MacOS/Emacs "\$@"
# EOS

chmod 755 ${prefix}/bin/emacs
