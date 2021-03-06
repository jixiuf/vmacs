#!/bin/sh
workingdir=`dirname $0`
# instead usr/include
# sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /
export FD_SETSIZE=10240

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

export LDFLAGS="-L/usr/local/lib/gcc/11"
export INSTALL="/usr/local/opt/coreutils/libexec/gnubin/install -c"
# make bootstrap BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq comp-speed 0)"'
echo more info see INSTALL.REPO when compile error
function catch_errors() {
    echo "script aborted, because of errors";
    exit $?;
}

trap catch_errors ERR;

if [ $# -gt 0  ]; then
    git clean -fdx;
    ./autogen.sh;
    CC='clang' \
    ./configure \
    --with-native-compilation \
    --prefix=${prefix} \
    --with-modules \
    --with-ns
fi
make -j 12

make install
rm -rf ${prefix}/Emacs.app
cp -rpf nextstep/Emacs.app  ${prefix}/Emacs.app
mv -f ${prefix}/bin/emacs ${prefix}/bin/emacsbak
cat >${prefix}/bin/emacs << EOS
#!/bin/bash
export LIBRARY_PATH=/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin19/11.1.0:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin20/11.1.0
exec ${prefix}/Emacs.app/Contents/MacOS/Emacs "\$@"
EOS

chmod 755 ${prefix}/bin/emacs
