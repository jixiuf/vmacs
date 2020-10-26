#!/bin/sh
workingdir=`dirname $0`
# instead usr/include
# sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /


# export PATH=/usr/local/opt/gcc/bin:$PATH
prefix=$HOME/gccemacs
if [ ! -d $prefix ]; then
    sudo mkdir -p $prefix ;
    # sudo chown ${USER}:admin  $prefix ;
fi
echo ${prefix}
export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:${PATH}"
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

export LDFLAGS="-L/usr/local/lib/gcc/10"
function catch_errors() {
    echo "script aborted, because of errors";
    exit $?;
}

trap catch_errors ERR;

if [ $# -gt 0  ]; then
    git clean -fdx
    ./autogen.sh
    CC='clang' \
      ./configure \
      --disable-silent-rules \
      --enable-locallisppath=/usr/local/share/emacs/site-lisp \
      --prefix=${prefix} \
      --with-nativecomp \
      --with-ns \
      --disable-ns-self-contained
fi
# make bootstrap BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq comp-speed 0)"'
export INSTALL="/usr/local/opt/coreutils/libexec/gnubin/install -c"

# sysctl hw.logicalcpu
make NATIVE_FAST_BOOT=1
make install #-j 8
# rm -rf ${prefix}/Emacs.app
# cp -rf nextstep/Emacs.app  ${prefix}/Emacs.app
cat >${prefix}/bin/gccemacs << EOS
#!/bin/bash
find ~/.emacs.d/eln-cache -type f -size -1  -exec rm -f {} \;
LIBRARY_PATH=/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin19/10.2.0 exec ${prefix}/bin/emacs "\$@"

EOS

chmod 755 ${prefix}/bin/gccemacs
ln -sf ${prefix}/bin/gccemacs ~/bin/gccemacs

make -C ~/.emacs.d native
