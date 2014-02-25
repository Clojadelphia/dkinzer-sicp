#!/bin/bash

VERSION='9.1.1'
ARCH=$(uname -m | tr '_' '-') 
NAME="mit-scheme-$VERSION"
URL="http://ftp.gnu.org/gnu/mit-scheme/stable.pkg/$VERSION/$NAME-$ARCH.tar.gz"

wget -nc $URL
tar xvf $NAME-$ARCH.tar.gz
pushd ./$NAME/src
./configure
make compile-microcode
sudo make install
popd

