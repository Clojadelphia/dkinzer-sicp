#!/bin/bash

VERSION='9.2'
ARCH=$(uname -m | tr '_' '-') 
NAME="mit-scheme-$VERSION"
URL="http://ftp.gnu.org/gnu/mit-scheme/stable.pkg/$VERSION/$NAME-$ARCH.tar.gz"

wget -nc $URL
tar xvf $NAME-$ARCH.tar.gz
pushd ./$NAME/src

if command -v mit-scheme;
then
  ./Setup.sh
  ./configure
  make compile-microcode
else
  ./configure
  make compile-microcode
fi

sudo make install

popd

