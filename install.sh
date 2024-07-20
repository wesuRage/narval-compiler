#!/bin/sh

[ "$UID" -ne 0 ] &&  echo "Run the script as root."; exit 1


if [ -z ${NARVAL_HOME+x} ]; then
    NARVAL_HOME=$(pwd)
    echo -e "\nNARVAL_HOME=$NARVAL_HOME" >> /etc/bash.bashrc
    . /etc/bash.bashrc
fi

cp tools/fasm /bin/basm

cargo build

cp target/debug/narval /bin/narval

