#!/bin/bash

NARVAL_HOME=$(pwd)
echo "export NARVAL_HOME=$NARVAL_HOME" >> ~/.bashrc
source ~/.bashrc

cargo build

cp target/debug/narval /bin/narval

