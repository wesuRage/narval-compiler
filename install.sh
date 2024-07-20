#!/bin/bash

cp tools/fasm /bin/basm

cargo build

cp target/debug/narval /bin/narval

