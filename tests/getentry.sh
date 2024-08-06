#!/bin/bash

raw=$(readelf -e main | grep Entry)
IFS=" " read -ra parsed <<< "$raw"
echo "${parsed[3]}";