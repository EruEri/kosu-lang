#!/usr/bin/env sh

cat $1/kosu_lang.opam | grep ^version | awk '{print $2}' | sed 's/\"//g' | tr -d '\t\n'