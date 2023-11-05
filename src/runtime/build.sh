#!/bin/sh

cc --std=c99 -fPIC -Wall -c src/*.c
ar rcs libkosu.a *.o