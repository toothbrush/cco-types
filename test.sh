#!/bin/sh
cat $1 | dist/build/parse-hm/parse-hm | dist/build/hm2systemf/hm2systemf | dist/build/pp-systemf/pp-systemf
