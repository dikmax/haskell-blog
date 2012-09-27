#!/bin/sh

python highlight.js/tools/build.py -n bash css haskell javascript php sql xml
echo 'goog.provide("hljs");' > static/js/highlight.pack.js
cat highlight.js/build/highlight.pack.js >> static/js/highlight.pack.js
