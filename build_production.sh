#!/bin/sh

./build_styles.sh

echo "Building highlight.js..."
./build_highlightjs.sh

echo "Compiling front script..."
closure/library/build/closurebuilder.py \
  --root=static/js/dikmax/ \
  --root=static/js/goog/ \
  --root=static/js/soy/ \
  --root=static/js/third_party/ \
  --namespace="dikmax.main" \
  --output_mode=compiled \
  --compiler_jar=closure/compiler/compiler.jar \
  --compiler_flags="--externs=closure/compiler/externs/webkit_console.js" \
  --compiler_flags="--use_types_for_optimization" \
  --compiler_flags="--compilation_level=ADVANCED_OPTIMIZATIONS" \
  --compiler_flags="--charset=UTF-8" \
  --compiler_flags="--define='EXCLUDE_VAULT=true'" \
  static/js/highlight.pack.js \
  > static/js/script.min.js

echo "Compiling vault script..."
closure/library/build/closurebuilder.py \
  --root=static/js/dikmax/ \
  --root=static/js/goog/ \
  --root=static/js/soy/ \
  --root=static/js/third_party/ \
  --namespace="dikmax.main" \
  --output_mode=compiled \
  --compiler_jar=closure/compiler/compiler.jar \
  --compiler_flags="--externs=closure/compiler/externs/webkit_console.js" \
  --compiler_flags="--use_types_for_optimization" \
  --compiler_flags="--compilation_level=ADVANCED_OPTIMIZATIONS" \
  --compiler_flags="--charset=UTF-8" \
  --compiler_flags="--define='EXCLUDE_FRONT=true'" \
  static/js/highlight.pack.js \
  > static/js/script.vault.min.js
