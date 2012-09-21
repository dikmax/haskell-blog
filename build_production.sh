#!/bin/sh

./update_templates.sh

echo "Fixing style..."
fixjsstyle --nojsdoc -r static/js/dikmax

echo "Updating dependencies..."
closure/library/build/depswriter.py \
  --root_with_prefix="static/js/third_party ../third_party" \
  --root_with_prefix="static/js/soy ../soy" \
  --root_with_prefix="static/js/dikmax ../dikmax" \
  --root=static/js/goog --output_file=static/js/goog/deps.js

echo "Building highlight.js..."
./build_highlightjs.sh

echo "Compiling testing version..."
closure/library/build/closurebuilder.py \
  --root=static/js/dikmax/ \
  --root=static/js/goog/ \
  --root=static/js/soy/ \
  --root=static/js/third_party/ \
  --namespace="dikmax.main" \
  --output_mode=compiled \
  --compiler_jar=closure/compiler/compiler.jar \
  --compiler_flags="--js=static/js/highlight.pack.js" \
  --compiler_flags="--externs=closure/compiler/externs/webkit_console.js" \
  --compiler_flags="--use_types_for_optimization" \
  --compiler_flags="--compilation_level=ADVANCED_OPTIMIZATIONS" \
  --compiler_flags="--charset=UTF-8" \
  > static/js/script.min.js
