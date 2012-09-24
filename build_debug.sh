#!/bin/sh

./update_templates.sh

echo "Fixing style..."
fixjsstyle --strict -r static/js/dikmax -x static/js/dikmax/Templates.js

echo "Updating dependencies..."
closure/library/build/depswriter.py \
  --root_with_prefix="static/js/third_party ../third_party" \
  --root_with_prefix="static/js/soy ../soy" \
  --root_with_prefix="static/js/dikmax ../dikmax" \
  --root=static/js/goog --output_file=static/js/goog/deps.js

echo "Building highlight.js..."
./build_highlightjs.sh

echo "Compiling front testing version..."
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
  --compiler_flags="--compilation_level=ADVANCED_OPTIMIZATIONS" \
  --compiler_flags="--use_types_for_optimization" \
  --compiler_flags="--warning_level=VERBOSE" \
  --compiler_flags="--debug" \
  --compiler_flags="--formatting=PRETTY_PRINT" \
  --compiler_flags="--formatting=PRINT_INPUT_DELIMITER" \
  --compiler_flags="--charset=UTF-8" \
  --compiler_flags="--create_source_map=static/js/script.testing.js.map" \
  --compiler_flags="--define='EXCLUDE_VAULT=true'" \
  > static/js/script.testing.js

sed -i 's/"static\//"\//g' static/js/script.testing.js.map
echo "//@ sourceMappingURL=script.testing.js.map" >> static/js/script.testing.js

echo "Compiling vault testing version..."
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
  --compiler_flags="--compilation_level=ADVANCED_OPTIMIZATIONS" \
  --compiler_flags="--use_types_for_optimization" \
  --compiler_flags="--warning_level=VERBOSE" \
  --compiler_flags="--debug" \
  --compiler_flags="--formatting=PRETTY_PRINT" \
  --compiler_flags="--formatting=PRINT_INPUT_DELIMITER" \
  --compiler_flags="--charset=UTF-8" \
  --compiler_flags="--create_source_map=static/js/script.vault.testing.js.map" \
  --compiler_flags="--define='EXCLUDE_FRONT=true'" \
  > static/js/script.vault.testing.js

sed -i 's/"static\//"\//g' static/js/script.vault.testing.js.map
echo "//@ sourceMappingURL=script.vault.testing.js.map" >> static/js/script.vault.testing.js
