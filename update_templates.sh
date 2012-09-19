#!/bin/sh

echo "Building templates..."
for file in $(find static/js/dikmax -iname "*.soy")
do
  echo $file
  java -jar closure/templates/SoyToJsSrcCompiler.jar --shouldGenerateJsdoc --shouldProvideRequireSoyNamespaces --outputPathFormat {INPUT_DIRECTORY}/{INPUT_FILE_NAME_NO_EXT}.js $file
done
