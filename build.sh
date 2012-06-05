#!/bin/sh
echo "Updating sources..."
git pull
echo "Building project..."
cabal configure
cabal build
echo "Stripping comments from executable..."
strip -p --strip-unneeded --remove-section=.comment -o haskell-blog dist/build/haskell-blog/haskell-blog
echo "Packing archive..."
tar --lzma -cf haskell-blog.tar.lzma haskell-blog snaplets static
