#!/bin/sh
echo "Updating sources..."
git pull

echo "Building project..."
cabal configure
cabal build

echo "Stripping comments from executables..."
strip -p --strip-unneeded --remove-section=.comment -o haskell-blog dist/build/haskell-blog/haskell-blog
strip -p --strip-unneeded --remove-section=.comment -o disqus-sync dist/build/disqus-sync/disqus-sync

./build_production.sh

echo "Packing archive..."
tar --lzma \
  --exclude 'static/js/dikmax/*' --exclude 'static/js/dikmax' \
  --exclude 'static/js/goog/*' --exclude 'static/js/goog' \
  --exclude 'static/js/soy/*' --exclude 'static/js/soy' \
  --exclude 'static/js/third_party/*' --exclude 'static/js/third_party' \
  --exclude 'static/js/highlight.pack.js' \
  --exclude 'static/less/*' --exclude 'static/less' \
  -cf haskell-blog.tar.lzma haskell-blog disqus-sync snaplets static

echo "Transfer archive to server..."
rcp haskell-blog.tar.lzma dikmax.name:/home/dikmax/www/haskell-blog.tar.lzma

#echo "Updating server..."
#ssh dikmax.name <<'ENDSSH'
#cd ~/www
#./update.sh
#ENDSSH
