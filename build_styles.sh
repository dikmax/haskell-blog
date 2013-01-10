#!/bin/sh

echo "Building css..."
lessc --compress static/less/front.less static/css/style.css
lessc --compress static/less/vault.less static/css/vault.css
