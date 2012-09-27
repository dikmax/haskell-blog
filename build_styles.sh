#!/bin/sh

echo "Building css..."
lessc static/less/front.less static/css/style.css
lessc static/less/vault.less static/css/vault.css
