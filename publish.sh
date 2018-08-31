#!/usr/bin/sh

set -e

commit=`git log -n1 --format=%H`

rm -rf public/*

hugo --minify

pushd public

git add --all .
git commit -m "Published from $commit"
git push

popd
