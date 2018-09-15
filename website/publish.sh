#!/usr/bin/sh

set -e

commit=`git log -n1 --format=%H`


# Be sure that we are up-to-date
pushd public
git pull --ff-only
popd

rm -rf public/*

hugo --minify

pushd public

git add --all .
git commit -m "Published from $commit"
git push

popd
