#!/bin/bash

version=$(sbcl --noinform --disable-ldb --lose-on-corruption --end-runtime-options --eval '(format t "~A" (asdf:component-version (asdf:find-system :docgen)))' --eval "(quit)")

echo -n "Building version $version, hit enter to continue"
read

mkdir docgen_$version
cp -ap src/main/* docgen_$version/
tar zcf docgen_${version}.tar.gz docgen_$version/
rm -rf docgen_$version

echo "All done, it's in docgen_${version}.tar.gz, you should tag it and push it up to github"
