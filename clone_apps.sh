#!/bin/sh
mkdir sparse
cd sparse
git init
git remote add -f origin https://github.com/aeternity/aeternity.git
git config core.sparseCheckout true

echo "aecore" >> .git/info/sparse-checkout
echo "aefate" >> .git/info/sparse-checkout
echo "aecontract" >> .git/info/sparse-checkout

git pull origin master
git checkout 92a15d46
cd ..


cd apps
yes | mix new aecore
yes | mix new aefate
yes | mix new aecontract
cd ..

cp -r ./sparse/apps .

