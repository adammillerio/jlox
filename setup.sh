#!/usr/bin/env bash
echo 'installing and enabling openjdk, password required for sudo'
~/aesource/aecode/tools/sh/utils/smn.sh java setup

echo 'installing maven'
brew install maven
