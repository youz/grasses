#!/bin/bash

# run hello.grass on grass.grass on grass.grass on $*

reposroot=`cd $(dirname "$0")/..; pwd`
hello="$reposroot/example/hello.grass"
grass="$reposroot/Grass/grass.grass"

echo "$(cat $grass)V$(cat $hello)" | eval "time $* $grass"
