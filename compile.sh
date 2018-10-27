#!/bin/bash

set -e

if [ -z "$1" ]
  then
    echo "No argument supplied"
    exit 1
fi
f=$1
n=${f%.shoo*}
cat $f | ./shoo.native > "$n.ll"
llc -relocation-model=pic "$n.ll"
cc -o "$n" "$n.s"
"./$n"
