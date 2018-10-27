#!/bin/bash

if [ -z "$1" ]
  then
    echo "No argument supplied"
fi

cat "$1" | ./shoo.native > "$1.ll"
llc -relocation-model=pic "$1.ll"
cc -o "$1" "$1.s"
"./$1"
