# shoo-lang

[![CircleCI](https://circleci.com/gh/sam-jay/shoo-lang/tree/master.svg?style=svg)](https://circleci.com/gh/sam-jay/shoo-lang/tree/master)
[![Coverage Status](https://coveralls.io/repos/github/sam-jay/shoo-lang/badge.svg?branch=master)](https://coveralls.io/github/sam-jay/shoo-lang?branch=master)

## Table of Contents

- [Requirements](#requirements)
- [Usage](#usage)
- [Introduction](#introduction)
- [Running tests](#running-tests)

## Requirements

Install OCaml and OPAM.
OCaml version 4.04.0 or higher is required.

* Install dependencies:
```sh
$ opam install ounit
```

## Usage

Build the compiler binary using make.

```sh
$ make
```

## Introduction

Shoo is a programming language similar to Go which provides intuitive primitives for concurrent programming.

Here is an example program written in Shoo:

```
pipe<int> messages = make(pipe<int>);

array< array<int>[10] >[2] tasks = [
  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
  [11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
];
  
function int sum(int x, int y) {
  return x + y;
}

function any foldl(func f, any acc, array<any> items) {
  if (length(items) == 0) { //length is a built-in function
    return acc;
  } else {
    return foldl(f, f(acc, first(items)), rest(items));
  }
}

for (int i; i < length(tasks); i++) {
  shoo function(array<int> task) {
    messages <- foldl(sum, 0, task);
  }(tasks[i]);
}

int final = 0;

for (int i; i < length(tasks); i++) {
  int result <- messages;
  final += result;
}

println(final);
```

## Running tests

```sh
$ make test
$ ./test.native
```