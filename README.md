# shoo-lang

[![CircleCI](https://circleci.com/gh/sam-jay/shoo-lang/tree/master.svg?style=svg)](https://circleci.com/gh/sam-jay/shoo-lang/tree/master)
[![Coverage Status](https://coveralls.io/repos/github/sam-jay/shoo-lang/badge.svg?branch=master)](https://coveralls.io/github/sam-jay/shoo-lang?branch=master)

## Table of Contents

- [Introduction](#introduction)
- [Requirements](#requirements)
- [Usage](#usage)
- [Example](#example)

## Introduction

Shoo is a general-purpose programming language that is statically scoped and strongly typed. It has imperative and functional programming features with C-like syntax.  Supporting first class functions,  structs,  and  arrays,  it  can  perform  reasonably  complex  tasks  in  a  single-threaded setting.

## Requirements

First, install LLVM 6.0.0. Then, install OCaml and OPAM. OCaml version 4.05.0 is required. 

## Usage

Build the compiler binary using make. Sometimes a clean is needed.

```
$ make clean
$ make
```

Take a look inside testall.sh for the variables which need to be set to the correct system paths of various programs, and set them accordingly. The path to the LLVM interpreter is variable "lli". Set the path to the LLVM compiler as "llc". Set the path to the C compiler as "cc". The following command runs all tests.

```
$./testall.sh
```
When you run the command above, all the tests that came with the compiler will be run and should pass. If they do not all pass, go back to section 2.1 Environment Setup and ensure that your environment is set up correctly.

To write Shoo source code, first create a file with a .shoo extension. Inside that file, write a program following the rules outlined in this language reference manual. Then, save your file and run it following the instructions below. If you need inspiration for your first Shoo program, feel free to copy any of the many examples provided throughout this manual.

To compile a Shoo program into LLVM code:
```
$ ./shoo.native <filename>
```

To compile and execute a Shoo program:
```
./run.sh <filename>
```

## Example

Here is a sample program written in Shoo:

```
struct Object {
	int index;
	int data;
}

function compareData(Object a, Object b) bool {
    return a.data < b.data;
}

function compareIndex(Object a, Object b) bool {
    return a.index < b.index;
}

function printData(array<Object> arr, int n) void {
	for (int i = 0; i < n; i++) {
		println(str_of_int(arr[i].data));
	}
	
	return;
}

function printIndex(array<Object> arr, int n) void {
	for (int i = 0; i < n; i++) {
		println(str_of_int(arr[i].index));
	}
	
	return;
}

function bubbleSort(array<Object> arr, int n, func(Object, Object; bool) compare) array<Object> {
	for (int i = 0; i < n - 1; i++) {
		for (int j = 0; j < n - i - 1; j++) {
			if (compare(arr[j + 1], arr[j])) {
				Object temp = arr[j];
				arr[j] = arr[j + 1];
				arr[j + 1] = temp;
			}
		}
	}
	
	return arr;
}

array<int> indices = [1,3,8,6,9,7,0,2,4,5];
array<int> datapoints = [106,101,104,108,105,103,102,109,107,100];

int n = 10;
array<Object> objects = new(array<Object>[n]);

for (int i = 0; i < n; i++) {
	objects[i] = new(Object);
	objects[i].index = indices[i]; 
	objects[i].data = datapoints[i];
}

printIndex(objects, n);
printData(objects, n);

bubbleSort(objects, n, compareIndex);
printIndex(objects, n);

bubbleSort(objects, n, compareData);
printData(objects, n);
```
