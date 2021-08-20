#!/bin/zsh

# Generate
stack run c program.ulc > c/program.c

# Build
clang c/io.c c/memory.c c/object.c c/program.c -o c/output.exe -I headers

# Run
c/output.exe
