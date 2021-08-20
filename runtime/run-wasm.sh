#!/bin/zsh

clang wasm/memory.c -o wasm/memory.wasm --target=wasm32 --no-standard-libraries -c -I headers
clang wasm/object.c -o wasm/object.wasm --target=wasm32 --no-standard-libraries -c -I headers

wasm-ld wasm/memory.wasm wasm/object.wasm wasm/program.wasm -o wasm/output.wasm --allow-undefined
