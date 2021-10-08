.PHONY: c wasm

c/program.c: program.ulc
	@stack exec ulc-compiler c program.ulc c/program.c

c/output: c/program.c
	@clang c/io.c c/memory.c c/object.c c/program.c -o c/output -I h -O3

c: c/output
	@c/output

wasm/memory.o: wasm/memory.c
	@clang wasm/memory.c -o wasm/memory.o --target=wasm32 --no-standard-libraries -I h -c -O3

wasm/object.o: wasm/object.c
	@clang wasm/object.c -o wasm/object.o --target=wasm32 --no-standard-libraries -I h -c -O3

wasm/program.o: program.ulc
	@stack exec ulc-compiler wasm program.ulc wasm/program.o

wasm/output.wasm: wasm/memory.o wasm/object.o wasm/program.o
	@wasm-ld wasm/memory.o wasm/object.o wasm/program.o -o wasm/output.wasm --allow-undefined --entry main -O3

wasm: wasm/output.wasm
	@stack exec ulc-server 8080 wasm
