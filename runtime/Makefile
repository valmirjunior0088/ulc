.SILENT:

build/program.c: program.ulc
	stack exec ulc-compiler c program.ulc build/program.c

build/output: deps/io.std.c deps/memory.std.c deps/object.c build/program.c
	clang deps/io.std.c deps/memory.std.c deps/object.c build/program.c -o build/output -I include -O3

.PHONY: c

c: build/output
	build/output

build/index.html: deps/index.html
	cp deps/index.html build/index.html

build/memory.wasm.o: deps/memory.wasm.c
	clang deps/memory.wasm.c -o build/memory.wasm.o --target=wasm32 --no-standard-libraries -I include -c -O3

build/object.wasm.o: deps/object.c
	clang deps/object.c -o build/object.wasm.o --target=wasm32 --no-standard-libraries -I include -c -O3

build/program.wasm.o: program.ulc
	stack exec ulc-compiler wasm program.ulc build/program.wasm.o

build/output.wasm: build/memory.wasm.o build/object.wasm.o build/program.wasm.o
	wasm-ld build/memory.wasm.o build/object.wasm.o build/program.wasm.o -o build/output.wasm --allow-undefined --entry main -O3

.PHONY: wasm

wasm: build/index.html build/output.wasm
	echo "Started server on http://localhost:8080"
	stack exec ulc-server 8080 build
