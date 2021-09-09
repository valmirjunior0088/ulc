.PHONY: c

c/program.c: program.ulc
	@stack run c program.ulc c/program.c

c/output.exe: c/program.c
	@clang c/io.c c/memory.c c/object.c c/program.c -o c/output.exe -I headers -O3

c: c/output.exe
	@c/output.exe

wasm/memory.wasm: wasm/memory.c
	@clang wasm/memory.c -o wasm/memory.wasm --target=wasm32 --no-standard-libraries -I headers -c -O3

wasm/object.wasm: wasm/object.c
	@clang wasm/object.c -o wasm/object.wasm --target=wasm32 --no-standard-libraries -I headers -c -O3

wasm/program.wasm: program.ulc
	@stack run wasm program.ulc wasm/program.wasm

wasm/output.wasm: wasm/memory.wasm wasm/object.wasm wasm/program.wasm
	@wasm-ld wasm/memory.wasm wasm/object.wasm wasm/program.wasm -o wasm/output.wasm --allow-undefined --entry main -O3
