function read_null_terminated_string(instance, pointer) {
  let memory = new Uint8Array(instance.exports.memory.buffer);
  let null_terminator_pointer = pointer;

  while (memory[null_terminator_pointer] != 0x00) {
    null_terminator_pointer += 1;
  }

  let bytes = memory.slice(pointer, null_terminator_pointer);

  return new TextDecoder('utf-8').decode(bytes);
}

function print_integer(integer) {
  document.body.innerHTML += integer;
}

function print_real(real) {
  document.body.innerHTML += real;
}

function print_string(instance, pointer) {
  document.body.innerHTML += read_null_terminated_string(instance, pointer);
}

function panic(instance, pointer) {
  throw new WebAssembly.RuntimeError(read_null_terminated_string(instance, pointer));
}

let { instance } = await WebAssembly.instantiateStreaming(fetch("output.wasm"), {
  env: {
    print_integer: integer => print_integer(integer),
    print_real: real => print_real(real),
    print_string: pointer => print_string(instance, pointer),
    panic: pointer => panic(instance, pointer),
  }
});

let exit_code = instance.exports.main();

if (exit_code != 0) {
  throw new WebAssembly.RuntimeError("Exit code was " + exit_code);
}
