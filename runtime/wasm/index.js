function read_null_terminated_string(instance, pointer) {
  let memory = new Uint8Array(instance.exports.memory.buffer);
  let null_terminator_pointer = pointer;

  while (memory[null_terminator_pointer] != 0x00) {
    null_terminator_pointer += 1;
  }

  let bytes = memory.slice(pointer, null_terminator_pointer);

  return new TextDecoder('utf-8').decode(bytes);
}

function io_integer(value) {
  document.body.innerHTML += value;
}

function io_real(value) {
  document.body.innerHTML += value;
}

function io_string(instance, pointer) {
  document.body.innerHTML += read_null_terminated_string(instance, pointer);
}

function io_panic(instance, pointer) {
  throw new WebAssembly.RuntimeError(read_null_terminated_string(instance, pointer));
}

let { instance } = await WebAssembly.instantiateStreaming(fetch("output.wasm"), {
  env: {
    io_integer: value => io_integer(value),
    io_real: value => io_real(value),
    io_string: pointer => io_string(instance, pointer),
    io_panic: pointer => io_panic(instance, pointer),
  }
});

let exit_code = instance.exports.main();

if (exit_code != 0) {
  throw new WebAssembly.RuntimeError("Exit code was " + exit_code);
}
