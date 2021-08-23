# ulc

Untyped lambda calculus and compilation (through closure conversion, to C and WebAssembly).

## Usage

Inside the `runtime` folder, run `make c` or `make wasm`. This will take care of compiling and running the `program.ulc` file.

## Syntax

Top-level definitions have a name, followed by a `=` and terminated by a `;`. The runtime will execute the definition named `run` and then print its memory representation. The following example will output `{ CLOSURE [ ] }` to the terminal once it is executed.

```
compose =
  a => b => c => a (b c);

run =
  compose;
```

Valid identifiers are words consisting of characters ranging `a..z` and `A..Z`.

The syntax of the definitions in the `program.ulc` file are based on the untyped lambda calculus:
- `abc` is a variable;
- `a => b` is an abstraction taking `a` as an argument and returning `b`;
- `a b` is an application of `a` to `b`.

Additionally, some literals are also supported:
- `49` is an integer;
- `51.5` is a real.

Last but not least, primitive operations on the aforementioned literals are also supported:
- `{integer_sum left right}` sums two integers;
- `{real_sum left right}` sums two reals.

It is important to note that, while it is valid to mention variables inside primitives, primitives cannot be partially applied. Eta-expansion is left to the user.

## Caveats

To run the `run` definition:
- A `main` function is defined in the C backend;
- A `_start` function is defined in the WebAssembly backend.

This means that you should avoid using these names for definitions.
