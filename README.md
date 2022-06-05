# Clara

Clara is an upcoming systems programming language.
The compiler generates native code through an LLVM backend.

See [the exampels folder](./examples/) to see the language in action.

Use the following commands to compile an example:

```
$ cargo install --path .
$ clara ./path/to/example
```

The compiler will generate an object file and place it in the `./build` directory.
Afterwards in can be linked (such as with GCC) to produce an executable:

```
$ gcc -no-pie ./build/the_object.o ./lib/clara.c
$ ./a.out
```

**Note:** Cargo, Rust and LLVM 13 are required to compile the Clara compiler.

## Features

The language is still under heavily development and its future is yet unclear.
That being said, the following features have already been implemented:

- [x] Functions
  - [x] Extern functions
- [x] Structs
  - [x] Transparent structs
  - [x] Opaque structs
  - [x] Struct literals
  - [x] Struct field access
- [ ] Pointers
  - [x] Pointer types
  - [ ] Creating pointer values
  - [ ] Dereferencing pointer values
- [ ] Values
  - [x] Strings
  - [x] Integers
  - [x] Bools
  - [x] Arrays
- [ ] Control flow
  - [x] If-else statements
  - [x] While loops
  - [x] For loops
- [ ] FFI
  - [x] Using C functions as extern functions
  - [x] Using opaque C structs
  - [ ] Using transparent C structs
- [ ] ...

## License

Licensed under the [MIT License](./LICENSE).
