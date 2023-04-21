# Kosu

Kosu is (or will be at least I hope) a statically-typed, expression-oriented language.

The philosophy of Kosu is to have as control over memory as C (manual memory management, pointers) while having some higher features like generics or sum type.

## Roadmap (For now)
- [x] Lexer
- [x] Parser
- [x] Better syntax error handling
- [x] Better logic error handling
- [x] Ast
- [x] Type checking the Ast
- [x] Annote the Ast with type
- [x] Create an intermediate representation using the 3 adress code method 
- [x] Generate basic Arm64 assembly for MacOs
- [x] Generate basic x86_64 assembly
- [x] Fix Syntax issues
- [x] Add while loop
- [x] Fix Immediate encoding on arm64
- [x] Implement Float for both architecture
- [x] Implement Operator overload

== Version 0.1.0 == 

- [ ] Write a register allocator throught graph-coloring method (Will be in an other repository, to be used as a depedency)
- [ ] Integer size inference
- [ ] Add anonymous function
- [ ] Add array Literral
- [ ] Fix stack base function parameters on arm64/x86_64
- [ ] Improve c compatibily ABI



## Example
```

const EXIT_SUCCESS = 0;

enum option(wrapper) {
  some(wrapper),
  none
}

struct point { 
  x: s8,
  y: u8
}

external malloc(_: u64) anyptr;

external print(format: stringl; ...) s32 = "printf";


fn default<t>(option: option(t), default: t) t {
    $ switch (option) {
        .none => { $ default }
        .some(x) => { $ x }
    }
}

// Single line comment
fn fibonacci(n: u32) u32 {
  $ cases {
  of n == 0u32 => { $ 0u32 }
  of n == 1u32 => { $ 1u32 }
  else { $ fibonacci(n - 1u32) + fibonacci(n - 2u32) }
  }
}

fn main() s32 {
  /*
    Multiple lines comment
  */
  const message_opt = .some("Hello world");
  const message = message_opt |> default("Never");
  discard print("%s\n", message);
  $ EXIT_SUCCESS
}
```

## How to build 
```bash
  $ opam install cmdliner menhir dune
  $ git clone https://github.com/EruEri/kosu-lang
  $ cd kosu-lang
  $ dune build
```

To compile the example above on MacOS (M1)
```bash
$ kosuc --arch arm64 --os macos -o example example.kosu
$ ./example
```
On MacOS (Intel)
```bash
$ kosuc --arch x86_64 --os macos -o example example.kosu
$ ./example
```
On FreeBSD (x86_64)
```bash
$ kosuc --arch x86_64 --os freebsd -o example example.kosu
$ ./example
```
On Linux (x86_64)
```bash
$ kosuc --arch x86_64 --os linux -o example example.kosu
$ ./example
```


