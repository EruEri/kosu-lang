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

- [ ] Write a register allocator throught graph-coloring method
- [x] Integer/Float size inference
- [ ] Add anonymous function
- [x] Add array Literral
- [x] Fix stack base function parameters on arm64/x86_64
- [ ] Improve c compatibily ABI

== Version 0.5.0 == 

Full rewirte

- New ast
  - change syntax : (removal of $)
  - new type
  - proper support for closure and function pointer 
- New typechecking algorithm ( kind of hindley milner algorithm)
- Remove kosu virtual machine:
  - Remove libffi dependance
- ... etc



## Example
```
const EXIT_SUCCESS : s32 = 0

enum option('wrapper) {
  some('wrapper),
  none
}

struct point { 
  x: s8,
  y: u8
}

external puts(format: stringl) s32 = "puts"

external kosu_u32_add(a: u32, b: u32) u32 = "kosu_u32_add"
external kosu_u32_sub(a: u32, b: u32) u32 = "kosu_u32_sub"
external kosu_u32_eq (a: u32, b: u32) bool = "kosu_u32_eq"


fn 'a . default(option: option('a), def: 'a) 'a =
    match option {
        | .none -> { def }
        | .some(x) -> { x }
    }


// Single line comment
fn fibonacci(n: u32) u32 = {
  var `+` = kosu_u32_add;
  var `-` = kosu_u32_sub;
  var `==` = kosu_u32_eq;
  cases {
    of n == 0 -> { 0 }
    of `==`(n, 1) -> { 1 }
    else { fibonacci(n - 1) + fibonacci(n - 2) }
  }
}

fn main() s32 {
  /*
    Multiple lines comment
  */
  const message_opt = .some("Hello world");
  const message =  default(message_opt, "Never");
  discard puts(message);
  0
}
```

## How to build 

### With Make
- GNU make as make
```bash
  $ opam install dune cmdliner menhir asai
  $ cd kosu-lang
  $ make configure
  $ ./configure
  $ make
  $ make install
```
- GNU make as gmake (example: FreeBSD)
```bash
  $ opam install dune cmdliner menhir asai
  $ cd kosu-lang
  $ gmake configure
  $ ./configure
  $ gmake
  $ gmake install
```

### With Opam
```bash
  $ opam install dune cmdliner menhir asai
  $ cd kosu-lang
  $ opam install .
```

Once build, the following programs are generated:

| command     | description |
| ----        | ------
| `kosuc`     | the Kosu native compiler
| `kosu`      | the Kosu utility



## How to run

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

## Other
- You can see more detail about:
  - in [Readme.md](/doc/Readme.md)
  - in [test](/test/files/)

