# Kosu

Kosu is (or will be at least I hope) a statically-typed, expression-oriented language.

The philosophy of Kosu is to have as control over memory as C (manual memory management, pointers) while having some higher features like generics or sum type.

## Roadmap (For now)
- [x] Lexer
- [x] Parser
- [x] Better syntax error handling
- [x] Ast
- [x] Type checking the Ast
- [x] Annote the Ast with type
- [x] Create an intermediate representation using the 3 adress code method 
- [ ] Write a register allocator throught graph-coloring method (Will be in an other repository, to be used as a depedency)
- [ ] Assembly generation
- [ ] Create a website explaining the syntax and the language in general

## Example
```

const EXIT_SUCCESS = 0;

enum (wrapper) {
  some(wrapper),
  none
} option;

struct { 
  x: s8,
  y: u8
} point;

external malloc(u64) anyptr;

external print(stringl; ...) s32 = "printf";


fn default<t>(option: (t) option, default: t) t {
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
  discard print("%s", message);
  $ EXIT_SUCCESS
}


```


