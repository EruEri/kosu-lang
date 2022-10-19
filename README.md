# Kosu

Kosu is (or will be at least I hope) a statically-typed, expression-oriented language.

The philosophy of Kosu is to have as control over memory as C (manual memory management, pointers) while having some higher features like generics or sum type.

## Roadmap (For now)
- [x] Lexer
- [x] Parser
- [x] Better syntax error handling
- [x] Ast
- [ ] Type checking the Ast <- Currently here
- [ ] Annote the Ast with type
- [ ] Assembly generation (LLVM or hand-written ARM64 assembly ?)
- [ ] Create a website explaining the syntax and the language in general

## Example
```
enum (wrapper) {
  some(wrapper),
  none
} option_t;

struct { 
  x: s8,
  y: u8
} point_t;

fn fibonacci(n: u32) u32 {
  $ cases {
  of n == 0u32 => { $ 0u32 }
  of n == 1u32 => { $ 1u32 }
  else { $ fibonacci(n - 1u32) + finonacci(n - 2u32) }
  }
}

```


