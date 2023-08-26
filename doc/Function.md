# Function

## Base function

```rust
    fn add(x: s32, y: s32) s32 {
        $ x + y
    }

    // Function which contains a single expression can be written with a "="
    // with an optional semicolon at the end
    fn add(x: s32, y: s32) s32 = x + y

    // Main
    // Kosu program entry point
    // main accepts two function signature
    fn main() s32;
    // or
    fn main(argc: s32, argv: **s8) s32;

    // function can also be generic
    fn value<t>(o: option(t), default: t) t = switch(o) {
        .none => (default)
        .some(v) => (v)
    }
```

## Operator

In kosu, some operators can be defined by the user.

```cpp
    // Binary operator
    operator +(lhs: t, rhs: t) t {
        ..
    }
    // Unary operator
    // Parenthesis and dot are meaningful
    operator (.-)(u: t) t {
        ...
    }
```

But those defintions are under some type constraints:

For a type ```t```

- Binary operator:
    - (+): t -> t -> t
    - (-): t -> t -> t
    - (*): t -> t -> t
    - (/): t -> t -> t
    - (%): t -> t -> t
    - (|): t -> t -> t
    - (&): t -> t -> t
    - (^): t -> t -> t
    - (\<\<): t -> t -> t
    - (>>): t -> t -> t
    - (==): t -> t -> bool
    - (<=>): t -> t -> order

- Unary operator:
    - (.-): t -> t
    - (.!): t -> t

- Special cases:
    - (==):
        - If defined then the ( != ) operator is created by the compiler
    - (<=>):
        - If defined then the following operators are created by the compiler:
            - (==)
            - (!=)
            - (\<)
            - (\<=)
            - (>)
            - (>=)