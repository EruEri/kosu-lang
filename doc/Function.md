# Function

```
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