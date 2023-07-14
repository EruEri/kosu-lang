# Type 
## Struct

```rust
   struct coordinate {
        x: u64,
        y: u64
   }

   const c = coordinate { x = 10u64, y : 0u64 };

   // struct can also contain generic type

   struct foo(t) {
        bar: t
   }
   const t = foo { bar = 10 }; // has the type foo(s32)
   const t2 = foo {bar = "Hello"}; // has the type foo(stringl)

   // if a variable within the scope of the declared structure a the same name as the field,
   // the struct field affection can be omitted

   const x : u64 = 10;
   const c2 = coordinate { x, y = 10 };
```

## Enum 
```rust
    enum status {
        ok,
        error
    }

    const s = .ok;
    // or to resolve conflict with existing enum variant
    const s2 = status::error;

    // enum variant can have associate type

    enum optional_bool {
        bnone,
        bsome(bool)
    }

    const b = .bsome(true)

    // enum can also contain generic

    enum option(t) {
        none,
        some(t)
    }
```