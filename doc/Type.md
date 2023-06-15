# Type 
## Struct
```
   struct coordinate {
        x: u64,
        y: u64
   }

   let c = coordinate { x = 10u64, y : 0u64 };

   // struct can also contain generic type

   struct foo(t) {
        bar: t
   }
   let t = foo { bar = 10 }; // has the type foo(s32)
   let t2 = foo {bar = "Hello"}; // has the type foo(stringl)
```

## Enum 
```
    enum status {
        ok,
        error
    }

    const s = .ok;
    // or to resolve conflict with existing enum variant
    consr s2 = status::error;

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