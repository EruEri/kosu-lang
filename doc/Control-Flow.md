# Control flow 
- in Kosu:
    - each control flow is an expression
    - each block return the last expression with start with ```$``` sign
        - if omitted, the block has the type unit

- if a conditional block contain only one expression, it can be written like
```nim
    discard if (true) (expr1) else (expr2);
    discard cases { 
        of true => (expr1)
        ... 
        else (expr2)
    }
```

## If

```rust
    const res : s32 = if (true) {
        $ 20
    } else {
        $ 10
    };

    // if the else branch is missing, the compiler create an else returning the unit type
    // so the if branch must return the unit type
```

## Cases

```nim
    discard cases {
        of true => {
            $ "Hello word"
        }
        of false => {
            $ "Hello never"
        }
        else {
            $ "Really never"
        }
    }

     // if the else branch is missing, the compiler create an else returning the unit type
    // so all the other branches must return the unit type
```

## While
```rust
    // the type of the while loop is unit
    // so type of while loop body must be unit too

    const unit : unit = while (true) {
        discard 10;
    }
```

## Switch
- currently the switch expression must be of the type of an enum

```rust
    enum direction {
        up, 
        right,
        down,
        left
    }

    const d = .up;
    discard switch(d) {
        .up | .down => (true)
        _ => (false)
    }

    // if the all the cases are handled the "_" is not required

    discard switch(d) {
        .up | .down => (true)
        .left | .right  => (false)
    }

    // We can also bind variable in the switch
    const o : option(s32) = .some(10);
    discard switch(o) {
        .none => (empty)
        .some(n) => {
            // do something with n
        }
    }
```