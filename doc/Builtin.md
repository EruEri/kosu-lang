- Kosu contains a set of builtin type and functions
## Builtin-type:
- **bool**: either ```true``` or ```false```
- **char**: an unsigned 8-bits integer
- **unit**: type which can represent the absence of value with one constructor with two syntax: 
    - the empty tuple: ```()```
    - keyword: ```empty```
- **stringl**: non-mutable null terminated string litteral
- **order**: type used for comparing value:
    - ```lt```: less than
    - ```eq```: equal
    - ```gt```: greater than
- **s8** : 8-bits signed integer
- **u8** : 8-bits unsigned integer
- **s16**: 16-bits signed integer
- **u16**: 16-bits unsigned integer
- **s32**: 32-bits signed integer (default integer type)
- **u32**: 32-bits unsigned integer
- **s64**: 64-bits signed integer
- **u64**: 64-bits unsigned integer
- **f32**: ieee 754 stantard single precision floating-point number (32 bits)
- **f64**: ieee 754 stantard double precision floating-point number (64 bits) (default float type)
- **\*t**: pointer to a variable of type **t**
- **(t1,..., tn)**: a tuple constituated of expression of type **(t1, ... , tn)**
- **array(N: t)** : an array of ```N``` elements of type ```t```
    - Kosu array are passed by value
- **anyptr**: 
    - Only available in the context of declaring a syscall or an external function:
    - Represent a pointer toward any type

## Builtin function

- builtin function start with an ```@```

- anynumber = u8 | s8 | u16 | s16 | u32 | s32 | u64 | s64 | f32 | f64
    - @tou8(anynumber | char) -> u8
    - @tos8(anynumber) -> s8
    - @tou16(anynumber) -> u16
    - @tos16(anynumber) -> s16
    - @tou32(anynumber) -> u32
    - @tos32(anynumber) -> s32
    - @tou64(anynumber) -> u64
    - @tos64(anynumber) -> s64
    - @tof32(anynumber) -> f32
    - @tof64(anynumber) -> f64
    - @stringlptr(stringl) -> *s8
    - @tagof(enum_type) -> u32

## Keywords

### Sizeof
- sizeof(type) -> u64
- sizeof(: expression) -> u64

### Discard
- the keyword ```discard``` is used if you don't need the result of an expression
```
    discard function_call();
```



## Builtin-Builtin-type

```
    // to declare a variable we use the const or var keyword
    const x = 10; // x has the type s32
    var f = 23.0; // f has the type f64
    // for other numeric type you need to suffix the value with the type
    const x2 = 8u8; // x has the type u8

    const f2 = 33.0f32 // f2 has the type f32

    // stringl
    const x = "Hello world";

    // an explicit type can be added optionally, or sometime required when the compiler can not fully infer the type
    // for example, the keyword expression nullptr need an explicit type
    const b : *s32 = nullptr;

    // tuple can be constructed by putting between parenthesis more than one expression separed by comma
    const t : (stringl, s32) = (x, 25)

    // to get the address of a variable, we use the &
    const ptr_t : *(stringl, s32) = &t;

    // to derefence, we use the *
    const t2 = *ptr_t;

    // All value of an array must be initialized
    const a : array(3, s32) = [1, 2, 3];

    // To initialize all the index with a default value
    const b = [3: expr];

```