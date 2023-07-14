# Changelog

## **Unreleased**
- [Struct Initialization with field name = variable name](https://github.com/EruEri/kosu-lang/pull/68)
    - Unnecessary to initialize a field if a variable with the same name exist in the scope
    - Update error message
    - Type contains buildin type are separed if the module_path is not empty

## **0.2.1**
- [Fix compare number with different sign](https://github.com/EruEri/kosu-lang/pull/66)

## **0.2.0**

- [Array builtin function](https://github.com/EruEri/kosu-lang/pull/63)
    - ```arrayptr```
    - ```arraylen```

- [keyword function 'addressof'](https://github.com/EruEri/kosu-lang/pull/62)

- [Pattern matching base](https://github.com/EruEri/kosu-lang/pull/61)
    - syntax
    - typage
    - TODO: check pattern exhaustives

- [Fix X86](https://github.com/EruEri/kosu-lang/pull/60)
    - Fix Shift operator
    - Fix mov integer greater than $2^{31} - 1$

- [Integer/Float size inference](https://github.com/EruEri/kosu-lang/pull/59)

- [Array subscript](https://github.com/EruEri/kosu-lang/pull/58)

- [Array litteral](https://github.com/EruEri/kosu-lang/pull/56)

- [feature/base_vm](https://github.com/EruEri/kosu-lang/pull/55)
    - Improve function call abi (stack parameters implemented)
    - Kosu virtual machine base code
    - Fix convert asttac -> cfg for switch
    - Kosu bytecode compiler kosuc.bc base code
    

- [Generate man page on install](https://github.com/EruEri/kosu-lang/pull/53)

- [built-in function 'tagof'](https://github.com/EruEri/kosu-lang/pull/52)
    - Changelog creation

## **0.1.0**

- Initial version