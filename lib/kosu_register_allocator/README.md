# **Kosu-register-allocator**

This library is supposed to become the register allocator of [Kosu](https://github.com/EruEri/kosu-lang).

The allocation and the graph coloration is based on the **Kempe**'s algorithm

To use the library:

- You need to transform your program into a 
a control flow graph defined into the functor [Cfg.Make](/lib/register_allocator/cfg.mli)

- Once you have the cfg, you can transfom it into more detailed iteration such as ```Detail``` which add information on the exit and entrance of variable for each basic block, or ```Liveness``` which add liveness information for each statemement.

- To color the **cfg_liveness**, you will need to instance the functor ```GreedyColoring```  which is contained into ``` Cfg.Make```. ```GreedyColoring``` expects a module containing some detail about the [ABI](/lib/register_allocator/cfg.mli).
- After that you can call the **coloration** function which return a [graph](/lib/register_allocator/graph.ml)

## **Build**

To build the library only:
```
$ dune build
```
is enough

## **San**
``Kosu-register-allocator`` embed also a minimalist 3 address-code language, ```san```. By default, the library is built without ```san``` but can be compiled with:
```
$ dune build --profile san
``` 

You will also need to install the following opam packages:
- menhir
- cmdliner

### Usage 

```san``` contains two subcommand:
- cfg:
    - Used to visual control flow graphs
- compile
    - Used to generate executable or assmebly files

## Disclaimer
- I did what I understood of the register allocation algorithm so I'm pretty sure that aren't the optimal or even without errors 
- ```san``` currently only target assembly on MacOS M1

## **License**
- **Kosu-register-allocator** is distribued under the GPL-3.0 or LGPL-3.0
- **San** is distribued under the GPL-3.0 



