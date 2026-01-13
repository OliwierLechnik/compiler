# Compiler for Custom Language

This repository contains a compiler for a language defined in `grammar.txt`.
The project is implemented by **Oliwier Lechnik** as part of the academic evaluation for the course **Języki Formalne i Techniki Translacji**.

---

## Completion Status

* [x] Convert grammar to LL(1) EBNF
* [x] Implement AST based on refactored grammar
* [x] Implement lexer
* [x] Implement parser
* [?] Implement semantic analysis (redeclaration, variable out-of-scope, wrong usage, initialization checks, immutability, etc.)
* [x] Implement intermediate representation (IR) as a control flow graph with SSA and phi nodes
* [x] Implement IR visualization script with debug info
* [x] Generate IR graph from AST
* [ ] Implement pseudo-VM instructions for graph flattening
* [ ] Implement register allocation
* [?] Translate abstract instructions to concrete instructions (multiplication, division, constant generation)
* [ ] Flatten IR graph (`ProgramIR -> [PseudoInstruction]`)
* [ ] Resolve labels
* [ ] Translate linearly into `[VMCommand]`
* [ ] Benchmarking and testing scripts
* [ ] Optimizations (mem2reg, copy propagation, constant propagation, linear collapsing, dead code elimination, inlining)

---

## Build & Run

```bash
# Build the compiler
cabal build -O2

# Run the compiler on a source file
cabal run exes -- <path to source code>
```

---

## Requirements

* [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
* [Cabal](https://www.haskell.org/cabal/)

For installation instructions, see the [official Haskell documentation](https://www.haskell.org/get-started/).

---

## Notes

* The compiler currently supports IR visualization, SSA with phi nodes, and a subset of semantic analysis.
* Future work includes VM instruction generation, register allocation, graph flattening, label resolution, optimizations, and full benchmarking.
