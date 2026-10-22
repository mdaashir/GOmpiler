# GOmpiler

Two compilers, two directions, one repository.

| | Input | Output | Branch |
| --- | --- | --- | --- |
| **GOmpiler** | C++ | x86-64 assembly → native ELF | `master` |
| **go2cpp** | Go | C++20 source | `go2cpp` |

They share nothing but a home: GOmpiler is a from-scratch C++ compiler, go2cpp is a transpiler that
lowers Go into C++ and lets your system compiler finish the job.

## GOmpiler

A C++ compiler written in Go. No LLVM, no external compiler — the pipeline tokenises the source,
builds an AST, runs semantic analysis, emits x86-64 assembly and hands it to `as` and `ld`.

```bash
gompiler hello.cpp -o hello
./hello
```

Read the [GOmpiler page](#/gompiler) for the pipeline, the CLI and what it supports.

## go2cpp

A real AST transpiler, not a text rewriter: local package imports are lowered into one translation
unit with a namespace per package, then compiled as C++20. Named interfaces become virtual base
classes, goroutines become `std::thread`, and channels and `select` are implemented in the generated
runtime.

```bash
go2cpp main.go -o main
./main
```

Read the [go2cpp page](#/go2cpp) for the language features and limits.

## Getting it

Prebuilt binaries for Linux, macOS and Windows are on the [downloads page](#/downloads). Every
release ships five targets plus `checksums.txt`.

## Repository layout

```
master    GOmpiler  — cmd/, pkg/compiler, pkg/codegen, pkg/lexer, pkg/parser, pkg/semantic
go2cpp    go2cpp    — cmd/go2cpp, pkg/ast, pkg/codegen, pkg/lexer, pkg/parser, pkg/selfhost
docs      this site — content/, src/, public/
```

Both projects are MIT licensed. Source, issues and releases live at
[github.com/mdaashir/GOmpiler](https://github.com/mdaashir/GOmpiler).
