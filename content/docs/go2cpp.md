# go2cpp — a Go → C++20 transpiler

go2cpp is an AST compiler, not a line-by-line rewriter. It parses Go, builds an AST, resolves local
package imports and lowers the whole program into a single C++20 translation unit. Your system C++
compiler then produces the binary.

## Pipeline

```
main.go → lexer → parser → AST → C++20 source → g++ → binary
```

Because every package is lowered into one translation unit, local imports work without a linker step
of their own: each package becomes its own namespace (`ast::`, `parser::`, `codegen::`, …).

## What is supported

- **Named interfaces** — lowered to abstract C++ base classes with virtual methods; assertions and
  type switches use `dynamic_cast`. `interface{}` maps to a type-erased, printable `any`.
- **Goroutines** — `go f()` starts a `std::thread`.
- **Channels** — buffered and unbuffered, with blocking receive.
- **`select`** — a scheduler loop over channel operations.
- **Local packages** — imports that resolve inside the module are compiled into the same unit.
- **Self-compilation** — the compiler's own AST layer compiles itself as part of CI.

## Command line

```bash
go2cpp [-S] [-o output] <input.go | directory>
```

| Flag | Meaning |
| --- | --- |
| `-S` | Print the generated C++ instead of compiling it |
| `-o` | Output path (default `a.out`) |
| `-version` | Print the stamped release version |

## Requirements

- A **C++20 compiler** (`g++` or `clang++`) on `PATH`, or set `CXX` to choose one.
- The input must live **inside a Go module** — go2cpp walks up to the nearest `go.mod` to resolve
  imports.

## Example

```go
package main

import "fmt"

func main() {
	total := 0
	for i := 1; i <= 5; i++ {
		total += i
	}
	fmt.Println("sum 1..5 =", total)
}
```

```bash
go2cpp main.go -o sum
./sum
# sum 1..5 = 15
```

Want to see the generated C++ without compiling?

```bash
go2cpp -S main.go | head -40
```

## Testing

```bash
go test ./...
bash scripts/difftest.sh      # differential tests: run each fixture under `go run` and as C++
bash scripts/selfcompile.sh   # the compiler compiles its own AST layer
```

The differential suite is the real correctness gate: every program is executed both ways and the
outputs must match. It currently covers 50 fixtures with no failures.
