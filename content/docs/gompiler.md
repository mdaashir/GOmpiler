# GOmpiler — a C++ compiler written in Go

GOmpiler compiles C++ to a native executable without LLVM. It does its own parsing, analysis and
code generation, then borrows the assembler and linker that are already on your machine.

## Pipeline

```
hello.cpp → lexer → parser → semantic analysis → x86-64 assembly → as → ld → hello
```

1. **Lexer** — tokenises the source, tracking positions for diagnostics.
2. **Parser** — builds an AST with precedence climbing for expressions.
3. **Semantic analysis** — resolves declarations, checks types, reports errors before codegen.
4. **Code generation** — walks the AST and emits x86-64 assembly.
5. **Assemble and link** — `as` and `ld` turn the assembly into an ELF binary.

## Command line

```bash
gompiler <source files...> [-o output] [-O] [--version]
```

| Flag | Meaning |
| --- | --- |
| `-o`, `--output` | Output file name (default `a.out`) |
| `-O`, `--optimize` | Enable constant folding |
| `--version` | Print the stamped release version |

Several source files can be passed in one invocation.

## Requirements

- **To build from source:** Go 1.24 or newer.
- **To run it:** `as` and `ld` (binutils) on `PATH`.

There is no runtime library and no LLVM dependency — the binary is a single static Go executable.

## Example

```cpp
#include <iostream>

int main() {
    int total = 0;
    for (int i = 1; i <= 5; i++) {
        total = total + i;
    }
    std::cout << "sum 1..5 = " << total << std::endl;
    return 0;
}
```

```bash
gompiler sum.cpp -o sum
./sum
# sum 1..5 = 15
```

## Testing

```bash
go test ./...
```

CI additionally compiles an example end to end, checks that the result is a valid ELF, runs it and
compares its output.

## Layout

```
cmd/            cobra CLI and version stamping
pkg/lexer/      tokeniser
pkg/parser/     recursive-descent parser
pkg/semantic/   declaration and type analysis
pkg/codegen/    x86-64 assembly emission
pkg/compiler/   the driver that ties the stages together
```
