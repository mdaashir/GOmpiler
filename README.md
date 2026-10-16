# GOmpiler — a C++ compiler written in Go

GOmpiler compiles a single C++ source file to a native x86-64 executable. It is
a complete, working pipeline rather than a sketch: source text is tokenized,
parsed into an AST, checked, lowered to x86-64 assembly, and assembled and
linked into a Linux ELF binary.

```
source.cpp
   │  lexer      -> tokens
   │  parser     -> AST
   │  semantic   -> symbol/scope + arity checks
   │  codegen    -> x86-64 assembly (GNU as, AT&T syntax)
   ▼  as + ld    -> static ELF executable
```

## Requirements

- **Go 1.24 or later** to build the compiler.
- **GNU binutils (`as`, `ld`)** to assemble and link the output. On Linux these
  are normally already installed. On Windows — where they usually are not — the
  compiler automatically runs them through **WSL** instead.

No C++ compiler, LLVM installation, or libc is required: generated programs use
Linux syscalls directly.

## Building

```bash
go build -o gompiler .
```

## Usage

```bash
./gompiler input.cpp -o output
```

Flags:

| Flag | Meaning |
| --- | --- |
| `-o, --output` | Output file name (default `a.out`) |
| `-O, --optimize` | Enable constant folding of literal expressions |

Generated executables are Linux ELF binaries. On Windows, run them through WSL
(for example `wsl -e ./output`).

### Example

```bash
./gompiler examples/hello.cpp -o hello
wsl -e ./hello      # on Windows;  ./hello  on Linux
```

```
Hello, World!
Result is greater than 25: 30
Counting down: 5 4 3 2 1
```

## Supported language

The compiler deliberately supports a focused, correct subset of C++ rather than
a broad, half-working surface.

**Supported**

- Preprocessor lines and `using namespace ...;` are accepted and ignored.
- Global and local variables of the built-in arithmetic types (`int`, `char`,
  `long`, `unsigned`, `bool`, ...), with or without initializers.
- Functions: prototypes and definitions, parameters, recursion, return values.
- Expressions: `+ - * / %`, `< > <= >= == !=`, `&&`, `||` (short-circuiting),
  unary `- ! ~`, `++`/`--`, assignment (`=`, `+=`, `-=`, `*=`, `/=`, `%=`),
  function calls, qualified names (`std::cout`), and parentheses with correct
  precedence and associativity.
- Statements: blocks, `if`/`else`, `for`, `while`, `do`/`while`, `switch`
  (lowered to an `if`/`else` chain), `break`, `continue`, `return`.
- Output with `std::cout << ... << std::endl` for string literals, integers,
  and character literals.

**Not supported** (reported as a clear error, never silently miscompiled)

- Classes, structs, enums, namespaces, templates, and `typedef`.
- Arrays and pointer/member access (`a[i]`, `obj.field`).
- Floating point, `new`/`delete`, exceptions, and the rest of the standard
  library beyond `cout`/`endl`.
- `switch` fallthrough.

Note that `switch` is handled by lowering it to an equivalent `if`/`else` chain,
so `break` terminates the enclosing case as expected.

## Project structure

| Path | Purpose |
| --- | --- |
| `pkg/lexer` | Turns source text into a token stream. |
| `pkg/ast` | AST node definitions used by every later stage. |
| `pkg/parser` | Precedence-climbing recursive-descent parser producing an AST. |
| `pkg/semantic` | Scope and symbol resolution, arity and type-usage checks. |
| `pkg/codegen` | Lowers the AST to x86-64 assembly, including a small runtime for `cout`. |
| `pkg/compiler` | Drives the pipeline and invokes `as`/`ld` (directly, or via WSL). |
| `cmd` | Cobra command-line interface. |
| `examples` | Sample C++ programs. |

## Tests

```bash
go test ./...
```

Unit tests cover the lexer, parser, semantic analyzer, and code generator. The
`pkg/compiler` test compiles a program end to end and verifies that the output is
a valid ELF file (it is skipped automatically if no assembler/linker is
available).

## License

This project is licensed under the MIT License — see the LICENSE file for details.
