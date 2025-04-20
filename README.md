# GOmpiler - A C++ Compiler Written in Go

GOmpiler is a C++ compiler implemented in Go, designed to demonstrate modern compiler construction techniques. It features a complete compilation pipeline including lexical analysis, parsing, semantic analysis, IR generation, optimization, and linking.

## Features

- Lexical analysis with comprehensive C++ token support
- Recursive descent parser generating an Abstract Syntax Tree (AST)
- Semantic analysis for type checking and scope resolution
- Intermediate Representation (IR) generation
- Basic optimization passes
- ELF binary generation with symbol resolution and relocation

## Project Structure

- `lexer.go`: Tokenizes source code into a stream of tokens
- `parser.go`: Constructs Abstract Syntax Tree from tokens
- `semantic.go`: Performs semantic analysis and type checking
- `ir_gen.go`: Generates intermediate representation
- `optimizer.go`: Implements optimization passes
- `linker.go`: Handles symbol resolution and generates executables
- `main.go`: Program entry point and file handling

## Building and Running

### Prerequisites

- Go 1.16 or later
- Git

### Build Instructions

1. Clone the repository:

```bash
git clone https://github.com/mdaashir/GOmpiler.git
cd GOmpiler
```

2. Build the compiler:

```bash
go build -o gompiler
```

### Usage

Compile a C++ source file:

```bash
./gompiler input.cpp -o output
```

Example with sample.c:

```bash
./gompiler sample.c -o program
```

## Implementation Details

### Lexer

The lexer performs token recognition using deterministic finite automata (DFA) principles. It handles:

- Keywords
- Identifiers
- Literals (numbers, strings)
- Operators and punctuation

### Parser

Implements a recursive descent parser that:

- Constructs an AST
- Handles operator precedence
- Supports C++ grammar constructs

### Semantic Analyzer

- Type checking
- Scope management
- Symbol table construction

### IR Generator

- Generates intermediate code
- Performs basic optimizations

### Linker

- Symbol resolution
- Section layout
- Relocation processing
- ELF binary generation

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
