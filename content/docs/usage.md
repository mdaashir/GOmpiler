# Usage

Both tools take a source file and an output name. That is the whole interface.

## GOmpiler

```bash
gompiler hello.cpp -o hello
./hello
```

With several files and constant folding:

```bash
gompiler main.cpp util.cpp -O -o app
```

Check the version:

```bash
gompiler --version
# gompiler version v2.0.0
```

## go2cpp

Run it inside a Go module — it walks up to the nearest `go.mod` to resolve local imports.

```bash
go2cpp main.go -o main
./main
```

Inspect the generated C++ without compiling:

```bash
go2cpp -S main.go
```

Point it at a different C++ compiler with `CXX`:

```bash
CXX=clang++ go2cpp main.go -o main
```

## A fair comparison

The same program, written twice:

```cpp
// sum.cpp — compiled by GOmpiler
#include <iostream>

int main() {
    int total = 0;
    for (int i = 1; i <= 5; i++) {
        total = total + i;
    }
    std::cout << "sum 1..5 = " << total << std::endl;
}
```

```go
// main.go — transpiled by go2cpp
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

Both print `sum 1..5 = 15`.

## Which one do I want?

- **You have C++ and want a small, self-contained compiler** that emits native code without LLVM →
  GOmpiler.
- **You have Go and want to reach a C++ toolchain or C++ libraries** → go2cpp.

They are independent projects. Neither one calls the other.

## Troubleshooting

| Symptom | Cause |
| --- | --- |
| `gompiler: exec: "as": executable file not found` | Install binutils (`as`, `ld`) |
| `error: go.mod not found above …` | Run go2cpp inside a Go module |
| `compile error: …` from go2cpp | The generated C++ failed to compile — check your `CXX` and that it supports C++20 |
| Output is not executable | `chmod +x` the downloaded binary |
