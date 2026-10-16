// Command go2cpp transpiles a Go source file to C++20.
//
// This is the AST-based front-end. Pass -S to print the generated C++ source;
// otherwise the generated source is piped through a C++ compiler to produce an
// executable.
package main

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"

	"github.com/mdaashir/go2cpp/pkg/codegen"
	"github.com/mdaashir/go2cpp/pkg/lexer"
	"github.com/mdaashir/go2cpp/pkg/parser"
)

func main() {
	out := flag.String("o", "", "output file (default a.out)")
	printOnly := flag.Bool("S", false, "print the generated C++ to stdout and exit")
	flag.Parse()

	args := flag.Args()
	if len(args) < 1 {
		fmt.Fprintln(os.Stderr, "usage: go2cpp [-S] [-o output] <input.go>")
		os.Exit(1)
	}

	cpp, err := transpile(args[0])
	if err != nil {
		fmt.Fprintln(os.Stderr, "error:", err)
		os.Exit(1)
	}

	if *printOnly {
		if *out != "" {
			if err := writeFile(*out, cpp); err != nil {
				fmt.Fprintln(os.Stderr, "error:", err)
				os.Exit(1)
			}
			return
		}
		fmt.Print(cpp)
		return
	}

	output := *out
	if output == "" {
		output = "a.out"
	}
	if err := compileCpp(cpp, output); err != nil {
		fmt.Fprintln(os.Stderr, "compile error:", err)
		os.Exit(1)
	}
}

// writeFile writes generated source to disk.
func writeFile(path, content string) error {
	return os.WriteFile(path, []byte(content), 0o644)
}

// transpile runs the lexer, parser and code generator over a file.
func transpile(path string) (string, error) {
	src, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	toks, err := lexer.New(string(src)).Scan()
	if err != nil {
		return "", err
	}
	file, err := parser.New(toks).Parse()
	if err != nil {
		return "", err
	}
	return codegen.Generate(file)
}

// compileCpp pipes the generated source through a C++ compiler.
func compileCpp(src, output string) error {
	cxx := os.Getenv("CXX")
	if cxx == "" {
		cxx = "g++"
	}
	cmd := exec.Command(cxx, "-x", "c++", "-std=c++2a", "-O2", "-pipe", "-fpermissive",
		"-Wfatal-errors", "-o", output, "-")
	cmd.Stdin = bytes.NewReader([]byte(src))
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}
