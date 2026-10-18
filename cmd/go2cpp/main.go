// Command go2cpp transpiles a Go source file to C++20.
//
// This is the AST-based front-end. It resolves local package imports, lowers
// every package into a single translation unit, and (unless -S is given) pipes
// that through a C++ compiler to produce an executable.
package main

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/mdaashir/go2cpp/pkg/ast"
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

// transpile loads the entry file's package (and its local imports) and lowers
// the whole program to C++.
func transpile(path string) (string, error) {
	pkgs, err := loadProgram(path)
	if err != nil {
		return "", err
	}
	return codegen.GenerateProgram(pkgs)
}

// loadProgram parses the package containing path plus every locally-imported
// package, returning them in dependency order (entry package last).
func loadProgram(entry string) ([]*codegen.Package, error) {
	root, modPath, err := moduleRoot(entry)
	if err != nil {
		return nil, err
	}

	visited := map[string]bool{}
	var pkgs []*codegen.Package

	// parseDir parses every non-test file in a directory as one package.
	parseDir := func(dir string) (string, []ast.Decl, []string, error) {
		files, err := filepath.Glob(filepath.Join(dir, "*.go"))
		if err != nil {
			return "", nil, nil, err
		}
		var decls []ast.Decl
		var imports []string
		pkgName := ""
		for _, f := range files {
			if strings.HasSuffix(f, "_test.go") {
				continue
			}
			name, d, im, err := parseFile(f)
			if err != nil {
				return "", nil, nil, err
			}
			if pkgName == "" {
				pkgName = name
			}
			decls = append(decls, d...)
			imports = append(imports, im...)
		}
		return pkgName, decls, imports, nil
	}

	var load func(dir string) error
	load = func(dir string) error {
		abs, err := filepath.Abs(dir)
		if err != nil {
			return err
		}
		if visited[abs] {
			return nil
		}
		visited[abs] = true
		pkgName, decls, imports, err := parseDir(dir)
		if err != nil {
			return err
		}
		if pkgName == "" {
			return nil // directory without Go files
		}
		for _, imp := range imports {
			if d, ok := localDir(root, modPath, imp); ok {
				if err := load(d); err != nil {
					return err
				}
			}
		}
		pkgs = append(pkgs, &codegen.Package{Name: pkgName, Decls: decls})
		return nil
	}

	// A file argument compiles just that file (like `go run file.go`); a
	// directory argument compiles the whole package.
	if info, err := os.Stat(entry); err == nil && !info.IsDir() {
		abs, _ := filepath.Abs(filepath.Dir(entry))
		visited[abs] = true
		pkgName, decls, imports, err := parseFile(entry)
		if err != nil {
			return nil, err
		}
		for _, imp := range imports {
			if d, ok := localDir(root, modPath, imp); ok {
				if err := load(d); err != nil {
					return nil, err
				}
			}
		}
		pkgs = append(pkgs, &codegen.Package{Name: pkgName, Decls: decls})
		return pkgs, nil
	}

	if err := load(entry); err != nil {
		return nil, err
	}
	return pkgs, nil
}

// parseFile parses one Go file into its package name, declarations and imports.
func parseFile(path string) (string, []ast.Decl, []string, error) {
	src, err := os.ReadFile(path)
	if err != nil {
		return "", nil, nil, err
	}
	file, err := parseSource(string(src))
	if err != nil {
		return "", nil, nil, fmt.Errorf("%s: %w", path, err)
	}
	var imports []string
	for _, imp := range file.Imports {
		imports = append(imports, strings.Trim(imp.Path, "\""))
	}
	return file.Package, file.Decls, imports, nil
}

// parseSource lexes and parses one Go source string.
func parseSource(src string) (*ast.File, error) {
	toks, err := lexer.New(src).Scan()
	if err != nil {
		return nil, err
	}
	return parser.New(toks).Parse()
}

// moduleRoot walks up from entry to find the enclosing go.mod, returning the
// module directory and its declared path.
func moduleRoot(entry string) (string, string, error) {
	dir, err := filepath.Abs(filepath.Dir(entry))
	if err != nil {
		return "", "", err
	}
	for {
		data, err := os.ReadFile(filepath.Join(dir, "go.mod"))
		if err == nil {
			for _, line := range strings.Split(string(data), "\n") {
				line = strings.TrimSpace(line)
				if strings.HasPrefix(line, "module ") {
					return dir, strings.TrimSpace(strings.TrimPrefix(line, "module ")), nil
				}
			}
			return dir, "", nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", "", fmt.Errorf("go.mod not found above %s", entry)
		}
		dir = parent
	}
}

// localDir maps an import path to a local directory, if it belongs to the
// module.
func localDir(root, modPath, imp string) (string, bool) {
	if modPath == "" {
		return "", false
	}
	if imp == modPath {
		return root, true
	}
	if strings.HasPrefix(imp, modPath+"/") {
		rel := strings.TrimPrefix(imp, modPath+"/")
		return filepath.Join(root, filepath.FromSlash(rel)), true
	}
	return "", false
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
