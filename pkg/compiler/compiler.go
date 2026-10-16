package compiler

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/mdaashir/GOmpiler-2/pkg/codegen"
	"github.com/mdaashir/GOmpiler-2/pkg/lexer"
	"github.com/mdaashir/GOmpiler-2/pkg/parser"
	"github.com/mdaashir/GOmpiler-2/pkg/semantic"
)

// CompileFile compiles a single C++ source file into an executable.
//
// The pipeline is: tokenize -> parse -> semantic analysis -> x86-64 assembly ->
// assemble and link with the GNU binutils (as/ld). The result is a Linux ELF
// executable. On Windows, where as/ld are usually unavailable, the assembler
// and linker are invoked through WSL.
func CompileFile(inputFile, outputFile string, optimize bool) error {
	source, err := os.ReadFile(inputFile)
	if err != nil {
		return fmt.Errorf("reading source file: %w", err)
	}

	tokens, err := lexer.New(string(source), inputFile).Tokenize()
	if err != nil {
		return fmt.Errorf("lexical analysis failed: %w", err)
	}

	program, err := parser.New(tokens).Parse()
	if err != nil {
		return fmt.Errorf("parsing failed: %w", err)
	}

	if err := semantic.New().Analyze(program); err != nil {
		return fmt.Errorf("semantic analysis failed: %w", err)
	}

	assembly, err := codegen.New(program, optimize).GenerateAssembly()
	if err != nil {
		return fmt.Errorf("code generation failed: %w", err)
	}

	absOut, err := filepath.Abs(outputFile)
	if err != nil {
		return fmt.Errorf("resolving output path: %w", err)
	}

	// Intermediates live in a temp directory so that generated assembly never
	// lands inside a Go package directory (which would break `go build`).
	tmpDir, err := os.MkdirTemp("", "gompiler")
	if err != nil {
		return fmt.Errorf("creating temp directory: %w", err)
	}
	defer os.RemoveAll(tmpDir)

	base := filepath.Base(absOut)
	asmFile := filepath.Join(tmpDir, base+".s")
	objFile := filepath.Join(tmpDir, base+".o")
	if err := os.WriteFile(asmFile, []byte(assembly), 0o644); err != nil {
		return fmt.Errorf("writing assembly: %w", err)
	}

	tc, err := detectToolchain()
	if err != nil {
		return err
	}
	if err := tc.link(asmFile, objFile, absOut); err != nil {
		return err
	}

	fmt.Printf("Successfully compiled %s -> %s\n", inputFile, outputFile)
	return nil
}

type toolchain struct {
	useWSL bool
}

func detectToolchain() (*toolchain, error) {
	if hasExecutable("as") && hasExecutable("ld") {
		return &toolchain{useWSL: false}, nil
	}
	if hasExecutable("wsl") {
		return &toolchain{useWSL: true}, nil
	}
	return nil, fmt.Errorf("no assembler/linker found: install the GNU binutils (as, ld), or enable WSL")
}

func hasExecutable(name string) bool {
	_, err := exec.LookPath(name)
	return err == nil
}

func (t *toolchain) link(asmFile, objFile, outFile string) error {
	if !t.useWSL {
		if err := run("as", "-o", objFile, asmFile); err != nil {
			return err
		}
		return run("ld", "-o", outFile, objFile)
	}

	if err := run("wsl", "-e", "as", "-o", toWSLPath(objFile), toWSLPath(asmFile)); err != nil {
		return err
	}
	return run("wsl", "-e", "ld", "-o", toWSLPath(outFile), toWSLPath(objFile))
}

func run(name string, args ...string) error {
	cmd := exec.Command(name, args...)
	output, err := cmd.CombinedOutput()
	if err != nil {
		trimmed := strings.TrimSpace(string(output))
		if trimmed != "" {
			return fmt.Errorf("%s %s: %v\n%s", name, strings.Join(args, " "), err, trimmed)
		}
		return fmt.Errorf("%s %s: %v", name, strings.Join(args, " "), err)
	}
	return nil
}

// toWSLPath converts a Windows path such as C:\dir\file into the /mnt/c/dir/file
// form that WSL sees.
func toWSLPath(path string) string {
	if len(path) >= 2 && path[1] == ':' {
		drive := strings.ToLower(string(path[0]))
		rest := strings.ReplaceAll(path[2:], "\\", "/")
		return "/mnt/" + drive + rest
	}
	return strings.ReplaceAll(path, "\\", "/")
}
