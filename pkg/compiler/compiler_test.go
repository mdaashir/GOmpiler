package compiler

import (
	"os"
	"path/filepath"
	"testing"
)

func TestCompileFileProducesELF(t *testing.T) {
	if _, err := detectToolchain(); err != nil {
		t.Skipf("no assembler/linker available: %v", err)
	}

	dir := t.TempDir()
	src := filepath.Join(dir, "hello.cpp")
	program := `#include <iostream>
int main() {
    std::cout << "hi from gompiler" << std::endl;
    return 0;
}`
	if err := os.WriteFile(src, []byte(program), 0o644); err != nil {
		t.Fatal(err)
	}

	out := filepath.Join(dir, "hello")
	if err := CompileFile(src, out, false); err != nil {
		t.Fatalf("compile failed: %v", err)
	}

	data, err := os.ReadFile(out)
	if err != nil {
		t.Fatalf("reading output: %v", err)
	}
	if len(data) < 4 || data[0] != 0x7f || data[1] != 'E' || data[2] != 'L' || data[3] != 'F' {
		t.Fatalf("output is not an ELF file (first bytes: % x)", data[:minInt(4, len(data))])
	}
}

func TestCompileFileRejectsUnsupportedSource(t *testing.T) {
	dir := t.TempDir()
	src := filepath.Join(dir, "person.cpp")
	if err := os.WriteFile(src, []byte(`class Person { public: int age; };`), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := CompileFile(src, filepath.Join(dir, "person"), false); err == nil {
		t.Fatal("expected an error for unsupported class syntax")
	}
}

func TestCompileFileReportsMissingFile(t *testing.T) {
	if err := CompileFile("does-not-exist.cpp", "out", false); err == nil {
		t.Fatal("expected an error for a missing input file")
	}
}

func minInt(a, b int) int {
	if a < b {
		return a
	}
	return b
}
