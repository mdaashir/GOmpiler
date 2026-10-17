package parser

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/mdaashir/go2cpp/pkg/lexer"
)

func parseSource(t *testing.T, src string) error {
	t.Helper()
	toks, err := lexer.New(src).Scan()
	if err != nil {
		return err
	}
	_, err = New(toks).Parse()
	return err
}

func TestParseSample(t *testing.T) {
	src := `package main

import (
	"fmt"
)

func add(a, b int) int {
	return a + b
}

func main() {
	m := map[string]int{"a": 1, "b": 2}
	for k, v := range m {
		fmt.Println(k, v)
	}
	switch x := add(1, 2); x {
	case 3:
		fmt.Println("three")
	default:
		fmt.Println("other")
	}
}
`
	if err := parseSource(t, src); err != nil {
		t.Fatalf("parse error: %v", err)
	}
}

// TestParseAllTestcases parses every file in testcases/ and reports the first
// error per file. This is the parser's main regression signal.
func TestParseAllTestcases(t *testing.T) {
	files, err := filepath.Glob(filepath.Join("..", "..", "testcases", "*.go"))
	if err != nil || len(files) == 0 {
		t.Fatalf("no testcases found: %v", err)
	}
	for _, f := range files {
		f := f
		t.Run(filepath.Base(f), func(t *testing.T) {
			src, err := os.ReadFile(f)
			if err != nil {
				t.Fatal(err)
			}
			if err := parseSource(t, string(src)); err != nil {
				t.Errorf("parse error: %v", err)
			}
		})
	}
}

// TestParseErrorReported checks that malformed input yields an error rather
// than an unrecovered panic.
func TestParseErrorReported(t *testing.T) {
	for _, src := range []string{
		"package main\nfunc main() { x := }\n",
		"package main\nfunc main() { if\n",
		"package\n",
		"package main\ntype T struct { X \n",
	} {
		toks, err := lexer.New(src).Scan()
		if err != nil {
			continue // lexer rejected it, which is also acceptable
		}
		if _, err := New(toks).Parse(); err == nil {
			t.Errorf("expected a parse error for %q", src)
		}
	}
}
