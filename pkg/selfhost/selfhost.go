// Package selfhost is the core entry point used to verify self-compilation: it
// runs the lexer, parser and code generator over Go source text.
//
// It deliberately depends only on the core packages (no os/flag/bytes), so that
// it stays inside the subset go2cpp can transpile.
package selfhost

import (
	"github.com/mdaashir/go2cpp/pkg/codegen"
	"github.com/mdaashir/go2cpp/pkg/lexer"
	"github.com/mdaashir/go2cpp/pkg/parser"
)

// Transpile lexes, parses and lowers Go source text to C++.
func Transpile(src string) (string, error) {
	toks, err := lexer.New(src).Scan()
	if err != nil {
		return "", err
	}
	file, err := parser.New(toks).Parse()
	if err != nil {
		return "", err
	}
	return codegen.Generate(file)
}
