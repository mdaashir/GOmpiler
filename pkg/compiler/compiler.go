package compiler

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/mdaashir/GOmpiler-2/pkg/lexer"
	"github.com/mdaashir/GOmpiler-2/pkg/parser"
	"github.com/mdaashir/GOmpiler-2/pkg/semantic"
	"github.com/mdaashir/GOmpiler-2/pkg/codegen"
)

// CompileFile compiles a C++ source file to an executable
func CompileFile(inputFile, outputFile string, optimize bool) error {
	// Read the source file
	source, err := ioutil.ReadFile(inputFile)
	if err != nil {
		return fmt.Errorf("reading source file: %w", err)
	}

	// Create a lexer instance
	l := lexer.New(string(source), inputFile)

	// Tokenize the source
	tokens, err := l.Tokenize()
	if err != nil {
		return fmt.Errorf("lexical analysis failed: %w", err)
	}

	// Parse the tokens into an AST
	p := parser.New(tokens)
	ast, err := p.Parse()
	if err != nil {
		return fmt.Errorf("parsing failed: %w", err)
	}

	// Perform semantic analysis
	analyzer := semantic.New()
	if err := analyzer.Analyze(ast); err != nil {
		return fmt.Errorf("semantic analysis failed: %w", err)
	}

	// Generate code
	generator := codegen.New(ast, optimize)
	if err := generator.Generate(outputFile); err != nil {
		return fmt.Errorf("code generation failed: %w", err)
	}

	fmt.Printf("Successfully compiled %s to %s\n", inputFile, outputFile)
	return nil
}
