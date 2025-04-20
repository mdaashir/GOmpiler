package codegen

import (
	"fmt"
	"os"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
	"github.com/mdaashir/GOmpiler-2/pkg/ast"
)

// Generator is responsible for generating code from an AST
type Generator struct {
	ast            *ast.Program
	optimize       bool
	module         *ir.Module
	currentFunc    *ir.Func
	namedValues    map[string]value.Value
	stringLiterals map[string]value.Value
}

// New creates a new code generator
func New(ast *ast.Program, optimize bool) *Generator {
	return &Generator{
		ast:            ast,
		optimize:       optimize,
		module:         ir.NewModule(),
		namedValues:    make(map[string]value.Value),
		stringLiterals: make(map[string]value.Value),
	}
}

// Generate generates code from the AST and writes it to the output file
func (g *Generator) Generate(outputFile string) error {
	// Initialize the module
	g.module.SourceFilename = "output"

	// Generate code for all declarations
	for _, decl := range g.ast.Declarations {
		if err := g.generateDeclaration(decl); err != nil {
			return err
		}
	}

	// Write the generated LLVM IR to a file
	irFile := outputFile + ".ll"
	if err := g.writeIR(irFile); err != nil {
		return err
	}

	fmt.Printf("Generated LLVM IR: %s\n", irFile)

	// Compile the LLVM IR to an executable
	if err := g.compileIR(irFile, outputFile); err != nil {
		return err
	}

	return nil
}

// writeIR writes the LLVM IR to a file
func (g *Generator) writeIR(filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return fmt.Errorf("creating IR file: %w", err)
	}
	defer func(file *os.File) {
		err := file.Close()
		if err != nil {

		}
	}(file)

	if _, err := file.WriteString(g.module.String()); err != nil {
		return fmt.Errorf("writing IR: %w", err)
	}

	return nil
}

// compileIR compiles the LLVM IR to an executable
func (g *Generator) compileIR(irFile, outputFile string) error {
	// This is a simplified implementation
	// In a real compiler, you would use LLVM's APIs to compile the IR to an executable
	// For now. We'll simulate it by calling external tools

	// First, compile the IR to an object file
	objFile := outputFile + ".o"
	fmt.Printf("Compiling IR to object file: %s -> %s\n", irFile, objFile)

	// Then link the object file to create an executable
	fmt.Printf("Linking object file to create executable: %s -> %s\n", objFile, outputFile)

	return nil
}

// generateDeclaration generates code for a declaration
func (g *Generator) generateDeclaration(decl ast.Declaration) error {
	switch d := decl.(type) {
	case *ast.FunctionDeclaration:
		return g.generateFunctionDeclaration()
	case *ast.VariableDeclaration:
		return g.generateGlobalVariableDeclaration()
	case *ast.ClassDeclaration:
		return g.generateClassDeclaration()
	case *ast.PreprocessorDirective:
		// Ignore preprocessor directives for code generation
		return nil
	case *ast.NamespaceDeclaration:
		// Generate code for declarations in the namespace
		for _, decl := range d.Declarations {
			if err := g.generateDeclaration(decl); err != nil {
				return err
			}
		}
		return nil
	default:
		// Skip other declarations for now
		return nil
	}
}

// generateFunctionDeclaration generates LLVM IR for a function declaration (stub)
func (g *Generator) generateFunctionDeclaration() error {
	// Simplified stub implementation
	// This would normally create LLVM IR for the function
	return nil
}

// generateGlobalVariableDeclaration generates LLVM IR for a global variable declaration (stub)
func (g *Generator) generateGlobalVariableDeclaration() error {
	// Simplified stub implementation
	// This would normally create LLVM IR for the global variable
	return nil
}

// generateClassDeclaration generates LLVM IR for a class declaration (stub)
func (g *Generator) generateClassDeclaration() error {
	// Simplified stub implementation
	// This would normally create LLVM IR for the class
	return nil
}

// getLLVMType converts a C++ type string to an LLVM type (stub)
func (g *Generator) getLLVMType() types.Type {
	// Simplified implementation - just return i32 for everything
	return types.I32
}

// Other necessary but stubbed-out methods to make the code compiler
func (g *Generator) generateExpression() (value.Value, error) {
	// Simplified stub implementation
	return constant.NewInt(types.I32, 0), nil
}

func (g *Generator) generateStatement() error {
	// Simplified stub implementation
	return nil
}

func (g *Generator) generateBlockStatement() error {
	// Simplified stub implementation
	return nil
}
