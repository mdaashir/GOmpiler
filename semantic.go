package main

type SymbolType string

const (
	VARIABLE SymbolType = "VARIABLE"
	FUNCTION SymbolType = "FUNCTION"
	CLASS    SymbolType = "CLASS"
	STRUCT   SymbolType = "STRUCT"
)

type Symbol struct {
	Name       string
	Type       SymbolType
	DataType   string
	Scope      *Scope
	Parameters []Symbol // For functions
	Fields     []Symbol // For classes/structs
}

type Scope struct {
	Parent    *Scope
	Symbols   map[string]Symbol
	Children  []*Scope
	ScopeName string
}

type SemanticAnalyzer struct {
	CurrentScope *Scope
	GlobalScope  *Scope
	Errors       []string
}

func NewSemanticAnalyzer() *SemanticAnalyzer {
	globalScope := &Scope{
		Parent:    nil,
		Symbols:   make(map[string]Symbol),
		Children:  make([]*Scope, 0),
		ScopeName: "global",
	}

	return &SemanticAnalyzer{
		CurrentScope: globalScope,
		GlobalScope:  globalScope,
		Errors:       make([]string, 0),
	}
}

func (sa *SemanticAnalyzer) EnterScope(name string) {
	newScope := &Scope{
		Parent:    sa.CurrentScope,
		Symbols:   make(map[string]Symbol),
		Children:  make([]*Scope, 0),
		ScopeName: name,
	}
	sa.CurrentScope.Children = append(sa.CurrentScope.Children, newScope)
	sa.CurrentScope = newScope
}

func (sa *SemanticAnalyzer) ExitScope() {
	if sa.CurrentScope.Parent != nil {
		sa.CurrentScope = sa.CurrentScope.Parent
	}
}
package main

import (
	"fmt"
)

type Type string

const (
	TypeInt     Type = "int"
	TypeChar    Type = "char"
	TypeFloat   Type = "float"
	TypeDouble  Type = "double"
	TypeVoid    Type = "void"
	TypeUnknown Type = "unknown"
)

// Symbol represents a variable, function, or type in the symbol table
type Symbol struct {
	Name       string
	Type       Type
	IsFunction bool
	Parameters []Symbol
	Scope      *Scope
}

// Scope represents a lexical scope with symbols and parent scope
type Scope struct {
	Symbols map[string]Symbol
	Parent  *Scope
}

// SymbolTable manages all symbols and scopes
type SymbolTable struct {
	GlobalScope *Scope
	CurrentScope *Scope
}

// SemanticError represents a semantic error
type SemanticError struct {
	Message string
	Node    *ASTNode
}

// SemanticAnalyzer performs semantic analysis on the AST
type SemanticAnalyzer struct {
	symbolTable *SymbolTable
	errors      []SemanticError
}

// NewSemanticAnalyzer creates a new semantic analyzer
func NewSemanticAnalyzer() *SemanticAnalyzer {
	globalScope := &Scope{
		Symbols: make(map[string]Symbol),
		Parent:  nil,
	}
	
	symbolTable := &SymbolTable{
		GlobalScope:  globalScope,
		CurrentScope: globalScope,
	}
	
	return &SemanticAnalyzer{
		symbolTable: symbolTable,
		errors:      []SemanticError{},
	}
}

// Analyze analyzes the AST for semantic errors
func (sa *SemanticAnalyzer) Analyze(node *ASTNode) (*SymbolTable, []SemanticError) {
	sa.analyzeNode(node)
	return sa.symbolTable, sa.errors
}

// analyzeNode recursively analyzes a node
func (sa *SemanticAnalyzer) analyzeNode(node *ASTNode) Type {
	if node == nil {
		return TypeUnknown
	}

	switch node.Type {
	case PROGRAM:
		return sa.analyzeProgram(node)
	case FUNCTION_DEF:
		return sa.analyzeFunction(node)
	case STATEMENT:
		return sa.analyzeStatement(node)
	case EXPRESSION:
		return sa.analyzeExpression(node)
	default:
		return TypeUnknown
	}
}

// analyzeProgram analyzes the program node (root of the AST)
func (sa *SemanticAnalyzer) analyzeProgram(node *ASTNode) Type {
	for _, child := range node.Children {
		sa.analyzeNode(child)
	}
	return TypeVoid
}

// analyzeFunction analyzes a function definition
func (sa *SemanticAnalyzer) analyzeFunction(node *ASTNode) Type {
	funcName := node.Value
	
	// Create a new scope for the function
	functionScope := &Scope{
		Symbols: make(map[string]Symbol),
		Parent:  sa.symbolTable.CurrentScope,
	}
	
	// Check if function is already defined
	if _, exists := sa.symbolTable.GlobalScope.Symbols[funcName]; exists {
		sa.errors = append(sa.errors, SemanticError{
			Message: fmt.Sprintf("Function '%s' is already defined", funcName),
			Node:    node,
		})
	}
	
	// Add function to symbol table
	returnType := TypeInt // Default to int, should be extracted from AST
	sa.symbolTable.GlobalScope.Symbols[funcName] = Symbol{
		Name:       funcName,
		Type:       returnType,
		IsFunction: true,
		Parameters: []Symbol{},
		Scope:      functionScope,
	}
	
	// Set current scope to function scope
	prevScope := sa.symbolTable.CurrentScope
	sa.symbolTable.CurrentScope = functionScope
	
	// Analyze function parameters
	if len(node.Children) > 0 && node.Children[0].Value == "Parameters" {
		for _, param := range node.Children[0].Children {
			paramType := TypeInt // Default, should parse from AST
			paramName := param.Value
			
			sa.symbolTable.CurrentScope.Symbols[paramName] = Symbol{
				Name:       paramName,
				Type:       paramType,
				IsFunction: false,
			}
		}
	}
	
	// Analyze function body
	for i := 1; i < len(node.Children); i++ {
		sa.analyzeNode(node.Children[i])
	}
	
	// Restore previous scope
	sa.symbolTable.CurrentScope = prevScope
	
	return returnType
}

// analyzeStatement analyzes a statement
func (sa *SemanticAnalyzer) analyzeStatement(node *ASTNode) Type {
	// Handle declarations
	if sa.isDeclaration(node) {
		return sa.analyzeDeclaration(node)
	}
	
	// Handle return statements
	if node.Value == "return" {
		if len(node.Children) > 0 {
			return sa.analyzeNode(node.Children[0])
		}
		return TypeVoid
	}
	
	// Handle blocks
	if node.Value == "Block" {
		// Create a new scope for the block
		blockScope := &Scope{
			Symbols: make(map[string]Symbol),
			Parent:  sa.symbolTable.CurrentScope,
		}
		
		prevScope := sa.symbolTable.CurrentScope
		sa.symbolTable.CurrentScope = blockScope
		
		for _, child := range node.Children {
			sa.analyzeNode(child)
		}
		
		// Restore previous scope
		sa.symbolTable.CurrentScope = prevScope
		return TypeVoid
	}
	
	// Handle other statements
	for _, child := range node.Children {
		sa.analyzeNode(child)
	}
	
	return TypeVoid
}

// isDeclaration checks if a node is a variable declaration
func (sa *SemanticAnalyzer) isDeclaration(node *ASTNode) bool {
	if node.Type != STATEMENT {
		return false
	}
	
	// Check if value starts with a known type
	return strings.HasPrefix(node.Value, "int ") ||
		strings.HasPrefix(node.Value, "char ") ||
		strings.HasPrefix(node.Value, "float ") ||
		strings.HasPrefix(node.Value, "double ")
}

// analyzeDeclaration analyzes a variable declaration
func (sa *SemanticAnalyzer) analyzeDeclaration(node *ASTNode) Type {
	parts := strings.SplitN(node.Value, " ", 2)
	if len(parts) != 2 {
		return TypeUnknown
	}
	
	typeName := parts[0]
	varName := parts[1]
	
	var varType Type
	switch typeName {
	case "int":
		varType = TypeInt
	case "char":
		varType = TypeChar
	case "float":
		varType = TypeFloat
	case "double":
		varType = TypeDouble
	default:
		varType = TypeUnknown
	}
	
	// Check if variable is already defined in current scope
	if _, exists := sa.symbolTable.CurrentScope.Symbols[varName]; exists {
		sa.errors = append(sa.errors, SemanticError{
			Message: fmt.Sprintf("Variable '%s' is already defined in this scope", varName),
			Node:    node,
		})
	}
	
	// Add variable to symbol table
	sa.symbolTable.CurrentScope.Symbols[varName] = Symbol{
		Name:       varName,
		Type:       varType,
		IsFunction: false,
	}
	
	// Analyze initializer if present
	if len(node.Children) > 0 {
		initType := sa.analyzeNode(node.Children[0])
		if initType != varType {
			sa.errors = append(sa.errors, SemanticError{
				Message: fmt.Sprintf("Cannot assign %s to %s", initType, varType),
				Node:    node,
			})
		}
	}
	
	return varType
}

// analyzeExpression analyzes an expression
func (sa *SemanticAnalyzer) analyzeExpression(node *ASTNode) Type {
	// Handle literals
	if sa.isNumericLiteral(node.Value) {
		if strings.Contains(node.Value, ".") {
			return TypeFloat
		}
		return TypeInt
	}
	
	// Handle identifiers
	if sa.isIdentifier(node.Value) {
		symbol := sa.lookupSymbol(node.Value)
		if symbol == nil {
			sa.errors = append(sa.errors, SemanticError{
				Message: fmt.Sprintf("Undefined variable '%s'", node.Value),
				Node:    node,
			})
			return TypeUnknown
		}
		return symbol.Type
	}
	
	// Handle binary operations
	if len(node.Children) == 2 {
		leftType := sa.analyzeNode(node.Children[0])
		rightType := sa.analyzeNode(node.Children[1])
		
		// Check type compatibility for operators
		switch node.Value {
		case "+", "-", "*", "/":
			if leftType == TypeInt && rightType == TypeInt {
				return TypeInt
			}
			return TypeFloat
		case "==", "!=", "<", ">", "<=", ">=":
			return TypeInt // Boolean result (represented as int)
		default:
			return TypeUnknown
		}
	}
	
	return TypeUnknown
}

// isNumericLiteral checks if a string is a numeric literal
func (sa *SemanticAnalyzer) isNumericLiteral(value string) bool {
	_, err := strconv.ParseFloat(value, 64)
	return err == nil
}

// isIdentifier checks if a string is an identifier
func (sa *SemanticAnalyzer) isIdentifier(value string) bool {
	if len(value) == 0 {
		return false
	}
	
	if !unicode.IsLetter(rune(value[0])) && value[0] != '_' {
		return false
	}
	
	for _, ch := range value[1:] {
		if !unicode.IsLetter(ch) && !unicode.IsDigit(ch) && ch != '_' {
			return false
		}
	}
	
	return true
}

// lookupSymbol looks up a symbol in the current scope and parent scopes
func (sa *SemanticAnalyzer) lookupSymbol(name string) *Symbol {
	scope := sa.symbolTable.CurrentScope
	
	for scope != nil {
		if symbol, exists := scope.Symbols[name]; exists {
			return &symbol
		}
		scope = scope.Parent
	}
	
	return nil
}
func (sa *SemanticAnalyzer) DeclareSymbol(symbol Symbol) bool {
	if _, exists := sa.CurrentScope.Symbols[symbol.Name]; exists {
		sa.Errors = append(sa.Errors, "Symbol '"+symbol.Name+"' already declared in current scope")
		return false
	}
	sa.CurrentScope.Symbols[symbol.Name] = symbol
	return true
}

func (sa *SemanticAnalyzer) LookupSymbol(name string) (Symbol, bool) {
	currentScope := sa.CurrentScope
	for currentScope != nil {
		if symbol, exists := currentScope.Symbols[name]; exists {
			return symbol, true
		}
		currentScope = currentScope.Parent
	}
	return Symbol{}, false
}

func (sa *SemanticAnalyzer) Analyze(node *ASTNode) {
	switch node.Type {
	case "VARIABLE_DECLARATION":
		sa.analyzeVariableDeclaration(node)
	case "FUNCTION_DECLARATION":
		sa.analyzeFunctionDeclaration(node)
	case "CLASS_DECLARATION":
		sa.analyzeClassDeclaration(node)
	}

	for _, child := range node.Children {
		sa.Analyze(child)
	}
}

func (sa *SemanticAnalyzer) analyzeVariableDeclaration(node *ASTNode) {
	varName := node.Children[0].Value
	varType := node.Children[1].Value

	symbol := Symbol{
		Name:     varName,
		Type:     VARIABLE,
		DataType: varType,
		Scope:    sa.CurrentScope,
	}

	sa.DeclareSymbol(symbol)
}

func (sa *SemanticAnalyzer) analyzeFunctionDeclaration(node *ASTNode) {
	funcName := node.Children[0].Value
	returnType := node.Children[1].Value

	params := make([]Symbol, 0)
	for _, paramNode := range node.Children[2].Children {
		params = append(params, Symbol{
			Name:     paramNode.Children[0].Value,
			Type:     VARIABLE,
			DataType: paramNode.Children[1].Value,
			Scope:    sa.CurrentScope,
		})
	}

	symbol := Symbol{
		Name:       funcName,
		Type:       FUNCTION,
		DataType:   returnType,
		Scope:      sa.CurrentScope,
		Parameters: params,
	}

	sa.DeclareSymbol(symbol)
	sa.EnterScope(funcName)
	for _, param := range params {
		sa.DeclareSymbol(param)
	}
}

func (sa *SemanticAnalyzer) analyzeClassDeclaration(node *ASTNode) {
	className := node.Children[0].Value

	fields := make([]Symbol, 0)
	for _, fieldNode := range node.Children[1].Children {
		fields = append(fields, Symbol{
			Name:     fieldNode.Children[0].Value,
			Type:     VARIABLE,
			DataType: fieldNode.Children[1].Value,
			Scope:    sa.CurrentScope,
		})
	}

	symbol := Symbol{
		Name:   className,
		Type:   CLASS,
		Scope:  sa.CurrentScope,
		Fields: fields,
	}

	sa.DeclareSymbol(symbol)
	sa.EnterScope(className)
	for _, field := range fields {
		sa.DeclareSymbol(field)
	}
}
