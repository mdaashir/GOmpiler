package semantic

import (
	"fmt"

	"github.com/mdaashir/GOmpiler-2/pkg/ast"
)

// Analyzer performs semantic analysis on the AST
type Analyzer struct {
	errors       []string
	scopes       []*Scope
	currentScope *Scope
}

// Scope represents a symbol scope
type Scope struct {
	Parent  *Scope
	Symbols map[string]Symbol
}

// Symbol represents a symbol in the symbol table
type Symbol struct {
	Name  string
	Type  string
	Kind  SymbolKind
	Scope *Scope
}

// SymbolKind represents the kind of symbol
type SymbolKind int

const (
	VariableSymbol SymbolKind = iota
	FunctionSymbol
	TypeSymbol
	NamespaceSymbol
)

// New creates a new semantic analyzer
func New() *Analyzer {
	globalScope := &Scope{
		Symbols: make(map[string]Symbol),
	}

	return &Analyzer{
		errors:       []string{},
		scopes:       []*Scope{globalScope},
		currentScope: globalScope,
	}
}

// Analyze performs semantic analysis on the AST
func (a *Analyzer) Analyze(program *ast.Program) error {
	// Initialize built-in types
	a.initBuiltinTypes()

	// First pass: collect declarations
	a.collectDeclarations(program)

	// Second pass: validate the AST
	a.validateProgram(program)

	if len(a.errors) > 0 {
		return fmt.Errorf("semantic errors: %v", a.errors)
	}

	return nil
}

// initBuiltinTypes initializes the built-in C++ types
func (a *Analyzer) initBuiltinTypes() {
	builtinTypes := []string{
		"void", "char", "short", "int", "long", "unsigned",
		"float", "double", "bool", "size_t", "wchar_t",
	}

	for _, typeName := range builtinTypes {
		a.currentScope.Symbols[typeName] = Symbol{
			Name:  typeName,
			Type:  typeName,
			Kind:  TypeSymbol,
			Scope: a.currentScope,
		}
	}
}

// collectDeclarations collects all declarations in the first pass
func (a *Analyzer) collectDeclarations(program *ast.Program) {
	for _, decl := range program.Declarations {
		a.collectDeclaration(decl)
	}
}

// collectDeclaration collects a single declaration
func (a *Analyzer) collectDeclaration(decl ast.Declaration) {
	switch d := decl.(type) {
	case *ast.FunctionDeclaration:
		a.collectFunctionDeclaration(d)
	case *ast.VariableDeclaration:
		a.collectVariableDeclaration(d)
	case *ast.ClassDeclaration:
		a.collectClassDeclaration(d)
	case *ast.EnumDeclaration:
		a.collectEnumDeclaration(d)
	case *ast.NamespaceDeclaration:
		a.collectNamespaceDeclaration(d)
	case *ast.TypedefDeclaration:
		a.collectTypedefDeclaration(d)
	case *ast.TemplateDeclaration:
		// Templates are complex, for now collect the inner declaration
		if d.Declaration != nil {
			a.collectDeclaration(d.Declaration)
		}
	}
}

// collectFunctionDeclaration adds a function to the symbol table
func (a *Analyzer) collectFunctionDeclaration(fn *ast.FunctionDeclaration) {
	if _, exists := a.currentScope.Symbols[fn.Name]; exists {
		a.addError(fmt.Sprintf("function '%s' already declared in this scope", fn.Name))
		return
	}

	a.currentScope.Symbols[fn.Name] = Symbol{
		Name:  fn.Name,
		Type:  fn.Type,
		Kind:  FunctionSymbol,
		Scope: a.currentScope,
	}

	if fn.Body != nil {
		// Create a new scope for the function body
		functionScope := &Scope{
			Parent:  a.currentScope,
			Symbols: make(map[string]Symbol),
		}

		// Add parameters to the function scope
		for _, param := range fn.Parameters {
			functionScope.Symbols[param.Name] = Symbol{
				Name:  param.Name,
				Type:  param.Type,
				Kind:  VariableSymbol,
				Scope: functionScope,
			}
		}

		// Push the function scope
		a.pushScope(functionScope)

		// Analyze the function body
		a.analyzeBlockStatement(fn.Body)

		// Pop the function scope
		a.popScope()
	}
}

// collectVariableDeclaration adds a variable to the symbol table
func (a *Analyzer) collectVariableDeclaration(vd *ast.VariableDeclaration) {
	if _, exists := a.currentScope.Symbols[vd.Name]; exists {
		a.addError(fmt.Sprintf("variable '%s' already declared in this scope", vd.Name))
		return
	}

	a.currentScope.Symbols[vd.Name] = Symbol{
		Name:  vd.Name,
		Type:  vd.Type,
		Kind:  VariableSymbol,
		Scope: a.currentScope,
	}

	if vd.Initializer != nil {
		// Check the initializer expression
		a.analyzeExpression()
	}
}

// collectClassDeclaration adds a class to the symbol table
func (a *Analyzer) collectClassDeclaration(cd *ast.ClassDeclaration) {
	if _, exists := a.currentScope.Symbols[cd.Name]; exists {
		a.addError(fmt.Sprintf("class '%s' already declared in this scope", cd.Name))
		return
	}

	a.currentScope.Symbols[cd.Name] = Symbol{
		Name:  cd.Name,
		Type:  cd.Name,
		Kind:  TypeSymbol,
		Scope: a.currentScope,
	}

	// Create a new scope for the class members
	classScope := &Scope{
		Parent:  a.currentScope,
		Symbols: make(map[string]Symbol),
	}

	// Push the class scope
	a.pushScope(classScope)

	// Collect class members
	for _, member := range cd.Members {
		a.collectDeclaration(member)
	}

	// Pop the class scope
	a.popScope()
}

// collectEnumDeclaration adds an enum to the symbol table
func (a *Analyzer) collectEnumDeclaration(ed *ast.EnumDeclaration) {
	if _, exists := a.currentScope.Symbols[ed.Name]; exists {
		a.addError(fmt.Sprintf("enum '%s' already declared in this scope", ed.Name))
		return
	}

	a.currentScope.Symbols[ed.Name] = Symbol{
		Name:  ed.Name,
		Type:  "enum",
		Kind:  TypeSymbol,
		Scope: a.currentScope,
	}

	// Add enum values as constants
	for _, value := range ed.Values {
		a.currentScope.Symbols[value] = Symbol{
			Name:  value,
			Type:  ed.Name,
			Kind:  VariableSymbol,
			Scope: a.currentScope,
		}
	}
}

// collectNamespaceDeclaration adds a namespace to the symbol table
func (a *Analyzer) collectNamespaceDeclaration(nd *ast.NamespaceDeclaration) {
	if _, exists := a.currentScope.Symbols[nd.Name]; exists {
		// Namespaces can be reopened, so this is not an error
		// We'll just add to the existing namespace
	} else {
		a.currentScope.Symbols[nd.Name] = Symbol{
			Name:  nd.Name,
			Type:  "namespace",
			Kind:  NamespaceSymbol,
			Scope: a.currentScope,
		}
	}

	// Create a new scope for the namespace
	namespaceScope := &Scope{
		Parent:  a.currentScope,
		Symbols: make(map[string]Symbol),
	}

	// Push the namespace scope
	a.pushScope(namespaceScope)

	// Collect namespace declarations
	for _, decl := range nd.Declarations {
		a.collectDeclaration(decl)
	}

	// Pop the namespace scope
	a.popScope()
}

// collectTypedefDeclaration adds a typedef to the symbol table
func (a *Analyzer) collectTypedefDeclaration(td *ast.TypedefDeclaration) {
	if _, exists := a.currentScope.Symbols[td.NewType]; exists {
		a.addError(fmt.Sprintf("type '%s' already declared in this scope", td.NewType))
		return
	}

	a.currentScope.Symbols[td.NewType] = Symbol{
		Name:  td.NewType,
		Type:  td.OriginalType,
		Kind:  TypeSymbol,
		Scope: a.currentScope,
	}
}

// validateProgram validates the entire program
func (a *Analyzer) validateProgram(program *ast.Program) {
	for _, decl := range program.Declarations {
		a.validateDeclaration(decl)
	}
}

// validateDeclaration validates a declaration
func (a *Analyzer) validateDeclaration(decl ast.Declaration) {
	switch d := decl.(type) {
	case *ast.FunctionDeclaration:
		a.validateFunctionDeclaration(d)
	case *ast.VariableDeclaration:
		a.validateVariableDeclaration(d)
	case *ast.ClassDeclaration:
		a.validateClassDeclaration(d)
	}
}

// validateFunctionDeclaration validates a function declaration
func (a *Analyzer) validateFunctionDeclaration(fn *ast.FunctionDeclaration) {
	// Check if the return type exists
	if !a.isValidType(fn.Type) {
		a.addError(fmt.Sprintf("unknown return type '%s' for function '%s'", fn.Type, fn.Name))
	}

	// Validate parameters
	for _, param := range fn.Parameters {
		if !a.isValidType(param.Type) {
			a.addError(fmt.Sprintf("unknown parameter type '%s' in function '%s'", param.Type, fn.Name))
		}
	}

	if fn.Body != nil {
		// Create a new scope for the function body
		functionScope := &Scope{
			Parent:  a.currentScope,
			Symbols: make(map[string]Symbol),
		}

		// Add parameters to the function scope
		for _, param := range fn.Parameters {
			functionScope.Symbols[param.Name] = Symbol{
				Name:  param.Name,
				Type:  param.Type,
				Kind:  VariableSymbol,
				Scope: functionScope,
			}
		}

		// Push function scope
		a.pushScope(functionScope)

		// Validate function body
		a.validateBlockStatement(fn.Body)

		// Pop function scope
		a.popScope()
	}
}

// validateVariableDeclaration validates a variable declaration
func (a *Analyzer) validateVariableDeclaration(vd *ast.VariableDeclaration) {
	// Check if the type exists
	if !a.isValidType(vd.Type) {
		a.addError(fmt.Sprintf("unknown type '%s' for variable '%s'", vd.Type, vd.Name))
	}

	if vd.Initializer != nil {
		// Validate initializer
		a.validateExpression(vd.Initializer)

		// Check type compatibility (simplified)
		// TODO: Implement proper type checking
	}
}

// validateClassDeclaration validates a class declaration
func (a *Analyzer) validateClassDeclaration(cd *ast.ClassDeclaration) {
	// Check base classes
	for _, baseClass := range cd.BaseClasses {
		found := false

		// Look for base class in current and parent scopes
		scope := a.currentScope
		for scope != nil {
			if symbol, exists := scope.Symbols[baseClass]; exists && symbol.Kind == TypeSymbol {
				found = true
				break
			}
			scope = scope.Parent
		}

		if !found {
			a.addError(fmt.Sprintf("unknown base class '%s' for class '%s'", baseClass, cd.Name))
		}
	}

	// Create a new scope for class members
	classScope := &Scope{
		Parent:  a.currentScope,
		Symbols: make(map[string]Symbol),
	}

	// Push class scope
	a.pushScope(classScope)

	// Validate class members
	for _, member := range cd.Members {
		a.validateDeclaration(member)
	}

	// Pop class scope
	a.popScope()
}

// validateBlockStatement validates a block statement
func (a *Analyzer) validateBlockStatement(block *ast.BlockStatement) {
	// Create a new scope for the block
	blockScope := &Scope{
		Parent:  a.currentScope,
		Symbols: make(map[string]Symbol),
	}

	// Push block scope
	a.pushScope(blockScope)

	// Validate statements in the block
	for _, stmt := range block.Statements {
		a.validateStatement(stmt)
	}

	// Pop block scope
	a.popScope()
}

// validateStatement validates a statement
func (a *Analyzer) validateStatement(stmt ast.Statement) {
	switch s := stmt.(type) {
	case *ast.BlockStatement:
		a.validateBlockStatement(s)
	case *ast.ExpressionStatement:
		a.validateExpression(s.Expression)
	case *ast.DeclarationStatement:
		a.validateDeclaration(s.Declaration)
	case *ast.IfStatement:
		a.validateIfStatement(s)
	case *ast.ForStatement:
		a.validateForStatement(s)
	case *ast.WhileStatement:
		a.validateWhileStatement(s)
	case *ast.DoWhileStatement:
		a.validateDoWhileStatement(s)
	case *ast.ReturnStatement:
		a.validateReturnStatement(s)
	}
}

// validateIfStatement validates an if statement
func (a *Analyzer) validateIfStatement(stmt *ast.IfStatement) {
	// Validate condition
	a.validateExpression(stmt.Condition)

	// Validate consequence
	a.validateStatement(stmt.Consequence)

	// Validate alternative (if exists)
	if stmt.Alternative != nil {
		a.validateStatement(stmt.Alternative)
	}
}

// validateForStatement validates a for statement
func (a *Analyzer) validateForStatement(stmt *ast.ForStatement) {
	// Create a new scope for the for loop
	loopScope := &Scope{
		Parent:  a.currentScope,
		Symbols: make(map[string]Symbol),
	}

	// Push loop scope
	a.pushScope(loopScope)

	// Validate initialization (if exists)
	if stmt.Initialization != nil {
		a.validateStatement(stmt.Initialization)
	}

	// Validate condition (if exists)
	if stmt.Condition != nil {
		a.validateExpression(stmt.Condition)
	}

	// Validate increment (if exists)
	if stmt.Increment != nil {
		a.validateExpression(stmt.Increment)
	}

	// Validate body
	a.validateStatement(stmt.Body)

	// Pop loop scope
	a.popScope()
}

// validateWhileStatement validates a while statement
func (a *Analyzer) validateWhileStatement(stmt *ast.WhileStatement) {
	// Validate condition
	a.validateExpression(stmt.Condition)

	// Create a new scope for the while loop
	loopScope := &Scope{
		Parent:  a.currentScope,
		Symbols: make(map[string]Symbol),
	}

	// Push loop scope
	a.pushScope(loopScope)

	// Validate body
	a.validateStatement(stmt.Body)

	// Pop loop scope
	a.popScope()
}

// validateDoWhileStatement validates a do-while statement
func (a *Analyzer) validateDoWhileStatement(stmt *ast.DoWhileStatement) {
	// Create a new scope for the do-while loop
	loopScope := &Scope{
		Parent:  a.currentScope,
		Symbols: make(map[string]Symbol),
	}

	// Push loop scope
	a.pushScope(loopScope)

	// Validate body
	a.validateStatement(stmt.Body)

	// Pop loop scope
	a.popScope()

	// Validate condition
	a.validateExpression(stmt.Condition)
}

// validateReturnStatement validates a return statement
func (a *Analyzer) validateReturnStatement(stmt *ast.ReturnStatement) {
	if stmt.Value != nil {
		a.validateExpression(stmt.Value)

		// TODO: Check if return type matches function return type
	}
}

// validateExpression validates an expression
func (a *Analyzer) validateExpression(expr ast.Expression) {
	switch e := expr.(type) {
	case *ast.AssignmentExpression:
		a.validateAssignmentExpression(e)
	case *ast.BinaryExpression:
		a.validateBinaryExpression(e)
	case *ast.UnaryExpression:
		a.validateUnaryExpression(e)
	case *ast.CallExpression:
		a.validateCallExpression(e)
	case *ast.MemberExpression:
		a.validateMemberExpression(e)
	case *ast.IndexExpression:
		a.validateIndexExpression(e)
	case *ast.Identifier:
		a.validateIdentifier(e)
	}
}

// validateAssignmentExpression validates an assignment expression
func (a *Analyzer) validateAssignmentExpression(expr *ast.AssignmentExpression) {
	// Validate left-hand side (must be an l-value)
	a.validateExpression(expr.Left)

	// Validate right-hand side
	a.validateExpression(expr.Right)

	// TODO: Check type compatibility
}

// validateBinaryExpression validates a binary expression
func (a *Analyzer) validateBinaryExpression(expr *ast.BinaryExpression) {
	// Validate left and right operands
	a.validateExpression(expr.Left)
	a.validateExpression(expr.Right)

	// TODO: Check operator validity and type compatibility
}

// validateUnaryExpression validates a unary expression
func (a *Analyzer) validateUnaryExpression(expr *ast.UnaryExpression) {
	// Validate operand
	a.validateExpression(expr.Right)

	// TODO: Check operator validity and type compatibility
}

// validateCallExpression validates a function call
func (a *Analyzer) validateCallExpression(expr *ast.CallExpression) {
	// Validate function expression
	a.validateExpression(expr.Function)

	// Validate arguments
	for _, arg := range expr.Arguments {
		a.validateExpression(arg)
	}

	// TODO: Check if function exists and argument types match parameters
}

// validateMemberExpression validates a member access expression
func (a *Analyzer) validateMemberExpression(expr *ast.MemberExpression) {
	// Validate object
	a.validateExpression(expr.Object)

	// Validate property
	a.validateExpression(expr.Property)

	// TODO: Check if member exists in the object's type
}

// validateIndexExpression validates an array index expression
func (a *Analyzer) validateIndexExpression(expr *ast.IndexExpression) {
	// Validate array
	a.validateExpression(expr.Array)

	// Validate index
	a.validateExpression(expr.Index)

	// TODO: Check if index is numeric and array is indexable
}

// validateIdentifier checks if an identifier is declared
func (a *Analyzer) validateIdentifier(expr *ast.Identifier) {
	// Look for the identifier in current and parent scopes
	found := false
	scope := a.currentScope

	for scope != nil {
		if _, exists := scope.Symbols[expr.Value]; exists {
			found = true
			break
		}
		scope = scope.Parent
	}

	if !found {
		a.addError(fmt.Sprintf("undefined identifier '%s'", expr.Value))
	}
}

// analyzeBlockStatement analyzes a block statement during the collection phase
func (a *Analyzer) analyzeBlockStatement(block *ast.BlockStatement) {
	// Create a new scope for the block
	blockScope := &Scope{
		Parent:  a.currentScope,
		Symbols: make(map[string]Symbol),
	}

	// Push block scope
	a.pushScope(blockScope)

	// Analyze statements in the block
	for _, stmt := range block.Statements {
		a.analyzeStatement(stmt)
	}

	// Pop block scope
	a.popScope()
}

// analyzeStatement analyzes a statement during the collection phase
func (a *Analyzer) analyzeStatement(stmt ast.Statement) {
	switch s := stmt.(type) {
	case *ast.BlockStatement:
		a.analyzeBlockStatement(s)
	case *ast.DeclarationStatement:
		a.collectDeclaration(s.Declaration)
	case *ast.ExpressionStatement:
		a.analyzeExpression()
	}
}

// analyzeExpression analyzes an expression during the collection phase
func (a *Analyzer) analyzeExpression() {
	// Simplified implementation
}

// isValidType checks if a type is valid in the current scope
func (a *Analyzer) isValidType(typeName string) bool {
	// TODO: Implement proper type checking with templates, pointers, etc.

	// For now, check if the type exists in any scope
	scope := a.currentScope
	for scope != nil {
		if symbol, exists := scope.Symbols[typeName]; exists && symbol.Kind == TypeSymbol {
			return true
		}
		scope = scope.Parent
	}

	// Handle built-in types
	builtinTypes := map[string]bool{
		"void": true, "char": true, "short": true, "int": true, "long": true,
		"unsigned": true, "float": true, "double": true, "bool": true,
		"size_t": true, "wchar_t": true,
	}

	return builtinTypes[typeName]
}

// pushScope pushes a new scope onto the scope stack
func (a *Analyzer) pushScope(scope *Scope) {
	a.scopes = append(a.scopes, scope)
	a.currentScope = scope
}

// popScope pops the current scope from the scope stack
func (a *Analyzer) popScope() {
	if len(a.scopes) > 1 {
		a.scopes = a.scopes[:len(a.scopes)-1]
		a.currentScope = a.scopes[len(a.scopes)-1]
	}
}

// addError adds an error message to the error list
func (a *Analyzer) addError(msg string) {
	a.errors = append(a.errors, msg)
}
