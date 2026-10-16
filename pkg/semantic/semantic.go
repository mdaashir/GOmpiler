package semantic

import (
	"fmt"
	"strings"

	"github.com/mdaashir/GOmpiler-2/pkg/ast"
)

// Analyzer performs semantic analysis on the AST.
type Analyzer struct {
	errors       []string
	scopes       []*Scope
	currentScope *Scope
}

// Scope represents a symbol scope.
type Scope struct {
	Parent  *Scope
	Symbols map[string]Symbol
}

// Symbol represents an entry in the symbol table.
type Symbol struct {
	Name   string
	Type   string
	Kind   SymbolKind
	Arity  int // number of parameters, for functions
	Scope  *Scope
	Global bool
}

// SymbolKind represents the kind of a symbol.
type SymbolKind int

const (
	VariableSymbol SymbolKind = iota
	FunctionSymbol
	TypeSymbol
)

// builtinTypes are the primitive types this compiler understands.
var builtinTypes = map[string]bool{
	"void": true, "char": true, "short": true, "int": true, "long": true,
	"unsigned": true, "signed": true, "float": true, "double": true,
	"bool": true,
}

// New creates a new semantic analyzer.
func New() *Analyzer {
	globalScope := &Scope{Symbols: make(map[string]Symbol)}
	a := &Analyzer{
		errors:       []string{},
		scopes:       []*Scope{globalScope},
		currentScope: globalScope,
	}
	a.initBuiltins()
	return a
}

// initBuiltins registers the standard stream objects this compiler supports.
func (a *Analyzer) initBuiltins() {
	for _, name := range []string{"std::cout", "std::cin", "std::cerr", "std::endl"} {
		a.currentScope.Symbols[name] = Symbol{
			Name:   name,
			Type:   "ostream",
			Kind:   VariableSymbol,
			Scope:  a.currentScope,
			Global: true,
		}
	}
}

// Analyze performs semantic analysis on the AST.
func (a *Analyzer) Analyze(program *ast.Program) error {
	a.collectGlobals(program)
	a.validateProgram(program)

	if len(a.errors) > 0 {
		return fmt.Errorf("semantic errors: %v", a.errors)
	}
	return nil
}

// collectGlobals registers all top-level functions and variables so that any
// of them can be referenced from anywhere, regardless of source order.
func (a *Analyzer) collectGlobals(program *ast.Program) {
	for _, decl := range program.Declarations {
		switch d := decl.(type) {
		case *ast.FunctionDeclaration:
			if existing, ok := a.currentScope.Symbols[d.Name]; ok {
				if existing.Kind == FunctionSymbol {
					// A prototype followed by its definition is not an error.
					continue
				}
				a.addError(fmt.Sprintf("%q is already declared", d.Name))
				continue
			}
			a.declareHere(d.Name, d.Type, FunctionSymbol, len(d.Parameters))
		case *ast.VariableDeclaration:
			a.declare(d.Name, d.Type, VariableSymbol, 0)
		}
	}
}

func (a *Analyzer) validateProgram(program *ast.Program) {
	for _, decl := range program.Declarations {
		switch d := decl.(type) {
		case *ast.FunctionDeclaration:
			a.validateFunction(d)
		case *ast.VariableDeclaration:
			a.validateGlobalVariable(d)
		}
	}
}

func (a *Analyzer) validateFunction(fn *ast.FunctionDeclaration) {
	if fn.Body == nil {
		return // prototype; already collected
	}
	if !a.isValidType(fn.Type) {
		a.addError(fmt.Sprintf("unknown return type %q for function %q", fn.Type, fn.Name))
	}
	for _, param := range fn.Parameters {
		if !a.isValidType(param.Type) {
			a.addError(fmt.Sprintf("unknown parameter type %q in function %q", param.Type, fn.Name))
		}
	}

	a.pushScope()
	for _, param := range fn.Parameters {
		a.declareHere(param.Name, param.Type, VariableSymbol, 0)
	}
	for _, stmt := range fn.Body.Statements {
		a.validateStatement(stmt)
	}
	a.popScope()
}

func (a *Analyzer) validateGlobalVariable(vd *ast.VariableDeclaration) {
	if !a.isValidType(vd.Type) {
		a.addError(fmt.Sprintf("unknown type %q for variable %q", vd.Type, vd.Name))
	}
	if vd.Initializer != nil {
		a.validateExpression(vd.Initializer)
	}
}

func (a *Analyzer) validateStatement(stmt ast.Statement) {
	switch s := stmt.(type) {
	case *ast.BlockStatement:
		a.pushScope()
		for _, inner := range s.Statements {
			a.validateStatement(inner)
		}
		a.popScope()
	case *ast.DeclarationStatement:
		a.validateLocalDeclaration(s.Declaration)
	case *ast.ExpressionStatement:
		a.validateExpression(s.Expression)
	case *ast.IfStatement:
		a.validateExpression(s.Condition)
		a.validateStatement(s.Consequence)
		if s.Alternative != nil {
			a.validateStatement(s.Alternative)
		}
	case *ast.ForStatement:
		a.pushScope()
		if s.Initialization != nil {
			a.validateStatement(s.Initialization)
		}
		if s.Condition != nil {
			a.validateExpression(s.Condition)
		}
		if s.Increment != nil {
			a.validateExpression(s.Increment)
		}
		a.validateStatement(s.Body)
		a.popScope()
	case *ast.WhileStatement:
		a.validateExpression(s.Condition)
		a.validateStatement(s.Body)
	case *ast.DoWhileStatement:
		a.validateStatement(s.Body)
		a.validateExpression(s.Condition)
	case *ast.ReturnStatement:
		if s.Value != nil {
			a.validateExpression(s.Value)
		}
	case *ast.BreakStatement, *ast.ContinueStatement, *ast.EmptyStatement:
		// nothing to check
	}
}

func (a *Analyzer) validateLocalDeclaration(decl ast.Declaration) {
	vd, ok := decl.(*ast.VariableDeclaration)
	if !ok {
		return
	}
	if !a.isValidType(vd.Type) {
		a.addError(fmt.Sprintf("unknown type %q for variable %q", vd.Type, vd.Name))
	}
	if vd.Initializer != nil {
		a.validateExpression(vd.Initializer)
	}
	a.declareHere(vd.Name, vd.Type, VariableSymbol, 0)
}

func (a *Analyzer) validateExpression(expr ast.Expression) {
	switch e := expr.(type) {
	case nil:
		return
	case *ast.Identifier:
		a.validateIdentifier(e)
	case *ast.AssignmentExpression:
		if !isLValue(e.Left) {
			a.addError(fmt.Sprintf("left side of %q is not assignable", e.Operator))
		}
		a.validateExpression(e.Left)
		a.validateExpression(e.Right)
	case *ast.BinaryExpression:
		a.validateExpression(e.Left)
		a.validateExpression(e.Right)
	case *ast.UnaryExpression:
		if e.Operator == "++" || e.Operator == "--" {
			if !isLValue(e.Right) {
				a.addError(fmt.Sprintf("operand of %q is not assignable", e.Operator))
			}
		}
		a.validateExpression(e.Right)
	case *ast.CallExpression:
		a.validateCall(e)
	case *ast.MemberExpression:
		a.validateExpression(e.Object)
		a.validateExpression(e.Property)
	case *ast.IndexExpression:
		a.validateExpression(e.Array)
		a.validateExpression(e.Index)
	}
}

func (a *Analyzer) validateCall(call *ast.CallExpression) {
	for _, arg := range call.Arguments {
		a.validateExpression(arg)
	}

	id, ok := call.Function.(*ast.Identifier)
	if !ok {
		a.validateExpression(call.Function)
		return
	}
	sym, found := a.lookup(id.Value)
	if !found {
		a.addError(fmt.Sprintf("call to undefined function %q", id.Value))
		return
	}
	if sym.Kind != FunctionSymbol {
		a.addError(fmt.Sprintf("%q is not a function", id.Value))
		return
	}
	if len(call.Arguments) != sym.Arity {
		a.addError(fmt.Sprintf("function %q expects %d argument(s), got %d",
			id.Value, sym.Arity, len(call.Arguments)))
	}
}

func (a *Analyzer) validateIdentifier(expr *ast.Identifier) {
	if _, found := a.lookup(expr.Value); !found {
		a.addError(fmt.Sprintf("undefined identifier %q", expr.Value))
	}
}

// isValidType reports whether a type specifier names a known type.
func (a *Analyzer) isValidType(typeName string) bool {
	normalized := strings.TrimSpace(typeName)
	normalized = strings.TrimRight(normalized, "*& ")
	normalized = strings.TrimSpace(normalized)
	if normalized == "" {
		return false
	}
	for _, word := range strings.Fields(normalized) {
		switch word {
		case "const", "static", "extern", "inline", "register", "volatile":
			continue
		}
		if !builtinTypes[word] {
			return false
		}
	}
	return true
}

func isLValue(expr ast.Expression) bool {
	switch e := expr.(type) {
	case *ast.Identifier:
		return true
	case *ast.MemberExpression:
		return true
	case *ast.IndexExpression:
		return true
	case *ast.UnaryExpression:
		return e.Operator == "*"
	}
	return false
}

func (a *Analyzer) lookup(name string) (Symbol, bool) {
	for scope := a.currentScope; scope != nil; scope = scope.Parent {
		if sym, ok := scope.Symbols[name]; ok {
			return sym, true
		}
	}
	return Symbol{}, false
}

func (a *Analyzer) declare(name, typeName string, kind SymbolKind, arity int) {
	if _, exists := a.currentScope.Symbols[name]; exists {
		a.addError(fmt.Sprintf("%q is already declared", name))
		return
	}
	a.declareHere(name, typeName, kind, arity)
}

func (a *Analyzer) declareHere(name, typeName string, kind SymbolKind, arity int) {
	if _, exists := a.currentScope.Symbols[name]; exists {
		a.addError(fmt.Sprintf("%q is already declared", name))
		return
	}
	a.currentScope.Symbols[name] = Symbol{
		Name:  name,
		Type:  typeName,
		Kind:  kind,
		Arity: arity,
		Scope: a.currentScope,
	}
}

func (a *Analyzer) pushScope() {
	scope := &Scope{Parent: a.currentScope, Symbols: make(map[string]Symbol)}
	a.scopes = append(a.scopes, scope)
	a.currentScope = scope
}

func (a *Analyzer) popScope() {
	if len(a.scopes) > 1 {
		a.scopes = a.scopes[:len(a.scopes)-1]
		a.currentScope = a.scopes[len(a.scopes)-1]
	}
}

func (a *Analyzer) addError(msg string) {
	a.errors = append(a.errors, msg)
}
