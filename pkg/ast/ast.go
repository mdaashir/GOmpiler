package ast

// Node represents a node in the AST
type Node interface {
	TokenLiteral() string
}

// Statement represents a statement in the AST
type Statement interface {
	Node
	statementNode()
}

// Expression represents an expression in the AST
type Expression interface {
	Node
	expressionNode()
}

// The Program represents a complete program
type Program struct {
	Declarations []Declaration
}

// Declaration represents a declaration statement
type Declaration interface {
	Node
	declarationNode()
}

// FunctionDeclaration represents a function declaration
type FunctionDeclaration struct {
	Type       string
	Name       string
	Parameters []Parameter
	Body       *BlockStatement
}

// Parameter represents a function parameter
type Parameter struct {
	Type string
	Name string
}

// VariableDeclaration represents a variable declaration
type VariableDeclaration struct {
	Type        string
	Name        string
	ArraySize   string
	Initializer Expression
}

// TypedefDeclaration represents a typedef declaration
type TypedefDeclaration struct {
	OriginalType string
	NewType      string
}

// ClassDeclaration represents a class/struct declaration
type ClassDeclaration struct {
	Name        string
	BaseClasses []string
	Members     []Declaration
}

// EnumDeclaration represents an enum declaration
type EnumDeclaration struct {
	Name   string
	Values []string
}

// NamespaceDeclaration represents a namespace declaration
type NamespaceDeclaration struct {
	Name         string
	Declarations []Declaration
}

// TemplateDeclaration represents a template declaration
type TemplateDeclaration struct {
	Declaration Declaration
}

// UsingDeclaration represents a using declaration
type UsingDeclaration struct {
	Namespace string
}

// PreprocessorDirective represents a preprocessor directive
type PreprocessorDirective struct {
	Directive string
}

// BlockStatement represents a block of statements
type BlockStatement struct {
	Statements []Statement
}

// EmptyStatement represents an empty statement (just a semicolon)
type EmptyStatement struct{}

// ExpressionStatement represents an expression used as a statement
type ExpressionStatement struct {
	Expression Expression
}

// DeclarationStatement represents a declaration used as a statement
type DeclarationStatement struct {
	Declaration Declaration
}

// IfStatement represents an if statement
type IfStatement struct {
	Condition   Expression
	Consequence Statement
	Alternative Statement
}

// ForStatement represents a for loop
type ForStatement struct {
	Initialization Statement
	Condition      Expression
	Increment      Expression
	Body           Statement
}

// WhileStatement represents a while loop
type WhileStatement struct {
	Condition Expression
	Body      Statement
}

// DoWhileStatement represents a do-while loop
type DoWhileStatement struct {
	Body      Statement
	Condition Expression
}

// SwitchStatement represents a switch statement
type SwitchStatement struct {
	Value Expression
	Body  Statement
}

// ReturnStatement represents a return statement
type ReturnStatement struct {
	Value Expression
}

// BreakStatement represents a break statement
type BreakStatement struct{}

// ContinueStatement represents a continue statement
type ContinueStatement struct{}

// GotoStatement represents a goto statement
type GotoStatement struct {
	Label string
}

// AssignmentExpression represents an assignment expression
type AssignmentExpression struct {
	Left     Expression
	Operator string
	Right    Expression
}

// UnaryExpression represents a unary expression
type UnaryExpression struct {
	Operator string
	Right    Expression
}

// CallExpression represents a function call
type CallExpression struct {
	Function  Expression
	Arguments []Expression
}

// MemberExpression represents a member access expression
type MemberExpression struct {
	Object   Expression
	Property Expression
}

// IndexExpression represents an array index expression
type IndexExpression struct {
	Array Expression
	Index Expression
}

// Identifier represents a variable name
type Identifier struct {
	Value string
}

// NumberLiteral represents a numeric literal
type NumberLiteral struct {
	Value string
}

// StringLiteral represents a string literal
type StringLiteral struct {
	Value string
}

// CharLiteral represents a character literal
type CharLiteral struct {
	Value string
}

// BooleanLiteral represents a boolean literal
type BooleanLiteral struct {
	Value bool
}

// NullExpression represents a null expression
type NullExpression struct{}

// TokenLiteral and node implementations
func (p *Program) TokenLiteral() string { return "program" }

func (fd *FunctionDeclaration) TokenLiteral() string { return fd.Name }
func (fd *FunctionDeclaration) declarationNode()     {}

func (vd *VariableDeclaration) TokenLiteral() string { return vd.Name }
func (vd *VariableDeclaration) declarationNode()     {}

func (td *TypedefDeclaration) TokenLiteral() string { return td.NewType }
func (td *TypedefDeclaration) declarationNode()     {}

func (cd *ClassDeclaration) TokenLiteral() string { return cd.Name }
func (cd *ClassDeclaration) declarationNode()     {}

func (ed *EnumDeclaration) TokenLiteral() string { return ed.Name }
func (ed *EnumDeclaration) declarationNode()     {}

func (nd *NamespaceDeclaration) TokenLiteral() string { return nd.Name }
func (nd *NamespaceDeclaration) declarationNode()     {}

func (td *TemplateDeclaration) TokenLiteral() string { return "template" }
func (td *TemplateDeclaration) declarationNode()     {}

func (ud *UsingDeclaration) TokenLiteral() string { return "using" }
func (ud *UsingDeclaration) declarationNode()     {}

func (pd *PreprocessorDirective) TokenLiteral() string { return pd.Directive }
func (pd *PreprocessorDirective) declarationNode()     {}

func (bs *BlockStatement) TokenLiteral() string { return "{" }
func (bs *BlockStatement) statementNode()       {}

func (es *EmptyStatement) TokenLiteral() string { return ";" }
func (es *EmptyStatement) statementNode()       {}

func (es *ExpressionStatement) TokenLiteral() string { return "expr" }
func (es *ExpressionStatement) statementNode()       {}

func (ds *DeclarationStatement) TokenLiteral() string { return "decl" }
func (ds *DeclarationStatement) statementNode()       {}

func (is *IfStatement) TokenLiteral() string { return "if" }
func (is *IfStatement) statementNode()       {}

func (fs *ForStatement) TokenLiteral() string { return "for" }
func (fs *ForStatement) statementNode()       {}

func (ws *WhileStatement) TokenLiteral() string { return "while" }
func (ws *WhileStatement) statementNode()       {}

func (dws *DoWhileStatement) TokenLiteral() string { return "do" }
func (dws *DoWhileStatement) statementNode()       {}

func (ss *SwitchStatement) TokenLiteral() string { return "switch" }
func (ss *SwitchStatement) statementNode()       {}

func (rs *ReturnStatement) TokenLiteral() string { return "return" }
func (rs *ReturnStatement) statementNode()       {}

func (bs *BreakStatement) TokenLiteral() string { return "break" }
func (bs *BreakStatement) statementNode()       {}

func (cs *ContinueStatement) TokenLiteral() string { return "continue" }
func (cs *ContinueStatement) statementNode()       {}

func (gs *GotoStatement) TokenLiteral() string { return "goto" }
func (gs *GotoStatement) statementNode()       {}

func (ae *AssignmentExpression) TokenLiteral() string { return "=" }
func (ae *AssignmentExpression) expressionNode()      {}

func (ue *UnaryExpression) TokenLiteral() string { return ue.Operator }
func (ue *UnaryExpression) expressionNode()      {}

func (ce *CallExpression) TokenLiteral() string { return "call" }
func (ce *CallExpression) expressionNode()      {}

func (me *MemberExpression) TokenLiteral() string { return "." }
func (me *MemberExpression) expressionNode()      {}

func (ie *IndexExpression) TokenLiteral() string { return "[]" }
func (ie *IndexExpression) expressionNode()      {}

func (i *Identifier) TokenLiteral() string { return i.Value }
func (i *Identifier) expressionNode()      {}

func (nl *NumberLiteral) TokenLiteral() string { return nl.Value }
func (nl *NumberLiteral) expressionNode()      {}

func (sl *StringLiteral) TokenLiteral() string { return sl.Value }
func (sl *StringLiteral) expressionNode()      {}

func (cl *CharLiteral) TokenLiteral() string { return cl.Value }
func (cl *CharLiteral) expressionNode()      {}

func (bl *BooleanLiteral) TokenLiteral() string {
	if bl.Value {
		return "true"
	}
	return "false"
}
func (bl *BooleanLiteral) expressionNode() {}

func (ne *NullExpression) TokenLiteral() string { return "null" }
func (ne *NullExpression) expressionNode()      {}
