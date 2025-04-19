// ContinueStatement represents a continue statement
type ContinueStatement struct{}

func (cs *ContinueStatement) TokenLiteral() string {
	return "continue"
}

func (cs *ContinueStatement) statementNode() {}

// GotoStatement represents a goto statement
type GotoStatement struct {
	Label string
}

func (gs *GotoStatement) TokenLiteral() string {
	return "goto"
}

func (gs *GotoStatement) statementNode() {}

// AssignmentExpression represents an assignment operation
type AssignmentExpression struct {
	Left     Expression
	Operator string
	Right    Expression
}

func (ae *AssignmentExpression) TokenLiteral() string {
	return ae.Operator
}

func (ae *AssignmentExpression) expressionNode() {}

// BinaryExpression represents a binary operation
type BinaryExpression struct {
	Left     Expression
	Operator string
	Right    Expression
}

func (be *BinaryExpression) TokenLiteral() string {
	return be.Operator
}

func (be *BinaryExpression) expressionNode() {}

// UnaryExpression represents a unary operation
type UnaryExpression struct {
	Operator string
	Right    Expression
}

func (ue *UnaryExpression) TokenLiteral() string {
	return ue.Operator
}

func (ue *UnaryExpression) expressionNode() {}

// CallExpression represents a function call
type CallExpression struct {
	Function  Expression
	Arguments []Expression
}

func (ce *CallExpression) TokenLiteral() string {
	return "call"
}

func (ce *CallExpression) expressionNode() {}

// MemberExpression represents accessing a member of an object
type MemberExpression struct {
	Object   Expression
	Property Expression
}

func (me *MemberExpression) TokenLiteral() string {
	return "."
}

func (me *MemberExpression) expressionNode() {}

// IndexExpression represents array indexing
type IndexExpression struct {
	Array Expression
	Index Expression
}

func (ie *IndexExpression) TokenLiteral() string {
	return "[]"
}

func (ie *IndexExpression) expressionNode() {}

// Identifier represents a variable name
type Identifier struct {
	Value string
}

func (i *Identifier) TokenLiteral() string {
	return i.Value
}

func (i *Identifier) expressionNode() {}

// NumberLiteral represents a numeric literal
type NumberLiteral struct {
	Value string
}

func (nl *NumberLiteral) TokenLiteral() string {
	return nl.Value
}

func (nl *NumberLiteral) expressionNode() {}

// StringLiteral represents a string literal
type StringLiteral struct {
	Value string
}

func (sl *StringLiteral) TokenLiteral() string {
	return sl.Value
}

func (sl *StringLiteral) expressionNode() {}

// CharLiteral represents a character literal
type CharLiteral struct {
	Value string
}

func (cl *CharLiteral) TokenLiteral() string {
	return cl.Value
}

func (cl *CharLiteral) expressionNode() {}

// BooleanLiteral represents a boolean literal
type BooleanLiteral struct {
	Value bool
}

func (bl *BooleanLiteral) TokenLiteral() string {
	if bl.Value {
		return "true"
	}
	return "false"
}

func (bl *BooleanLiteral) expressionNode() {}

// NullExpression represents a placeholder for parsing errors
type NullExpression struct{}

func (ne *NullExpression) TokenLiteral() string {
	return "null"
}

func (ne *NullExpression) expressionNode() {}

// ArrayLiteral represents an array literal
type ArrayLiteral struct {
	Elements []Expression
}

func (al *ArrayLiteral) TokenLiteral() string {
	return "array"
}

func (al *ArrayLiteral) expressionNode() {}

// NewExpression represents a new expression
type NewExpression struct {
	Type      string
	Arguments []Expression
}

func (ne *NewExpression) TokenLiteral() string {
	return "new"
}

func (ne *NewExpression) expressionNode() {}

// DeleteExpression represents a delete expression
type DeleteExpression struct {
	Target Expression
	Array  bool
}

func (de *DeleteExpression) TokenLiteral() string {
	if de.Array {
		return "delete[]"
	}
	return "delete"
}

func (de *DeleteExpression) expressionNode() {}

// CastExpression represents a type cast
type CastExpression struct {
	Type       string
	Expression Expression
}

func (ce *CastExpression) TokenLiteral() string {
	return "cast"
}

func (ce *CastExpression) expressionNode() {}

// ConditionalExpression represents a ternary conditional expression
type ConditionalExpression struct {
	Condition Expression
	TrueExpr  Expression
	FalseExpr Expression
}

func (ce *ConditionalExpression) TokenLiteral() string {
	return "?:"
}

func (ce *ConditionalExpression) expressionNode() {}

// LambdaExpression represents a lambda expression
type LambdaExpression struct {
	Captures   []string
	Parameters []Parameter
	Body       *BlockStatement
}

func (le *LambdaExpression) TokenLiteral() string {
	return "lambda"
}

func (le *LambdaExpression) expressionNode() {}
