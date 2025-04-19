package ast

// Expression represents any expression in the AST
type Expression interface {
	Node
	expressionNode()
	String() string
}
