package ast

import (
	"fmt"
	"github.com/mdaashir/GOmpiler-2/pkg/lexer"
)

// BinaryExpression represents a binary expression with an operator
type BinaryExpression struct {
	Token    lexer.Token // The operator token
	Left     Expression  // Left-hand side expression
	Operator string      // Operator (e.g., "+", "-", "*", "/", "==", etc.)
	Right    Expression  // Right-hand side expression
}

// TokenLiteral returns the token literal of the expression
func (be *BinaryExpression) TokenLiteral() string {
	return be.Token.Literal
}

// expressionNode marks this as an Expression node
func (be *BinaryExpression) expressionNode() {}
