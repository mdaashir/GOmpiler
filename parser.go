package main

import (
	"fmt"
)

type NodeType string

type ASTNode struct {
	Type     NodeType
	Value    string
	Children []*ASTNode
}

const (
	PROGRAM      NodeType = "PROGRAM"
	FUNCTION_DEF NodeType = "FUNCTION_DEF"
	STATEMENT    NodeType = "STATEMENT"
	EXPRESSION   NodeType = "EXPRESSION"
)

type Parser struct {
	tokens  []Token
	current int
}

func NewParser(tokens []Token) *Parser {
	return &Parser{tokens: tokens}
}

func (p *Parser) nextToken() Token {
	if p.current >= len(p.tokens) {
		return Token{Type: UNKNOWN, Value: ""}
	}
	tok := p.tokens[p.current]
	p.current++
	return tok
}

func (p *Parser) peekToken() Token {
	if p.current >= len(p.tokens) {
		return Token{Type: UNKNOWN, Value: ""}
	}
	return p.tokens[p.current]
}

func (p *Parser) Parse() *ASTNode {
	root := &ASTNode{Type: PROGRAM, Value: "Program"}
	for p.current < len(p.tokens) {
		node := p.parseStatement()
		if node != nil {
			root.Children = append(root.Children, node)
		}
	}
	return root
}

func (p *Parser) parseStatement() *ASTNode {
	tok := p.peekToken()
	switch tok.Type {
	case KEYWORD:
		return p.parseKeyword()
	case IDENTIFIER:
		return p.parseExpression()
	default:
		return nil
	}
}

func (p *Parser) parseKeyword() *ASTNode {
	tok := p.nextToken()
	node := &ASTNode{Type: STATEMENT, Value: tok.Value}
	return node
}

func (p *Parser) parseExpression() *ASTNode {
	tok := p.nextToken()
	node := &ASTNode{Type: EXPRESSION, Value: tok.Value}
	if p.peekToken().Type == OPERATOR {
		op := p.nextToken()
		right := p.parseExpression()
		operatorNode := &ASTNode{Type: EXPRESSION, Value: op.Value, Children: []*ASTNode{node, right}}
		return operatorNode
	}
	return node
}

func (p *Parser) PrintAST(node *ASTNode, depth int) {
	for i := 0; i < depth; i++ {
		fmt.Print("  ")
	}
	fmt.Println(node.Type, ":", node.Value)
	for _, child := range node.Children {
		p.PrintAST(child, depth+1)
	}
}
