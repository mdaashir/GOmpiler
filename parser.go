package main

import "fmt"

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
		fmt.Println("Parsing token:", p.tokens[p.current]) // Debug print
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
	case PREPROCESSOR:
		return p.parsePreprocessor()
	case KEYWORD:
		return p.parseKeyword()
	case IDENTIFIER:
		return p.parseExpression()
	default:
		// Consume the token to avoid infinite loops
		fmt.Println("Skipping unhandled token:", tok)
		p.nextToken()
		return nil
	}
}
func (p *Parser) parsePreprocessor() *ASTNode {
	tok := p.nextToken()
	node := &ASTNode{Type: STATEMENT, Value: tok.Value} // Change "PREPROCESSOR_NODE" to STATEMENT
	return node
}

func (p *Parser) parseKeyword() *ASTNode {
	tok := p.nextToken()
	node := &ASTNode{Type: STATEMENT, Value: tok.Value}
	if tok.Value == "return" {
		expr := p.parseExpression()
		if expr != nil { // Ensure return captures its value
			node.Children = append(node.Children, expr)
		}
	}
	return node
}

func (p *Parser) parseExpression() *ASTNode {
	tok := p.nextToken()
	node := &ASTNode{Type: EXPRESSION, Value: tok.Value}
	for p.peekToken().Type == OPERATOR {
		op := p.nextToken()
		right := p.parseExpression()
		if right != nil { // Ensure right-hand expression exists
			node = &ASTNode{Type: EXPRESSION, Value: op.Value, Children: []*ASTNode{node, right}}
		}
	}
	return node
}
