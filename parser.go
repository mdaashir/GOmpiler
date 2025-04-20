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

func (p *Parser) peekNextToken() Token {
	if p.current+1 >= len(p.tokens) {
		return Token{Type: UNKNOWN, Value: ""}
	}
	return p.tokens[p.current+1]
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
		if tok.Value == "int" || tok.Value == "char" || tok.Value == "float" {
			return p.parseVariableDeclaration()
		}
		return p.parseKeyword()
	case IDENTIFIER:
		if p.peekNextToken().Type == BRACKET && p.peekNextToken().Value == "(" {
			return p.parseFunction()
		}
		return p.parseExpression()
	case BRACKET:
		if tok.Value == "{" || tok.Value == "}" {
			return p.parseBlock()
		}
	default:
		fmt.Println("Skipping unhandled token:", tok)
		if p.current < len(p.tokens) {
			p.nextToken()
		}
		return nil
	}
	return nil
}

func (p *Parser) parsePreprocessor() *ASTNode {
	tok := p.nextToken()
	if tok.Type != PREPROCESSOR {
		return nil
	}

	node := &ASTNode{Type: NodeType(PREPROCESSOR), Value: tok.Value}

	directive := p.nextToken()
	node.Children = append(node.Children, &ASTNode{Type: NodeType(IDENTIFIER), Value: directive.Value})

	if directive.Value == "include" {
		openTok := p.nextToken()
		if openTok.Type == OPERATOR && (openTok.Value == "<" || openTok.Value == "\"") {
			fileNode := &ASTNode{Type: NodeType(STRING), Value: ""}

			for {
				nextTok := p.nextToken()
				if nextTok.Type == OPERATOR && ((openTok.Value == "<" && nextTok.Value == ">") ||
					(openTok.Value == "\"" && nextTok.Value == "\"")) {
					break
				}
				fileNode.Value += nextTok.Value
			}
			node.Children = append(node.Children, fileNode)
		}
	}
	return node
}

func (p *Parser) parseKeyword() *ASTNode {
	tok := p.nextToken()
	node := &ASTNode{Type: STATEMENT, Value: tok.Value}
	if tok.Value == "return" {
		expr := p.parseExpression()
		if expr != nil {
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
		if right != nil {
			node = &ASTNode{Type: EXPRESSION, Value: op.Value, Children: []*ASTNode{node, right}}
		}
	}
	return node
}

func (p *Parser) parseVariableDeclaration() *ASTNode {
	tok := p.nextToken()
	identifier := p.nextToken()
	node := &ASTNode{Type: STATEMENT, Value: tok.Value + " " + identifier.Value}

	if p.peekToken().Type == OPERATOR && p.peekToken().Value == "=" {
		p.nextToken()
		expr := p.parseExpression()
		node.Children = append(node.Children, expr)
	}

	if p.peekToken().Type == SEMICOLON {
		p.nextToken()
	}

	return node
}

func (p *Parser) parseFunction() *ASTNode {
	_ = p.nextToken()
	name := p.nextToken()

	if p.peekToken().Type != BRACKET || p.peekToken().Value != "(" {
		fmt.Println("Error: Expected '(' after function name")
		return nil
	}
	p.nextToken()

	paramsNode := &ASTNode{Type: STATEMENT, Value: "Parameters"}
	for p.peekToken().Type != BRACKET || (p.peekToken().Type == BRACKET && p.peekToken().Value != ")") {
		param := p.nextToken()
		if param.Type == IDENTIFIER || param.Type == KEYWORD {
			paramsNode.Children = append(paramsNode.Children, &ASTNode{Type: EXPRESSION, Value: param.Value})
		}
		if p.current >= len(p.tokens) {
			fmt.Println("Error: Unexpected end of tokens while parsing function parameters")
			return nil
		}
	}
	p.nextToken()

	if p.peekToken().Type != BRACKET || p.peekToken().Value != "{" {
		fmt.Println("Error: Expected '{' after function declaration")
		return nil
	}
	p.nextToken()

	body := &ASTNode{Type: FUNCTION_DEF, Value: name.Value, Children: []*ASTNode{paramsNode}}

	for {
		if p.current >= len(p.tokens) {
			fmt.Println("Error: Unexpected end of tokens while parsing function body")
			break
		}

		if p.peekToken().Type == BRACKET && p.peekToken().Value == "}" {
			p.nextToken()
			break
		}

		stmt := p.parseStatement()
		if stmt != nil {
			body.Children = append(body.Children, stmt)
		} else {
			p.nextToken()
		}
	}

	return body
}

func (p *Parser) parseBlock() *ASTNode {
	_ = p.nextToken()
	node := &ASTNode{Type: STATEMENT, Value: "Block"}

	for {
		if p.current >= len(p.tokens) {
			fmt.Println("Error: Unexpected end of tokens while parsing block")
			break
		}

		if p.peekToken().Type == BRACKET && p.peekToken().Value == "}" {
			p.nextToken()
			break
		}

		stmt := p.parseStatement()
		if stmt != nil {
			node.Children = append(node.Children, stmt)
		} else {
			p.nextToken()
		}
	}
	return node
}
