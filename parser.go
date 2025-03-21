package main

import (
	"fmt"
)

type Parser struct {
	tokens  []Token
	current int
}

func NewParser(tokens []Token) *Parser {
	return &Parser{tokens: tokens, current: 0}
}

func (p *Parser) nextToken() Token {
	if p.current < len(p.tokens) {
		tok := p.tokens[p.current]
		p.current++
		return tok
	}
	return Token{Type: UNKNOWN, Value: ""}
}

func (p *Parser) peekToken() Token {
	if p.current < len(p.tokens) {
		return p.tokens[p.current]
	}
	return Token{Type: UNKNOWN, Value: ""}
}

func (p *Parser) Parse() {
	for p.current < len(p.tokens) {
		tok := p.nextToken()
		switch tok.Type {
		case KEYWORD:
			p.parseKeyword(tok)
		case IDENTIFIER:
			p.parseIdentifier(tok)
		case NUMBER, STRING, CHAR:
			fmt.Println("Literal:", tok.Value)
		case OPERATOR, BRACKET, SEMICOLON, COMMA, DOT:
			fmt.Println("Symbol:", tok.Value)
		case COMMENT, PREPROCESSOR:
			fmt.Println("Meta Information:", tok.Value)
		default:
			fmt.Println("Unknown token:", tok.Value)
		}
	}
}

func (p *Parser) parseKeyword(tok Token) {
	fmt.Println("Keyword:", tok.Value)
}

func (p *Parser) parseIdentifier(tok Token) {
	fmt.Println("Identifier:", tok.Value)
}
