package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"unicode"
)

type TokenType string

type Token struct {
	Type  TokenType
	Value string
}

const (
	IDENTIFIER TokenType = "IDENTIFIER"
	KEYWORD    TokenType = "KEYWORD"
	NUMBER     TokenType = "NUMBER"
	OPERATOR   TokenType = "OPERATOR"
	STRING     TokenType = "STRING"
	CHAR       TokenType = "CHAR"
	BRACKET    TokenType = "BRACKET"
	SEMICOLON  TokenType = "SEMICOLON"
	COMMENT    TokenType = "COMMENT"
	UNKNOWN    TokenType = "UNKNOWN"
)

var keywords = map[string]bool{
	"int": true, "return": true, "if": true, "else": true,
	"while": true, "for": true, "char": true, "float": true,
	"double": true, "void": true, "struct": true,
}

func isOperator(ch rune) bool {
	return strings.ContainsRune("+-*/=<>!&|", ch)
}

func isBracket(ch rune) bool {
	return strings.ContainsRune("{}()[]", ch)
}

type Lexer struct {
	input    string
	position int
}

func NewLexer(input string) *Lexer {
	return &Lexer{input: input}
}

func (l *Lexer) nextChar() rune {
	if l.position >= len(l.input) {
		return 0
	}
	ch := rune(l.input[l.position])
	l.position++
	return ch
}

func (l *Lexer) peekChar() rune {
	if l.position >= len(l.input) {
		return 0
	}
	return rune(l.input[l.position])
}

func (l *Lexer) Lex() []Token {
	var tokens []Token
	for l.position < len(l.input) {
		ch := l.nextChar()

		if unicode.IsSpace(ch) {
			continue
		} else if ch == '/' && l.peekChar() == '/' {
			comment := "//"
			l.nextChar()
			for l.peekChar() != '\n' && l.peekChar() != 0 {
				comment += string(l.nextChar())
			}
			tokens = append(tokens, Token{COMMENT, comment})
		} else if ch == '/' && l.peekChar() == '*' {
			comment := "/*"
			l.nextChar()
			for !(l.peekChar() == '*' && l.input[l.position+1] == '/') && l.peekChar() != 0 {
				comment += string(l.nextChar())
			}
			comment += "*/"
			l.nextChar()
			l.nextChar()
			tokens = append(tokens, Token{COMMENT, comment})
		} else if ch == '"' {
			str := ""
			for l.peekChar() != '"' && l.peekChar() != 0 {
				str += string(l.nextChar())
			}
			l.nextChar()
			tokens = append(tokens, Token{STRING, str})
		} else if ch == '\'' {
			char := string(l.nextChar())
			l.nextChar()
			tokens = append(tokens, Token{CHAR, char})
		} else if unicode.IsLetter(ch) || ch == '_' {
			identifier := string(ch)
			for unicode.IsLetter(l.peekChar()) || unicode.IsDigit(l.peekChar()) || l.peekChar() == '_' {
				identifier += string(l.nextChar())
			}
			if keywords[identifier] {
				tokens = append(tokens, Token{KEYWORD, identifier})
			} else {
				tokens = append(tokens, Token{IDENTIFIER, identifier})
			}
		} else if unicode.IsDigit(ch) {
			number := string(ch)
			for unicode.IsDigit(l.peekChar()) {
				number += string(l.nextChar())
			}
			tokens = append(tokens, Token{NUMBER, number})
		} else if isOperator(ch) {
			tokens = append(tokens, Token{OPERATOR, string(ch)})
		} else if isBracket(ch) {
			tokens = append(tokens, Token{BRACKET, string(ch)})
		} else if ch == ';' {
			tokens = append(tokens, Token{SEMICOLON, string(ch)})
		} else {
			tokens = append(tokens, Token{UNKNOWN, string(ch)})
		}
	}
	return tokens
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run lexer.go <filename>")
		os.Exit(1)
	}

	filename := os.Args[1]
	file, err := os.Open(filename)
	if err != nil {
		fmt.Println("Error reading file:", err)
		os.Exit(1)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var content string
	for scanner.Scan() {
		content += scanner.Text() + "\n"
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error scanning file:", err)
		os.Exit(1)
	}

	lexer := NewLexer(content)
	tokens := lexer.Lex()
	for _, token := range tokens {
		fmt.Printf("Type: %-10s Value: %s\n", token.Type, token.Value)
	}
}
