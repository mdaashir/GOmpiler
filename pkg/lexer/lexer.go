package lexer

import (
	"fmt"
	"unicode"
	"unicode/utf8"
)

// TokenType represents the type of token
type TokenType int

// Token types
const (
	TokenEOF TokenType = iota
	TokenIdentifier
	TokenNumber
	TokenString
	TokenChar
	TokenOperator
	TokenPunctuation
	TokenKeyword
	TokenComment
	TokenPreprocessor
	TokenWhitespace
	TokenError
)

// Token represents a lexical token
type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Column  int
	File    string
}

// Lexer performs lexical analysis on C++ source code
type Lexer struct {
	input   string
	pos     int
	readPos int
	ch      rune
	line    int
	column  int
	file    string
}

// List of C++ keywords
var keywords = map[string]bool{
	"auto":             true,
	"break":            true,
	"case":             true,
	"class":            true,
	"const":            true,
	"continue":         true,
	"default":          true,
	"delete":           true,
	"do":               true,
	"else":             true,
	"enum":             true,
	"extern":           true,
	"for":              true,
	"if":               true,
	"inline":           true,
	"namespace":        true,
	"new":              true,
	"operator":         true,
	"private":          true,
	"protected":        true,
	"public":           true,
	"return":           true,
	"static":           true,
	"struct":           true,
	"switch":           true,
	"template":         true,
	"this":             true,
	"try":              true,
	"typedef":          true,
	"using":            true,
	"virtual":          true,
	"void":             true,
	"while":            true,
	"int":              true,
	"float":            true,
	"double":           true,
	"char":             true,
	"bool":             true,
	"unsigned":         true,
	"signed":           true,
	"short":            true,
	"long":             true,
	"const_cast":       true,
	"dynamic_cast":     true,
	"reinterpret_cast": true,
	"static_cast":      true,
	"sizeof":           true,
	"throw":            true,
	"catch":            true,
	"true":             true,
	"false":            true,
}

// New creates a new lexer for the given input
func New(input, file string) *Lexer {
	l := &Lexer{
		input:  input,
		file:   file,
		line:   1,
		column: 1,
	}
	l.readRune()
	return l
}

// readRune reads the next rune from the input
func (l *Lexer) readRune() {
	if l.readPos >= len(l.input) {
		l.ch = 0 // EOF
	} else {
		var size int
		l.ch, size = utf8.DecodeRuneInString(l.input[l.readPos:])
		l.pos = l.readPos
		l.readPos += size
	}
}

// peekRune returns the next rune without advancing the position
func (l *Lexer) peekRune() rune {
	if l.readPos >= len(l.input) {
		return 0 // EOF
	}
	r, _ := utf8.DecodeRuneInString(l.input[l.readPos:])
	return r
}

// Tokenize breaks the input into tokens
func (l *Lexer) Tokenize() ([]Token, error) {
	var tokens []Token
	for {
		token := l.NextToken()
		if token.Type == TokenError {
			return nil, fmt.Errorf("lexical error at %s:%d:%d: %s", token.File, token.Line, token.Column, token.Literal)
		}
		if token.Type != TokenWhitespace && token.Type != TokenComment {
			tokens = append(tokens, token)
		}
		if token.Type == TokenEOF {
			break
		}
	}
	return tokens, nil
}

// NextToken returns the next token in the input
func (l *Lexer) NextToken() Token {
	var tok Token

	// Skip whitespace
	l.skipWhitespace()

	tok.Line = l.line
	tok.Column = l.column
	tok.File = l.file

	switch l.ch {
	case 0: // EOF
		tok.Type = TokenEOF
		tok.Literal = ""
	case '/':
		if l.peekRune() == '/' {
			tok.Type = TokenComment
			tok.Literal = l.readLineComment()
		} else if l.peekRune() == '*' {
			tok.Type = TokenComment
			tok.Literal = l.readBlockComment()
		} else {
			tok.Type = TokenOperator
			tok.Literal = string(l.ch)
			l.readRune()
		}
	case '#':
		tok.Type = TokenPreprocessor
		tok.Literal = l.readPreprocessor()
	case '"':
		tok.Type = TokenString
		tok.Literal = l.readString()
	case '\'':
		tok.Type = TokenChar
		tok.Literal = l.readChar()
	case '{', '}', '(', ')', '[', ']', ';', ',', '.', ':', '?':
		tok.Type = TokenPunctuation
		tok.Literal = string(l.ch)
		l.readRune()
	case '+', '-', '*', '=', '!', '<', '>', '&', '|', '%', '^', '~':
		tok.Type = TokenOperator
		tok.Literal = l.readOperator()
	default:
		if isLetter(l.ch) || l.ch == '_' {
			tok.Literal = l.readIdentifier()
			if keywords[tok.Literal] {
				tok.Type = TokenKeyword
			} else {
				tok.Type = TokenIdentifier
			}
		} else if isDigit(l.ch) {
			tok.Type = TokenNumber
			tok.Literal = l.readNumber()
		} else {
			tok.Type = TokenError
			tok.Literal = fmt.Sprintf("unexpected character: %q", l.ch)
			l.readRune()
		}
	}

	return tok
}

// Skip whitespace characters
func (l *Lexer) skipWhitespace() {
	for unicode.IsSpace(l.ch) {
		if l.ch == '\n' {
			l.line++
			l.column = 1
		} else {
			l.column++
		}
		l.readRune()
	}
}

// Read an identifier (variable name, function name, etc.)
func (l *Lexer) readIdentifier() string {
	startPos := l.pos
	for isLetter(l.ch) || isDigit(l.ch) || l.ch == '_' {
		l.column++
		l.readRune()
	}
	return l.input[startPos:l.pos]
}

// Read a number (integer or floating-point)
func (l *Lexer) readNumber() string {
	startPos := l.pos
	for isDigit(l.ch) || l.ch == '.' || isHexDigit(l.ch) || l.ch == 'x' || l.ch == 'X' {
		l.column++
		l.readRune()
	}
	return l.input[startPos:l.pos]
}

// Read a string literal
func (l *Lexer) readString() string {
	l.column++ // Skip the opening quote
	l.readRune()
	startPos := l.pos
	for l.ch != '"' && l.ch != 0 {
		if l.ch == '\\' {
			l.column++
			l.readRune() // Skip the escape character
		}
		if l.ch == '\n' {
			l.line++
			l.column = 1
		} else {
			l.column++
		}
		l.readRune()
	}
	str := l.input[startPos:l.pos]
	if l.ch == '"' {
		l.column++
		l.readRune() // Skip the closing quote
	}
	return "\"" + str + "\""
}

// Read a character literal
func (l *Lexer) readChar() string {
	l.column++ // Skip the opening quote
	l.readRune()
	startPos := l.pos
	for l.ch != '\'' && l.ch != 0 {
		if l.ch == '\\' {
			l.column++
			l.readRune() // Skip the escape character
		}
		if l.ch == '\n' {
			l.line++
			l.column = 1
		} else {
			l.column++
		}
		l.readRune()
	}
	str := l.input[startPos:l.pos]
	if l.ch == '\'' {
		l.column++
		l.readRune() // Skip the closing quote
	}
	return "'" + str + "'"
}

// Read an operator
func (l *Lexer) readOperator() string {
	startPos := l.pos
	l.column++
	l.readRune()

	// Handle multi-character operators
	if l.pos > 0 {
		prev := l.input[startPos]
		switch prev {
		case '+':
			if l.ch == '+' || l.ch == '=' {
				l.column++
				l.readRune()
			}
		case '-':
			if l.ch == '-' || l.ch == '=' || l.ch == '>' {
				l.column++
				l.readRune()
			}
		case '*':
			if l.ch == '=' {
				l.column++
				l.readRune()
			}
		case '/':
			if l.ch == '=' {
				l.column++
				l.readRune()
			}
		case '=':
			if l.ch == '=' {
				l.column++
				l.readRune()
			}
		case '!':
			if l.ch == '=' {
				l.column++
				l.readRune()
			}
		case '<':
			if l.ch == '=' {
				// <= operator
				l.column++
				l.readRune()
			} else if l.ch == '<' {
				// << operator (stream insertion)
				l.column++
				l.readRune()
				// Check for <<= operator
				if l.ch == '=' {
					l.column++
					l.readRune()
				}
			}
		case '>':
			if l.ch == '=' {
				// >= operator
				l.column++
				l.readRune()
			} else if l.ch == '>' {
				// >> operator (stream extraction)
				l.column++
				l.readRune()
				// Check for >>= operator
				if l.ch == '=' {
					l.column++
					l.readRune()
				}
			}
		case '&':
			if l.ch == '&' || l.ch == '=' {
				l.column++
				l.readRune()
			}
		case '|':
			if l.ch == '|' || l.ch == '=' {
				l.column++
				l.readRune()
			}
		case '%':
			if l.ch == '=' {
				l.column++
				l.readRune()
			}
		case '^':
			if l.ch == '=' {
				l.column++
				l.readRune()
			}
		case ':':
			// Handle scope resolution operator ::
			if l.ch == ':' {
				l.column++
				l.readRune()
			}
		}
	}

	return l.input[startPos:l.pos]
}

// Read a line comment
func (l *Lexer) readLineComment() string {
	startPos := l.pos
	l.column++ // Skip the first '/'
	l.readRune()
	l.column++ // Skip the second '/'
	l.readRune()

	for l.ch != '\n' && l.ch != 0 {
		l.column++
		l.readRune()
	}

	return l.input[startPos:l.pos]
}

// Read a block comment
func (l *Lexer) readBlockComment() string {
	startPos := l.pos
	l.column++ // Skip the '/'
	l.readRune()
	l.column++ // Skip the '*'
	l.readRune()

	for !(l.ch == '*' && l.peekRune() == '/') && l.ch != 0 {
		if l.ch == '\n' {
			l.line++
			l.column = 1
		} else {
			l.column++
		}
		l.readRune()
	}

	if l.ch != 0 { // If not EOF
		l.column++ // Skip the '*'
		l.readRune()
		l.column++ // Skip the '/'
		l.readRune()
	}

	return l.input[startPos:l.pos]
}

// Read a preprocessor directive
func (l *Lexer) readPreprocessor() string {
	startPos := l.pos
	l.column++ // Skip the '#'
	l.readRune()

	// Skip initial whitespace
	for unicode.IsSpace(l.ch) && l.ch != '\n' {
		l.column++
		l.readRune()
	}

	// Read directive name
	for isLetter(l.ch) {
		l.column++
		l.readRune()
	}

	// Read until the end of the line
	for l.ch != '\n' && l.ch != 0 {
		// Handle line continuation
		if l.ch == '\\' && l.peekRune() == '\n' {
			l.column++
			l.readRune() // Skip the '\'
			l.line++
			l.column = 1
			l.readRune() // Skip the newline
		} else {
			if l.ch == '\n' {
				l.line++
				l.column = 1
			} else {
				l.column++
			}
			l.readRune()
		}
	}

	return l.input[startPos:l.pos]
}

// Helper functions for character classification
func isLetter(ch rune) bool {
	return unicode.IsLetter(ch)
}

func isDigit(ch rune) bool {
	return unicode.IsDigit(ch)
}

func isHexDigit(ch rune) bool {
	return isDigit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')
}
