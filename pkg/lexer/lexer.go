// Package lexer turns Go source text into a stream of tokens.
//
// It implements the same automatic-semicolon-insertion rules the Go spec
// describes: a semicolon is inserted at the end of a line when the last token
// on that line could end a statement (identifier, literal, one of the closing
// brackets, ++/--, or one of the keywords break/continue/fallthrough/return).
package lexer

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Type classifies a token.
type Type string

const (
	EOF     Type = "EOF"
	IDENT   Type = "IDENT"
	INT     Type = "INT"
	FLOAT   Type = "FLOAT"
	IMAG    Type = "IMAG"
	STRING  Type = "STRING"
	CHAR    Type = "CHAR"
	KEYWORD Type = "KEYWORD"
	OP      Type = "OP"
)

// Token is a single lexical unit.
type Token struct {
	Type    Type
	Literal string
	Line    int
	Col     int
}

func (t Token) String() string {
	if t.Type == EOF {
		return "EOF"
	}
	return fmt.Sprintf("%s(%q)", t.Type, t.Literal)
}

// Keywords recognised by Go. Predeclared identifiers such as int or nil are
// deliberately NOT keywords: they are ordinary identifiers.
var Keywords = map[string]bool{
	"break": true, "case": true, "chan": true, "const": true, "continue": true,
	"default": true, "defer": true, "else": true, "fallthrough": true, "for": true,
	"func": true, "go": true, "goto": true, "if": true, "import": true,
	"interface": true, "map": true, "package": true, "range": true, "return": true,
	"select": true, "struct": true, "switch": true, "type": true, "var": true,
}

// operators, longest first so that greedy matching works.
var operators = []string{
	"<<=", ">>=", "&^=", "...",
	"&&", "||", "<-", "++", "--", "==", "!=", "<=", ">=", ":=", "+=", "-=",
	"*=", "/=", "%=", "&=", "|=", "^=", "<<", ">>", "&^",
	"+", "-", "*", "/", "%", "&", "|", "^", "!", "=", "<", ">",
	"(", ")", "[", "]", "{", "}", ",", ";", ".", ":",
}

// Lexer scans a source string. It is single-use: call Scan once.
type Lexer struct {
	src    string
	pos    int
	line   int
	col    int
	tokens []Token
}

// New returns a Lexer for the given source.
func New(src string) *Lexer {
	return &Lexer{src: src, line: 1, col: 1}
}

// Scan returns every token, ending with a synthetic EOF token.
func (l *Lexer) Scan() ([]Token, error) {
	for {
		tok, err := l.next()
		if err != nil {
			return nil, err
		}
		l.tokens = append(l.tokens, tok)
		if tok.Type == EOF {
			return l.tokens, nil
		}
	}
}

func (l *Lexer) peek() rune {
	if l.pos >= len(l.src) {
		return 0
	}
	r, _ := utf8.DecodeRuneInString(l.src[l.pos:])
	return r
}

func (l *Lexer) peekAt(n int) rune {
	p := l.pos
	for i := 0; i < n; i++ {
		if p >= len(l.src) {
			return 0
		}
		_, w := utf8.DecodeRuneInString(l.src[p:])
		p += w
	}
	if p >= len(l.src) {
		return 0
	}
	r, _ := utf8.DecodeRuneInString(l.src[p:])
	return r
}

func (l *Lexer) advance() rune {
	r := l.peek()
	if r == 0 {
		return 0
	}
	_, w := utf8.DecodeRuneInString(l.src[l.pos:])
	l.pos += w
	if r == '\n' {
		l.line++
		l.col = 1
	} else {
		l.col++
	}
	return r
}

// asciiEndsStatement reports whether a token can legally be the final token of
// a statement, which is what triggers semicolon insertion.
func asciiEndsStatement(t Token) bool {
	switch t.Type {
	case IDENT, INT, FLOAT, IMAG, STRING, CHAR:
		return true
	case KEYWORD:
		switch t.Literal {
		case "break", "continue", "fallthrough", "return":
			return true
		}
		return false
	case OP:
		switch t.Literal {
		case ")", "]", "}", "++", "--":
			return true
		}
		return false
	}
	return false
}

func (l *Lexer) last() Token {
	if len(l.tokens) == 0 {
		return Token{Type: EOF}
	}
	return l.tokens[len(l.tokens)-1]
}

func (l *Lexer) errorf(format string, args ...interface{}) error {
	return fmt.Errorf("lexer: %d:%d: %s", l.line, l.col, fmt.Sprintf(format, args...))
}

// next scans a single token.
func (l *Lexer) next() (Token, error) {
	// Skip whitespace, handling newlines with semicolon insertion.
	for {
		r := l.peek()
		if r == 0 {
			// End of input: insert a trailing semicolon if needed.
			if asciiEndsStatement(l.last()) {
				l.tokens = append(l.tokens, Token{Type: OP, Literal: ";", Line: l.line, Col: l.col})
			}
			return Token{Type: EOF, Line: l.line, Col: l.col}, nil
		}
		if r == '\n' {
			line, col := l.line, l.col
			l.advance()
			if asciiEndsStatement(l.last()) {
				return Token{Type: OP, Literal: ";", Line: line, Col: col}, nil
			}
			continue
		}
		if r == ' ' || r == '\t' || r == '\r' {
			l.advance()
			continue
		}
		// Comments.
		if r == '/' && l.peekAt(1) == '/' {
			for l.peek() != '\n' && l.peek() != 0 {
				l.advance()
			}
			continue
		}
		if r == '/' && l.peekAt(1) == '*' {
			startLine, startCol := l.line, l.col
			l.advance() // '/'
			l.advance() // '*'
			closed := false
			for l.peek() != 0 {
				if l.peek() == '*' && l.peekAt(1) == '/' {
					l.advance()
					l.advance()
					closed = true
					break
				}
				l.advance()
			}
			if !closed {
				return Token{}, fmt.Errorf("lexer: %d:%d: unterminated block comment", startLine, startCol)
			}
			// A block comment containing a newline acts like a newline for ASI.
			continue
		}
		break
	}

	line, col := l.line, l.col
	r := l.peek()

	switch {
	case isLetter(r):
		return l.scanIdent(line, col), nil
	case unicode.IsDigit(r) || (r == '.' && unicode.IsDigit(l.peekAt(1))):
		return l.scanNumber(line, col)
	case r == '"':
		return l.scanString(line, col)
	case r == '`':
		return l.scanRawString(line, col)
	case r == '\'':
		return l.scanChar(line, col)
	default:
		return l.scanOperator(line, col)
	}
}

func isLetter(r rune) bool {
	return r == '_' || unicode.IsLetter(r)
}

func isDigit(r rune) bool {
	return unicode.IsDigit(r)
}

func (l *Lexer) scanIdent(line, col int) Token {
	text := ""
	for {
		r := l.peek()
		if isLetter(r) || isDigit(r) {
			text += string(l.advance())
			continue
		}
		break
	}
	word := text
	if Keywords[word] {
		return Token{Type: KEYWORD, Literal: word, Line: line, Col: col}
	}
	return Token{Type: IDENT, Literal: word, Line: line, Col: col}
}

func (l *Lexer) scanNumber(line, col int) (Token, error) {
	text := ""
	isFloat := false
	// Radix prefixes.
	if l.peek() == '0' {
		nxt := l.peekAt(1)
		if nxt == 'x' || nxt == 'X' || nxt == 'b' || nxt == 'B' || nxt == 'o' || nxt == 'O' {
			text += string(l.advance())
			text += string(l.advance())
			for isHexDigit(l.peek()) || l.peek() == '_' {
				text += string(l.advance())
			}
			return l.finishNumber(line, col, text, false)
		}
	}
	for unicode.IsDigit(l.peek()) || l.peek() == '_' {
		text += string(l.advance())
	}
	if l.peek() == '.' {
		isFloat = true
		text += string(l.advance())
		for unicode.IsDigit(l.peek()) || l.peek() == '_' {
			text += string(l.advance())
		}
	}
	if l.peek() == 'e' || l.peek() == 'E' {
		isFloat = true
		text += string(l.advance())
		if l.peek() == '+' || l.peek() == '-' {
			text += string(l.advance())
		}
		for unicode.IsDigit(l.peek()) {
			text += string(l.advance())
		}
	}
	return l.finishNumber(line, col, text, isFloat)
}

func (l *Lexer) finishNumber(line, col int, text string, isFloat bool) (Token, error) {
	// Imaginary literals end in 'i'.
	if l.peek() == 'i' {
		l.advance()
		return Token{Type: IMAG, Literal: text + "i", Line: line, Col: col}, nil
	}
	if isFloat {
		return Token{Type: FLOAT, Literal: text, Line: line, Col: col}, nil
	}
	return Token{Type: INT, Literal: text, Line: line, Col: col}, nil
}

func isHexDigit(r rune) bool {
	return unicode.IsDigit(r) || (r >= 'a' && r <= 'f') || (r >= 'A' && r <= 'F')
}

func (l *Lexer) scanString(line, col int) (Token, error) {
	text := ""
	text += string(l.advance()) // opening quote
	for {
		r := l.peek()
		if r == 0 || r == '\n' {
			return Token{}, fmt.Errorf("lexer: %d:%d: unterminated string literal", line, col)
		}
		if r == '\\' {
			text += string(l.advance())
			text += string(l.advance())
			continue
		}
		text += string(l.advance())
		if r == '"' {
			break
		}
	}
	return Token{Type: STRING, Literal: text, Line: line, Col: col}, nil
}

func (l *Lexer) scanRawString(line, col int) (Token, error) {
	text := ""
	text += string(l.advance()) // opening backtick
	for {
		r := l.peek()
		if r == 0 {
			return Token{}, fmt.Errorf("lexer: %d:%d: unterminated raw string literal", line, col)
		}
		text += string(l.advance())
		if r == '`' {
			break
		}
	}
	return Token{Type: STRING, Literal: text, Line: line, Col: col}, nil
}

func (l *Lexer) scanChar(line, col int) (Token, error) {
	text := ""
	text += string(l.advance()) // opening quote
	for {
		r := l.peek()
		if r == 0 || r == '\n' {
			return Token{}, fmt.Errorf("lexer: %d:%d: unterminated rune literal", line, col)
		}
		if r == '\\' {
			text += string(l.advance())
			text += string(l.advance())
			continue
		}
		text += string(l.advance())
		if r == '\'' {
			break
		}
	}
	return Token{Type: CHAR, Literal: text, Line: line, Col: col}, nil
}

func (l *Lexer) scanOperator(line, col int) (Token, error) {
	for _, op := range operators {
		if strings.HasPrefix(l.src[l.pos:], op) {
			for i := 0; i < len(op); i++ {
				l.advance()
			}
			return Token{Type: OP, Literal: op, Line: line, Col: col}, nil
		}
	}
	r := l.advance()
	return Token{}, fmt.Errorf("lexer: %d:%d: unexpected character %q", line, col, r)
}
