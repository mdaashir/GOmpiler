package parser

import (
	"fmt"

	"github.com/mdaashir/GOmpiler-2/pkg/ast"
	"github.com/mdaashir/GOmpiler-2/pkg/lexer"
)

// Parser represents a parser for C++ code
type Parser struct {
	tokens    []lexer.Token
	curToken  lexer.Token
	peekToken lexer.Token
	pos       int
	errors    []string
}

// New creates a new parser for the given tokens
func New(tokens []lexer.Token) *Parser {
	p := &Parser{
		tokens: tokens,
		pos:    0,
		errors: []string{},
	}

	// Set the current and peek tokens
	if len(tokens) > 0 {
		p.curToken = tokens[0]
	}
	if len(tokens) > 1 {
		p.peekToken = tokens[1]
	}

	return p
}

// Parse parses the tokens into an AST
func (p *Parser) Parse() (*ast.Program, error) {
	program := &ast.Program{
		Declarations: []ast.Declaration{},
	}

	// Parse declarations until end of file
	for p.curToken.Type != lexer.TokenEOF {
		if decl := p.parseDeclaration(); decl != nil {
			program.Declarations = append(program.Declarations, decl)
		}
		p.nextToken()
	}

	if len(p.errors) > 0 {
		return nil, fmt.Errorf("parsing errors: %v", p.errors)
	}

	return program, nil
}

// nextToken advances to the next token
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.pos++

	if p.pos < len(p.tokens) {
		p.peekToken = p.tokens[p.pos]
	} else {
		p.peekToken = lexer.Token{Type: lexer.TokenEOF}
	}
}

// addError adds an error message to the parser's error list
func (p *Parser) addError(msg string) {
	p.errors = append(p.errors, fmt.Sprintf("line %d: %s", p.curToken.Line, msg))
}

// parseDeclaration parses a declaration
func (p *Parser) parseDeclaration() ast.Declaration {
	switch p.curToken.Type {
	case lexer.TokenKeyword:
		switch p.curToken.Literal {
		case "class", "struct":
			return p.parseClassOrStructDeclaration()
		case "typedef":
			return p.parseTypedefDeclaration()
		case "template":
			return p.parseTemplateDeclaration()
		case "using":
			return p.parseUsingDeclaration()
		case "namespace":
			return p.parseNamespaceDeclaration()
		case "enum":
			return p.parseEnumDeclaration()
		default:
			return p.parseVariableOrFunctionDeclaration()
		}
	case lexer.TokenPreprocessor:
		return p.parsePreprocessorDirective()
	default:
		p.addError(fmt.Sprintf("unexpected token: %s", p.curToken.Literal))
		return nil
	}
}

// parseClassOrStructDeclaration parses a class or struct declaration
func (p *Parser) parseClassOrStructDeclaration() ast.Declaration {
	// For now, just return a simple class declaration as a placeholder
	return &ast.ClassDeclaration{
		Name:        "DummyClass",
		BaseClasses: []string{},
		Members:     []ast.Declaration{},
	}
}

// parseNamespaceDeclaration parses a namespace declaration
func (p *Parser) parseNamespaceDeclaration() ast.Declaration {
	// For now, just return a simple namespace declaration as a placeholder
	return &ast.NamespaceDeclaration{
		Name:         "DummyNamespace",
		Declarations: []ast.Declaration{},
	}
}

// parseEnumDeclaration parses an enum declaration
func (p *Parser) parseEnumDeclaration() ast.Declaration {
	// For now, just return a simple enum declaration as a placeholder
	return &ast.EnumDeclaration{
		Name:   "DummyEnum",
		Values: []string{"Value1", "Value2"},
	}
}

func (p *Parser) parseTypedefDeclaration() ast.Declaration {
	p.nextToken() // Skip 'typedef'

	// Skip typedef declaration for now
	for p.curToken.Type != lexer.TokenEOF &&
		!(p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";") {
		p.nextToken()
	}

	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
		p.nextToken() // Skip ';'
	}

	return &ast.TypedefDeclaration{
		OriginalType: "int",
		NewType:      "DummyType",
	}
}

func (p *Parser) parseTemplateDeclaration() ast.Declaration {
	p.nextToken() // Skip 'template'

	// Skip template parameters
	if p.curToken.Type == lexer.TokenOperator && p.curToken.Literal == "<" {
		p.nextToken() // Skip '<'

		// Skip to closing '>'
		angleCount := 1
		for angleCount > 0 && p.curToken.Type != lexer.TokenEOF {
			if p.curToken.Type == lexer.TokenOperator {
				if p.curToken.Literal == "<" {
					angleCount++
				} else if p.curToken.Literal == ">" {
					angleCount--
				}
			}
			p.nextToken()
		}
	}

	// Parse the templated declaration
	var decl ast.Declaration
	if p.curToken.Type == lexer.TokenKeyword {
		switch p.curToken.Literal {
		case "class", "struct":
			decl = p.parseClassOrStructDeclaration()
		default:
			decl = p.parseVariableOrFunctionDeclaration()
		}
	}

	return &ast.TemplateDeclaration{
		Declaration: decl,
	}
}

func (p *Parser) parseUsingDeclaration() ast.Declaration {
	p.nextToken() // Skip 'using'

	// Skip using declaration for now
	for p.curToken.Type != lexer.TokenEOF &&
		!(p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";") {
		p.nextToken()
	}

	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
		p.nextToken() // Skip ';'
	}

	return &ast.UsingDeclaration{
		Namespace: "std",
	}
}

func (p *Parser) parsePreprocessorDirective() ast.Declaration {
	directive := p.curToken.Literal
	p.nextToken() // Skip preprocessor directive

	return &ast.PreprocessorDirective{
		Directive: directive,
	}
}

func (p *Parser) parseVariableOrFunctionDeclaration() ast.Declaration {
	// For now, just return a simple variable declaration
	return &ast.VariableDeclaration{
		Type:        "int",
		Name:        "dummyVar",
		ArraySize:   "",
		Initializer: nil,
	}
}

func (p *Parser) parseType() string {
	var _ string

	// For now, return a simple type
	return "int"
}

// Stub for other parser functions...
// These would normally be filled in with full implementations
func (p *Parser) parseFunction(typeSpec, name string) ast.Declaration {
	return &ast.FunctionDeclaration{
		Type:       typeSpec,
		Name:       name,
		Parameters: []ast.Parameter{},
		Body:       nil,
	}
}

func (p *Parser) parseParameters() []ast.Parameter {
	return []ast.Parameter{}
}

func (p *Parser) parseVariable(typeSpec, name string) ast.Declaration {
	return &ast.VariableDeclaration{
		Type:        typeSpec,
		Name:        name,
		ArraySize:   "",
		Initializer: nil,
	}
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	return &ast.BlockStatement{
		Statements: []ast.Statement{},
	}
}

func (p *Parser) parseStatement() ast.Statement {
	return &ast.EmptyStatement{}
}

func (p *Parser) parseDeclarationStatement() ast.Statement {
	return &ast.DeclarationStatement{
		Declaration: &ast.VariableDeclaration{
			Type:        "int",
			Name:        "dummyVar",
			ArraySize:   "",
			Initializer: nil,
		},
	}
}

func (p *Parser) parseExpressionStatement() ast.Statement {
	return &ast.ExpressionStatement{
		Expression: &ast.NullExpression{},
	}
}

func (p *Parser) parseIfStatement() ast.Statement {
	return &ast.IfStatement{
		Condition:   &ast.BooleanLiteral{Value: true},
		Consequence: &ast.BlockStatement{Statements: []ast.Statement{}},
		Alternative: nil,
	}
}

func (p *Parser) parseForStatement() ast.Statement {
	return &ast.ForStatement{
		Body: &ast.BlockStatement{Statements: []ast.Statement{}},
	}
}

func (p *Parser) parseWhileStatement() ast.Statement {
	return &ast.WhileStatement{
		Body: &ast.BlockStatement{Statements: []ast.Statement{}},
	}
}

func (p *Parser) parseDoWhileStatement() ast.Statement {
	return &ast.DoWhileStatement{
		Body: &ast.BlockStatement{Statements: []ast.Statement{}},
	}
}

func (p *Parser) parseSwitchStatement() ast.Statement {
	return &ast.SwitchStatement{
		Body: &ast.BlockStatement{Statements: []ast.Statement{}},
	}
}

func (p *Parser) parseReturnStatement() ast.Statement {
	return &ast.ReturnStatement{
		Value: nil,
	}
}

func (p *Parser) parseBreakStatement() ast.Statement {
	return &ast.BreakStatement{}
}

func (p *Parser) parseContinueStatement() ast.Statement {
	return &ast.ContinueStatement{}
}

func (p *Parser) parseGotoStatement() ast.Statement {
	return &ast.GotoStatement{
		Label: "dummyLabel",
	}
}

func (p *Parser) parseExpression() ast.Expression {
	return &ast.NullExpression{}
}

func (p *Parser) parseAssignmentExpression() ast.Expression {
	return &ast.NullExpression{}
}

func (p *Parser) parsePrimaryExpression() ast.Expression {
	return &ast.NullExpression{}
}
