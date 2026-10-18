package parser

import (
	"fmt"
	"strings"

	"github.com/mdaashir/GOmpiler-2/pkg/ast"
	"github.com/mdaashir/GOmpiler-2/pkg/lexer"
)

// Operator precedence levels, lowest to highest.
const (
	precLowest = iota
	precAssign
	precOr
	precAnd
	precEquality
	precRelational
	precShift
	precAdditive
	precMultiplicative
)

// typeKeywords lists the keywords that may begin a type specifier.
var typeKeywords = map[string]bool{
	"void": true, "char": true, "short": true, "int": true, "long": true,
	"unsigned": true, "signed": true, "float": true, "double": true,
	"bool": true, "auto": true, "const": true, "static": true,
	"extern": true, "inline": true,
}

// binaryPrecedence maps binary operator literals to their precedence.
var binaryPrecedence = map[string]int{
	"=": precAssign, "+=": precAssign, "-=": precAssign,
	"*=": precAssign, "/=": precAssign, "%=": precAssign,
	"||": precOr,
	"&&": precAnd,
	"==": precEquality, "!=": precEquality,
	"<": precRelational, ">": precRelational,
	"<=": precRelational, ">=": precRelational,
	"<<": precShift, ">>": precShift,
	"+": precAdditive, "-": precAdditive,
	"*": precMultiplicative, "/": precMultiplicative, "%": precMultiplicative,
}

// assignmentOperators are right-associative.
var assignmentOperators = map[string]bool{
	"=": true, "+=": true, "-=": true, "*=": true, "/=": true, "%=": true,
}

// Parser turns a token stream into an AST.
type Parser struct {
	tokens    []lexer.Token
	curToken  lexer.Token
	peekToken lexer.Token
	pos       int
	errors    []string
	fatal     bool
}

// New creates a new parser for the given tokens.
func New(tokens []lexer.Token) *Parser {
	p := &Parser{
		tokens: tokens,
		errors: []string{},
	}
	if len(tokens) > 0 {
		p.curToken = tokens[0]
	} else {
		p.curToken = lexer.Token{Type: lexer.TokenEOF}
	}
	if len(tokens) > 1 {
		p.peekToken = tokens[1]
	} else {
		p.peekToken = lexer.Token{Type: lexer.TokenEOF}
	}
	return p
}

// nextToken advances the parser to the next token.
func (p *Parser) nextToken() {
	p.pos++
	if p.pos < len(p.tokens) {
		p.curToken = p.tokens[p.pos]
	} else {
		p.curToken = lexer.Token{Type: lexer.TokenEOF}
	}
	if p.pos+1 < len(p.tokens) {
		p.peekToken = p.tokens[p.pos+1]
	} else {
		p.peekToken = lexer.Token{Type: lexer.TokenEOF}
	}
}

func (p *Parser) addError(msg string) {
	p.errors = append(p.errors, fmt.Sprintf("line %d: %s", p.curToken.Line, msg))
}

func (p *Parser) curIsPunct(lit string) bool {
	return p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == lit
}

func (p *Parser) curIsOp(lit string) bool {
	return p.curToken.Type == lexer.TokenOperator && p.curToken.Literal == lit
}

func (p *Parser) curIsKw(lit string) bool {
	return p.curToken.Type == lexer.TokenKeyword && p.curToken.Literal == lit
}

func (p *Parser) expectPunct(lit string) bool {
	if p.curIsPunct(lit) {
		p.nextToken()
		return true
	}
	p.addError(fmt.Sprintf("expected %q, got %q", lit, p.curToken.Literal))
	return false
}

func (p *Parser) consumeOptionalSemicolon() {
	if p.curIsPunct(";") {
		p.nextToken()
	}
}

func (p *Parser) recoverToSemicolon() {
	for p.curToken.Type != lexer.TokenEOF && !p.curIsPunct(";") {
		p.nextToken()
	}
	p.consumeOptionalSemicolon()
}

// Parse parses the token stream into an AST.
func (p *Parser) Parse() (*ast.Program, error) {
	program := &ast.Program{Declarations: []ast.Declaration{}}

	for p.curToken.Type != lexer.TokenEOF && !p.fatal {
		before := p.pos
		if decl := p.parseTopLevel(); decl != nil {
			program.Declarations = append(program.Declarations, decl)
		}
		if p.pos == before {
			p.nextToken()
		}
	}

	if len(p.errors) > 0 {
		return nil, fmt.Errorf("parsing errors: %v", p.errors)
	}
	return program, nil
}

func (p *Parser) parseTopLevel() ast.Declaration {
	switch {
	case p.curToken.Type == lexer.TokenPreprocessor:
		directive := p.curToken.Literal
		p.nextToken()
		return &ast.PreprocessorDirective{Directive: directive}
	case p.curIsKw("using"):
		for p.curToken.Type != lexer.TokenEOF && !p.curIsPunct(";") {
			p.nextToken()
		}
		p.consumeOptionalSemicolon()
		return &ast.UsingDeclaration{Namespace: "std"}
	case p.curIsKw("namespace"):
		p.addError("namespaces are not supported")
		p.fatal = true
		return nil
	case p.curIsKw("class"), p.curIsKw("struct"):
		p.addError("classes and structs are not supported")
		p.fatal = true
		return nil
	case p.curIsKw("enum"):
		p.addError("enums are not supported")
		p.fatal = true
		return nil
	case p.curIsKw("template"):
		p.addError("templates are not supported")
		p.fatal = true
		return nil
	case p.curIsKw("typedef"):
		p.addError("typedef is not supported")
		p.fatal = true
		return nil
	case p.isDeclarationStart():
		return p.parseDeclaration()
	case p.curToken.Type == lexer.TokenIdentifier:
		return p.parseDeclaration()
	default:
		p.addError(fmt.Sprintf("unexpected token at global scope: %q", p.curToken.Literal))
		p.nextToken()
		return nil
	}
}

func (p *Parser) isDeclarationStart() bool {
	if p.curToken.Type == lexer.TokenKeyword && typeKeywords[p.curToken.Literal] {
		return true
	}
	// A user-defined type name: `Widget w` / `Foo x = ...`.
	return p.curToken.Type == lexer.TokenIdentifier && p.peekToken.Type == lexer.TokenIdentifier
}

func (p *Parser) parseTypeSpec() string {
	parts := []string{}
	for p.curToken.Type == lexer.TokenKeyword && typeKeywords[p.curToken.Literal] {
		parts = append(parts, p.curToken.Literal)
		p.nextToken()
	}
	// A user-defined type name, e.g. `Widget w`.
	if len(parts) == 0 && p.curToken.Type == lexer.TokenIdentifier && p.peekToken.Type == lexer.TokenIdentifier {
		parts = append(parts, p.curToken.Literal)
		p.nextToken()
	}
	for p.curToken.Type == lexer.TokenOperator && (p.curToken.Literal == "*" || p.curToken.Literal == "&") {
		parts = append(parts, p.curToken.Literal)
		p.nextToken()
	}
	return strings.Join(parts, " ")
}

func (p *Parser) parseDeclaration() ast.Declaration {
	typeSpec := p.parseTypeSpec()

	if p.curToken.Type != lexer.TokenIdentifier {
		p.addError(fmt.Sprintf("expected identifier at global scope, got %q", p.curToken.Literal))
		p.recoverToSemicolon()
		return nil
	}
	name := p.curToken.Literal
	p.nextToken()

	if p.curIsPunct("(") {
		return p.parseFunction(typeSpec, name)
	}
	return p.parseVariableDeclaration(typeSpec, name)
}

func (p *Parser) parseFunction(returnType, name string) ast.Declaration {
	p.nextToken() // consume '('

	params := []ast.Parameter{}
	if !p.curIsPunct(")") {
		for {
			paramType := p.parseTypeSpec()
			paramName := ""
			if p.curToken.Type == lexer.TokenIdentifier {
				paramName = p.curToken.Literal
				p.nextToken()
			}
			if paramType == "void" && paramName == "" {
				// `f(void)` style empty parameter list
				break
			}
			if paramType == "" {
				p.addError(fmt.Sprintf("expected parameter type in %q", name))
				break
			}
			params = append(params, ast.Parameter{Type: paramType, Name: paramName})
			if p.curIsPunct(",") {
				p.nextToken()
				continue
			}
			break
		}
	}
	p.expectPunct(")")

	switch {
	case p.curIsPunct(";"):
		p.nextToken()
		return &ast.FunctionDeclaration{Type: returnType, Name: name, Parameters: params}
	case p.curIsPunct("{"):
		body := p.parseBlockStatement()
		return &ast.FunctionDeclaration{Type: returnType, Name: name, Parameters: params, Body: body}
	default:
		p.addError(fmt.Sprintf("expected '{' or ';' after function %q, got %q", name, p.curToken.Literal))
		p.recoverToSemicolon()
		return &ast.FunctionDeclaration{Type: returnType, Name: name, Parameters: params}
	}
}

func (p *Parser) parseVariableDeclaration(typeSpec, name string) ast.Declaration {
	var initializer ast.Expression
	if p.curIsOp("=") {
		p.nextToken()
		initializer = p.parseExpression(precLowest)
	}
	if !p.curIsPunct(";") {
		p.addError(fmt.Sprintf("expected ';' after declaration of %q, got %q", name, p.curToken.Literal))
		p.recoverToSemicolon()
	} else {
		p.nextToken()
	}
	return &ast.VariableDeclaration{Type: typeSpec, Name: name, Initializer: initializer}
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Statements: []ast.Statement{}}
	if !p.expectPunct("{") {
		return block
	}
	for p.curToken.Type != lexer.TokenEOF && !p.curIsPunct("}") {
		before := p.pos
		if stmt := p.parseStatement(); stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		if p.pos == before {
			p.nextToken()
		}
	}
	p.expectPunct("}")
	return block
}

func (p *Parser) parseStatement() ast.Statement {
	switch {
	case p.curIsPunct("{"):
		return p.parseBlockStatement()
	case p.curIsPunct(";"):
		p.nextToken()
		return &ast.EmptyStatement{}
	case p.curIsKw("if"):
		return p.parseIfStatement()
	case p.curIsKw("while"):
		return p.parseWhileStatement()
	case p.curIsKw("do"):
		return p.parseDoWhileStatement()
	case p.curIsKw("for"):
		return p.parseForStatement()
	case p.curIsKw("switch"):
		return p.parseSwitchStatement()
	case p.curIsKw("return"):
		return p.parseReturnStatement()
	case p.curIsKw("break"):
		p.nextToken()
		p.consumeOptionalSemicolon()
		return &ast.BreakStatement{}
	case p.curIsKw("continue"):
		p.nextToken()
		p.consumeOptionalSemicolon()
		return &ast.ContinueStatement{}
	case p.isDeclarationStart():
		if decl := p.parseLocalDeclaration(); decl != nil {
			return &ast.DeclarationStatement{Declaration: decl}
		}
		return &ast.EmptyStatement{}
	default:
		expr := p.parseExpression(precLowest)
		if !p.curIsPunct(";") {
			p.addError(fmt.Sprintf("expected ';' after expression, got %q", p.curToken.Literal))
			p.recoverToSemicolon()
		} else {
			p.nextToken()
		}
		return &ast.ExpressionStatement{Expression: expr}
	}
}

func (p *Parser) parseLocalDeclaration() ast.Declaration {
	typeSpec := p.parseTypeSpec()
	if p.curToken.Type != lexer.TokenIdentifier {
		p.addError(fmt.Sprintf("expected identifier in declaration, got %q", p.curToken.Literal))
		p.recoverToSemicolon()
		return nil
	}
	name := p.curToken.Literal
	p.nextToken()
	return p.parseVariableDeclaration(typeSpec, name)
}

func (p *Parser) parseParenExpression() ast.Expression {
	if !p.expectPunct("(") {
		return &ast.NullExpression{}
	}
	expr := p.parseExpression(precLowest)
	p.expectPunct(")")
	return expr
}

func (p *Parser) parseIfStatement() ast.Statement {
	p.nextToken() // 'if'
	condition := p.parseParenExpression()
	consequence := p.parseStatement()

	var alternative ast.Statement
	if p.curIsKw("else") {
		p.nextToken()
		alternative = p.parseStatement()
	}
	return &ast.IfStatement{Condition: condition, Consequence: consequence, Alternative: alternative}
}

func (p *Parser) parseWhileStatement() ast.Statement {
	p.nextToken() // 'while'
	condition := p.parseParenExpression()
	body := p.parseStatement()
	return &ast.WhileStatement{Condition: condition, Body: body}
}

func (p *Parser) parseDoWhileStatement() ast.Statement {
	p.nextToken() // 'do'
	body := p.parseStatement()
	if !p.curIsKw("while") {
		p.addError("expected 'while' after do-block")
	} else {
		p.nextToken()
	}
	condition := p.parseParenExpression()
	p.consumeOptionalSemicolon()
	return &ast.DoWhileStatement{Body: body, Condition: condition}
}

func (p *Parser) parseForStatement() ast.Statement {
	p.nextToken() // 'for'
	if !p.expectPunct("(") {
		return &ast.EmptyStatement{}
	}

	var initialization ast.Statement
	if !p.curIsPunct(";") {
		if p.isDeclarationStart() {
			if decl := p.parseLocalDeclaration(); decl != nil {
				initialization = &ast.DeclarationStatement{Declaration: decl}
			}
		} else {
			expr := p.parseExpression(precLowest)
			p.consumeOptionalSemicolon()
			initialization = &ast.ExpressionStatement{Expression: expr}
		}
	} else {
		p.nextToken()
	}

	var condition ast.Expression
	if !p.curIsPunct(";") {
		condition = p.parseExpression(precLowest)
	}
	p.expectPunct(";")

	var increment ast.Expression
	if !p.curIsPunct(")") {
		increment = p.parseExpression(precLowest)
	}
	p.expectPunct(")")

	body := p.parseStatement()
	return &ast.ForStatement{
		Initialization: initialization,
		Condition:      condition,
		Increment:      increment,
		Body:           body,
	}
}

// parseSwitchStatement lowers a switch into an equivalent if/else-if chain.
// Fallthrough is not supported; each case ends at the next case or at `break`.
func (p *Parser) parseSwitchStatement() ast.Statement {
	p.nextToken() // 'switch'
	subject := p.parseParenExpression()
	if !p.expectPunct("{") {
		return &ast.EmptyStatement{}
	}

	type clause struct {
		value ast.Expression // nil for default
		body  []ast.Statement
	}
	clauses := []clause{}
	curIdx := -1

	for p.curToken.Type != lexer.TokenEOF && !p.curIsPunct("}") {
		switch {
		case p.curIsKw("case"):
			p.nextToken()
			value := p.parseExpression(precLowest)
			p.expectPunct(":")
			clauses = append(clauses, clause{value: value})
			curIdx = len(clauses) - 1
		case p.curIsKw("default"):
			p.nextToken()
			p.expectPunct(":")
			clauses = append(clauses, clause{})
			curIdx = len(clauses) - 1
		case p.curIsKw("break"):
			p.nextToken()
			p.consumeOptionalSemicolon()
			curIdx = -1
		default:
			stmt := p.parseStatement()
			if curIdx < 0 {
				p.addError("statement in switch outside of a case label")
			} else if stmt != nil {
				clauses[curIdx].body = append(clauses[curIdx].body, stmt)
			}
		}
	}
	p.expectPunct("}")

	var defaultBody []ast.Statement
	type branch struct {
		value ast.Expression
		body  []ast.Statement
	}
	branches := []branch{}
	for _, c := range clauses {
		if c.value == nil {
			defaultBody = c.body
			continue
		}
		branches = append(branches, branch{value: c.value, body: c.body})
	}

	var node ast.Statement
	if len(defaultBody) > 0 {
		node = &ast.BlockStatement{Statements: defaultBody}
	}
	for i := len(branches) - 1; i >= 0; i-- {
		condition := &ast.BinaryExpression{
			Operator: "==",
			Left:     subject,
			Right:    branches[i].value,
		}
		node = &ast.IfStatement{
			Condition:   condition,
			Consequence: &ast.BlockStatement{Statements: branches[i].body},
			Alternative: node,
		}
	}
	if node == nil {
		return &ast.EmptyStatement{}
	}
	return node
}

func (p *Parser) parseReturnStatement() ast.Statement {
	p.nextToken() // 'return'
	if p.curIsPunct(";") {
		p.nextToken()
		return &ast.ReturnStatement{}
	}
	value := p.parseExpression(precLowest)
	if !p.curIsPunct(";") {
		p.addError("expected ';' after return statement")
		p.recoverToSemicolon()
	} else {
		p.nextToken()
	}
	return &ast.ReturnStatement{Value: value}
}

// parseExpression parses an expression using precedence climbing.
func (p *Parser) parseExpression(minPrec int) ast.Expression {
	left := p.parseUnary()

	for {
		if p.curToken.Type != lexer.TokenOperator {
			break
		}
		op := p.curToken.Literal
		prec, ok := binaryPrecedence[op]
		if !ok || prec < minPrec {
			break
		}

		opToken := p.curToken
		p.nextToken()

		nextMin := prec + 1
		if assignmentOperators[op] {
			nextMin = prec // right-associative
		}
		right := p.parseExpression(nextMin)

		if assignmentOperators[op] {
			left = &ast.AssignmentExpression{Left: left, Operator: op, Right: right}
		} else {
			left = &ast.BinaryExpression{Token: opToken, Operator: op, Left: left, Right: right}
		}
	}

	return left
}

func (p *Parser) parseUnary() ast.Expression {
	if p.curToken.Type == lexer.TokenOperator {
		switch p.curToken.Literal {
		case "-", "+", "!", "~", "*", "&", "++", "--":
			op := p.curToken.Literal
			p.nextToken()
			return &ast.UnaryExpression{Operator: op, Right: p.parseUnary()}
		}
	}
	return p.parsePostfix()
}

func (p *Parser) parsePostfix() ast.Expression {
	expr := p.parsePrimary()

	for {
		switch {
		case p.curIsPunct("("):
			expr = p.parseCallExpression(expr)
		case p.curIsPunct("["):
			p.nextToken()
			index := p.parseExpression(precLowest)
			p.expectPunct("]")
			expr = &ast.IndexExpression{Array: expr, Index: index}
		case p.curIsPunct("."):
			p.nextToken()
			if p.curToken.Type != lexer.TokenIdentifier {
				p.addError(fmt.Sprintf("expected identifier after '.', got %q", p.curToken.Literal))
				return expr
			}
			property := &ast.Identifier{Value: p.curToken.Literal}
			p.nextToken()
			expr = &ast.MemberExpression{Object: expr, Property: property}
		case p.curIsOp("->"):
			p.nextToken()
			if p.curToken.Type != lexer.TokenIdentifier {
				p.addError(fmt.Sprintf("expected identifier after '->', got %q", p.curToken.Literal))
				return expr
			}
			property := &ast.Identifier{Value: p.curToken.Literal}
			p.nextToken()
			expr = &ast.MemberExpression{Object: expr, Property: property}
		case p.curIsOp("++"), p.curIsOp("--"):
			op := p.curToken.Literal
			p.nextToken()
			expr = &ast.UnaryExpression{Operator: op, Right: expr}
		default:
			return expr
		}
	}
}

func (p *Parser) parsePrimary() ast.Expression {
	switch p.curToken.Type {
	case lexer.TokenNumber:
		value := &ast.NumberLiteral{Value: p.curToken.Literal}
		p.nextToken()
		return value
	case lexer.TokenString:
		value := &ast.StringLiteral{Value: p.curToken.Literal}
		p.nextToken()
		return value
	case lexer.TokenChar:
		value := &ast.CharLiteral{Value: p.curToken.Literal}
		p.nextToken()
		return value
	case lexer.TokenKeyword:
		if p.curToken.Literal == "true" || p.curToken.Literal == "false" {
			value := p.curToken.Literal == "true"
			p.nextToken()
			return &ast.BooleanLiteral{Value: value}
		}
		p.addError(fmt.Sprintf("unexpected keyword in expression: %q", p.curToken.Literal))
		p.nextToken()
		return &ast.NullExpression{}
	case lexer.TokenIdentifier:
		return &ast.Identifier{Value: p.parseQualifiedName()}
	case lexer.TokenPunctuation:
		if p.curToken.Literal == "(" {
			p.nextToken()
			expr := p.parseExpression(precLowest)
			p.expectPunct(")")
			return expr
		}
	}

	p.addError(fmt.Sprintf("unexpected token in expression: %q", p.curToken.Literal))
	p.nextToken()
	return &ast.NullExpression{}
}

func (p *Parser) parseQualifiedName() string {
	name := p.curToken.Literal
	p.nextToken()
	for p.curIsOp("::") {
		p.nextToken()
		if p.curToken.Type != lexer.TokenIdentifier {
			p.addError(fmt.Sprintf("expected identifier after '::', got %q", p.curToken.Literal))
			break
		}
		name += "::" + p.curToken.Literal
		p.nextToken()
	}
	return name
}

func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	p.nextToken() // consume '('

	args := []ast.Expression{}
	if !p.curIsPunct(")") {
		for {
			args = append(args, p.parseExpression(precLowest))
			if p.curIsPunct(",") {
				p.nextToken()
				continue
			}
			break
		}
	}
	p.expectPunct(")")

	return &ast.CallExpression{Function: function, Arguments: args}
}
