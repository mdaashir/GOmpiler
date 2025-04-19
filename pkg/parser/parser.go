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
		switch p.curToken.Type {
		case lexer.TokenPreprocessor:
			if decl := p.parsePreprocessorDirective(); decl != nil {
				program.Declarations = append(program.Declarations, decl)
			}
		case lexer.TokenKeyword:
			if decl := p.parseDeclaration(); decl != nil {
				program.Declarations = append(program.Declarations, decl)
			}
		case lexer.TokenPunctuation:
			// Skip punctuation tokens at the top level
			p.nextToken()
		case lexer.TokenIdentifier:
			// Handle global function definitions that might start with a type alias or similar
			if decl := p.parseVariableOrFunctionDeclaration(); decl != nil {
				program.Declarations = append(program.Declarations, decl)
			} else {
				// Skip unexpected identifier
				p.nextToken()
			}
		default:
			// Skip unexpected tokens
			p.addError(fmt.Sprintf("unexpected token at global scope: %s", p.curToken.Literal))
			p.nextToken()
		}
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
	// Save the current position to backtrack if needed
	// Get the complete type with qualifiers
	typeSpec := p.parseCompleteType()

	// Check for a name
	if p.curToken.Type != lexer.TokenIdentifier {
		p.addError(fmt.Sprintf("expected identifier, got %s", p.curToken.Literal))
		return nil
	}

	name := p.curToken.Literal
	p.nextToken()

	// If the next token is '(', this is a function declaration
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "(" {
		p.nextToken() // Skip '('

		// Parse parameters
		var params []ast.Parameter

		// If not immediately closing ')', there are parameters
		if !(p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ")") {
			for {
				// Simple parameter parsing - just skip tokens until ',' or ')'
				paramType := ""
				paramName := ""

				// Try to get parameter type
				if p.curToken.Type == lexer.TokenKeyword {
					paramType = p.curToken.Literal
					p.nextToken()

					// Check for reference/pointer
					if p.curToken.Type == lexer.TokenOperator && (p.curToken.Literal == "&" || p.curToken.Literal == "*") {
						paramType += p.curToken.Literal
						p.nextToken()
					}

					// Get parameter name if available
					if p.curToken.Type == lexer.TokenIdentifier {
						paramName = p.curToken.Literal
						p.nextToken()
					}
				} else {
					// Skip unknown tokens until delimiter
					for p.curToken.Type != lexer.TokenEOF &&
						!(p.curToken.Type == lexer.TokenPunctuation &&
							(p.curToken.Literal == "," || p.curToken.Literal == ")")) {
						p.nextToken()
					}
				}

				params = append(params, ast.Parameter{
					Type: paramType,
					Name: paramName,
				})

				// Check if we're at the end of parameters
				if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ")" {
					break
				}

				// Skip the comma
				if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "," {
					p.nextToken()
				} else {
					p.addError(fmt.Sprintf("expected ',' or ')', got %s", p.curToken.Literal))
					return nil
				}
			}
		}

		p.nextToken() // Skip ')'

		// Check if there's a function body
		if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "{" {
			// Parse function body
			body := p.parseBlockStatement()

			return &ast.FunctionDeclaration{
				Type:       typeSpec,
				Name:       name,
				Parameters: params,
				Body:       body,
			}
		} else {
			// This is a function prototype, skip to ';'
			for p.curToken.Type != lexer.TokenEOF &&
				!(p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";") {
				p.nextToken()
			}

			if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
				p.nextToken() // Skip ';'
			}

			return &ast.FunctionDeclaration{
				Type:       typeSpec,
				Name:       name,
				Parameters: params,
				Body:       nil,
			}
		}
	} else {
		// This is a variable declaration

		// Check for array size
		arraySize := ""
		if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "[" {
			p.nextToken() // Skip '['

			// Collect array size expression
			start := p.pos
			for p.curToken.Type != lexer.TokenEOF &&
				!(p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "]") {
				p.nextToken()
			}

			if p.pos > start {
				arraySize = "SomeSize" // Placeholder for actual size expression
			}

			if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "]" {
				p.nextToken() // Skip ']'
			}
		}

		// Check for initializer
		var initializer ast.Expression
		if p.curToken.Type == lexer.TokenOperator && p.curToken.Literal == "=" {
			p.nextToken() // Skip '='

			// Parse initializer expression (simplified for now)
			initializer = p.parseExpression()
		}

		// Skip to semicolon
		for p.curToken.Type != lexer.TokenEOF &&
			!(p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";") {
			p.nextToken()
		}

		if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
			p.nextToken() // Skip ';'
		}

		return &ast.VariableDeclaration{
			Type:        typeSpec,
			Name:        name,
			ArraySize:   arraySize,
			Initializer: initializer,
		}
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
	switch p.curToken.Type {
	case lexer.TokenKeyword:
		switch p.curToken.Literal {
		case "if":
			return p.parseIfStatement()
		case "for":
			return p.parseForStatement()
		case "while":
			return p.parseWhileStatement()
		case "do":
			return p.parseDoWhileStatement()
		case "switch":
			return p.parseSwitchStatement()
		case "return":
			return p.parseReturnStatement()
		case "break":
			p.nextToken() // Skip 'break'
			// Skip to semicolon
			if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
				p.nextToken() // Skip ';'
			}
			return &ast.BreakStatement{}
		case "continue":
			p.nextToken() // Skip 'continue'
			// Skip to semicolon
			if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
				p.nextToken() // Skip ';'
			}
			return &ast.ContinueStatement{}
		case "goto":
			return p.parseGotoStatement()
		case "int", "float", "double", "char", "bool", "void", "unsigned", "class", "struct", "enum", "const":
			// Variable declaration or function prototype
			decl := p.parseVariableOrFunctionDeclaration()
			if decl != nil {
				return &ast.DeclarationStatement{Declaration: decl}
			}
			return &ast.EmptyStatement{}
		default:
			// Skip unknown keyword
			p.nextToken()
			return &ast.EmptyStatement{}
		}
	case lexer.TokenPunctuation:
		if p.curToken.Literal == "{" {
			return p.parseBlockStatement()
		} else if p.curToken.Literal == ";" {
			p.nextToken() // Skip ';'
			return &ast.EmptyStatement{}
		}
		// Fall through to default case
	case lexer.TokenIdentifier:
		// This could be an expression statement (assignment, function call, etc.)
		stmt := &ast.ExpressionStatement{
			Expression: p.parseExpression(),
		}

		// Skip to semicolon
		for p.curToken.Type != lexer.TokenEOF &&
			!(p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";") {
			p.nextToken()
		}

		if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
			p.nextToken() // Skip ';'
		}

		return stmt
	default:
		panic("unhandled default case")
	}

	// For any other token types, create an empty statement and advance
	p.nextToken()
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
	p.nextToken() // Skip 'for'

	// Create default values
	var initialization ast.Statement
	var condition ast.Expression
	var increment ast.Expression

	// Parse the for loop header
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "(" {
		p.nextToken() // Skip '('

		// Parse initialization
		if p.curToken.Type != lexer.TokenPunctuation || p.curToken.Literal != ";" {
			// This is a declaration or expression
			if p.curToken.Type == lexer.TokenKeyword {
				// Variable declaration
				decl := p.parseVariableOrFunctionDeclaration()
				if decl != nil {
					initialization = &ast.DeclarationStatement{Declaration: decl}
				}
			} else {
				// Expression statement
				expr := p.parseExpression()
				initialization = &ast.ExpressionStatement{Expression: expr}

				// Skip to semicolon
				for p.curToken.Type != lexer.TokenEOF &&
					!(p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";") {
					p.nextToken()
				}
			}
		}

		// Skip semicolon after initialization
		if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
			p.nextToken() // Skip ';'
		} else {
			p.addError(fmt.Sprintf("expected ';', got %s", p.curToken.Literal))
		}

		// Parse condition
		if p.curToken.Type != lexer.TokenPunctuation || p.curToken.Literal != ";" {
			condition = p.parseExpression()
		}

		// Skip semicolon after condition
		if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
			p.nextToken() // Skip ';'
		} else {
			p.addError(fmt.Sprintf("expected ';', got %s", p.curToken.Literal))
		}

		// Parse increment
		if p.curToken.Type != lexer.TokenPunctuation || p.curToken.Literal != ")" {
			increment = p.parseExpression()
		}

		// Skip closing parenthesis
		if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ")" {
			p.nextToken() // Skip ')'
		} else {
			p.addError(fmt.Sprintf("expected ')', got %s", p.curToken.Literal))
		}
	} else {
		p.addError(fmt.Sprintf("expected '(' after 'for', got %s", p.curToken.Literal))
	}

	// Parse loop body
	body := p.parseStatement()

	return &ast.ForStatement{
		Initialization: initialization,
		Condition:      condition,
		Increment:      increment,
		Body:           body,
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

// parseOperatorExpression parses a binary expression with an operator
func (p *Parser) parseOperatorExpression(left ast.Expression) ast.Expression {
	// Create a binary expression
	expression := &ast.BinaryExpression{
		Left:     left,
		Operator: p.curToken.Literal,
		Token:    p.curToken,
	}

	// Remember precedence of current operator
	precedence := p.curPrecedence()

	// Move to the next token
	p.nextToken()

	// Parse the right side of the expression with appropriate precedence
	expression.Right = p.parseExpression(precedence)

	return expression
}

// curPrecedence returns the precedence of the current token
func (p *Parser) curPrecedence() int {
	if precedence, ok := precedences[p.curToken.Type]; ok {
		if p.curToken.Type == lexer.TokenOperator {
			switch p.curToken.Literal {
			case "=":
				return ASSIGN
			case "==", "!=":
				return EQUALS
			case "<", ">", "<=", ">=":
				return LESSGREATER
			case "+", "-":
				return SUM
			case "*", "/", "%":
				return PRODUCT
			}
		}
		return precedence
	}
	return LOWEST
}

func (p *Parser) parseGotoStatement() ast.Statement {
	return &ast.GotoStatement{
		Label: "dummyLabel",
	}
}

func (p *Parser) parseExpression() ast.Expression {
	// For C++ stream operations like cout << value
	if p.curToken.Type == lexer.TokenIdentifier {
		identifier := &ast.Identifier{Value: p.curToken.Literal}
		p.nextToken()

		// If this is a stream operation (<<, >>)
		if p.curToken.Type == lexer.TokenOperator && (p.curToken.Literal == "<<" || p.curToken.Literal == ">>") {
			var expr ast.Expression = identifier

			// Continue parsing as long as there are stream operators
			for p.curToken.Type == lexer.TokenOperator && (p.curToken.Literal == "<<" || p.curToken.Literal == ">>") {
				operator := p.curToken.Literal
				p.nextToken()

				// Parse the right side of the operator
				var right ast.Expression

				switch p.curToken.Type {
				case lexer.TokenIdentifier:
					right = &ast.Identifier{Value: p.curToken.Literal}
					p.nextToken()
				case lexer.TokenString:
					right = &ast.StringLiteral{Value: p.curToken.Literal}
					p.nextToken()
				case lexer.TokenNumber:
					right = &ast.NumberLiteral{Value: p.curToken.Literal}
					p.nextToken()
				case lexer.TokenChar:
					right = &ast.CharLiteral{Value: p.curToken.Literal}
					p.nextToken()
				default:
					// If we can't identify the right operand, try parsing it as an expression
					right = p.parsePrimaryExpression()
				}

				// Create a binary expression for this stream operation
				expr = &ast.BinaryExpression{
					Left:     expr,
					Operator: operator,
					Right:    right,
				}
			}

			return expr
		}

		// If it's potentially a member access or function call
		if p.curToken.Type == lexer.TokenPunctuation {
			if p.curToken.Literal == "." || (p.curToken.Type == lexer.TokenOperator && p.curToken.Literal == "->") {
				return p.parseMemberExpression(identifier)
			} else if p.curToken.Literal == "(" {
				return p.parseCallExpression(identifier)
			}
		}

		// If it's a comparison operator
		if p.curToken.Type == lexer.TokenOperator {
			if p.curToken.Literal == "=" || p.curToken.Literal == ">" || p.curToken.Literal == "<" ||
				p.curToken.Literal == ">=" || p.curToken.Literal == "<=" || p.curToken.Literal == "!=" ||
				p.curToken.Literal == "==" {
				return p.parseOperatorExpression(identifier)
			}
		}

		return identifier
	}

	// Handle other expression types
	switch p.curToken.Type {
	case lexer.TokenNumber:
		num := &ast.NumberLiteral{Value: p.curToken.Literal}
		p.nextToken()
		return num
	case lexer.TokenString:
		str := &ast.StringLiteral{Value: p.curToken.Literal}
		p.nextToken()
		return str
	case lexer.TokenChar:
		char := &ast.CharLiteral{Value: p.curToken.Literal}
		p.nextToken()
		return char
	case lexer.TokenKeyword:
		if p.curToken.Literal == "true" || p.curToken.Literal == "false" {
			value := p.curToken.Literal == "true"
			p.nextToken()
			return &ast.BooleanLiteral{Value: value}
		}
	default:
		panic("unhandled default case")
	}

	// Default to parsing a primary expression
	return p.parsePrimaryExpression()
}

func (p *Parser) parseAssignmentExpression() ast.Expression {
	return &ast.NullExpression{}
}

func (p *Parser) parsePrimaryExpression() ast.Expression {
	switch p.curToken.Type {
	case lexer.TokenIdentifier:
		identifier := &ast.Identifier{Value: p.curToken.Literal}
		p.nextToken() // Skip identifier

		// Check for function call
		if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "(" {
			p.nextToken() // Skip '('

			var args []ast.Expression

			// If not immediately closing ')', there are arguments
			if !(p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ")") {
				for {
					// Parse argument expression
					arg := p.parseExpression()
					args = append(args, arg)

					// Check if we're at the end of arguments
					if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ")" {
						break
					}

					// Skip the comma
					if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "," {
						p.nextToken()
					} else {
						p.addError(fmt.Sprintf("expected ',' or ')', got %s", p.curToken.Literal))
						break
					}
				}
			}

			p.nextToken() // Skip ')'

			return &ast.CallExpression{
				Function:  identifier,
				Arguments: args,
			}
		}

		// Check for member access
		if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "." {
			p.nextToken() // Skip '.'

			if p.curToken.Type != lexer.TokenIdentifier {
				p.addError(fmt.Sprintf("expected identifier after '.', got %s", p.curToken.Literal))
				return identifier
			}

			property := &ast.Identifier{Value: p.curToken.Literal}
			p.nextToken() // Skip property name

			return &ast.MemberExpression{
				Object:   identifier,
				Property: property,
			}
		}

		// Check for arrow operator
		if p.curToken.Type == lexer.TokenOperator && p.curToken.Literal == "->" {
			p.nextToken() // Skip '->'

			if p.curToken.Type != lexer.TokenIdentifier {
				p.addError(fmt.Sprintf("expected identifier after '->', got %s", p.curToken.Literal))
				return identifier
			}

			property := &ast.Identifier{Value: p.curToken.Literal}
			p.nextToken() // Skip property name

			// Create a member expression with arrow operator
			return &ast.MemberExpression{
				Object:   identifier,
				Property: property,
			}
		}

		return identifier

	case lexer.TokenNumber:
		num := &ast.NumberLiteral{Value: p.curToken.Literal}
		p.nextToken() // Skip number
		return num

	case lexer.TokenString:
		str := &ast.StringLiteral{Value: p.curToken.Literal}
		p.nextToken() // Skip string
		return str

	case lexer.TokenChar:
		char := &ast.CharLiteral{Value: p.curToken.Literal}
		p.nextToken() // Skip char
		return char

	case lexer.TokenKeyword:
		if p.curToken.Literal == "true" || p.curToken.Literal == "false" {
			value := p.curToken.Literal == "true"
			p.nextToken() // Skip boolean literal
			return &ast.BooleanLiteral{Value: value}
		}
		// Fall through to default

	case lexer.TokenOperator:
		// Unary operator
		operator := p.curToken.Literal
		p.nextToken() // Skip operator

		operand := p.parsePrimaryExpression()
		return &ast.UnaryExpression{
			Operator: operator,
			Right:    operand,
		}

	case lexer.TokenPunctuation:
		if p.curToken.Literal == "(" {
			p.nextToken() // Skip '('

			expr := p.parseExpression()

			if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ")" {
				p.nextToken() // Skip ')'
				return expr
			} else {
				p.addError(fmt.Sprintf("expected ')', got %s", p.curToken.Literal))
			}
		}
	default:
		panic("unhandled default case")
	}

	// If nothing else matches, create a null expression
	if p.curToken.Type != lexer.TokenEOF {
		p.nextToken() // Skip the current token
	}
	return &ast.NullExpression{}
}
