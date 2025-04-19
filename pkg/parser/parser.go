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
	
	return &ast.TypedefDeclaration{}
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
	
	return &ast.UsingDeclaration{}
}

func (p *Parser) parsePreprocessorDirective() ast.Declaration {
	directive := p.curToken.Literal
	p.nextToken() // Skip preprocessor directive
	
	return &ast.PreprocessorDirective{
		Directive: directive,
	}
}

func (p *Parser) parseVariableOrFunctionDeclaration() ast.Declaration {
	// Parse type
	typeSpec := p.parseType()
	
	// Parse name
	var name string
	if p.curToken.Type == lexer.TokenIdentifier {
		name = p.curToken.Literal
		p.nextToken()
	} else {
		p.addError("expected identifier in declaration")
		return nil
	}
	
	// Check if it's a function or variable
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "(" {
		return p.parseFunction(typeSpec, name)
	} else {
		return p.parseVariable(typeSpec, name)
	}
}

func (p *Parser) parseType() string {
	var typeStr string
	
	// Parse type qualifiers and specifiers
	for p.curToken.Type == lexer.TokenKeyword {
		if p.curToken.Literal == "const" || 
		   p.curToken.Literal == "static" || 
		   p.curToken.Literal == "extern" ||
		   p.curToken.Literal == "volatile" ||
		   p.curToken.Literal == "virtual" ||
		   p.curToken.Literal == "inline" ||
		   p.curToken.Literal == "void" ||
		   p.curToken.Literal == "int" ||
		   p.curToken.Literal == "float" ||
		   p.curToken.Literal == "double" ||
		   p.curToken.Literal == "char" ||
		   p.curToken.Literal == "bool" ||
		   p.curToken.Literal == "unsigned" ||
		   p.curToken.Literal == "signed" ||
		   p.curToken.Literal == "short" ||
		   p.curToken.Literal == "long" {
			
			if len(typeStr) > 0 {
				typeStr += " "
			}
			typeStr += p.curToken.Literal
			p.nextToken()
		} else {
			break
		}
	}
	
	// Handle user-defined types
	if p.curToken.Type == lexer.TokenIdentifier {
		if len(typeStr) > 0 {
			typeStr += " "
		}
		typeStr += p.curToken.Literal
		p.nextToken()
	}
	
	// Handle template type arguments
	if p.curToken.Type == lexer.TokenOperator && p.curToken.Literal == "<" {
		typeStr += "<"
		p.nextToken() // Skip '<'
		
		// Skip template arguments for now
		angleCount := 1
		for angleCount > 0 && p.curToken.Type != lexer.TokenEOF {
			typeStr += p.curToken.Literal
			
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
	
	// Handle pointers and references
	for p.curToken.Type == lexer.TokenOperator && 
		(p.curToken.Literal == "*" || p.curToken.Literal == "&" || p.curToken.Literal == "&&") {
		typeStr += p.curToken.Literal
		p.nextToken()
	}
	
	return typeStr
}

func (p *Parser) parseFunction(typeSpec, name string) ast.Declaration {
	p.nextToken() // Skip '('
	
	// Parse parameters
	var params []ast.Parameter
	if !(p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ")") {
		params = p.parseParameters()
	}
	
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ")" {
		p.nextToken() // Skip ')'
	} else {
		p.addError("expected ')' after function parameters")
	}
	
	// Function body or declaration
	var body *ast.BlockStatement
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "{" {
		body = p.parseBlockStatement()
	} else if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
		p.nextToken() // Skip ';'
	} else {
		p.addError("expected '{' or ';' after function declaration")
	}
	
	return &ast.FunctionDeclaration{
		Type:       typeSpec,
		Name:       name,
		Parameters: params,
		Body:       body,
	}
}

func (p *Parser) parseParameters() []ast.Parameter {
	var params []ast.Parameter
	
	for {
		paramType := p.parseType()
		
		var paramName string
		if p.curToken.Type == lexer.TokenIdentifier {
			paramName = p.curToken.Literal
			p.nextToken()
		}
		
		// Skip default arguments
		if p.curToken.Type == lexer.TokenOperator && p.curToken.Literal == "=" {
			p.nextToken() // Skip '='
			p.parseExpression()
		}
		
		params = append(params, ast.Parameter{
			Type: paramType,
			Name: paramName,
		})
		
		if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "," {
			p.nextToken() // Skip ','
		} else {
			break
		}
	}
	
	return params
}

func (p *Parser) parseVariable(typeSpec, name string) ast.Declaration {
	// Handle array declaration
	var arraySize string
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "[" {
		p.nextToken() // Skip '['
		
		if p.curToken.Type == lexer.TokenNumber {
			arraySize = p.curToken.Literal
			p.nextToken()
		}
		
		if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "]" {
			p.nextToken() // Skip ']'
		} else {
			p.addError("expected ']' in array declaration")
		}
	}
	
	// Handle initialization
	var initializer ast.Expression
	if p.curToken.Type == lexer.TokenOperator && p.curToken.Literal == "=" {
		p.nextToken() // Skip '='
		initializer = p.parseExpression()
	}
	
	// Skip to semicolon
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
		p.nextToken() // Skip ';'
	} else {
		p.addError("expected ';' after variable declaration")
	}
	
	return &ast.VariableDeclaration{
		Type:        typeSpec,
		Name:        name,
		ArraySize:   arraySize,
		Initializer: initializer,
	}
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{
		Statements: []ast.Statement{},
	}
	
	p.nextToken() // Skip '{'
	
	for p.curToken.Type != lexer.TokenEOF && 
		!(p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "}") {
		
		if stmt := p.parseStatement(); stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
	}
	
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "}" {
		p.nextToken() // Skip '}'
	} else {
		p.addError("expected '}' after block statement")
	}
	
	return block
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
			return p.parseBreakStatement()
		case "continue":
			return p.parseContinueStatement()
		case "goto":
			return p.parseGotoStatement()
		default:
			// It could be a variable declaration
			return p.parseDeclarationStatement()
		}
	case lexer.TokenPunctuation:
		if p.curToken.Literal == "{" {
			return p.parseBlockStatement()
		} else if p.curToken.Literal == ";" {
			p.nextToken() // Skip ';'
			return &ast.EmptyStatement{}
		}
	}
	
	// Default to expression statement
	return p.parseExpressionStatement()
}

func (p *Parser) parseDeclarationStatement() ast.Statement {
	decl := p.parseVariableOrFunctionDeclaration()
	if decl != nil {
		return &ast.DeclarationStatement{
			Declaration: decl,
		}
	}
	return nil
}

func (p *Parser) parseExpressionStatement() ast.Statement {
	expr := p.parseExpression()
	
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
		p.nextToken() // Skip ';'
	} else {
		p.addError("expected ';' after expression statement")
	}
	
	return &ast.ExpressionStatement{
		Expression: expr,
	}
}

func (p *Parser) parseIfStatement() ast.Statement {
	p.nextToken() // Skip 'if'
	
	if p.curToken.Type != lexer.TokenPunctuation || p.curToken.Literal != "(" {
		p.addError("expected '(' after 'if'")
		return nil
	}
	p.nextToken() // Skip '('
	
	condition := p.parseExpression()
	
	if p.curToken.Type != lexer.TokenPunctuation || p.curToken.Literal != ")" {
		p.addError("expected ')' after if condition")
		return nil
	}
	p.nextToken() // Skip ')'
	
	consequence := p.parseStatement()
	
	var alternative ast.Statement
	if p.curToken.Type == lexer.TokenKeyword && p.curToken.Literal == "else" {
		p.nextToken() // Skip 'else'
		alternative = p.parseStatement()
	}
	
	return &ast.IfStatement{
		Condition:   condition,
		Consequence: consequence,
		Alternative: alternative,
	}
}

// Placeholder implementations for parsing other statements
func (p *Parser) parseForStatement() ast.Statement {
	p.nextToken() // Skip 'for'
	
	// Skip for statement for now
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "(" {
		p.nextToken() // Skip '('
		
		// Skip to closing ')'
		parenCount := 1
		for parenCount > 0 && p.curToken.Type != lexer.TokenEOF {
			if p.curToken.Type == lexer.TokenPunctuation {
				if p.curToken.Literal == "(" {
					parenCount++
				} else if p.curToken.Literal == ")" {
					parenCount--
				}
			}
			p.nextToken()
		}
	}
	
	// Parse the loop body
	body := p.parseStatement()
	
	return &ast.ForStatement{
		Body: body,
	}
}

func (p *Parser) parseWhileStatement() ast.Statement {
	p.nextToken() // Skip 'while'
	
	// Skip while statement for now
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "(" {
		p.nextToken() // Skip '('
		
		// Skip to closing ')'
		parenCount := 1
		for parenCount > 0 && p.curToken.Type != lexer.TokenEOF {
			if p.curToken.Type == lexer.TokenPunctuation {
				if p.curToken.Literal == "(" {
					parenCount++
				} else if p.curToken.Literal == ")" {
					parenCount--
				}
			}
			p.nextToken()
		}
	}
	
	// Parse the loop body
	body := p.parseStatement()
	
	return &ast.WhileStatement{
		Body: body,
	}
}

func (p *Parser) parseDoWhileStatement() ast.Statement {
	p.nextToken() // Skip 'do'
	
	// Parse the loop body
	body := p.parseStatement()
	
	// Expect 'while'
	if p.curToken.Type != lexer.TokenKeyword || p.curToken.Literal != "while" {
		p.addError("expected 'while' after 'do' body")
	} else {
		p.nextToken() // Skip 'while'
	}
	
	// Skip condition for now
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "(" {
		p.nextToken() // Skip '('
		
		// Skip to closing ')'
		parenCount := 1
		for parenCount > 0 && p.curToken.Type != lexer.TokenEOF {
			if p.curToken.Type == lexer.TokenPunctuation {
				if p.curToken.Literal == "(" {
					parenCount++
				} else if p.curToken.Literal == ")" {
					parenCount--
				}
			}
			p.nextToken()
		}
	}
	
	// Expect semicolon
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
		p.nextToken() // Skip ';'
	} else {
		p.addError("expected ';' after do-while statement")
	}
	
	return &ast.DoWhileStatement{
		Body: body,
	}
}

func (p *Parser) parseSwitchStatement() ast.Statement {
	p.nextToken() // Skip 'switch'
	
	// Skip switch statement for now
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == "(" {
		p.nextToken() // Skip '('
		
		// Skip to closing ')'
		parenCount := 1
		for parenCount > 0 && p.curToken.Type != lexer.TokenEOF {
			if p.curToken.Type == lexer.TokenPunctuation {
				if p.curToken.Literal == "(" {
					parenCount++
				} else if p.curToken.Literal == ")" {
					parenCount--
				}
			}
			p.nextToken()
		}
	}
	
	// Parse the switch body
	body := p.parseStatement()
	
	return &ast.SwitchStatement{
		Body: body,
	}
}

func (p *Parser) parseReturnStatement() ast.Statement {
	p.nextToken() // Skip 'return'
	
	var value ast.Expression
	if p.curToken.Type != lexer.TokenPunctuation || p.curToken.Literal != ";" {
		value = p.parseExpression()
	}
	
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
		p.nextToken() // Skip ';'
	} else {
		p.addError("expected ';' after return statement")
	}
	
	return &ast.ReturnStatement{
		Value: value,
	}
}

func (p *Parser) parseBreakStatement() ast.Statement {
	p.nextToken() // Skip 'break'
	
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
		p.nextToken() // Skip ';'
	} else {
		p.addError("expected ';' after break statement")
	}
	
	return &ast.BreakStatement{}
}

func (p *Parser) parseContinueStatement() ast.Statement {
	p.nextToken() // Skip 'continue'
	
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
		p.nextToken() // Skip ';'
	} else {
		p.addError("expected ';' after continue statement")
	}
	
	return &ast.ContinueStatement{}
}

func (p *Parser) parseGotoStatement() ast.Statement {
	p.nextToken() // Skip 'goto'
	
	var label string
	if p.curToken.Type == lexer.TokenIdentifier {
		label = p.curToken.Literal
		p.nextToken()
	} else {
		p.addError("expected label after 'goto'")
	}
	
	if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ";" {
		p.nextToken() // Skip ';'
	} else {
		p.addError("expected ';' after goto statement")
	}
	
	return &ast.GotoStatement{
		Label: label,
	}
}

// parseExpression parses expressions with precedence climbing
func (p *Parser) parseExpression() ast.Expression {
	return p.parseAssignmentExpression()
}

func (p *Parser) parseAssignmentExpression() ast.Expression {
	// Simple implementation for now
	expr := p.parsePrimaryExpression()
	
	if p.curToken.Type == lexer.TokenOperator && (
		p.curToken.Literal == "=" ||
		p.curToken.Literal == "+=" ||
		p.curToken.Literal == "-=" ||
		p.curToken.Literal == "*=" ||
		p.curToken.Literal == "/=" ||
		p.curToken.Literal == "%=" ||
		p.curToken.Literal == "&=" ||
		p.curToken.Literal == "|=" ||
		p.curToken.Literal == "^=" ||
		p.curToken.Literal == "<<=" ||
		p.curToken.Literal == ">>=" ) {
		
		operator := p.curToken.Literal
		p.nextToken() // Skip operator
		
		right := p.parseAssignmentExpression()
		
		return &ast.AssignmentExpression{
			Left:     expr,
			Operator: operator,
			Right:    right,
		}
	}
	
	return expr
}

func (p *Parser) parsePrimaryExpression() ast.Expression {
	switch p.curToken.Type {
	case lexer.TokenIdentifier:
		identifier := p.curToken.Literal
		p.nextToken()
		return &ast.Identifier{
			Value: identifier,
		}
	case lexer.TokenNumber:
		number := p.curToken.Literal
		p.nextToken()
		return &ast.NumberLiteral{
			Value: number,
		}
	case lexer.TokenString:
		str := p.curToken.Literal
		p.nextToken()
		return &ast.StringLiteral{
			Value: str,
		}
	case lexer.TokenChar:
		char := p.curToken.Literal
		p.nextToken()
		return &ast.CharLiteral{
			Value: char,
		}
	case lexer.TokenKeyword:
		if p.curToken.Literal == "true" || p.curToken.Literal == "false" {
			value := p.curToken.Literal == "true"
			p.nextToken()
			return &ast.BooleanLiteral{
				Value: value,
			}
		}
	case lexer.TokenPunctuation:
		if p.curToken.Literal == "(" {
			p.nextToken() // Skip '('
			expr := p.parseExpression()
			
			if p.curToken.Type == lexer.TokenPunctuation && p.curToken.Literal == ")" {
				p.nextToken() // Skip ')'
				return expr
			} else {
				p.addError("expected ')'")
			}
		}
	}
	
	// Default to a null expression on error
	p.addError(fmt.Sprintf("unexpected token in expression: %s", p.curToken.Literal))
	p.nextToken()
	return &ast.NullExpression{}
}
