	// Compile the LLVM IR to an executable
	if err := g.compileIR(irFile, outputFile); err != nil {
		return err
	}
	
	return nil
}

// writeIR writes the LLVM IR to a file
func (g *Generator) writeIR(filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return fmt.Errorf("creating IR file: %w", err)
	}
	defer file.Close()
	
	if _, err := file.WriteString(g.module.String()); err != nil {
		return fmt.Errorf("writing IR: %w", err)
	}
	
	return nil
}

// compileIR compiles the LLVM IR to an executable
func (g *Generator) compileIR(irFile, outputFile string) error {
	// This is a simplified implementation
	// In a real compiler, you would use LLVM's APIs to compile the IR to an executable
	// For now, we'll simulate it by calling external tools
	
	// First, compile the IR to an object file
	objFile := outputFile + ".o"
	fmt.Printf("Compiling IR to object file: %s -> %s\n", irFile, objFile)
	
	// Then link the object file to create an executable
	fmt.Printf("Linking object file to create executable: %s -> %s\n", objFile, outputFile)
	
	return nil
}

// generateDeclaration generates code for a declaration
func (g *Generator) generateDeclaration(decl ast.Declaration) error {
	switch d := decl.(type) {
	case *ast.FunctionDeclaration:
		return g.generateFunctionDeclaration(d)
	case *ast.VariableDeclaration:
		return g.generateGlobalVariableDeclaration(d)
	case *ast.ClassDeclaration:
		return g.generateClassDeclaration(d)
	default:
		// Skip other declarations for now
		return nil
	}
}

// generateFunctionDeclaration generates LLVM IR for a function declaration
func (g *Generator) generateFunctionDeclaration(fn *ast.FunctionDeclaration) error {
	// Create function type
	returnType := g.getLLVMType(fn.Type)
	var paramTypes []types.Type
	var paramNames []string
	
	for _, param := range fn.Parameters {
		paramTypes = append(paramTypes, g.getLLVMType(param.Type))
		paramNames = append(paramNames, param.Name)
	}
	
	funcType := types.NewFunc(returnType, paramTypes...)
	
	// Create function
	function := g.module.NewFunc(fn.Name, returnType, nil)
	for i, paramName := range paramNames {
		param := function.NewParam(paramName, paramTypes[i])
		g.namedValues[paramName] = param
	}
	
	// Skip function body if this is just a declaration
	if fn.Body == nil {
		return nil
	}
	
	// Generate function body
	g.currentFunc = function
	entry := function.NewBlock("entry")
	g.builder = ir.NewBuilder(entry)
	
	// Generate code for function body
	if err := g.generateBlockStatement(fn.Body); err != nil {
		return err
	}
	
	// Ensure the function has a terminator
	if !g.builder.GetBlock().Term.IsTerminator() {
		if returnType.Equal(types.Void) {
			g.builder.CreateRetVoid()
		} else {
			// Add a default return value (this is a simplification)
			g.builder.CreateRet(constant.NewInt(returnType.(types.IntType), 0))
		}
	}
	
	g.currentFunc = nil
	return nil
}

// generateGlobalVariableDeclaration generates LLVM IR for a global variable declaration
func (g *Generator) generateGlobalVariableDeclaration(vd *ast.VariableDeclaration) error {
	// Determine the variable type
	varType := g.getLLVMType(vd.Type)
	
	// Create the global variable
	global := g.module.NewGlobal(vd.Name, varType)
	
	// Set the initializer if one is provided
	if vd.Initializer != nil {
		initialValue, err := g.generateExpression(vd.Initializer)
		if err != nil {
			return err
		}
		
		// For simplicity, we only handle constant initializers for globals
		if constVal, ok := initialValue.(constant.Constant); ok {
			global.Init = constVal
		} else {
			// Not a constant, provide a default initializer
			global.Init = constant.NewZeroInitializer(varType)
		}
	} else {
		// No initializer, provide a default one
		global.Init = constant.NewZeroInitializer(varType)
	}
	
	return nil
}

// generateClassDeclaration generates LLVM IR for a class declaration
func (g *Generator) generateClassDeclaration(cd *ast.ClassDeclaration) error {
	// Create a struct type for the class
	var memberTypes []types.Type
	
	// Collect member types
	for _, member := range cd.Members {
		if vd, ok := member.(*ast.VariableDeclaration); ok {
			memberTypes = append(memberTypes, g.getLLVMType(vd.Type))
		}
	}
	
	// Create the struct type
	structType := types.NewStruct(memberTypes...)
	
	// Add the struct type to the module
	g.module.NewTypeDef(cd.Name, structType)
	
	// Generate code for methods
	for _, member := range cd.Members {
		if fd, ok := member.(*ast.FunctionDeclaration); ok {
			if err := g.generateFunctionDeclaration(fd); err != nil {
				return err
			}
		}
	}
	
	return nil
}

// generateBlockStatement generates LLVM IR for a block statement
func (g *Generator) generateBlockStatement(block *ast.BlockStatement) error {
	for _, stmt := range block.Statements {
		if err := g.generateStatement(stmt); err != nil {
			return err
		}
	}
	
	return nil
}

// generateStatement generates LLVM IR for a statement
func (g *Generator) generateStatement(stmt ast.Statement) error {
	switch s := stmt.(type) {
	case *ast.BlockStatement:
		return g.generateBlockStatement(s)
	case *ast.ExpressionStatement:
		_, err := g.generateExpression(s.Expression)
		return err
	case *ast.DeclarationStatement:
		return g.generateDeclarationStatement(s)
	case *ast.ReturnStatement:
		return g.generateReturnStatement(s)
	case *ast.IfStatement:
		return g.generateIfStatement(s)
	case *ast.WhileStatement:
		return g.generateWhileStatement(s)
	case *ast.ForStatement:
		return g.generateForStatement(s)
	default:
		// Skip other statements for now
		return nil
	}
}

// generateDeclarationStatement generates LLVM IR for a declaration statement
func (g *Generator) generateDeclarationStatement(stmt *ast.DeclarationStatement) error {
	switch d := stmt.Declaration.(type) {
	case *ast.VariableDeclaration:
		return g.generateLocalVariableDeclaration(d)
	default:
		// Skip other declarations for now
		return nil
	}
}

// generateLocalVariableDeclaration generates LLVM IR for a local variable declaration
func (g *Generator) generateLocalVariableDeclaration(vd *ast.VariableDeclaration) error {
	// Determine the variable type
	varType := g.getLLVMType(vd.Type)
	
	// Allocate space for the variable on the stack
	alloca := g.builder.CreateAlloca(varType, vd.Name)
	
	// Store the variable in the symbol table
	g.namedValues[vd.Name] = alloca
	
	// Initialize the variable if an initializer is provided
	if vd.Initializer != nil {
		// Generate code for the initializer
		initValue, err := g.generateExpression(vd.Initializer)
		if err != nil {
			return err
		}
		
		// Store the initial value in the variable
		g.builder.CreateStore(initValue, alloca)
	}
	
	return nil
}

// generateReturnStatement generates LLVM IR for a return statement
func (g *Generator) generateReturnStatement(stmt *ast.ReturnStatement) error {
	if stmt.Value == nil {
		// Return void
		g.builder.CreateRetVoid()
		return nil
	}
	
	// Generate the return value
	retValue, err := g.generateExpression(stmt.Value)
	if err != nil {
		return err
	}
	
	// Create the return instruction
	g.builder.CreateRet(retValue)
	
	return nil
}

// generateIfStatement generates LLVM IR for an if statement
func (g *Generator) generateIfStatement(stmt *ast.IfStatement) error {
	// Generate the condition
	condValue, err := g.generateExpression(stmt.Condition)
	if err != nil {
		return err
	}
	
	// Create basic blocks for the then, else, and merge parts
	thenBlock := g.currentFunc.NewBlock("then")
	var elseBlock *ir.Block
	if stmt.Alternative != nil {
		elseBlock = g.currentFunc.NewBlock("else")
	}
	mergeBlock := g.currentFunc.NewBlock("merge")
	
	// Create the conditional branch
	if stmt.Alternative != nil {
		g.builder.CreateCondBr(condValue, thenBlock, elseBlock)
	} else {
		g.builder.CreateCondBr(condValue, thenBlock, mergeBlock)
	}
	
	// Generate code for the then block
	g.builder.SetBlock(thenBlock)
	if err := g.generateStatement(stmt.Consequence); err != nil {
		return err
	}
	if !g.builder.GetBlock().Term.IsTerminator() {
		g.builder.CreateBr(mergeBlock)
	}
	
	// Generate code for the else block if it exists
	if stmt.Alternative != nil {
		g.builder.SetBlock(elseBlock)
		if err := g.generateStatement(stmt.Alternative); err != nil {
			return err
		}
		if !g.builder.GetBlock().Term.IsTerminator() {
			g.builder.CreateBr(mergeBlock)
		}
	}
	
	// Continue in the merge block
	g.builder.SetBlock(mergeBlock)
	
	return nil
}

// generateWhileStatement generates LLVM IR for a while statement
func (g *Generator) generateWhileStatement(stmt *ast.WhileStatement) error {
	// Create basic blocks for the loop condition, body, and after parts
	condBlock := g.currentFunc.NewBlock("while.cond")
	bodyBlock := g.currentFunc.NewBlock("while.body")
	afterBlock := g.currentFunc.NewBlock("while.after")
	
	// Branch to the condition block
	g.builder.CreateBr(condBlock)
	
	// Generate code for the condition block
	g.builder.SetBlock(condBlock)
	condValue, err := g.generateExpression(stmt.Condition)
	if err != nil {
		return err
	}
	g.builder.CreateCondBr(condValue, bodyBlock, afterBlock)
	
	// Generate code for the body block
	g.builder.SetBlock(bodyBlock)
	if err := g.generateStatement(stmt.Body); err != nil {
		return err
	}
	if !g.builder.GetBlock().Term.IsTerminator() {
		g.builder.CreateBr(condBlock)
	}
	
	// Continue in the after block
	g.builder.SetBlock(afterBlock)
	
	return nil
}

// generateForStatement generates LLVM IR for a for statement
func (g *Generator) generateForStatement(stmt *ast.ForStatement) error {
	// Generate initialization if it exists
	if stmt.Initialization != nil {
		if err := g.generateStatement(stmt.Initialization); err != nil {
			return err
		}
	}
	
	// Create basic blocks for the loop condition, body, increment, and after parts
	condBlock := g.currentFunc.NewBlock("for.cond")
	bodyBlock := g.currentFunc.NewBlock("for.body")
	incrBlock := g.currentFunc.NewBlock("for.incr")
	afterBlock := g.currentFunc.NewBlock("for.after")
	
	// Branch to the condition block
	g.builder.CreateBr(condBlock)
	
	// Generate code for the condition block
	g.builder.SetBlock(condBlock)
	var condValue value.Value
	if stmt.Condition != nil {
		var err error
		condValue, err = g.generateExpression(stmt.Condition)
		if err != nil {
			return err
		}
	} else {
		// If no condition is provided, use 'true'
		condValue = constant.NewInt(types.I1, 1)
	}
	g.builder.CreateCondBr(condValue, bodyBlock, afterBlock)
	
	// Generate code for the body block
	g.builder.SetBlock(bodyBlock)
	if err := g.generateStatement(stmt.Body); err != nil {
		return err
	}
	if !g.builder.GetBlock().Term.IsTerminator() {
		g.builder.CreateBr(incrBlock)
	}
	
	// Generate code for the increment block
	g.builder.SetBlock(incrBlock)
	if stmt.Increment != nil {
		if _, err := g.generateExpression(stmt.Increment); err != nil {
			return err
		}
	}
	g.builder.CreateBr(condBlock)
	
	// Continue in the after block
	g.builder.SetBlock(afterBlock)
	
	return nil
}

// generateExpression generates LLVM IR for an expression
func (g *Generator) generateExpression(expr ast.Expression) (value.Value, error) {
	switch e := expr.(type) {
	case *ast.Identifier:
		return g.generateIdentifier(e)
	case *ast.NumberLiteral:
		return g.generateNumberLiteral(e)
	case *ast.StringLiteral:
		return g.generateStringLiteral(e)
	case *ast.BooleanLiteral:
		return g.generateBooleanLiteral(e)
	case *ast.AssignmentExpression:
		return g.generateAssignmentExpression(e)
	case *ast.BinaryExpression:
		return g.generateBinaryExpression(e)
	case *ast.UnaryExpression:
		return g.generateUnaryExpression(e)
	case *ast.CallExpression:
		return g.generateCallExpression(e)
	default:
		return nil, fmt.Errorf("unsupported expression type: %T", expr)
	}
}

// generateIdentifier generates LLVM IR for an identifier
func (g *Generator) generateIdentifier(expr *ast.Identifier) (value.Value, error) {
	// Look up the variable in the symbol table
	val, ok := g.namedValues[expr.Value]
	if !ok {
		return nil, fmt.Errorf("unknown variable: %s", expr.Value)
	}
	
	// If it's an alloca, load its value
	if alloca, ok := val.(*ir.InstAlloca); ok {
		return g.builder.CreateLoad(alloca.ElemType, alloca), nil
	}
	
	return val, nil
}

// generateNumberLiteral generates LLVM IR for a number literal
func (g *Generator) generateNumberLiteral(expr *ast.NumberLiteral) (value.Value, error) {
	// Simplified implementation: treat all numbers as integers
	// A real compiler would handle different numeric types
	if strings.Contains(expr.Value, ".") {
		// Float literal
		val, err := constant.NewFloatFromString(types.Double, expr.Value)
		if err != nil {
			return nil, fmt.Errorf("invalid float literal: %s", expr.Value)
		}
		return val, nil
	} else {
		// Integer literal
		val, err := constant.NewIntFromString(types.I32, expr.Value)
		if err != nil {
			return nil, fmt.Errorf("invalid integer literal: %s", expr.Value)
		}
		return val, nil
	}
}

// generateStringLiteral generates LLVM IR for a string literal
func (g *Generator) generateStringLiteral(expr *ast.StringLiteral) (value.Value, error) {
	// Create a global variable for the string
	value := expr.Value[1 : len(expr.Value)-1] // Remove quotes
	if global, ok := g.stringLiterals[value]; ok {
		// Reuse existing string literal
		return global, nil
	}
	
	// Create a new string literal
	data := constant.NewCharArrayFromString(value + "\x00") // Null-terminated
	global := g.module.NewGlobalDef(".str", data)
	global.Linkage = ir.LinkagePrivate
	global.Immutable = true
	global.UnnamedAddr = true
	g.stringLiterals[value] = global
	
	// Return a pointer to the first character
	zero := constant.NewInt(types.I32, 0)
	indices := []value.Value{zero, zero}
	return g.builder.CreateGEP(global.ContentType, global, indices...), nil
}

// generateBooleanLiteral generates LLVM IR for a boolean literal
func (g *Generator) generateBooleanLiteral(expr *ast.BooleanLiteral) (value.Value, error) {
	if expr.Value {
		return constant.NewInt(types.I1, 1), nil
	}
	return constant.NewInt(types.I1, 0), nil
}

// generateAssignmentExpression generates LLVM IR for an assignment expression
func (g *Generator) generateAssignmentExpression(expr *ast.AssignmentExpression) (value.Value, error) {
	// Get the variable to assign to
	var target value.Value
	
	if id, ok := expr.Left.(*ast.Identifier); ok {
		// Simple variable assignment
		val, ok := g.namedValues[id.Value]
		if !ok {
			return nil, fmt.Errorf("unknown variable: %s", id.Value)
		}
		target = val
	} else {
		// More complex left-hand side (e.g., dereferencing a pointer)
		return nil, fmt.Errorf("unsupported left-hand side in assignment")
	}
	
	// Generate the right-hand side value
	right, err := g.generateExpression(expr.Right)
	if err != nil {
		return nil, err
	}
	
	// Store the value in the target
	g.builder.CreateStore(right, target)
	
	// Assignment expressions return the assigned value
	return right, nil
}

// generateBinaryExpression generates LLVM IR for a binary expression
func (g *Generator) generateBinaryExpression(expr *ast.BinaryExpression) (value.Value, error) {
	// Generate left and right operands
	left, err := g.generateExpression(expr.Left)
	if err != nil {
		return nil, err
	}
	
	right, err := g.generateExpression(expr.Right)
	if err != nil {
		return nil, err
	}
	
	// Handle different operators
	switch expr.Operator {
	case "+":
		// Check if integers or floats
		if left.Type().Equal(types.I32) && right.Type().Equal(types.I32) {
			return g.builder.CreateAdd(left, right), nil
		} else if left.Type().Equal(types.Double) && right.Type().Equal(types.Double) {
			return g.builder.CreateFAdd(left, right), nil
		} else {
			return nil, fmt.Errorf("incompatible types for addition")
		}
	case "-":
		if left.Type().Equal(types.I32) && right.Type().Equal(types.I32) {
			return g.builder.CreateSub(left, right), nil
		} else if left.Type().Equal(types.Double) && right.Type().Equal(types.Double) {
			return g.builder.CreateFSub(left, right), nil
		} else {
			return nil, fmt.Errorf("incompatible types for subtraction")
		}
	case "*":
		if left.Type().Equal(types.I32) && right.Type().Equal(types.I32) {
			return g.builder.CreateMul(left, right), nil
		} else if left.Type().Equal(types.Double) && right.Type().Equal(types.Double) {
			return g.builder.CreateFMul(left, right), nil
		} else {
			return nil, fmt.Errorf("incompatible types for multiplication")
		}
	case "/":
		if left.Type().Equal(types.I32) && right.Type().Equal(types.I32) {
			return g.builder.CreateSDiv(left, right), nil
		} else if left.Type().Equal(types.Double) && right.Type().Equal(types.Double) {
			return g.builder.CreateFDiv(left, right), nil
		} else {
			return nil, fmt.Errorf("incompatible types for division")
		}
	case "%":
		if left.Type().Equal(types.I32) && right.Type().Equal(types.I32) {
			return g.builder.CreateSRem(left, right), nil
		} else {
			return nil, fmt.Errorf("modulo operation only supported for integers")
		}
	case "<":
		if left.Type().Equal(types.I32) && right.Type().Equal(types.I32) {
			return g.builder.CreateICmp(llvm.IntSLT, left, right), nil
		} else if left.Type().Equal(types.Double) && right.Type().Equal(types.Double) {
			return g.builder.CreateFCmp(llvm.FloatOLT, left, right), nil
		} else {
			return nil, fmt.Errorf("incompatible types for comparison")
		}
	case ">":
		if left.Type().Equal(types.I32) && right.Type().Equal(types.I32) {
			return g.builder.CreateICmp(llvm.IntSGT, left, right), nil
		} else if left.Type().Equal(types.Double) && right.Type().Equal(types.Double) {
			return g.builder.CreateFCmp(llvm.FloatOGT, left, right), nil
		} else {
			return nil, fmt.Errorf("incompatible types for comparison")
		}
	case "<=":
		if left.Type().Equal(types.I32) && right.Type().Equal(types.I32) {
			return g.builder.CreateICmp(llvm.IntSLE, left, right), nil
		} else if left.Type().Equal(types.Double) && right.Type().Equal(types.Double) {
			return g.builder.CreateFCmp(llvm.FloatOLE, left, right), nil
		} else {
			return nil, fmt.Errorf("incompatible types for comparison")
		}
	case ">=":
		if left.Type().Equal(types.I32) && right.Type().Equal(types.I32) {
			return g.builder.CreateICmp(llvm.IntSGE, left, right), nil
		} else if left.Type().Equal(types.Double) && right.Type().Equal(types.Double) {
			return g.builder.CreateFCmp(llvm.FloatOGE, left, right), nil
		} else {
			return nil, fmt.Errorf("incompatible types for comparison")
		}
	case "==":
		if left.Type().Equal(types.I32) && right.Type().Equal(types.I32) {
			return g.builder.CreateICmp(llvm.IntEQ, left, right), nil
		} else if left.Type().Equal(types.Double) && right.Type().Equal(types.Double) {
			return g.builder.CreateFCmp(llvm.FloatOEQ, left, right), nil
		} else {
			return nil, fmt.Errorf("incompatible types for comparison")
		}
	case "!=":
		if left.Type().Equal(types.I32) && right.Type().Equal(types.I32) {
			return g.builder.CreateICmp(llvm.IntNE, left, right), nil
		} else if left.Type().Equal(types.Double) && right.Type().Equal(types.Double) {
			return g.builder.CreateFCmp(llvm.FloatONE, left, right), nil
		} else {
			return nil, fmt.Errorf("incompatible types for comparison")
		}
	case "&&":
		// Ensure both operands are boolean
		if !left.Type().Equal(types.I1) || !right.Type().Equal(types.I1) {
			return nil, fmt.Errorf("logical AND requires boolean operands")
		}
		return g.builder.CreateAnd(left, right), nil
	case "||":
		// Ensure both operands are boolean
		if !left.Type().Equal(types.I1) || !right.Type().Equal(types.I1) {
			return nil, fmt.Errorf("logical OR requires boolean operands")
		}
		return g.builder.CreateOr(left, right), nil
	default:
		return nil, fmt.Errorf("unsupported binary operator: %s", expr.Operator)
	}
}

// generateUnaryExpression generates LLVM IR for a unary expression
func (g *Generator) generateUnaryExpression(expr *ast.UnaryExpression) (value.Value, error) {
	// Generate the operand
	operand, err := g.generateExpression(expr.Right)
	if err != nil {
		return nil, err
	}
	
	// Handle different operators
	switch expr.Operator {
	case "-":
		if operand.Type().Equal(types.I32) {
			zero := constant.NewInt(types.I32, 0)
			return g.builder.CreateSub(zero, operand), nil
		} else if operand.Type().Equal(types.Double) {
			return g.builder.CreateFNeg(operand), nil
		} else {
			return nil, fmt.Errorf("negation only supported for numeric types")
		}
	case "!":
		if !operand.Type().Equal(types.I1) {
			return nil, fmt.Errorf("logical NOT requires a boolean operand")
		}
		return g.builder.CreateNot(operand), nil
	case "&":
		// Get the address of a variable
		if id, ok := expr.Right.(*ast.Identifier); ok {
			val, ok := g.namedValues[id.Value]
			if !ok {
				return nil, fmt.Errorf("unknown variable: %s", id.Value)
			}
			return val, nil
		}
		return nil, fmt.Errorf("address-of operator requires a variable")
	case "*":
		// Dereference a pointer
		return g.builder.CreateLoad(operand.Type().(*types.PointerType).ElemType, operand), nil
	default:
		return nil, fmt.Errorf("unsupported unary operator: %s", expr.Operator)
	}
}

// generateCallExpression generates LLVM IR for a function call
func (g *Generator) generateCallExpression(expr *ast.CallExpression) (value.Value, error) {
	// Get the function to call
	var fn value.Value
	
	if id, ok := expr.Function.(*ast.Identifier); ok {
		// Look up the function in the module
		fn = g.module.Func(id.Value)
		if fn == nil {
			return nil, fmt.Errorf("unknown function: %s", id.Value)
		}
	} else {
		// Function pointer or more complex expression
		var err error
		fn, err = g.generateExpression(expr.Function)
		if err != nil {
			return nil, err
		}
	}
	
	// Generate arguments
	var args []value.Value
	for _, arg := range expr.Arguments {
		argVal, err := g.generateExpression(arg)
		if err != nil {
			return nil, err
		}
		args = append(args, argVal)
	}
	
	// Call the function
	return g.builder.CreateCall(fn.Type().(*types.PointerType).ElemType, fn, args...), nil
}

// getLLVMType converts a C++ type string to an LLVM type
func (g *Generator) getLLVMType(typeStr string) types.Type {
	// Simplified implementation
	// A real compiler would handle more complex type parsing
	
	// Remove whitespace
	typeStr = strings.TrimSpace(typeStr)
	
	// Check for pointer or reference types
	if strings.HasSuffix(typeStr, "*") {
		elemType := g.getLLVMType(strings.TrimSuffix(typeStr, "*"))
		return types.NewPointer(elemType)
	}
	if strings.HasSuffix(typeStr, "&") {
		elemType := g.getLLVMType(strings.TrimSuffix(typeStr, "&"))
		return types.NewPointer(elemType) // References are implemented as pointers
	}
	
	// Check for array types
	if idx := strings.Index(typeStr, "["); idx != -1 {
		if strings.HasSuffix(typeStr, "]") {
			elemType := g.getLLVMType(strings.TrimSpace(typeStr[:idx]))
			sizeStr := typeStr[idx+1 : len(typeStr)-1]
			if size, err := strconv.Atoi(sizeStr); err == nil {
				return types.NewArray(uint64(size), elemType)
			}
		}
	}
	
	// Basic types
	switch typeStr {
	case "void":
		return types.Void
	case "bool":
		return types.I1
	case "char":
		return types.I8
	case "short":
		return types.I16
	case "int":
		return types.I32
	case "long":
		return types.I64
	case "long long":
		return types.I64
	case "float":
		return types.Float
	case "double":
		return types.Double
	default:
		// Try to look up user-defined types
		if typeDef := g.module.TypeDef(typeStr); typeDef != nil {
			return typeDef
		}
		
		// Default to pointer to i8 for unknown types
		return types.NewPointer(types.I8)
	}
}
