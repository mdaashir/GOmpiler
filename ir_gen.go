package main

type IROpType string

const (
	ADD    IROpType = "ADD"
	SUB    IROpType = "SUB"
	MUL    IROpType = "MUL"
	DIV    IROpType = "DIV"
	ASSIGN IROpType = "ASSIGN"
	JMP    IROpType = "JMP"
	JZ     IROpType = "JZ"
	JNZ    IROpType = "JNZ"
	LABEL  IROpType = "LABEL"
	CALL   IROpType = "CALL"
	RET    IROpType = "RET"
)

type IRInstruction struct {
	Op    IROpType
	Dest  string
	Arg1  string
	Arg2  string
	Label string
}

type BasicBlock struct {
	Label        string
	Instructions []IRInstruction
	Successors   []*BasicBlock
	Predecessors []*BasicBlock
}

type IRGenerator struct {
	tempCount    int
	labelCount   int
	blocks       []*BasicBlock
	currentBlock *BasicBlock
}

func NewIRGenerator() *IRGenerator {
	startBlock := &BasicBlock{
		Label:        "entry",
		Instructions: make([]IRInstruction, 0),
		Successors:   make([]*BasicBlock, 0),
		Predecessors: make([]*BasicBlock, 0),
	}

	return &IRGenerator{
		tempCount:    0,
		labelCount:   0,
		blocks:       []*BasicBlock{startBlock},
		currentBlock: startBlock,
	}
}

func (g *IRGenerator) NewTemp() string {
	temp := "t" + string(g.tempCount)
	g.tempCount++
	return temp
}

func (g *IRGenerator) NewLabel() string {
	label := "L" + string(g.labelCount)
	g.labelCount++
	return label
}

func (g *IRGenerator) CreateBasicBlock(label string) *BasicBlock {
	block := &BasicBlock{
		Label:        label,
		Instructions: make([]IRInstruction, 0),
		Successors:   make([]*BasicBlock, 0),
		Predecessors: make([]*BasicBlock, 0),
	}
	g.blocks = append(g.blocks, block)
	return block
}

func (g *IRGenerator) AddInstruction(inst IRInstruction) {
	g.currentBlock.Instructions = append(g.currentBlock.Instructions, inst)
}

func (g *IRGenerator) GenerateIR(node *ASTNode) string {
	switch node.Type {
	case "BINARY_EXPRESSION":
		return g.generateBinaryExpression(node)
	case "VARIABLE_DECLARATION":
		return g.generateVariableDeclaration(node)
	case "IF_STATEMENT":
		g.generateIfStatement(node)
		return ""
	case "WHILE_STATEMENT":
		g.generateWhileStatement(node)
		return ""
	case "FUNCTION_DECLARATION":
		g.generateFunctionDeclaration(node)
		return ""
	}
	return ""
}

func (g *IRGenerator) generateBinaryExpression(node *ASTNode) string {
	left := g.GenerateIR(node.Children[0])
	right := g.GenerateIR(node.Children[1])
	result := g.NewTemp()

	var op IROpType
	switch node.Value {
	case "+":
		op = ADD
	case "-":
		op = SUB
	case "*":
		op = MUL
	case "/":
		op = DIV
	}

	g.AddInstruction(IRInstruction{
		Op:   op,
		Dest: result,
		Arg1: left,
		Arg2: right,
	})

	return result
}
package main

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

// IRType represents the type of an IR instruction
type IRType string

const (
	IRAlloc   IRType = "ALLOC"   // Allocate memory for a variable
	IRStore   IRType = "STORE"   // Store a value to a variable
	IRLoad    IRType = "LOAD"    // Load a value from a variable
	IRAdd     IRType = "ADD"     // Add two values
	IRSub     IRType = "SUB"     // Subtract two values
	IRMul     IRType = "MUL"     // Multiply two values
	IRDiv     IRType = "DIV"     // Divide two values
	IRCmp     IRType = "CMP"     // Compare two values
	IRJmp     IRType = "JMP"     // Unconditional jump
	IRJe      IRType = "JE"      // Jump if equal
	IRJne     IRType = "JNE"     // Jump if not equal
	IRJl      IRType = "JL"      // Jump if less
	IRJle     IRType = "JLE"     // Jump if less or equal
	IRJg      IRType = "JG"      // Jump if greater
	IRJge     IRType = "JGE"     // Jump if greater or equal
	IRCall    IRType = "CALL"    // Call a function
	IRRet     IRType = "RET"     // Return from a function
	IRLabel   IRType = "LABEL"   // Label for jumps
	IRConst   IRType = "CONST"   // Constant value
	IRFunc    IRType = "FUNC"    // Function definition
	IRParam   IRType = "PARAM"   // Function parameter
	IRPush    IRType = "PUSH"    // Push value onto stack
	IRPop     IRType = "POP"     // Pop value from stack
)

// IRInstruction represents a single instruction in the IR
type IRInstruction struct {
	Type       IRType
	Dest       string
	Op1        string
	Op2        string
	Value      string
	Label      string
	LineNumber int
}

// IRBlock represents a basic block in the IR
type IRBlock struct {
	Label        string
	Instructions []IRInstruction
	Next         *IRBlock
	Branch       *IRBlock
}

// IRFunction represents a function in the IR
type IRFunction struct {
	Name       string
	Parameters []string
	Blocks     []*IRBlock
	EntryBlock *IRBlock
	ReturnType Type
}

// IRProgram represents the entire program in IR form
type IRProgram struct {
	Functions []IRFunction
	Globals   map[string]string
}

// IRGenerator translates AST to IR
type IRGenerator struct {
	program       IRProgram
	currentFunc   *IRFunction
	currentBlock  *IRBlock
	symbolTable   *SymbolTable
	tempCounter   int
	labelCounter  int
	stringLiterals map[string]string
}

// NewIRGenerator creates a new IR generator
func NewIRGenerator(symbolTable *SymbolTable) *IRGenerator {
	return &IRGenerator{
		program: IRProgram{
			Functions: []IRFunction{},
			Globals:   make(map[string]string),
		},
		symbolTable:    symbolTable,
		tempCounter:    0,
		labelCounter:   0,
		stringLiterals: make(map[string]string),
	}
}

// Generate generates IR from the AST
func (g *IRGenerator) Generate(node *ASTNode) IRProgram {
	g.generateNode(node)
	return g.program
}

// generateNode generates IR for a node
func (g *IRGenerator) generateNode(node *ASTNode) string {
	if node == nil {
		return ""
	}

	switch node.Type {
	case PROGRAM:
		return g.generateProgram(node)
	case FUNCTION_DEF:
		return g.generateFunction(node)
	case STATEMENT:
		return g.generateStatement(node)
	case EXPRESSION:
		return g.generateExpression(node)
	default:
		return ""
	}
}

// generateProgram generates IR for the program node
func (g *IRGenerator) generateProgram(node *ASTNode) string {
	for _, child := range node.Children {
		g.generateNode(child)
	}
	return ""
}

// generateFunction generates IR for a function definition
func (g *IRGenerator) generateFunction(node *ASTNode) string {
	funcName := node.Value
	
	// Create a new function in the IR
	g.currentFunc = &IRFunction{
		Name:       funcName,
		Parameters: []string{},
		Blocks:     []*IRBlock{},
		ReturnType: TypeInt, // Default
	}
	
	// Create entry block
	entryBlock := &IRBlock{
		Label:        g.generateLabel("entry"),
		Instructions: []IRInstruction{},
	}
	g.currentFunc.EntryBlock = entryBlock
	g.currentFunc.Blocks = append(g.currentFunc.Blocks, entryBlock)
	g.currentBlock = entryBlock
	
	// Add function IR instruction
	g.emitInstruction(IRInstruction{
		Type:  IRFunc,
		Value: funcName,
	})
	
	// Process parameters
	if len(node.Children) > 0 && node.Children[0].Type == STATEMENT && node.Children[0].Value == "Parameters" {
		for _, param := range node.Children[0].Children {
			paramName := param.Value
			g.currentFunc.Parameters = append(g.currentFunc.Parameters, paramName)
			
			// Emit parameter instruction
			g.emitInstruction(IRInstruction{
				Type:  IRParam,
				Dest:  paramName,
				Value: paramName,
			})
			
			// Allocate space for parameter
			g.emitInstruction(IRInstruction{
				Type:  IRAlloc,
				Dest:  paramName,
				Value: paramName,
			})
		}
	}
	
	// Process function body
	for i := 1; i < len(node.Children); i++ {
		g.generateNode(node.Children[i])
	}
	
	// Add implicit return if not present
	lastInst := g.getLastInstruction()
	if lastInst == nil || lastInst.Type != IRRet {
		g.emitInstruction(IRInstruction{
			Type: IRRet,
		})
	}
	
	g.program.Functions = append(g.program.Functions, *g.currentFunc)
	return ""
}

// generateStatement generates IR for a statement
func (g *IRGenerator) generateStatement(node *ASTNode) string {
	if g.isDeclaration(node) {
		return g.generateDeclaration(node)
	}
	
	if node.Value == "return" {
		if len(node.Children) > 0 {
			result := g.generateNode(node.Children[0])
			g.emitInstruction(IRInstruction{
				Type: IRRet,
				Op1:  result,
			})
		} else {
			g.emitInstruction(IRInstruction{
				Type: IRRet,
			})
		}
		return ""
	}
	
	if node.Value == "Block" {
		for _, child := range node.Children {
			g.generateNode(child)
		}
		return ""
	}
	
	// Generate IR for child nodes
	for _, child := range node.Children {
		g.generateNode(child)
	}
	
	return ""
}

// isDeclaration checks if a node is a variable declaration
func (g *IRGenerator) isDeclaration(node *ASTNode) bool {
	if node.Type != STATEMENT {
		return false
	}
	
	return strings.HasPrefix(node.Value, "int ") ||
		strings.HasPrefix(node.Value, "char ") ||
		strings.HasPrefix(node.Value, "float ") ||
		strings.HasPrefix(node.Value, "double ")
}

// generateDeclaration generates IR for a variable declaration
func (g *IRGenerator) generateDeclaration(node *ASTNode) string {
	parts := strings.SplitN(node.Value, " ", 2)
	if len(parts) != 2 {
		return ""
	}
	
	varName := parts[1]
	
	// Allocate memory for the variable
	g.emitInstruction(IRInstruction{
		Type:  IRAlloc,
		Dest:  varName,
		Value: varName,
	})
	
	// Handle initialization if present
	if len(node.Children) > 0 {
		initialValue := g.generateNode(node.Children[0])
		g.emitInstruction(IRInstruction{
			Type:  IRStore,
			Dest:  varName,
			Op1:   initialValue,
			Value: varName,
		})
	}
	
	return varName
}

// generateExpression generates IR for an expression
func (g *IRGenerator) generateExpression(node *ASTNode) string {
	// Handle literal values
	if g.isNumericLiteral(node.Value) {
		temp := g.generateTemp()
		g.emitInstruction(IRInstruction{
			Type:  IRConst,
			Dest:  temp,
			Value: node.Value,
		})
		return temp
	}
	
	// Handle identifiers (variables)
	if g.isIdentifier(node.Value) {
		temp := g.generateTemp()
		g.emitInstruction(IRInstruction{
			Type:  IRLoad,
			Dest:  temp,
			Op1:   node.Value,
			Value: node.Value,
		})
		return temp
	}
	
	// Handle binary operations
	if len(node.Children) == 2 {
		left := g.generateNode(node.Children[0])
		right := g.generateNode(node.Children[1])
		result := g.generateTemp()
		
		var opType IRType
		switch node.Value {
		case "+":
			opType = IRAdd
		case "-":
			opType = IRSub
		case "*":
			opType = IRMul
		case "/":
			opType = IRDiv
		case "==", "!=", "<", ">", "<=", ">=":
			// Comparison operations
			g.emitInstruction(IRInstruction{
				Type:  IRCmp,
				Dest:  result,
				Op1:   left,
				Op2:   right,
				Value: node.Value,
			})
			return result
		default:
			return "" // Unsupported operation
		}
		
		g.emitInstruction(IRInstruction{
			Type:  opType,
			Dest:  result,
			Op1:   left,
			Op2:   right,
		})
		
		return result
	}
	
	// Handle function calls
	if node.Type == EXPRESSION && len(node.Children) >= 1 && node.Children[0].Type == EXPRESSION {
		// Assuming function call with name as value and arguments as children
		funcName := node.Value
		
		// Push arguments
		for _, arg := range node.Children {
			argValue := g.generateNode(arg)
			g.emitInstruction(IRInstruction{
				Type: IRPush,
				Op1:  argValue,
			})
		}
		
		// Call function
		result := g.generateTemp()
		g.emitInstruction(IRInstruction{
			Type:  IRCall,
			Dest:  result,
			Value: funcName,
		})
		
		// Pop arguments
		if len(node.Children) > 0 {
			g.emitInstruction(IRInstruction{
				Type:  IRPop,
				Value: strconv.Itoa(len(node.Children)),
			})
		}
		
		return result
	}
	
	return ""
}

// Helper methods

// isNumericLiteral checks if a string is a numeric literal
func (g *IRGenerator) isNumericLiteral(value string) bool {
	_, err := strconv.ParseFloat(value, 64)
	return err == nil
}

// isIdentifier checks if a string is an identifier
func (g *IRGenerator) isIdentifier(value string) bool {
	if len(value) == 0 {
		return false
	}
	
	if !unicode.IsLetter(rune(value[0])) && value[0] != '_' {
		return false
	}
	
	for _, ch := range value[1:] {
		if !unicode.IsLetter(ch) && !unicode.IsDigit(ch) && ch != '_' {
			return false
		}
	}
	
	return true
}

// generateTemp generates a temporary variable name
func (g *IRGenerator) generateTemp() string {
	temp := fmt.Sprintf("t%d", g.tempCounter)
	g.tempCounter++
	return temp
}

// generateLabel generates a label name
func (g *IRGenerator) generateLabel(prefix string) string {
	label := fmt.Sprintf("%s_%d", prefix, g.labelCounter)
	g.labelCounter++
	return label
}

// emitInstruction adds an instruction to the current block
func (g *IRGenerator) emitInstruction(instruction IRInstruction) {
	if g.currentBlock != nil {
		g.currentBlock.Instructions = append(g.currentBlock.Instructions, instruction)
	}
}

// getLastInstruction returns the last instruction in the current block
func (g *IRGenerator) getLastInstruction() *IRInstruction {
	if g.currentBlock != nil && len(g.currentBlock.Instructions) > 0 {
		return &g.currentBlock.Instructions[len(g.currentBlock.Instructions)-1]
	}
	return nil
}

// createBasicBlock creates a new basic block and adds it to the current function
func (g *IRGenerator) createBasicBlock(label string) *IRBlock {
	block := &IRBlock{
		Label:        label,
		Instructions: []IRInstruction{},
	}
	
	if g.currentFunc != nil {
		g.currentFunc.Blocks = append(g.currentFunc.Blocks, block)
	}
	
	return block
}

// printIR prints the IR for debugging
func (g *IRGenerator) PrintIR() string {
	var builder strings.Builder
	
	// Print globals
	for name, val := range g.program.Globals {
		builder.WriteString(fmt.Sprintf("GLOBAL %s = %s\n", name, val))
	}
	
	// Print string literals
	for str, label := range g.stringLiterals {
		builder.WriteString(fmt.Sprintf("STRING %s = \"%s\"\n", label, str))
	}
	
	builder.WriteString("\n")
	
	// Print functions
	for _, function := range g.program.Functions {
		builder.WriteString(fmt.Sprintf("FUNCTION %s:\n", function.Name))
		
		// Print parameters
		for _, param := range function.Parameters {
			builder.WriteString(fmt.Sprintf("  PARAM %s\n", param))
		}
		
		// Print blocks
		for _, block := range function.Blocks {
			builder.WriteString(fmt.Sprintf("%s:\n", block.Label))
			
			// Print instructions
			for _, inst := range block.Instructions {
				builder.WriteString(fmt.Sprintf("  %s", inst.Type))
				
				if inst.Dest != "" {
					builder.WriteString(fmt.Sprintf(" %s", inst.Dest))
				}
				
				if inst.Op1 != "" {
					builder.WriteString(fmt.Sprintf(", %s", inst.Op1))
				}
				
				if inst.Op2 != "" {
					builder.WriteString(fmt.Sprintf(", %s", inst.Op2))
				}
				
				if inst.Value != "" && inst.Type != IRFunc && inst.Type != IRConst {
					builder.WriteString(fmt.Sprintf(" ; %s", inst.Value))
				} else if inst.Type == IRConst {
					builder.WriteString(fmt.Sprintf(" = %s", inst.Value))
				}
				
				builder.WriteString("\n")
			}
			
			builder.WriteString("\n")
		}
	}
	
	return builder.String()
}
func (g *IRGenerator) generateVariableDeclaration(node *ASTNode) string {
	varName := node.Children[0].Value
	if len(node.Children) > 1 { // Has initializer
		initValue := g.GenerateIR(node.Children[1])
		g.AddInstruction(IRInstruction{
			Op:   ASSIGN,
			Dest: varName,
			Arg1: initValue,
		})
	}
	return varName
}

func (g *IRGenerator) generateIfStatement(node *ASTNode) {
	condition := g.GenerateIR(node.Children[0])
	thenLabel := g.NewLabel()
	endLabel := g.NewLabel()

	// Jump to else block if condition is false
	g.AddInstruction(IRInstruction{
		Op:    JZ,
		Arg1:  condition,
		Label: endLabel,
	})

	// Then block
	thenBlock := g.CreateBasicBlock(thenLabel)
	g.currentBlock = thenBlock
	g.GenerateIR(node.Children[1])

	// End block
	endBlock := g.CreateBasicBlock(endLabel)
	g.currentBlock = endBlock
}

func (g *IRGenerator) generateWhileStatement(node *ASTNode) {
	startLabel := g.NewLabel()
	bodyLabel := g.NewLabel()
	endLabel := g.NewLabel()

	// Start block (condition)
	startBlock := g.CreateBasicBlock(startLabel)
	g.currentBlock = startBlock
	condition := g.GenerateIR(node.Children[0])

	// Jump to end if condition is false
	g.AddInstruction(IRInstruction{
		Op:    JZ,
		Arg1:  condition,
		Label: endLabel,
	})

	// Body block
	bodyBlock := g.CreateBasicBlock(bodyLabel)
	g.currentBlock = bodyBlock
	g.GenerateIR(node.Children[1])

	// Jump back to condition
	g.AddInstruction(IRInstruction{
		Op:    JMP,
		Label: startLabel,
	})

	// End block
	endBlock := g.CreateBasicBlock(endLabel)
	g.currentBlock = endBlock
}

func (g *IRGenerator) generateFunctionDeclaration(node *ASTNode) {
	funcName := node.Children[0].Value
	funcLabel := g.NewLabel()

	// Function entry block
	funcBlock := g.CreateBasicBlock(funcLabel)
	g.currentBlock = funcBlock

	// Add function label
	g.AddInstruction(IRInstruction{
		Op:    LABEL,
		Label: funcName,
	})

	// Generate IR for function body
	g.GenerateIR(node.Children[len(node.Children)-1])

	// Add return instruction
	g.AddInstruction(IRInstruction{
		Op: RET,
	})
}
