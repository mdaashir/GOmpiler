package codegen

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mdaashir/GOmpiler-2/pkg/ast"
)

// Generator lowers an AST to x86-64 assembly (GNU as, AT&T syntax).
//
// Calling convention: arguments are pushed right-to-left by the caller and
// read by the callee at 16(%rbp), 24(%rbp), ... The return value is in %rax.
// Output is written directly to stdout with Linux syscalls, so no libc is
// required and the result is a self-contained static ELF executable.
type Generator struct {
	program  *ast.Program
	optimize bool

	text   strings.Builder
	data   strings.Builder
	rodata strings.Builder

	strings   map[string]string
	stringSeq int
	globals   map[string]bool
	err       error

	// per-function state
	slots        map[string]int
	loopStack    []loopLabels
	epilogueName string
}

type loopLabels struct {
	breakLabel    string
	continueLabel string
}

// New creates a code generator for the given program.
func New(program *ast.Program, optimize bool) *Generator {
	return &Generator{
		program:  program,
		optimize: optimize,
		strings:  make(map[string]string),
		globals:  make(map[string]bool),
	}
}

// GenerateAssembly produces the complete assembly source for the program.
func (g *Generator) GenerateAssembly() (string, error) {
	for _, decl := range g.program.Declarations {
		if vd, ok := decl.(*ast.VariableDeclaration); ok {
			g.globals[vd.Name] = true
		}
	}

	for _, decl := range g.program.Declarations {
		switch d := decl.(type) {
		case *ast.VariableDeclaration:
			g.genGlobal(d)
		case *ast.FunctionDeclaration:
			if d.Body != nil {
				g.genFunction(d)
			}
		}
	}

	if g.err != nil {
		return "", g.err
	}

	var out strings.Builder
	out.WriteString("\t.text\n")
	out.WriteString("\t.globl _start\n_start:\n")
	out.WriteString("\tcall main\n")
	out.WriteString("\tmovq %rax, %rdi\n")
	out.WriteString("\tmovq $60, %rax\n")
	out.WriteString("\tsyscall\n\n")
	out.WriteString(runtimeAsm)
	out.WriteString(g.text.String())

	if g.data.Len() > 0 {
		out.WriteString("\n\t.data\n")
		out.WriteString(g.data.String())
	}
	if g.rodata.Len() > 0 {
		out.WriteString("\n\t.section .rodata\n")
		out.WriteString(g.rodata.String())
	}
	out.WriteString("\n\t.section .note.GNU-stack,\"\",@progbits\n")
	return out.String(), nil
}

func (g *Generator) emit(format string, args ...interface{}) {
	fmt.Fprintf(&g.text, format+"\n", args...)
}

func (g *Generator) fail(format string, args ...interface{}) {
	if g.err == nil {
		g.err = fmt.Errorf(format, args...)
	}
}

func (g *Generator) internString(literal string) string {
	if label, ok := g.strings[literal]; ok {
		return label
	}
	label := fmt.Sprintf(".Lstr%d", g.stringSeq)
	g.stringSeq++
	g.strings[literal] = label
	fmt.Fprintf(&g.rodata, "%s:\n\t.asciz %s\n", label, literal)
	return label
}

func (g *Generator) genGlobal(vd *ast.VariableDeclaration) {
	value := int64(0)
	if n, ok := vd.Initializer.(*ast.NumberLiteral); ok {
		if parsed, ok := parseIntLiteral(n.Value); ok {
			value = parsed
		}
	}
	fmt.Fprintf(&g.data, "%s:\n\t.quad %d\n", vd.Name, value)
}

func (g *Generator) genFunction(fn *ast.FunctionDeclaration) {
	g.slots = make(map[string]int)
	g.loopStack = nil
	g.epilogueName = ".Lret_" + fn.Name

	for _, p := range fn.Parameters {
		if p.Name == "" || p.Name == "void" {
			continue
		}
		g.slotFor(p.Name)
	}
	collectLocals(fn.Body, g.slots)

	frameSize := align16(len(g.slots) * 8)

	g.emit("\n%s:", fn.Name)
	g.emit("\tpushq %%rbp")
	g.emit("\tmovq %%rsp, %%rbp")
	if frameSize > 0 {
		g.emit("\tsubq $%d, %%rsp", frameSize)
	}
	g.emit("\txorl %%eax, %%eax")

	// Copy incoming parameters into their stack slots.
	for i, p := range fn.Parameters {
		if p.Name == "" || p.Name == "void" {
			continue
		}
		g.emit("\tmovq %d(%%rbp), %%rax", 16+8*i)
		g.emit("\tmovq %%rax, %s(%%rbp)", g.slotOperand(p.Name))
	}

	for _, stmt := range fn.Body.Statements {
		g.genStatement(stmt)
	}

	g.emit("%s:", g.epilogueName)
	g.emit("\tleave")
	g.emit("\tret")
}

func (g *Generator) slotFor(name string) int {
	if idx, ok := g.slots[name]; ok {
		return idx
	}
	idx := len(g.slots) + 1
	g.slots[name] = idx
	return idx
}

func (g *Generator) slotOperand(name string) string {
	return fmt.Sprintf("-%d", 8*g.slots[name])
}

func collectLocals(stmt ast.Statement, slots map[string]int) {
	switch s := stmt.(type) {
	case *ast.BlockStatement:
		for _, inner := range s.Statements {
			collectLocals(inner, slots)
		}
	case *ast.DeclarationStatement:
		if vd, ok := s.Declaration.(*ast.VariableDeclaration); ok {
			if _, exists := slots[vd.Name]; !exists {
				slots[vd.Name] = len(slots) + 1
			}
		}
	case *ast.IfStatement:
		collectLocals(s.Consequence, slots)
		if s.Alternative != nil {
			collectLocals(s.Alternative, slots)
		}
	case *ast.ForStatement:
		if s.Initialization != nil {
			collectLocals(s.Initialization, slots)
		}
		collectLocals(s.Body, slots)
	case *ast.WhileStatement:
		collectLocals(s.Body, slots)
	case *ast.DoWhileStatement:
		collectLocals(s.Body, slots)
	}
}

func align16(n int) int {
	if n <= 0 {
		return 0
	}
	return ((n + 15) / 16) * 16
}

var labelSeq int

func (g *Generator) newLabel(prefix string) string {
	labelSeq++
	return fmt.Sprintf(".L%s%d", prefix, labelSeq)
}

func (g *Generator) genStatement(stmt ast.Statement) {
	if g.err != nil {
		return
	}
	switch s := stmt.(type) {
	case nil:
	case *ast.BlockStatement:
		for _, inner := range s.Statements {
			g.genStatement(inner)
		}
	case *ast.EmptyStatement:
	case *ast.DeclarationStatement:
		g.genDeclaration(s.Declaration)
	case *ast.ExpressionStatement:
		g.genExpressionStatement(s.Expression)
	case *ast.IfStatement:
		g.genIf(s)
	case *ast.ForStatement:
		g.genFor(s)
	case *ast.WhileStatement:
		g.genWhile(s)
	case *ast.DoWhileStatement:
		g.genDoWhile(s)
	case *ast.ReturnStatement:
		if s.Value != nil {
			g.genExpr(s.Value)
		} else {
			g.emit("\txorl %%eax, %%eax")
		}
		g.emit("\tjmp %s", g.epilogueName)
	case *ast.BreakStatement:
		if len(g.loopStack) == 0 {
			g.fail("break outside of a loop")
			return
		}
		g.emit("\tjmp %s", g.loopStack[len(g.loopStack)-1].breakLabel)
	case *ast.ContinueStatement:
		if len(g.loopStack) == 0 {
			g.fail("continue outside of a loop")
			return
		}
		g.emit("\tjmp %s", g.loopStack[len(g.loopStack)-1].continueLabel)
	}
}

func (g *Generator) genDeclaration(decl ast.Declaration) {
	vd, ok := decl.(*ast.VariableDeclaration)
	if !ok {
		return
	}
	if vd.Initializer != nil {
		g.genExpr(vd.Initializer)
	} else {
		g.emit("\txorl %%eax, %%eax")
	}
	g.emit("\tmovq %%rax, %s(%%rbp)", g.slotOperand(vd.Name))
}

func (g *Generator) genExpressionStatement(expr ast.Expression) {
	if be, ok := expr.(*ast.BinaryExpression); ok && be.Operator == "<<" {
		if g.genOutputChain(be) {
			return
		}
	}
	g.genExpr(expr)
}

// genOutputChain detects a `std::cout << ...` chain and emits print calls for
// each streamed value. It reports whether expr was such a chain.
func (g *Generator) genOutputChain(expr ast.Expression) bool {
	parts := flattenStream(expr)
	if len(parts) < 2 {
		return false
	}
	stream, ok := parts[0].(*ast.Identifier)
	if !ok || (stream.Value != "std::cout" && stream.Value != "std::cerr") {
		return false
	}
	for _, arg := range parts[1:] {
		g.genPrintArg(arg)
	}
	return true
}

func flattenStream(expr ast.Expression) []ast.Expression {
	if be, ok := expr.(*ast.BinaryExpression); ok && be.Operator == "<<" {
		return append(flattenStream(be.Left), be.Right)
	}
	return []ast.Expression{expr}
}

func (g *Generator) genPrintArg(arg ast.Expression) {
	if id, ok := arg.(*ast.Identifier); ok && id.Value == "std::endl" {
		g.emit("\tmovq $10, %%rdi")
		g.emit("\tcall __print_char")
		return
	}
	if str, ok := arg.(*ast.StringLiteral); ok {
		label := g.internString(str.Value)
		g.emit("\tleaq %s(%%rip), %%rdi", label)
		g.emit("\tcall __print_str")
		return
	}
	if ch, ok := arg.(*ast.CharLiteral); ok {
		g.emit("\tmovq $%d, %%rdi", charCode(ch.Value))
		g.emit("\tcall __print_char")
		return
	}
	g.genExpr(arg)
	g.emit("\tmovq %%rax, %%rdi")
	g.emit("\tcall __print_int")
}

func (g *Generator) genIf(s *ast.IfStatement) {
	elseLabel := g.newLabel("else")
	endLabel := g.newLabel("endif")

	g.genExpr(s.Condition)
	g.emit("\tcmpq $0, %%rax")
	g.emit("\tje %s", elseLabel)

	g.genStatement(s.Consequence)
	if s.Alternative != nil {
		g.emit("\tjmp %s", endLabel)
	}
	g.emit("%s:", elseLabel)
	if s.Alternative != nil {
		g.genStatement(s.Alternative)
		g.emit("%s:", endLabel)
	}
}

func (g *Generator) genFor(s *ast.ForStatement) {
	condLabel := g.newLabel("forcond")
	contLabel := g.newLabel("forcont")
	endLabel := g.newLabel("forend")

	if s.Initialization != nil {
		g.genStatement(s.Initialization)
	}
	g.emit("%s:", condLabel)
	if s.Condition != nil {
		g.genExpr(s.Condition)
		g.emit("\tcmpq $0, %%rax")
		g.emit("\tje %s", endLabel)
	}

	g.loopStack = append(g.loopStack, loopLabels{breakLabel: endLabel, continueLabel: contLabel})
	g.genStatement(s.Body)
	g.loopStack = g.loopStack[:len(g.loopStack)-1]

	g.emit("%s:", contLabel)
	if s.Increment != nil {
		g.genExpr(s.Increment)
	}
	g.emit("\tjmp %s", condLabel)
	g.emit("%s:", endLabel)
}

func (g *Generator) genWhile(s *ast.WhileStatement) {
	condLabel := g.newLabel("whilecond")
	endLabel := g.newLabel("whileend")

	g.emit("%s:", condLabel)
	g.genExpr(s.Condition)
	g.emit("\tcmpq $0, %%rax")
	g.emit("\tje %s", endLabel)

	g.loopStack = append(g.loopStack, loopLabels{breakLabel: endLabel, continueLabel: condLabel})
	g.genStatement(s.Body)
	g.loopStack = g.loopStack[:len(g.loopStack)-1]

	g.emit("\tjmp %s", condLabel)
	g.emit("%s:", endLabel)
}

func (g *Generator) genDoWhile(s *ast.DoWhileStatement) {
	bodyLabel := g.newLabel("dobody")
	condLabel := g.newLabel("docond")
	endLabel := g.newLabel("doend")

	g.emit("%s:", bodyLabel)
	g.loopStack = append(g.loopStack, loopLabels{breakLabel: endLabel, continueLabel: condLabel})
	g.genStatement(s.Body)
	g.loopStack = g.loopStack[:len(g.loopStack)-1]

	g.emit("%s:", condLabel)
	g.genExpr(s.Condition)
	g.emit("\tcmpq $0, %%rax")
	g.emit("\tjne %s", bodyLabel)
	g.emit("%s:", endLabel)
}

func (g *Generator) genExpr(expr ast.Expression) {
	if g.err != nil {
		return
	}
	if g.optimize {
		if folded, ok := foldConstant(expr); ok {
			g.emit("\tmovq $%d, %%rax", folded)
			return
		}
	}
	switch e := expr.(type) {
	case nil:
		g.emit("\txorl %%eax, %%eax")
	case *ast.NumberLiteral:
		value, ok := parseIntLiteral(e.Value)
		if !ok {
			g.fail("unsupported numeric literal %q", e.Value)
			return
		}
		g.emit("\tmovq $%d, %%rax", value)
	case *ast.StringLiteral:
		label := g.internString(e.Value)
		g.emit("\tleaq %s(%%rip), %%rax", label)
	case *ast.CharLiteral:
		g.emit("\tmovq $%d, %%rax", charCode(e.Value))
	case *ast.BooleanLiteral:
		if e.Value {
			g.emit("\tmovq $1, %%rax")
		} else {
			g.emit("\txorl %%eax, %%eax")
		}
	case *ast.NullExpression:
		g.emit("\txorl %%eax, %%eax")
	case *ast.Identifier:
		g.genLoad(e.Value)
	case *ast.AssignmentExpression:
		g.genAssign(e)
	case *ast.BinaryExpression:
		g.genBinary(e)
	case *ast.UnaryExpression:
		g.genUnary(e)
	case *ast.CallExpression:
		g.genCall(e)
	case *ast.IndexExpression:
		g.fail("array indexing is not supported")
	case *ast.MemberExpression:
		g.fail("member access is not supported")
	default:
		g.fail("unsupported expression")
	}
}

func (g *Generator) genLoad(name string) {
	if _, ok := g.slots[name]; ok {
		g.emit("\tmovq %s(%%rbp), %%rax", g.slotOperand(name))
		return
	}
	if g.globals[name] {
		g.emit("\tmovq %s(%%rip), %%rax", name)
		return
	}
	g.fail("reference to undefined variable %q", name)
}

func (g *Generator) storeIdent(name string) {
	if _, ok := g.slots[name]; ok {
		g.emit("\tmovq %%rax, %s(%%rbp)", g.slotOperand(name))
		return
	}
	if g.globals[name] {
		g.emit("\tmovq %%rax, %s(%%rip)", name)
		return
	}
	g.fail("assignment to undefined variable %q", name)
}

func (g *Generator) genAssign(e *ast.AssignmentExpression) {
	id, ok := e.Left.(*ast.Identifier)
	if !ok {
		g.fail("assignment target is not supported")
		return
	}

	if e.Operator == "=" {
		g.genExpr(e.Right)
		g.storeIdent(id.Value)
		return
	}

	g.genLoad(id.Value)
	g.emit("\tpushq %%rax")
	g.genExpr(e.Right)
	g.emit("\tmovq %%rax, %%rcx")
	g.emit("\tpopq %%rax")
	switch e.Operator {
	case "+=":
		g.emit("\taddq %%rcx, %%rax")
	case "-=":
		g.emit("\tsubq %%rcx, %%rax")
	case "*=":
		g.emit("\timulq %%rcx, %%rax")
	case "/=":
		g.emit("\tmovq %%rcx, %%r10")
		g.emit("\tcqto")
		g.emit("\tidivq %%r10")
	case "%=":
		g.emit("\tmovq %%rcx, %%r10")
		g.emit("\tcqto")
		g.emit("\tidivq %%r10")
		g.emit("\tmovq %%rdx, %%rax")
	}
	g.storeIdent(id.Value)
}

func (g *Generator) genBinary(e *ast.BinaryExpression) {
	switch e.Operator {
	case "&&":
		g.genLogicalAnd(e)
		return
	case "||":
		g.genLogicalOr(e)
		return
	}

	g.genExpr(e.Left)
	g.emit("\tpushq %%rax")
	g.genExpr(e.Right)
	g.emit("\tpopq %%rcx")

	switch e.Operator {
	case "+":
		g.emit("\taddq %%rcx, %%rax")
	case "-":
		g.emit("\tsubq %%rax, %%rcx")
		g.emit("\tmovq %%rcx, %%rax")
	case "*":
		g.emit("\timulq %%rcx, %%rax")
	case "/":
		g.emit("\tmovq %%rax, %%r10")
		g.emit("\tmovq %%rcx, %%rax")
		g.emit("\tcqto")
		g.emit("\tidivq %%r10")
	case "%":
		g.emit("\tmovq %%rax, %%r10")
		g.emit("\tmovq %%rcx, %%rax")
		g.emit("\tcqto")
		g.emit("\tidivq %%r10")
		g.emit("\tmovq %%rdx, %%rax")
	case "==":
		g.emit("\tcmpq %%rax, %%rcx")
		g.emit("\tsete %%al")
		g.emit("\tmovzbl %%al, %%eax")
	case "!=":
		g.emit("\tcmpq %%rax, %%rcx")
		g.emit("\tsetne %%al")
		g.emit("\tmovzbl %%al, %%eax")
	case "<":
		g.emit("\tcmpq %%rax, %%rcx")
		g.emit("\tsetl %%al")
		g.emit("\tmovzbl %%al, %%eax")
	case ">":
		g.emit("\tcmpq %%rax, %%rcx")
		g.emit("\tsetg %%al")
		g.emit("\tmovzbl %%al, %%eax")
	case "<=":
		g.emit("\tcmpq %%rax, %%rcx")
		g.emit("\tsetle %%al")
		g.emit("\tmovzbl %%al, %%eax")
	case ">=":
		g.emit("\tcmpq %%rax, %%rcx")
		g.emit("\tsetge %%al")
		g.emit("\tmovzbl %%al, %%eax")
	case "<<":
		g.emit("\tmovq %%rcx, %%r11")
		g.emit("\tmovq %%rax, %%rcx")
		g.emit("\tshlq %%cl, %%r11")
		g.emit("\tmovq %%r11, %%rax")
	case ">>":
		g.emit("\tmovq %%rcx, %%r11")
		g.emit("\tmovq %%rax, %%rcx")
		g.emit("\tsarq %%cl, %%r11")
		g.emit("\tmovq %%r11, %%rax")
	default:
		g.fail("unsupported binary operator %q", e.Operator)
	}
}

func (g *Generator) genLogicalAnd(e *ast.BinaryExpression) {
	falseLabel := g.newLabel("andfalse")
	endLabel := g.newLabel("andend")

	g.genExpr(e.Left)
	g.emit("\tcmpq $0, %%rax")
	g.emit("\tje %s", falseLabel)
	g.genExpr(e.Right)
	g.emit("\tcmpq $0, %%rax")
	g.emit("\tje %s", falseLabel)
	g.emit("\tmovq $1, %%rax")
	g.emit("\tjmp %s", endLabel)
	g.emit("%s:", falseLabel)
	g.emit("\txorl %%eax, %%eax")
	g.emit("%s:", endLabel)
}

func (g *Generator) genLogicalOr(e *ast.BinaryExpression) {
	trueLabel := g.newLabel("ortrue")
	endLabel := g.newLabel("orend")

	g.genExpr(e.Left)
	g.emit("\tcmpq $0, %%rax")
	g.emit("\tjne %s", trueLabel)
	g.genExpr(e.Right)
	g.emit("\tcmpq $0, %%rax")
	g.emit("\tjne %s", trueLabel)
	g.emit("\txorl %%eax, %%eax")
	g.emit("\tjmp %s", endLabel)
	g.emit("%s:", trueLabel)
	g.emit("\tmovq $1, %%rax")
	g.emit("%s:", endLabel)
}

func (g *Generator) genUnary(e *ast.UnaryExpression) {
	switch e.Operator {
	case "++", "--":
		id, ok := e.Right.(*ast.Identifier)
		if !ok {
			g.fail("operand of %q is not supported", e.Operator)
			return
		}
		g.genLoad(id.Value)
		if e.Operator == "++" {
			g.emit("\taddq $1, %%rax")
		} else {
			g.emit("\tsubq $1, %%rax")
		}
		g.storeIdent(id.Value)
	case "-":
		g.genExpr(e.Right)
		g.emit("\tnegq %%rax")
	case "+":
		g.genExpr(e.Right)
	case "!":
		g.genExpr(e.Right)
		g.emit("\tcmpq $0, %%rax")
		g.emit("\tsete %%al")
		g.emit("\tmovzbl %%al, %%eax")
	case "~":
		g.genExpr(e.Right)
		g.emit("\tnotq %%rax")
	default:
		g.fail("unsupported unary operator %q", e.Operator)
	}
}

func (g *Generator) genCall(call *ast.CallExpression) {
	id, ok := call.Function.(*ast.Identifier)
	if !ok {
		g.fail("only direct function calls are supported")
		return
	}
	for i := len(call.Arguments) - 1; i >= 0; i-- {
		g.genExpr(call.Arguments[i])
		g.emit("\tpushq %%rax")
	}
	g.emit("\tcall %s", id.Value)
	if n := len(call.Arguments); n > 0 {
		g.emit("\taddq $%d, %%rsp", n*8)
	}
}

// foldConstant evaluates an expression made entirely of integer literals.
func foldConstant(expr ast.Expression) (int64, bool) {
	switch e := expr.(type) {
	case *ast.NumberLiteral:
		return parseIntLiteral(e.Value)
	case *ast.UnaryExpression:
		if e.Operator == "-" || e.Operator == "+" || e.Operator == "~" {
			v, ok := foldConstant(e.Right)
			if !ok {
				return 0, false
			}
			switch e.Operator {
			case "-":
				return -v, true
			case "~":
				return ^v, true
			default:
				return v, true
			}
		}
	case *ast.BinaryExpression:
		left, okLeft := foldConstant(e.Left)
		right, okRight := foldConstant(e.Right)
		if !okLeft || !okRight {
			return 0, false
		}
		switch e.Operator {
		case "+":
			return left + right, true
		case "-":
			return left - right, true
		case "*":
			return left * right, true
		case "/":
			if right == 0 {
				return 0, false
			}
			return left / right, true
		case "%":
			if right == 0 {
				return 0, false
			}
			return left % right, true
		case "==":
			return boolToInt(left == right), true
		case "!=":
			return boolToInt(left != right), true
		case "<":
			return boolToInt(left < right), true
		case ">":
			return boolToInt(left > right), true
		case "<=":
			return boolToInt(left <= right), true
		case ">=":
			return boolToInt(left >= right), true
		}
	}
	return 0, false
}

func boolToInt(b bool) int64 {
	if b {
		return 1
	}
	return 0
}

func parseIntLiteral(raw string) (int64, bool) {
	s := strings.TrimSpace(raw)
	s = strings.TrimRight(s, "uUlL")
	if s == "" {
		return 0, false
	}
	lower := strings.ToLower(s)
	if !strings.HasPrefix(lower, "0x") && strings.ContainsAny(s, ".eE") {
		return 0, false
	}
	if v, err := strconv.ParseInt(s, 0, 64); err == nil {
		return v, true
	}
	if u, err := strconv.ParseUint(s, 0, 64); err == nil {
		return int64(u), true
	}
	return 0, false
}

func charCode(literal string) int {
	s := literal
	if len(literal) >= 2 && literal[0] == '\'' && literal[len(literal)-1] == '\'' {
		s = literal[1 : len(literal)-1]
	}
	if strings.HasPrefix(s, "\\") && len(s) >= 2 {
		switch s[1] {
		case 'n':
			return 10
		case 't':
			return 9
		case 'r':
			return 13
		case '0':
			return 0
		case '\\':
			return 92
		case '\'':
			return 39
		case '"':
			return 34
		}
	}
	if r := []rune(s); len(r) > 0 {
		return int(r[0])
	}
	return 0
}

const runtimeAsm = `
__print_str:
	movq %rdi, %rsi
	xorl %edx, %edx
1:	cmpb $0, (%rsi,%rdx)
	je 2f
	incq %rdx
	jmp 1b
2:	movq $1, %rax
	movq $1, %rdi
	syscall
	ret

__print_char:
	subq $8, %rsp
	movb %dil, (%rsp)
	movq $1, %rax
	movq $1, %rdi
	movq %rsp, %rsi
	movq $1, %rdx
	syscall
	addq $8, %rsp
	ret

__print_int:
	subq $32, %rsp
	movq %rdi, %rax
	leaq 31(%rsp), %rsi
	movb $0, (%rsi)
	movq $10, %rcx
	testq %rax, %rax
	jne 1f
	decq %rsi
	movb $48, (%rsi)
	jmp 4f
1:	movq $1, %r8
	testq %rax, %rax
	jns 2f
	negq %rax
	movq $-1, %r8
2:	xorl %edx, %edx
3:	divq %rcx
	addb $48, %dl
	decq %rsi
	movb %dl, (%rsi)
	testq %rax, %rax
	jne 2b
	cmpq $-1, %r8
	jne 4f
	decq %rsi
	movb $45, (%rsi)
4:	leaq 31(%rsp), %rdx
	subq %rsi, %rdx
	movq $1, %rax
	movq $1, %rdi
	syscall
	addq $32, %rsp
	ret

`
