package parser

import (
	"strings"
	"testing"

	"github.com/mdaashir/GOmpiler-2/pkg/ast"
	"github.com/mdaashir/GOmpiler-2/pkg/lexer"
)

func mustParse(t *testing.T, src string) *ast.Program {
	t.Helper()
	tokens, err := lexer.New(src, "test.cpp").Tokenize()
	if err != nil {
		t.Fatalf("lex error: %v", err)
	}
	program, err := New(tokens).Parse()
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	return program
}

func firstFunction(t *testing.T, program *ast.Program) *ast.FunctionDeclaration {
	t.Helper()
	for _, decl := range program.Declarations {
		if fn, ok := decl.(*ast.FunctionDeclaration); ok {
			return fn
		}
	}
	t.Fatal("no function declaration found")
	return nil
}

func firstStatement(t *testing.T, fn *ast.FunctionDeclaration) ast.Statement {
	t.Helper()
	if fn.Body == nil || len(fn.Body.Statements) == 0 {
		t.Fatal("function has no body statements")
	}
	return fn.Body.Statements[0]
}

func TestOperatorPrecedence(t *testing.T) {
	fn := firstFunction(t, mustParse(t, `int main(){ int x = a + b * c; return x; }`))
	decl := firstStatement(t, fn).(*ast.DeclarationStatement)
	init := decl.Declaration.(*ast.VariableDeclaration).Initializer

	plus, ok := init.(*ast.BinaryExpression)
	if !ok || plus.Operator != "+" {
		t.Fatalf("expected top-level '+', got %#v", init)
	}
	times, ok := plus.Right.(*ast.BinaryExpression)
	if !ok || times.Operator != "*" {
		t.Fatalf("expected '*' as right operand of '+', got %#v", plus.Right)
	}
}

func TestParenthesesOverridePrecedence(t *testing.T) {
	fn := firstFunction(t, mustParse(t, `int main(){ int x = (a + b) * c; return x; }`))
	decl := firstStatement(t, fn).(*ast.DeclarationStatement)
	init := decl.Declaration.(*ast.VariableDeclaration).Initializer

	times, ok := init.(*ast.BinaryExpression)
	if !ok || times.Operator != "*" {
		t.Fatalf("expected top-level '*', got %#v", init)
	}
	if plus, ok := times.Left.(*ast.BinaryExpression); !ok || plus.Operator != "+" {
		t.Fatalf("expected '+' as left operand of '*', got %#v", times.Left)
	}
}

func TestAssignmentIsRightAssociative(t *testing.T) {
	fn := firstFunction(t, mustParse(t, `int main(){ a = b = 3; return 0; }`))
	stmt := firstStatement(t, fn).(*ast.ExpressionStatement)

	outer, ok := stmt.Expression.(*ast.AssignmentExpression)
	if !ok || outer.Operator != "=" {
		t.Fatalf("expected assignment, got %#v", stmt.Expression)
	}
	if _, ok := outer.Right.(*ast.AssignmentExpression); !ok {
		t.Fatalf("expected nested assignment on the right, got %#v", outer.Right)
	}
}

func TestIfElseParsing(t *testing.T) {
	fn := firstFunction(t, mustParse(t, `int main(){ if (x > 1) { y = 1; } else { y = 2; } return 0; }`))
	stmt := firstStatement(t, fn).(*ast.IfStatement)

	if cond, ok := stmt.Condition.(*ast.BinaryExpression); !ok || cond.Operator != ">" {
		t.Fatalf("expected '>' condition, got %#v", stmt.Condition)
	}
	if _, ok := stmt.Consequence.(*ast.BlockStatement); !ok {
		t.Fatalf("expected block consequence, got %#v", stmt.Consequence)
	}
	if stmt.Alternative == nil {
		t.Fatal("expected an else branch")
	}
}

func TestForLoopParsing(t *testing.T) {
	fn := firstFunction(t, mustParse(t, `int main(){ for (int i = 0; i < 5; i++) { x = i; } return 0; }`))
	stmt := firstStatement(t, fn).(*ast.ForStatement)

	if stmt.Initialization == nil || stmt.Condition == nil || stmt.Increment == nil {
		t.Fatalf("for loop header incomplete: %#v", stmt)
	}
	if init, ok := stmt.Initialization.(*ast.DeclarationStatement); !ok {
		t.Fatalf("expected declaration init, got %#v", stmt.Initialization)
	} else if init.Declaration.(*ast.VariableDeclaration).Name != "i" {
		t.Fatalf("expected loop variable i, got %#v", init.Declaration)
	}
	if incr, ok := stmt.Increment.(*ast.UnaryExpression); !ok || incr.Operator != "++" {
		t.Fatalf("expected '++' increment, got %#v", stmt.Increment)
	}
}

func TestCoutChainIsLeftAssociative(t *testing.T) {
	fn := firstFunction(t, mustParse(t, `int main(){ std::cout << "a" << x << std::endl; return 0; }`))
	stmt := firstStatement(t, fn).(*ast.ExpressionStatement)

	outer, ok := stmt.Expression.(*ast.BinaryExpression)
	if !ok || outer.Operator != "<<" {
		t.Fatalf("expected '<<' chain, got %#v", stmt.Expression)
	}
	if _, ok := outer.Right.(*ast.Identifier); !ok {
		t.Fatalf("expected std::endl on the right, got %#v", outer.Right)
	}
	inner, ok := outer.Left.(*ast.BinaryExpression)
	if !ok || inner.Operator != "<<" {
		t.Fatalf("expected nested '<<' on the left, got %#v", outer.Left)
	}
	head, ok := inner.Left.(*ast.BinaryExpression)
	if !ok {
		t.Fatalf("expected the head of the chain to be a '<<' expression, got %#v", inner.Left)
	}
	if id, ok := head.Left.(*ast.Identifier); !ok || id.Value != "std::cout" {
		t.Fatalf("expected std::cout at the head of the chain, got %#v", head.Left)
	}
}

func TestQualifiedName(t *testing.T) {
	fn := firstFunction(t, mustParse(t, `int main(){ std::cout << std::endl; return 0; }`))
	stmt := firstStatement(t, fn).(*ast.ExpressionStatement)
	outer := stmt.Expression.(*ast.BinaryExpression)
	head := outer.Left.(*ast.Identifier)
	if head.Value != "std::cout" {
		t.Fatalf("expected std::cout, got %q", head.Value)
	}
}

func TestCallArguments(t *testing.T) {
	fn := firstFunction(t, mustParse(t, `int main(){ return add(x, 2 * y, 3); }`))
	stmt := firstStatement(t, fn).(*ast.ReturnStatement)

	call, ok := stmt.Value.(*ast.CallExpression)
	if !ok {
		t.Fatalf("expected call, got %#v", stmt.Value)
	}
	if len(call.Arguments) != 3 {
		t.Fatalf("expected 3 arguments, got %d", len(call.Arguments))
	}
	if _, ok := call.Arguments[1].(*ast.BinaryExpression); !ok {
		t.Fatalf("expected binary expression argument, got %#v", call.Arguments[1])
	}
}

func TestUnsupportedConstructIsReported(t *testing.T) {
	src := `class Person { public: int age; }; int main(){ return 0; }`
	tokens, err := lexer.New(src, "test.cpp").Tokenize()
	if err != nil {
		t.Fatalf("lex error: %v", err)
	}
	_, err = New(tokens).Parse()
	if err == nil {
		t.Fatal("expected an error for an unsupported class declaration")
	}
	if !strings.Contains(err.Error(), "classes and structs are not supported") {
		t.Fatalf("unexpected error: %v", err)
	}
}

func TestParseErrorHasLineNumber(t *testing.T) {
	_, err := New(mustTokenize(t, "int main(){ return 1 }")).Parse()
	if err == nil {
		t.Fatal("expected a parse error for the missing semicolon")
	}
	if !strings.Contains(err.Error(), "line 1") {
		t.Fatalf("expected a line number in the error, got: %v", err)
	}
}

func mustTokenize(t *testing.T, src string) []lexer.Token {
	t.Helper()
	tokens, err := lexer.New(src, "test.cpp").Tokenize()
	if err != nil {
		t.Fatalf("lex error: %v", err)
	}
	return tokens
}
