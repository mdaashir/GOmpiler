package codegen

import (
	"strings"
	"testing"

	"github.com/mdaashir/GOmpiler-2/pkg/lexer"
	"github.com/mdaashir/GOmpiler-2/pkg/parser"
)

func generate(t *testing.T, src string, optimize bool) string {
	t.Helper()
	tokens, err := lexer.New(src, "test.cpp").Tokenize()
	if err != nil {
		t.Fatalf("lex error: %v", err)
	}
	program, err := parser.New(tokens).Parse()
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	asm, err := New(program, optimize).GenerateAssembly()
	if err != nil {
		t.Fatalf("codegen error: %v", err)
	}
	return asm
}

func TestPrintStringLiteral(t *testing.T) {
	asm := generate(t, `int main(){ std::cout << "Hello" << std::endl; return 0; }`, false)

	if !strings.Contains(asm, `.asciz "Hello"`) {
		t.Errorf("expected the string literal in .rodata:\n%s", asm)
	}
	if !strings.Contains(asm, "call __print_str") {
		t.Errorf("expected a call to __print_str:\n%s", asm)
	}
	if !strings.Contains(asm, "call __print_char") {
		t.Errorf("expected std::endl to emit a newline via __print_char:\n%s", asm)
	}
}

func TestExitWithReturnValue(t *testing.T) {
	asm := generate(t, `int main(){ return 42; }`, false)

	if !strings.Contains(asm, "movq $42, %rax") {
		t.Errorf("expected the return value to be loaded into rax:\n%s", asm)
	}
	if !strings.Contains(asm, "movq $60, %rax") {
		t.Errorf("expected the exit syscall number in _start:\n%s", asm)
	}
}

func TestFunctionArgumentsArePushedRightToLeft(t *testing.T) {
	asm := generate(t, `int add(int a, int b){ return a + b; }
int main(){ return add(1, 2); }`, false)

	if !strings.Contains(asm, "call add") {
		t.Errorf("expected a call to add:\n%s", asm)
	}
	if !strings.Contains(asm, "addq $16, %rsp") {
		t.Errorf("expected the caller to clean up two arguments:\n%s", asm)
	}
}

func TestConstantFoldingWithOptimize(t *testing.T) {
	src := `int main(){ int x = 5 * 4; return x; }`

	optimized := generate(t, src, true)
	if !strings.Contains(optimized, "movq $20, %rax") {
		t.Errorf("expected 5 * 4 to fold to 20:\n%s", optimized)
	}
	if strings.Contains(optimized, "imulq") {
		t.Errorf("expected the multiplication to be folded away:\n%s", optimized)
	}

	unoptimized := generate(t, src, false)
	if !strings.Contains(unoptimized, "imulq") {
		t.Errorf("expected the multiplication to remain without -O:\n%s", unoptimized)
	}
}

func TestDivisionUsesIdiv(t *testing.T) {
	asm := generate(t, `int main(){ return 17 / 5; }`, false)
	if !strings.Contains(asm, "idivq") {
		t.Errorf("expected signed division via idivq:\n%s", asm)
	}
}

func TestUnsupportedMemberAccessIsAnError(t *testing.T) {
	tokens, _ := lexer.New(`int main(){ return obj.field; }`, "test.cpp").Tokenize()
	program, err := parser.New(tokens).Parse()
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if _, err := New(program, false).GenerateAssembly(); err == nil {
		t.Fatal("expected code generation to reject member access")
	}
}

func TestGlobalVariableEmittedInData(t *testing.T) {
	asm := generate(t, `const int MAX = 100;
int main(){ return MAX; }`, false)

	if !strings.Contains(asm, ".data") || !strings.Contains(asm, "MAX:") {
		t.Errorf("expected the global to appear in .data:\n%s", asm)
	}
	if !strings.Contains(asm, "MAX(%rip)") {
		t.Errorf("expected RIP-relative access to the global:\n%s", asm)
	}
}
