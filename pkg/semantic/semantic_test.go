package semantic

import (
	"strings"
	"testing"

	"github.com/mdaashir/GOmpiler-2/pkg/lexer"
	"github.com/mdaashir/GOmpiler-2/pkg/parser"
)

func analyze(t *testing.T, src string) error {
	t.Helper()
	tokens, err := lexer.New(src, "test.cpp").Tokenize()
	if err != nil {
		t.Fatalf("lex error: %v", err)
	}
	program, err := parser.New(tokens).Parse()
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	return New().Analyze(program)
}

func TestAcceptsWellFormedProgram(t *testing.T) {
	src := `int add(int a, int b){ return a + b; }
int main(){
    int x = 10;
    int y = add(x, 20);
    if (y > 25) { std::cout << "big" << std::endl; }
    for (int i = 0; i < 3; i++) { y = y + i; }
    return y;
}`
	if err := analyze(t, src); err != nil {
		t.Fatalf("expected success, got: %v", err)
	}
}

// A local declared in a block must be visible for the rest of that block.
func TestLocalVariableResolution(t *testing.T) {
	src := `int main(){ int a = 1; int b = a + 2; return b; }`
	if err := analyze(t, src); err != nil {
		t.Fatalf("expected success, got: %v", err)
	}
}

func TestForLoopVariableIsVisibleInBody(t *testing.T) {
	src := `int main(){ int total = 0; for (int i = 0; i < 5; i++) { total = total + i; } return total; }`
	if err := analyze(t, src); err != nil {
		t.Fatalf("expected success, got: %v", err)
	}
}

func TestUndefinedIdentifier(t *testing.T) {
	err := analyze(t, `int main(){ return z; }`)
	if err == nil || !strings.Contains(err.Error(), "undefined identifier") {
		t.Fatalf("expected undefined identifier error, got: %v", err)
	}
}

func TestArityMismatch(t *testing.T) {
	src := `int add(int a, int b){ return a + b; }
int main(){ return add(1); }`
	err := analyze(t, src)
	if err == nil || !strings.Contains(err.Error(), "expects 2 argument") {
		t.Fatalf("expected arity error, got: %v", err)
	}
}

func TestCallToUndefinedFunction(t *testing.T) {
	err := analyze(t, `int main(){ return nope(1); }`)
	if err == nil || !strings.Contains(err.Error(), "undefined function") {
		t.Fatalf("expected undefined function error, got: %v", err)
	}
}

func TestPrototypeThenDefinitionIsAllowed(t *testing.T) {
	src := `int add(int a, int b);
int main(){ return add(1, 2); }
int add(int a, int b){ return a + b; }`
	if err := analyze(t, src); err != nil {
		t.Fatalf("expected success, got: %v", err)
	}
}

func TestDuplicateVariableIsRejected(t *testing.T) {
	err := analyze(t, `int main(){ int a = 1; int a = 2; return a; }`)
	if err == nil || !strings.Contains(err.Error(), "already declared") {
		t.Fatalf("expected duplicate declaration error, got: %v", err)
	}
}

func TestUnknownTypeIsRejected(t *testing.T) {
	err := analyze(t, `int main(){ Widget w = 0; return 0; }`)
	if err == nil || !strings.Contains(err.Error(), "unknown type") {
		t.Fatalf("expected unknown type error, got: %v", err)
	}
}

func TestAssignmentToNonLValueIsRejected(t *testing.T) {
	err := analyze(t, `int main(){ 1 + 2 = 3; return 0; }`)
	if err == nil || !strings.Contains(err.Error(), "not assignable") {
		t.Fatalf("expected lvalue error, got: %v", err)
	}
}

func TestBuiltinStreamObjectsAreKnown(t *testing.T) {
	if err := analyze(t, `int main(){ std::cout << "hi" << std::endl; return 0; }`); err != nil {
		t.Fatalf("expected std::cout/std::endl to resolve, got: %v", err)
	}
}
