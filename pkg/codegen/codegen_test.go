package codegen

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/mdaashir/go2cpp/pkg/lexer"
	"github.com/mdaashir/go2cpp/pkg/parser"
)

func generate(t *testing.T, src string) string {
	t.Helper()
	toks, err := lexer.New(src).Scan()
	if err != nil {
		t.Fatalf("lex: %v", err)
	}
	file, err := parser.New(toks).Parse()
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	out, err := Generate(file)
	if err != nil {
		t.Fatalf("generate: %v", err)
	}
	return out
}

func TestHello(t *testing.T) {
	out := generate(t, `package main
import "fmt"
func main() { fmt.Println("Hello, World!") }
`)
	if !strings.Contains(out, `_format_output(std::cout, "Hello, World!")`) {
		t.Errorf("expected a cout _format_output call, got:\n%s", out)
	}
	if !strings.Contains(out, "auto main() -> int") {
		t.Errorf("expected an int main, got:\n%s", out)
	}
}

func TestStringLiteralBecomesString(t *testing.T) {
	out := generate(t, `package main
func main() { msg := "hi"; _ = msg }
`)
	if !strings.Contains(out, `std::string msg = "hi";`) {
		t.Errorf("string literal should produce std::string, got:\n%s", out)
	}
}

func TestMultipleReturn(t *testing.T) {
	out := generate(t, `package main
func addsub(x int) (a, b int) { return x + 2, x - 2 }
func main() { y, z := addsub(4); _ = y; _ = z }
`)
	if !strings.Contains(out, "auto addsub(int x) -> std::tuple<int, int>") {
		t.Errorf("expected a tuple return type, got:\n%s", out)
	}
	if !strings.Contains(out, "std::get<0>(_ret") || !strings.Contains(out, "std::get<1>(_ret") {
		t.Errorf("expected tuple destructuring, got:\n%s", out)
	}
}

func TestIota(t *testing.T) {
	out := generate(t, `package main
const ( a = iota; b; c )
func main() { _ = a; _ = b; _ = c }
`)
	for _, want := range []string{"constexpr auto a = 0;", "constexpr auto b = 1;", "constexpr auto c = 2;"} {
		if !strings.Contains(out, want) {
			t.Errorf("expected %q, got:\n%s", want, out)
		}
	}
}

func TestStructStrMethod(t *testing.T) {
	out := generate(t, `package main
type Vec3 struct { X float64; Y float64 }
func main() { v := Vec3{1, 2}; _ = v }
`)
	if !strings.Contains(out, "class Vec3 { public:") {
		t.Errorf("expected a class, got:\n%s", out)
	}
	if !strings.Contains(out, "std::string _str()") {
		t.Errorf("expected a _str method, got:\n%s", out)
	}
}

func TestStringsHelpers(t *testing.T) {
	out := generate(t, `package main
import "strings"
func main() { _ = strings.Contains("a", "b") }
`)
	if !strings.Contains(out, "stringsContains(") {
		t.Errorf("expected stringsContains helper call, got:\n%s", out)
	}
	if !strings.Contains(out, "inline bool stringsContains") {
		t.Errorf("expected the stringsContains helper definition, got:\n%s", out)
	}
}

func TestNamedInterfaceDispatch(t *testing.T) {
	out := generate(t, `package main
type Animal interface{ Speak() string }
type Dog struct{ name string }
func (d *Dog) Speak() string { return d.name }
func main() { var a Animal = &Dog{name: "x"}; _ = a.Speak() }
`)
	if !strings.Contains(out, "class Animal { public:") {
		t.Errorf("expected an abstract Animal class, got:\n%s", out)
	}
	if !strings.Contains(out, "virtual std::string Speak() = 0;") {
		t.Errorf("expected a pure virtual Speak, got:\n%s", out)
	}
	if !strings.Contains(out, "class Dog : public Animal") {
		t.Errorf("expected Dog to derive from Animal, got:\n%s", out)
	}
}

func TestInterfaceAny(t *testing.T) {
	out := generate(t, `package main
func id(x interface{}) interface{} { return x }
func main() { _ = id(3) }
`)
	if !strings.Contains(out, "go2cpp::any") {
		t.Errorf("expected go2cpp::any, got:\n%s", out)
	}
	if !strings.Contains(out, "class any") {
		t.Errorf("expected the any helper definition, got:\n%s", out)
	}
}

func TestConcurrencyRuntime(t *testing.T) {
	out := generate(t, `package main
func main() {
	ch := make(chan int, 1)
	ch <- 1
	_ = <-ch
}
`)
	if !strings.Contains(out, "go2cpp::channel<int>") {
		t.Errorf("expected channel<int>, got:\n%s", out)
	}
	if !strings.Contains(out, "class channel") {
		t.Errorf("expected the channel helper definition, got:\n%s", out)
	}
}

// TestGenerateAllTestcases ensures every existing testcase lowers without error.
func TestGenerateAllTestcases(t *testing.T) {
	files, err := filepath.Glob(filepath.Join("..", "..", "testdata", "*.go"))
	if err != nil || len(files) == 0 {
		t.Fatalf("no testcases found: %v", err)
	}
	for _, f := range files {
		f := f
		t.Run(filepath.Base(f), func(t *testing.T) {
			src, err := os.ReadFile(f)
			if err != nil {
				t.Fatal(err)
			}
			toks, err := lexer.New(string(src)).Scan()
			if err != nil {
				t.Fatal(err)
			}
			file, err := parser.New(toks).Parse()
			if err != nil {
				t.Fatal(err)
			}
			if _, err := Generate(file); err != nil {
				t.Errorf("generate failed: %v", err)
			}
		})
	}
}
