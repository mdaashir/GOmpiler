package lexer

import "testing"

func scan(t *testing.T, src string) []Token {
	t.Helper()
	toks, err := New(src).Scan()
	if err != nil {
		t.Fatalf("Scan(%q) error: %v", src, err)
	}
	return toks
}

func TestBasicTokens(t *testing.T) {
	toks := scan(t, `package main`)
	want := []struct {
		typ Type
		lit string
	}{
		{KEYWORD, "package"},
		{IDENT, "main"},
		{OP, ";"},
		{EOF, ""},
	}
	if len(toks) != len(want) {
		t.Fatalf("got %d tokens %v, want %d", len(toks), toks, len(want))
	}
	for i, w := range want {
		if toks[i].Type != w.typ || toks[i].Literal != w.lit {
			t.Errorf("token %d = %s(%q), want %s(%q)", i, toks[i].Type, toks[i].Literal, w.typ, w.lit)
		}
	}
}

func TestSemicolonInsertion(t *testing.T) {
	toks := scan(t, "x := 1\ny := 2\n")
	// Expect: x := 1 ; y := 2 ; EOF
	semis := 0
	for _, tk := range toks {
		if tk.Type == OP && tk.Literal == ";" {
			semis++
		}
	}
	if semis != 2 {
		t.Fatalf("expected 2 inserted semicolons, got %d: %v", semis, toks)
	}
}

func TestNoSemicolonInsideParens(t *testing.T) {
	toks := scan(t, "f(\n1,\n2,\n)")
	depth := 0
	for _, tk := range toks {
		if tk.Type != OP {
			continue
		}
		switch tk.Literal {
		case "(":
			depth++
		case ")":
			depth--
		case ";":
			if depth > 0 {
				t.Fatalf("semicolon inserted inside parentheses: %v", toks)
			}
		}
	}
}

func TestNumbers(t *testing.T) {
	cases := map[string]Type{
		"42":     INT,
		"0xff":   INT,
		"0b1010": INT,
		"3.14":   FLOAT,
		"1e9":    FLOAT,
		"2i":     IMAG,
	}
	for src, want := range cases {
		toks := scan(t, src)
		if toks[0].Type != want {
			t.Errorf("Scan(%q)[0].Type = %s, want %s", src, toks[0].Type, want)
		}
	}
}

func TestStrings(t *testing.T) {
	toks := scan(t, "\"hello\\n\"")
	if toks[0].Type != STRING || toks[0].Literal != `"hello\n"` {
		t.Errorf("string token = %v", toks[0])
	}
	toks = scan(t, "`raw\nstring`")
	if toks[0].Type != STRING || toks[0].Literal != "`raw\nstring`" {
		t.Errorf("raw string token = %v", toks[0])
	}
	toks = scan(t, "'a'")
	if toks[0].Type != CHAR {
		t.Errorf("char token = %v", toks[0])
	}
}

func TestOperators(t *testing.T) {
	toks := scan(t, "a := b << 2")
	var ops []string
	for _, tk := range toks {
		if tk.Type == OP {
			ops = append(ops, tk.Literal)
		}
	}
	// :=, <<, ;
	want := map[string]bool{":=": true, "<<": true, ";": true}
	for _, op := range ops {
		if !want[op] {
			t.Errorf("unexpected operator %q in %v", op, ops)
		}
	}
}

func TestComments(t *testing.T) {
	toks := scan(t, "a // comment\n/* block */ b")
	var idents []string
	for _, tk := range toks {
		if tk.Type == IDENT {
			idents = append(idents, tk.Literal)
		}
	}
	if len(idents) != 2 || idents[0] != "a" || idents[1] != "b" {
		t.Fatalf("idents = %v, want [a b]", idents)
	}
}
