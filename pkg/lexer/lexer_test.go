package lexer

import "testing"

func tokenize(t *testing.T, src string) []Token {
	t.Helper()
	tokens, err := New(src, "test.cpp").Tokenize()
	if err != nil {
		t.Fatalf("tokenize error: %v", err)
	}
	return tokens
}

func TestScopeResolutionOperator(t *testing.T) {
	tokens := tokenize(t, "std::cout")
	want := []struct {
		typ TokenType
		lit string
	}{
		{TokenIdentifier, "std"},
		{TokenOperator, "::"},
		{TokenIdentifier, "cout"},
		{TokenEOF, ""},
	}
	if len(tokens) != len(want) {
		t.Fatalf("expected %d tokens, got %d: %#v", len(want), len(tokens), tokens)
	}
	for i, w := range want {
		if tokens[i].Type != w.typ || tokens[i].Literal != w.lit {
			t.Errorf("token %d: got (%v, %q), want (%v, %q)", i, tokens[i].Type, tokens[i].Literal, w.typ, w.lit)
		}
	}
}

func TestDivideAssignOperator(t *testing.T) {
	tokens := tokenize(t, "x/=2")
	if tokens[1].Type != TokenOperator || tokens[1].Literal != "/=" {
		t.Fatalf("expected '/=' operator, got (%v, %q)", tokens[1].Type, tokens[1].Literal)
	}
}

func TestKeywordsAndIdentifiers(t *testing.T) {
	tokens := tokenize(t, "int main")
	if tokens[0].Type != TokenKeyword || tokens[0].Literal != "int" {
		t.Errorf("expected keyword int, got (%v, %q)", tokens[0].Type, tokens[0].Literal)
	}
	if tokens[1].Type != TokenIdentifier || tokens[1].Literal != "main" {
		t.Errorf("expected identifier main, got (%v, %q)", tokens[1].Type, tokens[1].Literal)
	}
}

func TestCommentsAndWhitespaceAreSkipped(t *testing.T) {
	tokens := tokenize(t, "// a comment\n/* block */ int")
	if len(tokens) != 2 {
		t.Fatalf("expected 2 tokens (int, EOF), got %d: %#v", len(tokens), tokens)
	}
	if tokens[0].Literal != "int" {
		t.Errorf("expected int, got %q", tokens[0].Literal)
	}
}

func TestStringEscapesArePreserved(t *testing.T) {
	tokens := tokenize(t, `"line\nbreak"`)
	if tokens[0].Type != TokenString {
		t.Fatalf("expected a string token, got %v", tokens[0].Type)
	}
	if tokens[0].Literal != `"line\nbreak"` {
		t.Errorf("expected escapes preserved, got %q", tokens[0].Literal)
	}
}

func TestPreprocessorDirective(t *testing.T) {
	tokens := tokenize(t, "#include <iostream>")
	if tokens[0].Type != TokenPreprocessor {
		t.Fatalf("expected a preprocessor token, got %v", tokens[0].Type)
	}
}
