// Package parser builds an ast.File from a token stream produced by pkg/lexer.
//
// It is a hand-written recursive-descent parser with precedence climbing for
// binary expressions. Errors are recorded on the parser (a "sticky" error) and
// every recursive helper returns early once one is set, so no panic/recover is
// needed.
package parser

import (
	"fmt"

	"github.com/mdaashir/go2cpp/pkg/ast"
	"github.com/mdaashir/go2cpp/pkg/lexer"
)

// rawItem is one comma-separated item while parsing a parameter list.
type rawItem struct {
	isDeferredIdent bool
	name            string
	names           []string
	typ             ast.Expr
}

// Parser consumes tokens.
type Parser struct {
	toks []lexer.Token
	pos  int
	// inControlHeader is true while parsing the header of if/for/switch, where
	// `{` must not be taken as the start of a composite literal of a bare type
	// name (mirroring the restriction in the Go spec).
	inControlHeader bool
	// err is the first error encountered. Once set, parsing unwinds.
	err error
}

// New returns a Parser over the given tokens.
func New(toks []lexer.Token) *Parser {
	return &Parser{toks: toks}
}

// ---------------------------------------------------------------- token helpers

func (p *Parser) cur() lexer.Token {
	if p.pos >= len(p.toks) {
		return lexer.Token{Type: lexer.EOF}
	}
	return p.toks[p.pos]
}

func (p *Parser) peek() lexer.Token {
	if p.pos+1 >= len(p.toks) {
		return lexer.Token{Type: lexer.EOF}
	}
	return p.toks[p.pos+1]
}

func (p *Parser) next() lexer.Token {
	t := p.cur()
	if p.pos < len(p.toks) {
		p.pos++
	}
	return t
}

func (p *Parser) at(lit string) bool {
	t := p.cur()
	return (t.Type == lexer.OP || t.Type == lexer.KEYWORD) && t.Literal == lit
}

func (p *Parser) atType(typ lexer.Type) bool { return p.cur().Type == typ }

func (p *Parser) accept(lit string) bool {
	if p.at(lit) {
		p.next()
		return true
	}
	return false
}

func (p *Parser) expect(lit string) lexer.Token {
	if !p.at(lit) {
		p.fail("expected %q, got %s", lit, p.cur())
	}
	return p.next()
}

func (p *Parser) fail(format string, args ...interface{}) {
	if p.err == nil {
		p.err = fmt.Errorf("%d:%d: %s", p.cur().Line, p.cur().Col, fmt.Sprintf(format, args...))
	}
}

func (p *Parser) semi() {
	p.accept(";")
}

func (p *Parser) posOf(t lexer.Token) ast.Position {
	return ast.Position{Line: t.Line, Col: t.Col}
}

// Parse parses a whole source file.
func (p *Parser) Parse() (*ast.File, error) {
	f := p.parseFile()
	if p.err != nil {
		return nil, fmt.Errorf("parser: %s", p.err)
	}
	return f, nil
}

func (p *Parser) parseFile() *ast.File {
	if p.err != nil {
		return nil
	}
	start := p.cur()
	p.expect("package")
	name := p.cur()
	if name.Type != lexer.IDENT {
		p.fail("expected package name, got %s", name)
		return nil
	}
	p.next()
	p.semi()

	file := &ast.File{Position: p.posOf(start), Package: name.Literal}
	for p.at("import") {
		file.Imports = append(file.Imports, p.parseImport()...)
	}
	for !p.atType(lexer.EOF) && p.err == nil {
		file.Decls = append(file.Decls, p.parseDecl())
	}
	return file
}

func (p *Parser) parseImport() []*ast.ImportSpec {
	if p.err != nil {
		return nil
	}
	start := p.expect("import")
	var specs []*ast.ImportSpec
	if p.accept("(") {
		for !p.at(")") && !p.atType(lexer.EOF) && p.err == nil {
			specs = append(specs, p.parseImportSpec(start))
			p.semi()
		}
		p.expect(")")
	} else {
		specs = append(specs, p.parseImportSpec(start))
	}
	p.semi()
	return specs
}

func (p *Parser) parseImportSpec(start lexer.Token) *ast.ImportSpec {
	if p.err != nil {
		return nil
	}
	spec := &ast.ImportSpec{Position: p.posOf(start)}
	if p.atType(lexer.IDENT) || p.at(".") {
		spec.Name = p.next().Literal
	}
	if !p.atType(lexer.STRING) {
		p.fail("expected import path, got %s", p.cur())
		return spec
	}
	spec.Path = p.next().Literal
	return spec
}

func (p *Parser) parseDecl() ast.Decl {
	if p.err != nil {
		return nil
	}
	start := p.cur()
	switch {
	case p.at("func"):
		return p.parseFuncDecl()
	case p.at("var"), p.at("const"), p.at("type"):
		tok := p.next().Literal
		return p.parseGenDecl(start, tok)
	default:
		p.fail("unexpected %s at top level", p.cur())
		return nil
	}
}

func (p *Parser) parseFuncDecl() *ast.FuncDecl {
	if p.err != nil {
		return nil
	}
	start := p.expect("func")
	fn := &ast.FuncDecl{Position: p.posOf(start)}
	// method receiver
	if p.at("(") {
		fn.Recv = p.parseFieldList()
	}
	name := p.cur()
	if name.Type != lexer.IDENT {
		p.fail("expected function name, got %s", name)
		return nil
	}
	p.next()
	fn.Name = name.Literal
	fn.Type = p.parseFuncSignature()
	if p.at("{") {
		fn.Body = p.parseBlock()
	}
	p.semi()
	return fn
}

// parseFuncSignature parses `(params) results`.
func (p *Parser) parseFuncSignature() *ast.FuncType {
	if p.err != nil {
		return nil
	}
	start := p.cur()
	ft := &ast.FuncType{Position: p.posOf(start)}
	params := p.parseFieldList()
	ft.Params = params.List
	// results
	if p.at("(") {
		results := p.parseFieldList()
		ft.Results = results.List
	} else if p.startsType() && !p.at("{") {
		// single unnamed result
		ft.Results = []*ast.Field{{Position: p.posOf(p.cur()), Type: p.parseType()}}
	}
	return ft
}

func (p *Parser) parseGenDecl(start lexer.Token, tok string) *ast.GenDecl {
	if p.err != nil {
		return nil
	}
	gd := &ast.GenDecl{Position: p.posOf(start), Tok: tok}
	if p.accept("(") {
		for !p.at(")") && !p.atType(lexer.EOF) && p.err == nil {
			gd.Specs = append(gd.Specs, p.parseSpec(tok))
			p.semi()
		}
		p.expect(")")
	} else {
		gd.Specs = append(gd.Specs, p.parseSpec(tok))
	}
	p.semi()
	return gd
}

func (p *Parser) parseSpec(tok string) ast.Spec {
	if p.err != nil {
		return nil
	}
	start := p.cur()
	if tok == "type" {
		name := p.cur()
		if name.Type != lexer.IDENT {
			p.fail("expected type name, got %s", name)
			return nil
		}
		p.next()
		spec := &ast.TypeSpec{Position: p.posOf(start), Name: name.Literal}
		if p.accept("=") {
			spec.Assign = true
		}
		spec.Type = p.parseType()
		return spec
	}
	// var / const
	spec := &ast.ValueSpec{Position: p.posOf(start)}
	for {
		n := p.cur()
		if n.Type != lexer.IDENT {
			p.fail("expected identifier in %s declaration, got %s", tok, n)
			return spec
		}
		p.next()
		spec.Names = append(spec.Names, n.Literal)
		if !p.accept(",") {
			break
		}
	}
	if p.startsType() && !p.at("=") {
		spec.Type = p.parseType()
	}
	if p.accept("=") {
		spec.Values = p.parseExprList()
	}
	return spec
}

// --------------------------------------------------------------- field lists

// parseFieldList parses `( a, b int, c string )` or `( int, error )`.
func (p *Parser) parseFieldList() *ast.FieldList {
	start := p.cur()
	fl := &ast.FieldList{Position: p.posOf(start)}
	if p.err != nil {
		return fl
	}
	p.expect("(")
	var items []rawItem
	for !p.at(")") && !p.atType(lexer.EOF) && p.err == nil {
		items = append(items, p.parseParamItem())
		if !p.accept(",") {
			break
		}
	}
	p.expect(")")
	fl.List = groupFields(items)
	return fl
}

func (p *Parser) parseParamItem() rawItem {
	if p.err != nil {
		return rawItem{}
	}
	if p.at("...") {
		start := p.next()
		elem := p.parseType()
		return rawItem{typ: &ast.Ellipsis{Position: p.posOf(start), Elem: elem}}
	}
	if p.atType(lexer.IDENT) && !p.isQualifiedType() {
		name := p.next()
		if p.startsType() && !p.at(",") {
			var typ ast.Expr
			if p.at("...") {
				st := p.next()
				typ = &ast.Ellipsis{Position: p.posOf(st), Elem: p.parseType()}
			} else {
				typ = p.parseType()
			}
			return rawItem{names: []string{name.Literal}, typ: typ}
		}
		return rawItem{isDeferredIdent: true, name: name.Literal}
	}
	return rawItem{typ: p.parseType()}
}

// isQualifiedType reports whether the current identifier is the start of a
// qualified type such as `pkg.Type`.
func (p *Parser) isQualifiedType() bool {
	return p.peek().Type == lexer.OP && p.peek().Literal == "."
}

func groupFields(items []rawItem) []*ast.Field {
	var fields []*ast.Field
	var pending []string
	for _, it := range items {
		if it.isDeferredIdent {
			pending = append(pending, it.name)
			continue
		}
		names := append(append([]string{}, pending...), it.names...)
		pending = nil
		fields = append(fields, &ast.Field{Names: names, Type: it.typ})
	}
	for _, n := range pending {
		fields = append(fields, &ast.Field{Type: &ast.Ident{Name: n}})
	}
	return fields
}

// startsType reports whether the current token can begin a type.
func (p *Parser) startsType() bool {
	t := p.cur()
	switch t.Type {
	case lexer.IDENT:
		return true
	case lexer.KEYWORD:
		switch t.Literal {
		case "map", "chan", "func", "struct", "interface":
			return true
		}
		return false
	case lexer.OP:
		switch t.Literal {
		case "*", "[", "(", "<-", "...":
			return true
		}
	}
	return false
}

// ------------------------------------------------------------------- types

func (p *Parser) parseType() ast.Expr {
	if p.err != nil {
		return nil
	}
	t := p.cur()
	switch {
	case p.at("*"):
		p.next()
		return &ast.PtrType{Position: p.posOf(t), Elem: p.parseType()}
	case p.at("["):
		p.next()
		if p.accept("]") {
			return &ast.SliceType{Position: p.posOf(t), Elem: p.parseType()}
		}
		var length ast.Expr
		if p.at("...") {
			p.next()
		} else {
			length = p.parseExpr()
		}
		p.expect("]")
		return &ast.ArrayType{Position: p.posOf(t), Len: length, Elem: p.parseType()}
	case p.at("map"):
		p.next()
		p.expect("[")
		key := p.parseType()
		p.expect("]")
		return &ast.MapType{Position: p.posOf(t), Key: key, Value: p.parseType()}
	case p.at("chan"):
		p.next()
		dir := "both"
		if p.accept("<-") {
			dir = "send"
		}
		return &ast.ChanType{Position: p.posOf(t), Dir: dir, Value: p.parseType()}
	case p.at("<-"):
		p.next()
		p.expect("chan")
		return &ast.ChanType{Position: p.posOf(t), Dir: "recv", Value: p.parseType()}
	case p.at("func"):
		p.next()
		return p.parseFuncSignature()
	case p.at("struct"):
		return p.parseStructType()
	case p.at("interface"):
		return p.parseInterfaceType()
	case p.at("("):
		p.next()
		inner := p.parseType()
		p.expect(")")
		return inner
	case p.at("..."):
		p.next()
		return &ast.Ellipsis{Position: p.posOf(t), Elem: p.parseType()}
	case t.Type == lexer.IDENT:
		p.next()
		if p.accept(".") {
			sel := p.cur()
			if sel.Type != lexer.IDENT {
				p.fail("expected identifier after '.', got %s", sel)
				return nil
			}
			p.next()
			return &ast.SelectorExpr{Position: p.posOf(t), X: &ast.Ident{Position: p.posOf(t), Name: t.Literal}, Sel: sel.Literal}
		}
		return &ast.Ident{Position: p.posOf(t), Name: t.Literal}
	default:
		p.fail("expected type, got %s", t)
		return nil
	}
}

func (p *Parser) parseStructType() *ast.StructType {
	if p.err != nil {
		return nil
	}
	start := p.expect("struct")
	st := &ast.StructType{Position: p.posOf(start)}
	p.expect("{")
	for !p.at("}") && !p.atType(lexer.EOF) && p.err == nil {
		if p.at("*") {
			typ := p.parseType()
			st.Fields = append(st.Fields, &ast.Field{Type: typ})
			p.semi()
			continue
		}
		// embedded field: a single bare type name with no following type
		if p.atType(lexer.IDENT) && p.peek().Type == lexer.OP &&
			(p.peek().Literal == ";" || p.peek().Literal == "}") {
			name := p.next()
			st.Fields = append(st.Fields, &ast.Field{Type: &ast.Ident{Position: p.posOf(name), Name: name.Literal}})
			p.semi()
			continue
		}
		var names []string
		for p.atType(lexer.IDENT) {
			names = append(names, p.next().Literal)
			if !p.accept(",") {
				break
			}
		}
		typ := p.parseType()
		st.Fields = append(st.Fields, &ast.Field{Names: names, Type: typ})
		p.semi()
	}
	p.expect("}")
	return st
}

func (p *Parser) parseInterfaceType() *ast.InterfaceType {
	if p.err != nil {
		return nil
	}
	start := p.expect("interface")
	it := &ast.InterfaceType{Position: p.posOf(start)}
	p.expect("{")
	for !p.at("}") && !p.atType(lexer.EOF) && p.err == nil {
		name := p.cur()
		if name.Type != lexer.IDENT {
			p.fail("expected method name in interface, got %s", name)
			return it
		}
		p.next()
		if !p.at("(") {
			// embedded interface (e.g. `Node`)
			it.Methods = append(it.Methods, &ast.Field{
				Position: p.posOf(name),
				Names:    []string{name.Literal},
			})
			p.semi()
			continue
		}
		ft := p.parseFuncSignature()
		it.Methods = append(it.Methods, &ast.Field{
			Position: p.posOf(name),
			Names:    []string{name.Literal},
			Type:     ft,
		})
		p.semi()
	}
	p.expect("}")
	return it
}

// ------------------------------------------------------------- statements

func (p *Parser) parseBlock() *ast.BlockStmt {
	if p.err != nil {
		return nil
	}
	start := p.expect("{")
	b := &ast.BlockStmt{Position: p.posOf(start)}
	for !p.at("}") && !p.atType(lexer.EOF) && p.err == nil {
		b.List = append(b.List, p.parseStmt())
		p.semi()
	}
	p.expect("}")
	return b
}

func (p *Parser) parseStmt() ast.Stmt {
	if p.err != nil {
		return nil
	}
	switch {
	case p.at("var"), p.at("const"), p.at("type"):
		start := p.cur()
		tok := p.next().Literal
		return &ast.DeclStmt{Position: p.posOf(start), Decl: p.parseGenDecl(start, tok)}
	case p.at("if"):
		return p.parseIf()
	case p.at("for"):
		return p.parseFor()
	case p.at("switch"):
		return p.parseSwitch()
	case p.at("select"):
		return p.parseSelect()
	case p.at("return"):
		start := p.next()
		r := &ast.ReturnStmt{Position: p.posOf(start)}
		if !p.at(";") && !p.at("}") && !p.atType(lexer.EOF) {
			r.Results = p.parseExprList()
		}
		return r
	case p.at("break"), p.at("continue"), p.at("fallthrough"), p.at("goto"):
		start := p.next()
		b := &ast.BranchStmt{Position: p.posOf(start), Tok: start.Literal}
		if p.atType(lexer.IDENT) {
			b.Label = p.next().Literal
		}
		return b
	case p.at("defer"):
		start := p.next()
		return &ast.DeferStmt{Position: p.posOf(start), Call: p.parseExpr()}
	case p.at("go"):
		start := p.next()
		return &ast.GoStmt{Position: p.posOf(start), Call: p.parseExpr()}
	case p.at("func"):
		// a func literal used as an expression statement (called immediately)
		return &ast.ExprStmt{Position: p.posOf(p.cur()), X: p.parseExpr()}
	case p.atType(lexer.IDENT) && p.peek().Type == lexer.OP && p.peek().Literal == ":":
		label := p.next()
		p.expect(":")
		return &ast.LabeledStmt{Position: p.posOf(label), Label: label.Literal, Stmt: p.parseStmt()}
	default:
		return p.parseSimpleStmt()
	}
}

func (p *Parser) parseSimpleStmt() ast.Stmt {
	if p.err != nil {
		return nil
	}
	start := p.cur()
	lhs := p.parseExprList()
	if p.err != nil {
		return nil
	}
	if t := p.cur(); t.Type == lexer.OP {
		switch t.Literal {
		case "=", ":=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "&^=":
			p.next()
			rhs := p.parseExprList()
			return &ast.AssignStmt{Position: p.posOf(start), Lhs: lhs, Op: t.Literal, Rhs: rhs}
		case "++", "--":
			p.next()
			return &ast.IncDecStmt{Position: p.posOf(start), X: lhs[0], Op: t.Literal}
		case "<-":
			p.next()
			val := p.parseExpr()
			return &ast.SendStmt{Position: p.posOf(start), Chan: lhs[0], Value: val}
		}
	}
	if len(lhs) != 1 {
		p.fail("expected single expression in statement")
		return nil
	}
	return &ast.ExprStmt{Position: p.posOf(start), X: lhs[0]}
}

func (p *Parser) parseIf() ast.Stmt {
	if p.err != nil {
		return nil
	}
	start := p.expect("if")
	stmt := &ast.IfStmt{Position: p.posOf(start)}
	p.inControlHeader = true
	var first ast.Stmt
	if p.accept(";") {
		// no init
	} else {
		first = p.parseSimpleStmt()
	}
	if p.err != nil {
		p.inControlHeader = false
		return nil
	}
	if p.accept(";") {
		stmt.Init = first
		stmt.Cond = p.parseExpr()
	} else {
		// first was actually the condition
		if es, ok := first.(*ast.ExprStmt); ok {
			stmt.Cond = es.X
		} else {
			p.fail("expected condition in if statement")
			p.inControlHeader = false
			return nil
		}
	}
	p.inControlHeader = false
	stmt.Body = p.parseBlock()
	if p.at("else") {
		p.next()
		if p.at("if") {
			stmt.Else = p.parseIf()
		} else {
			stmt.Else = p.parseBlock()
		}
	}
	return stmt
}

func (p *Parser) parseFor() ast.Stmt {
	if p.err != nil {
		return nil
	}
	start := p.expect("for")
	stmt := &ast.ForStmt{Position: p.posOf(start)}
	if p.at("{") {
		stmt.Body = p.parseBlock()
		return stmt
	}
	p.inControlHeader = true
	if p.rangeAhead() {
		r := &ast.RangeStmt{Position: p.posOf(start)}
		if !p.at("range") {
			lhs := p.parseExprList()
			if p.err != nil {
				p.inControlHeader = false
				return nil
			}
			if len(lhs) == 1 {
				r.Key = lhs[0]
			} else {
				r.Key = lhs[0]
				r.Value = lhs[1]
			}
			r.Tok = p.next().Literal // := or =
		}
		p.expect("range")
		r.X = p.parseExpr()
		p.inControlHeader = false
		r.Body = p.parseBlock()
		return r
	}
	// three-clause (or bare condition)
	if !p.at(";") {
		first := p.parseSimpleStmt()
		if p.err != nil {
			p.inControlHeader = false
			return nil
		}
		if p.at("{") {
			// `for cond {` — first is the condition
			if es, ok := first.(*ast.ExprStmt); ok {
				stmt.Cond = es.X
			} else {
				stmt.Init = first
			}
		} else {
			stmt.Init = first
		}
	}
	if p.accept(";") {
		if !p.at(";") {
			stmt.Cond = p.parseExpr()
		}
		p.expect(";")
		if !p.at("{") {
			stmt.Post = p.parseSimpleStmt()
		}
	}
	p.inControlHeader = false
	stmt.Body = p.parseBlock()
	return stmt
}

// rangeAhead reports whether the for header (up to `{` or `;`) contains `range`.
func (p *Parser) rangeAhead() bool {
	depth := 0
	for i := p.pos; i < len(p.toks); i++ {
		t := p.toks[i]
		if t.Type == lexer.OP {
			switch t.Literal {
			case "(", "[":
				depth++
			case ")", "]":
				depth--
			case "{", ";":
				if depth == 0 {
					return false
				}
			}
		}
		if depth == 0 && t.Type == lexer.KEYWORD && t.Literal == "range" {
			return true
		}
		if t.Type == lexer.EOF {
			return false
		}
	}
	return false
}

func (p *Parser) parseSwitch() ast.Stmt {
	if p.err != nil {
		return nil
	}
	start := p.expect("switch")
	p.inControlHeader = true
	var initStmt, tagStmt ast.Stmt
	var tag ast.Expr
	if !p.at("{") {
		first := p.parseSimpleStmt()
		if p.err != nil {
			p.inControlHeader = false
			return nil
		}
		if p.accept(";") {
			initStmt = first
			if !p.at("{") {
				tagStmt = p.parseSimpleStmt()
			}
		} else {
			tagStmt = first
		}
	}
	p.inControlHeader = false
	// Type switch?
	if as, ok := tagStmt.(*ast.AssignStmt); ok && len(as.Rhs) == 1 {
		if ta, ok := as.Rhs[0].(*ast.TypeAssertExpr); ok && ta.Type == nil {
			ts := &ast.TypeSwitchStmt{Position: p.posOf(start), Init: initStmt, Assign: as}
			ts.Body = p.parseCaseClauses()
			return ts
		}
	}
	if es, ok := tagStmt.(*ast.ExprStmt); ok {
		if ta, ok := es.X.(*ast.TypeAssertExpr); ok && ta.Type == nil {
			// switch x.(type) — no bound variable
			ts := &ast.TypeSwitchStmt{Position: p.posOf(start), Init: initStmt, Assign: es}
			ts.Body = p.parseCaseClauses()
			return ts
		}
	}
	if es, ok := tagStmt.(*ast.ExprStmt); ok {
		tag = es.X
	} else if tagStmt != nil {
		// a tag that was parsed as an assignment/expr — take expression if possible
		if es2, ok := tagStmt.(*ast.ExprStmt); ok {
			tag = es2.X
		}
	}
	sw := &ast.SwitchStmt{Position: p.posOf(start), Init: initStmt, Tag: tag}
	sw.Body = p.parseCaseClauses()
	return sw
}

func (p *Parser) parseCaseClauses() []*ast.CaseClause {
	if p.err != nil {
		return nil
	}
	p.expect("{")
	var clauses []*ast.CaseClause
	for !p.at("}") && !p.atType(lexer.EOF) && p.err == nil {
		start := p.cur()
		c := &ast.CaseClause{Position: p.posOf(start)}
		if p.accept("case") {
			c.List = p.parseExprList()
		} else if p.accept("default") {
			// no list
		} else {
			p.fail("expected case or default, got %s", p.cur())
			return clauses
		}
		p.expect(":")
		for !p.at("case") && !p.at("default") && !p.at("}") && !p.atType(lexer.EOF) && p.err == nil {
			c.Body = append(c.Body, p.parseStmt())
			p.semi()
		}
		clauses = append(clauses, c)
	}
	p.expect("}")
	return clauses
}

func (p *Parser) parseSelect() ast.Stmt {
	if p.err != nil {
		return nil
	}
	start := p.expect("select")
	sl := &ast.SelectStmt{Position: p.posOf(start)}
	p.expect("{")
	for !p.at("}") && !p.atType(lexer.EOF) && p.err == nil {
		c := &ast.CommClause{Position: p.posOf(p.cur())}
		if p.accept("case") {
			c.Comm = p.parseSimpleStmt()
		} else if p.accept("default") {
			// no comm
		} else {
			p.fail("expected case or default in select, got %s", p.cur())
			return sl
		}
		p.expect(":")
		for !p.at("case") && !p.at("default") && !p.at("}") && !p.atType(lexer.EOF) && p.err == nil {
			c.Body = append(c.Body, p.parseStmt())
			p.semi()
		}
		sl.Body = append(sl.Body, c)
	}
	p.expect("}")
	return sl
}

// ---------------------------------------------------------- expressions

func (p *Parser) parseExprList() []ast.Expr {
	if p.err != nil {
		return nil
	}
	var list []ast.Expr
	list = append(list, p.parseExpr())
	for p.accept(",") {
		if p.err != nil {
			return list
		}
		list = append(list, p.parseExpr())
	}
	return list
}

var precedence = map[string]int{
	"||": 1,
	"&&": 2,
	"==": 3, "!=": 3, "<": 3, "<=": 3, ">": 3, ">=": 3,
	"+": 4, "-": 4, "|": 4, "^": 4,
	"*": 5, "/": 5, "%": 5, "<<": 5, ">>": 5, "&": 5, "&^": 5,
}

func (p *Parser) parseExpr() ast.Expr {
	return p.parseBinaryExpr(1)
}

func (p *Parser) parseBinaryExpr(minPrec int) ast.Expr {
	if p.err != nil {
		return nil
	}
	left := p.parseUnary()
	for {
		if p.err != nil {
			return left
		}
		t := p.cur()
		if t.Type != lexer.OP {
			break
		}
		prec, ok := precedence[t.Literal]
		if !ok || prec < minPrec {
			break
		}
		p.next()
		right := p.parseBinaryExpr(prec + 1)
		left = &ast.BinaryExpr{Position: left.Pos(), Op: t.Literal, X: left, Y: right}
	}
	return left
}

func (p *Parser) parseUnary() ast.Expr {
	if p.err != nil {
		return nil
	}
	t := p.cur()
	if t.Type == lexer.OP {
		switch t.Literal {
		case "+", "-", "!", "^", "*", "&", "<-":
			p.next()
			return &ast.UnaryExpr{Position: p.posOf(t), Op: t.Literal, X: p.parseUnary()}
		}
	}
	return p.parsePrimary()
}

func (p *Parser) parsePrimary() ast.Expr {
	if p.err != nil {
		return nil
	}
	x := p.parseOperand()
	for {
		if p.err != nil {
			return x
		}
		switch {
		case p.at("."):
			p.next()
			if p.at("(") {
				// type assertion
				p.next()
				var typ ast.Expr
				if p.at("type") {
					p.next()
				} else {
					typ = p.parseType()
				}
				p.expect(")")
				x = &ast.TypeAssertExpr{Position: x.Pos(), X: x, Type: typ}
				continue
			}
			sel := p.cur()
			if sel.Type != lexer.IDENT {
				p.fail("expected selector, got %s", sel)
				return x
			}
			p.next()
			x = &ast.SelectorExpr{Position: x.Pos(), X: x, Sel: sel.Literal}
		case p.at("["):
			p.next()
			var low, high, max ast.Expr
			if !p.at(":") {
				low = p.parseExpr()
			}
			if p.accept(":") {
				if !p.at(":") && !p.at("]") {
					high = p.parseExpr()
				}
				if p.accept(":") {
					if !p.at("]") {
						max = p.parseExpr()
					}
				}
				p.expect("]")
				x = &ast.SliceExpr{Position: x.Pos(), X: x, Low: low, High: high, Max: max}
			} else {
				p.expect("]")
				x = &ast.IndexExpr{Position: x.Pos(), X: x, Index: low}
			}
		case p.at("("):
			p.next()
			call := &ast.CallExpr{Position: x.Pos(), Fun: x}
			if !p.at(")") {
				call.Args = p.parseExprList()
				if p.at("...") {
					p.next()
					call.Ellipsis = true
				}
			}
			p.expect(")")
			x = call
		case p.at("{") && p.isCompositeLitType(x) && !p.inControlHeader:
			x = p.parseCompositeLit(x)
		default:
			return x
		}
	}
}

func (p *Parser) isCompositeLitType(x ast.Expr) bool {
	switch x.(type) {
	case *ast.Ident, *ast.SelectorExpr, *ast.SliceType, *ast.ArrayType, *ast.MapType, *ast.StructType, *ast.PtrType:
		return true
	}
	return false
}

func (p *Parser) parseOperand() ast.Expr {
	if p.err != nil {
		return nil
	}
	t := p.cur()
	switch {
	case t.Type == lexer.INT, t.Type == lexer.FLOAT, t.Type == lexer.IMAG, t.Type == lexer.STRING, t.Type == lexer.CHAR:
		p.next()
		return &ast.BasicLit{Position: p.posOf(t), Kind: string(t.Type), Value: t.Literal}
	case t.Type == lexer.IDENT:
		p.next()
		return &ast.Ident{Position: p.posOf(t), Name: t.Literal}
	case p.at("("):
		p.next()
		inner := p.parseExpr()
		p.expect(")")
		return &ast.ParenExpr{Position: p.posOf(t), X: inner}
	case p.at("func"):
		p.next()
		ft := p.parseFuncSignature()
		body := p.parseBlock()
		return &ast.FuncLit{Position: p.posOf(t), Type: ft, Body: body}
	case p.at("["):
		return p.parseType()
	case p.at("map"):
		return p.parseType()
	case p.at("struct"):
		return p.parseType()
	case p.at("chan"):
		return p.parseType()
	default:
		p.fail("expected expression, got %s", t)
		return nil
	}
}

func (p *Parser) parseCompositeLit(typ ast.Expr) ast.Expr {
	if p.err != nil {
		return nil
	}
	start := p.expect("{")
	lit := &ast.CompositeLit{Position: p.posOf(start), Type: typ}
	for !p.at("}") && !p.atType(lexer.EOF) && p.err == nil {
		var elt ast.Expr
		if p.at("{") {
			elt = p.parseCompositeLit(nil)
		} else {
			first := p.parseExpr()
			if p.err != nil {
				return lit
			}
			if p.accept(":") {
				var v ast.Expr
				if p.at("{") {
					v = p.parseCompositeLit(nil)
				} else {
					v = p.parseExpr()
				}
				elt = &ast.KeyValueExpr{Position: first.Pos(), Key: first, Value: v}
			} else {
				elt = first
			}
		}
		lit.Elts = append(lit.Elts, elt)
		if !p.accept(",") {
			break
		}
	}
	p.expect("}")
	return lit
}
