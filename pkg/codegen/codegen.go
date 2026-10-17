// Package codegen lowers an ast.File to C++20 source.
//
// The generated code mirrors the runtime conventions of the original
// line-oriented transpiler (std::cout for fmt.Println/Print, a small set of
// injected helpers, x86-free static structure printing) but is driven by the
// AST instead of by regular expressions on lines.
package codegen

import (
	"fmt"
	"strings"

	"github.com/mdaashir/go2cpp/pkg/ast"
)

// strBuilder is a minimal string accumulator. The project deliberately avoids
// the standard-library builder here so that the compiler can transpile its own
// source.
type strBuilder struct {
	s string
}

func (b *strBuilder) WriteString(x string) {
	b.s += x
}

func (b *strBuilder) String() string {
	return b.s
}

// Generator accumulates generated C++ source.
type Generator struct {
	sb     strBuilder
	indent int

	structNames  map[string]bool
	structFields map[string][]string
	aliases      map[string]ast.Expr
	types        map[string]ast.Expr
	methods      map[string][]*ast.FuncDecl
	embedOf      map[string][]string
	ptrVars      map[string]bool
	recvName     string
	recvPtr      bool

	needContains   bool
	needHasPrefix  bool
	needHasSuffix  bool
	needTrimSpace  bool
	needSplit      bool
	needIndex      bool
	needReplace    bool
	needFmtSprintf bool
	needPrintf     bool
	needStrconv    bool
	usesPrint      bool
	usesBuiltinOut bool
	needRuntime    bool
	needAny        bool

	switchCounter int
	selectCounter int
	deferCounter  int
	iotaIndex     int

	curResults []*ast.Field
	err        error
}

// Generate compiles a parsed file to C++ source text.
func Generate(f *ast.File) (string, error) {
	g := &Generator{
		structNames:  map[string]bool{},
		structFields: map[string][]string{},
		aliases:      map[string]ast.Expr{},
		types:        map[string]ast.Expr{},
		methods:      map[string][]*ast.FuncDecl{},
		embedOf:      map[string][]string{},
		ptrVars:      map[string]bool{},
	}
	g.collectTypes(f)
	g.emitDecls(f.Decls)
	if g.err != nil {
		return "", g.err
	}
	return g.preamble() + g.sb.String(), nil
}

func (g *Generator) fail(format string, args ...interface{}) {
	if g.err == nil {
		g.err = fmt.Errorf("codegen: "+format, args...)
	}
}

// ---------------------------------------------------------------- pre-scan

func (g *Generator) collectTypes(f *ast.File) {
	for _, d := range f.Decls {
		if fd, ok := d.(*ast.FuncDecl); ok && fd.Recv != nil && len(fd.Recv.List) > 0 {
			name, _ := baseTypeName(fd.Recv.List[0].Type)
			g.methods[name] = append(g.methods[name], fd)
			continue
		}
		gd, ok := d.(*ast.GenDecl)
		if !ok || gd.Tok != "type" {
			continue
		}
		for _, s := range gd.Specs {
			ts := s.(*ast.TypeSpec)
			if st, ok := ts.Type.(*ast.StructType); ok {
				g.structNames[ts.Name] = true
				var fields []string
				for _, fld := range st.Fields {
					if len(fld.Names) == 0 {
						// embedded field: member named after its base type
						base, _ := baseTypeName(fld.Type)
						g.embedOf[ts.Name] = append(g.embedOf[ts.Name], base)
						continue
					}
					fields = append(fields, fld.Names...)
				}
				g.structFields[ts.Name] = fields
			} else {
				g.aliases[ts.Name] = ts.Type
			}
		}
	}
}

// baseTypeName returns the bare type name and whether it is a pointer type.
func baseTypeName(t ast.Expr) (string, bool) {
	switch tt := t.(type) {
	case *ast.Ident:
		return tt.Name, false
	case *ast.StarExpr:
		n, _ := baseTypeName(tt.X)
		return n, true
	case *ast.PtrType:
		n, _ := baseTypeName(tt.Elem)
		return n, true
	}
	return "unknown", false
}

// --------------------------------------------------------------- writing

func (g *Generator) write(format string, args ...interface{}) {
	for i := 0; i < g.indent; i++ {
		g.sb.WriteString("    ")
	}
	g.sb.WriteString(fmt.Sprintf(format, args...))
}

func (g *Generator) writeln(format string, args ...interface{}) {
	g.write(format, args...)
	g.sb.WriteString("\n")
}

// ----------------------------------------------------------------- types

func (g *Generator) cppType(e ast.Expr) string {
	switch t := e.(type) {
	case nil:
		return "void"
	case *ast.Ident:
		switch t.Name {
		case "string":
			return "std::string"
		case "float64":
			return "double"
		case "float32":
			return "float"
		case "uint64":
			return "std::uint64_t"
		case "uint32":
			return "std::uint32_t"
		case "uint16":
			return "std::uint16_t"
		case "uint8":
			return "std::uint8_t"
		case "uint":
			return "unsigned int"
		case "int64":
			return "std::int64_t"
		case "int32":
			return "std::int32_t"
		case "int16":
			return "std::int16_t"
		case "int8":
			return "std::int8_t"
		case "byte":
			return "std::uint8_t"
		case "rune":
			return "std::int32_t"
		case "bool":
			return "bool"
		case "int":
			return "int"
		case "error":
			return "std::string"
		}
		return t.Name
	case *ast.SliceType:
		return "std::vector<" + g.cppType(t.Elem) + ">"
	case *ast.ArrayType:
		return "std::vector<" + g.cppType(t.Elem) + ">"
	case *ast.MapType:
		return "std::unordered_map<" + g.cppType(t.Key) + ", " + g.cppType(t.Value) + ">"
	case *ast.PtrType:
		return g.cppType(t.Elem) + "*"
	case *ast.StarExpr:
		return g.cppType(t.X) + "*"
	case *ast.ChanType:
		g.needRuntime = true
		return "go2cpp::channel<" + g.cppType(t.Value) + ">"
	case *ast.InterfaceType:
		g.needAny = true
		return "go2cpp::any"
	case *ast.SelectorExpr:
		return t.Sel
	case *ast.Ellipsis:
		return g.cppType(t.Elem)
	case *ast.FuncType:
		return "std::function<void()>"
	}
	return "auto"
}

func (g *Generator) zeroValue(e ast.Expr) string {
	switch t := e.(type) {
	case *ast.Ident:
		switch t.Name {
		case "string":
			return `""`
		case "bool":
			return "false"
		case "int", "int8", "int16", "int32", "int64",
			"uint", "uint8", "uint16", "uint32", "uint64",
			"byte", "float32", "float64", "rune":
			return "0"
		}
		if g.structNames[t.Name] {
			return "{}"
		}
		if target, ok := g.aliases[t.Name]; ok {
			return g.zeroValue(target)
		}
		return "0"
	case *ast.SliceType, *ast.MapType:
		return "{}"
	case *ast.PtrType, *ast.StarExpr:
		return "nullptr"
	case *ast.InterfaceType:
		return "{}"
	}
	return "{}"
}

// ------------------------------------------------------------------ decls

func (g *Generator) emitDecls(decls []ast.Decl) {
	// 1. Type declarations first, so prototypes and bodies can name them.
	for _, d := range decls {
		if gd, ok := d.(*ast.GenDecl); ok && gd.Tok == "type" {
			g.emitGenDecl(gd, true)
		}
	}
	// 2. Forward declarations for free functions.
	for _, d := range decls {
		if fd, ok := d.(*ast.FuncDecl); ok && fd.Recv == nil && fd.Name != "main" {
			g.emitFuncProto(fd)
		}
	}
	// 3. Bodies, variables and constants (types already emitted).
	for _, d := range decls {
		if gd, ok := d.(*ast.GenDecl); ok && gd.Tok == "type" {
			continue
		}
		g.emitDecl(d)
	}
}

func (g *Generator) emitFuncProto(fd *ast.FuncDecl) {
	g.writeln("%s;", g.funcSignature(fd))
}

func (g *Generator) funcSignature(fd *ast.FuncDecl) string {
	params := g.emitParams(fd.Type.Params)
	ret := g.resultType(fd.Type.Results)
	if fd.Recv == nil && fd.Name == "main" {
		return "auto main() -> int"
	}
	name := fd.Name
	if fd.Recv != nil && len(fd.Recv.List) > 0 {
		rf := fd.Recv.List[0]
		recvType := g.cppType(rf.Type)
		recvName := "self"
		if len(rf.Names) > 0 {
			recvName = rf.Names[0]
		}
		base, _ := baseTypeName(rf.Type)
		name = base + "_" + fd.Name
		if params != "" {
			params = recvType + " " + recvName + ", " + params
		} else {
			params = recvType + " " + recvName
		}
	}
	return fmt.Sprintf("auto %s(%s) -> %s", name, params, ret)
}

func (g *Generator) resultType(results []*ast.Field) string {
	types := g.resultTypes(results)
	switch len(types) {
	case 0:
		return "void"
	case 1:
		return types[0]
	default:
		return "std::tuple<" + strings.Join(types, ", ") + ">"
	}
}

// resultTypes flattens a result field list, expanding grouped named results
// such as `(a, b int)` into two entries.
func (g *Generator) resultTypes(results []*ast.Field) []string {
	var out []string
	for _, fld := range results {
		if len(fld.Names) == 0 {
			out = append(out, g.cppType(fld.Type))
			continue
		}
		for range fld.Names {
			out = append(out, g.cppType(fld.Type))
		}
	}
	return out
}

func (g *Generator) emitParams(fields []*ast.Field) string {
	var parts []string
	for _, fld := range fields {
		typ := g.cppType(fld.Type)
		if _, ok := fld.Type.(*ast.Ellipsis); ok {
			// variadic — approximate with an initializer list is not possible
			// here; emit the element type and a name.
		}
		if len(fld.Names) == 0 {
			parts = append(parts, typ)
			continue
		}
		for _, n := range fld.Names {
			parts = append(parts, typ+" "+n)
		}
	}
	return strings.Join(parts, ", ")
}

func (g *Generator) emitDecl(d ast.Decl) {
	switch t := d.(type) {
	case *ast.FuncDecl:
		if t.Recv != nil {
			return // emitted as a class member
		}
		g.emitFunc(t)
	case *ast.GenDecl:
		g.emitGenDecl(t, true)
	default:
		g.fail("unsupported declaration %T", d)
	}
}

func (g *Generator) emitFunc(fd *ast.FuncDecl) {
	g.writeln("%s", g.funcSignature(fd))
	g.writeln("{")
	g.indent++
	g.curResults = fd.Type.Results
	savedRecv, savedPtr := g.recvName, g.recvPtr
	g.recvName, g.recvPtr = "", false
	if fd.Name == "main" {
		g.emitStmts(fd.Body.List)
		g.writeln("return 0;")
	} else {
		g.emitStmts(fd.Body.List)
	}
	g.recvName, g.recvPtr = savedRecv, savedPtr
	g.indent--
	g.writeln("}")
	g.writeln("")
}

// methodSignature is funcSignature without the receiver parameter.
func (g *Generator) methodSignature(fd *ast.FuncDecl) string {
	params := g.emitParams(fd.Type.Params)
	ret := g.resultType(fd.Type.Results)
	return fmt.Sprintf("auto %s(%s) -> %s", fd.Name, params, ret)
}

// emitMethod emits a method as a member function of its receiver type. Inside
// the body the Go receiver name maps to `this` / `(*this)`.
func (g *Generator) emitMethod(fd *ast.FuncDecl) {
	g.writeln("%s", g.methodSignature(fd))
	g.writeln("{")
	g.indent++
	g.curResults = fd.Type.Results
	rf := fd.Recv.List[0]
	_, ptr := baseTypeName(rf.Type)
	savedRecv, savedPtr := g.recvName, g.recvPtr
	g.recvPtr = ptr
	g.recvName = ""
	if len(rf.Names) > 0 {
		g.recvName = rf.Names[0]
	}
	g.emitStmts(fd.Body.List)
	g.recvName, g.recvPtr = savedRecv, savedPtr
	g.indent--
	g.writeln("}")
}

// emitPromoted forwards a method of an embedded type.
func (g *Generator) emitPromoted(fd *ast.FuncDecl, base string) {
	params := g.emitParams(fd.Type.Params)
	ret := g.resultType(fd.Type.Results)
	var names []string
	for _, fld := range fd.Type.Params {
		names = append(names, fld.Names...)
	}
	g.writeln("auto %s(%s) -> %s", fd.Name, params, ret)
	g.writeln("{")
	g.indent++
	g.writeln("return %s.%s(%s);", base, fd.Name, strings.Join(names, ", "))
	g.indent--
	g.writeln("}")
}

func (g *Generator) emitGenDecl(gd *ast.GenDecl, topLevel bool) {
	switch gd.Tok {
	case "type":
		for _, s := range gd.Specs {
			g.emitTypeSpec(s.(*ast.TypeSpec))
		}
	case "var":
		for _, s := range gd.Specs {
			g.emitValueSpec(s.(*ast.ValueSpec), false)
		}
	case "const":
		g.iotaIndex = 0
		for _, s := range gd.Specs {
			g.emitValueSpec(s.(*ast.ValueSpec), true)
			g.iotaIndex++
		}
	}
}

func (g *Generator) emitTypeSpec(ts *ast.TypeSpec) {
	if st, ok := ts.Type.(*ast.StructType); ok {
		g.writeln("class %s { public:", ts.Name)
		g.indent++
		for _, fld := range st.Fields {
			if len(fld.Names) == 0 {
				// embedded field: an object member named after its type
				base, _ := baseTypeName(fld.Type)
				g.writeln("%s %s;", base, base)
				continue
			}
			typ := g.cppType(fld.Type)
			for _, n := range fld.Names {
				g.writeln("%s %s;", typ, n)
			}
		}
		g.emitStrMethod(ts.Name)
		for _, fd := range g.methods[ts.Name] {
			g.emitMethod(fd)
		}
		// promote methods of embedded types
		for _, base := range g.embedOf[ts.Name] {
			for _, fd := range g.methods[base] {
				g.emitPromoted(fd, base)
			}
		}
		g.indent--
		g.writeln("};")
		g.writeln("")
		return
	}
	g.writeln("using %s = %s;", ts.Name, g.cppType(ts.Type))
}

func (g *Generator) emitStrMethod(name string) {
	fields := g.structFields[name]
	if len(fields) == 0 {
		return
	}
	g.usesPrint = true
	g.writeln("std::string _str()")
	g.writeln("{")
	g.indent++
	g.writeln(`std::stringstream _ss;`)
	g.writeln(`_ss << "{";`)
	for i, f := range fields {
		if i > 0 {
			g.writeln(`_ss << " ";`)
		}
		g.writeln("_format_output(_ss, %s);", f)
	}
	g.writeln(`_ss << "}";`)
	g.writeln("return _ss.str();")
	g.indent--
	g.writeln("}")
}

func (g *Generator) emitValueSpec(vs *ast.ValueSpec, isConst bool) {
	if isConst {
		g.emitConstSpec(vs)
		return
	}
	if len(vs.Values) == 0 && vs.Type != nil {
		for _, n := range vs.Names {
			g.writeln("%s %s = %s;", g.cppType(vs.Type), n, g.zeroValue(vs.Type))
			g.recordType(n, vs.Type)
		}
		return
	}
	for i, n := range vs.Names {
		var val string
		if i < len(vs.Values) {
			val = g.emitExpr(vs.Values[i])
		} else {
			val = "{}"
		}
		if vs.Type != nil {
			g.writeln("%s %s = %s;", g.cppType(vs.Type), n, val)
			g.recordType(n, vs.Type)
		} else if i < len(vs.Values) && isStringLit(vs.Values[i]) {
			g.writeln("std::string %s = %s;", n, val)
		} else {
			g.writeln("auto %s = %s;", n, val)
			if i < len(vs.Values) {
				g.recordTypeExpr(n, vs.Values[i])
			}
		}
	}
}

func (g *Generator) emitConstSpec(vs *ast.ValueSpec) {
	for i, n := range vs.Names {
		var val string
		switch {
		case i < len(vs.Values):
			val = g.emitConstExpr(vs.Values[i])
		default:
			val = fmt.Sprintf("%d", g.iotaIndex)
		}
		if vs.Type != nil {
			g.writeln("constexpr %s %s = %s;", g.cppType(vs.Type), n, val)
		} else {
			g.writeln("constexpr auto %s = %s;", n, val)
		}
	}
}

func (g *Generator) emitConstExpr(e ast.Expr) string {
	if id, ok := e.(*ast.Ident); ok && id.Name == "iota" {
		return fmt.Sprintf("%d", g.iotaIndex)
	}
	return g.emitExpr(e)
}

func (g *Generator) recordType(name string, t ast.Expr) { g.types[name] = t }

func (g *Generator) recordTypeExpr(name string, value ast.Expr) {
	switch t := value.(type) {
	case *ast.CompositeLit:
		if t.Type != nil {
			g.types[name] = t.Type
		}
	}
}

// -------------------------------------------------------------- statements

func (g *Generator) emitStmts(list []ast.Stmt) {
	for _, s := range list {
		g.emitStmt(s)
	}
}

func (g *Generator) emitStmt(s ast.Stmt) {
	switch t := s.(type) {
	case *ast.ExprStmt:
		g.writeln("%s;", g.emitExpr(t.X))
	case *ast.AssignStmt:
		g.emitAssign(t)
	case *ast.IncDecStmt:
		g.writeln("%s%s;", g.emitExpr(t.X), t.Op)
	case *ast.DeclStmt:
		g.emitGenDecl(t.Decl.(*ast.GenDecl), false)
	case *ast.ReturnStmt:
		g.emitReturn(t)
	case *ast.IfStmt:
		g.emitIf(t)
	case *ast.ForStmt:
		g.emitFor(t)
	case *ast.RangeStmt:
		g.emitRange(t)
	case *ast.SwitchStmt:
		g.emitSwitch(t)
	case *ast.SelectStmt:
		g.emitSelect(t)
	case *ast.TypeSwitchStmt:
		g.emitTypeSwitch(t)
	case *ast.BranchStmt:
		g.emitBranch(t)
	case *ast.DeferStmt:
		g.emitDefer(t)
	case *ast.LabeledStmt:
		g.writeln("%s:", t.Label)
		g.emitStmt(t.Stmt)
	case *ast.BlockStmt:
		g.writeln("{")
		g.indent++
		g.emitStmts(t.List)
		g.indent--
		g.writeln("}")
	case *ast.GoStmt:
		g.needRuntime = true
		g.writeln("go2cpp::go([&]() { %s; });", g.emitExpr(t.Call))
	case *ast.SendStmt:
		g.needRuntime = true
		g.writeln("(%s).send(%s);", g.emitExpr(t.Chan), g.emitExpr(t.Value))
	default:
		g.fail("unsupported statement %T", s)
	}
}

func (g *Generator) emitAssign(a *ast.AssignStmt) {
	if a.Op == ":=" {
		g.emitDefine(a)
		return
	}
	if a.Op == "=" {
		var lhs, rhs []string
		for _, e := range a.Lhs {
			lhs = append(lhs, g.emitExpr(e))
		}
		for _, e := range a.Rhs {
			rhs = append(rhs, g.emitExpr(e))
		}
		if len(a.Lhs) == 1 && isBlank(a.Lhs[0]) {
			g.writeln("(void)(%s);", rhs[0])
			return
		}
		g.writeln("%s = %s;", strings.Join(lhs, ", "), strings.Join(rhs, ", "))
		return
	}
	// compound assignment
	g.writeln("%s %s %s;", g.emitExpr(a.Lhs[0]), a.Op, g.emitExpr(a.Rhs[0]))
}

func (g *Generator) emitDefine(a *ast.AssignStmt) {
	// multiple lhs bound from a single (tuple-returning) rhs
	if len(a.Lhs) > 1 && len(a.Rhs) == 1 {
		blank := false
		for _, e := range a.Lhs {
			if id, ok := e.(*ast.Ident); ok && id.Name == "_" {
				blank = true
			}
		}
		if !blank {
			var names []string
			for _, e := range a.Lhs {
				names = append(names, e.(*ast.Ident).Name)
			}
			g.writeln("auto [%s] = %s;", strings.Join(names, ", "), g.emitExpr(a.Rhs[0]))
			return
		}
		// blanks present: destructure via a temporary
		g.writeln("auto _tmp%d = %s;", g.deferCounter, g.emitExpr(a.Rhs[0]))
		tmp := fmt.Sprintf("_tmp%d", g.deferCounter)
		g.deferCounter++
		for i, e := range a.Lhs {
			id := e.(*ast.Ident)
			if id.Name == "_" {
				continue
			}
			g.writeln("auto %s = std::get<%d>(%s);", id.Name, i, tmp)
		}
		return
	}
	// pairwise
	for i, e := range a.Lhs {
		id, ok := e.(*ast.Ident)
		if !ok {
			g.fail("unsupported := target %T", e)
			return
		}
		if id.Name == "_" {
			g.writeln("(void)(%s);", g.emitExpr(a.Rhs[i]))
			continue
		}
		rhs := g.emitExpr(a.Rhs[i])
		if isStringLit(a.Rhs[i]) {
			g.writeln("std::string %s = %s;", id.Name, rhs)
		} else {
			g.writeln("auto %s = %s;", id.Name, rhs)
		}
		if isPointerExpr(a.Rhs[i]) {
			g.ptrVars[id.Name] = true
		}
		g.recordTypeExpr(id.Name, a.Rhs[i])
	}
}

func (g *Generator) emitReturn(r *ast.ReturnStmt) {
	if len(r.Results) == 0 {
		g.writeln("return;")
		return
	}
	if len(r.Results) == 1 {
		g.writeln("return %s;", g.emitExpr(r.Results[0]))
		return
	}
	var vals []string
	for _, e := range r.Results {
		vals = append(vals, g.emitExpr(e))
	}
	types := g.resultTypes(g.curResults)
	g.writeln("return std::tuple<%s>{%s};", strings.Join(types, ", "), strings.Join(vals, ", "))
}

func (g *Generator) emitIf(st *ast.IfStmt) {
	head := ""
	if st.Init != nil {
		head = g.inlineStmt(st.Init) + "; "
	}
	g.writeln("if (%s%s)", head, g.emitExpr(st.Cond))
	g.emitBlock(st.Body)
	if st.Else != nil {
		if elseif, ok := st.Else.(*ast.IfStmt); ok {
			// else if — render on one line
			g.write("else ")
			g.emitIfInline(elseif)
		} else if blk, ok := st.Else.(*ast.BlockStmt); ok {
			g.writeln("else")
			g.emitBlock(blk)
		}
	}
}

func (g *Generator) emitIfInline(st *ast.IfStmt) {
	head := ""
	if st.Init != nil {
		head = g.inlineStmt(st.Init) + "; "
	}
	g.writeln("if (%s%s)", head, g.emitExpr(st.Cond))
	g.emitBlock(st.Body)
	if st.Else != nil {
		if elseif, ok := st.Else.(*ast.IfStmt); ok {
			g.write("else ")
			g.emitIfInline(elseif)
		} else if blk, ok := st.Else.(*ast.BlockStmt); ok {
			g.writeln("else")
			g.emitBlock(blk)
		}
	}
}

// inlineStmt renders a statement that can appear in an if/for header (no semicolon).
func (g *Generator) inlineStmt(s ast.Stmt) string {
	switch t := s.(type) {
	case *ast.AssignStmt:
		if t.Op == ":=" {
			if len(t.Lhs) == 1 {
				return fmt.Sprintf("auto %s = %s", t.Lhs[0].(*ast.Ident).Name, g.emitExpr(t.Rhs[0]))
			}
		}
		// "=" style
		var lhs, rhs []string
		for _, e := range t.Lhs {
			lhs = append(lhs, g.emitExpr(e))
		}
		for _, e := range t.Rhs {
			rhs = append(rhs, g.emitExpr(e))
		}
		return fmt.Sprintf("%s %s %s", strings.Join(lhs, ", "), t.Op, strings.Join(rhs, ", "))
	case *ast.IncDecStmt:
		return g.emitExpr(t.X) + t.Op
	case *ast.ExprStmt:
		return g.emitExpr(t.X)
	}
	g.fail("unsupported header statement %T", s)
	return ""
}

func (g *Generator) emitBlock(b *ast.BlockStmt) {
	g.writeln("{")
	g.indent++
	g.emitStmts(b.List)
	g.indent--
	g.writeln("}")
}

func (g *Generator) emitFor(st *ast.ForStmt) {
	init := ""
	if st.Init != nil {
		init = g.inlineStmt(st.Init)
	}
	cond := ""
	if st.Cond != nil {
		cond = g.emitExpr(st.Cond)
	}
	post := ""
	if st.Post != nil {
		post = g.inlineStmt(st.Post)
	}
	if init == "" && cond == "" && post == "" {
		g.writeln("for (;;)")
	} else {
		g.writeln("for (%s; %s; %s)", init, cond, post)
	}
	g.emitBlock(st.Body)
}

func (g *Generator) emitRange(st *ast.RangeStmt) {
	mapIter := g.isMapExpr(st.X)
	if mapIter {
		g.writeln("for (const auto& _pair : %s)", g.emitExpr(st.X))
		g.writeln("{")
		g.indent++
		if st.Key != nil && !isBlank(st.Key) {
			g.writeln("auto %s = _pair.first;", st.Key.(*ast.Ident).Name)
		}
		if st.Value != nil && !isBlank(st.Value) {
			g.writeln("auto %s = _pair.second;", st.Value.(*ast.Ident).Name)
		}
		g.emitStmts(st.Body.List)
		g.indent--
		g.writeln("}")
		return
	}
	// slice/array
	if st.Value != nil && !isBlank(st.Value) && st.Key == nil {
		g.writeln("for (auto _e : %s)", g.emitExpr(st.X))
		g.writeln("{")
		g.indent++
		g.writeln("auto %s = _e;", st.Value.(*ast.Ident).Name)
		g.emitStmts(st.Body.List)
		g.indent--
		g.writeln("}")
		return
	}
	if st.Value != nil && !isBlank(st.Value) {
		g.writeln("for (std::size_t _i = 0; _i < %s.size(); _i++)", g.emitExpr(st.X))
		g.writeln("{")
		g.indent++
		if st.Key != nil && !isBlank(st.Key) {
			g.writeln("auto %s = _i;", st.Key.(*ast.Ident).Name)
		}
		g.writeln("auto %s = %s[_i];", st.Value.(*ast.Ident).Name, g.emitExpr(st.X))
		g.emitStmts(st.Body.List)
		g.indent--
		g.writeln("}")
		return
	}
	// key only over slice
	g.writeln("for (std::size_t _i = 0; _i < %s.size(); _i++)", g.emitExpr(st.X))
	g.writeln("{")
	g.indent++
	if st.Key != nil && !isBlank(st.Key) {
		g.writeln("auto %s = _i;", st.Key.(*ast.Ident).Name)
	}
	g.emitStmts(st.Body.List)
	g.indent--
	g.writeln("}")
}

func isBlank(e ast.Expr) bool {
	id, ok := e.(*ast.Ident)
	return ok && id.Name == "_"
}

func isStringLit(e ast.Expr) bool {
	bl, ok := e.(*ast.BasicLit)
	return ok && bl.Kind == "STRING"
}

// selBase renders the base of a selector and the member-access operator,
// mapping a method receiver name to `this` / `(*this)`.
func (g *Generator) selBase(id *ast.Ident) (string, string) {
	if g.recvName != "" && id.Name == g.recvName {
		if g.recvPtr {
			return "this", "->"
		}
		return "(*this)", "."
	}
	if g.ptrVars[id.Name] {
		return id.Name, "->"
	}
	return id.Name, "."
}

// isPointerExpr reports whether an expression produces a pointer (`&T{...}` or
// `new(T)`).
func isPointerExpr(e ast.Expr) bool {
	switch t := e.(type) {
	case *ast.UnaryExpr:
		return t.Op == "&"
	case *ast.CallExpr:
		if id, ok := t.Fun.(*ast.Ident); ok {
			return id.Name == "new"
		}
	}
	return false
}

func (g *Generator) isMapExpr(e ast.Expr) bool {
	id, ok := e.(*ast.Ident)
	if !ok {
		return false
	}
	t, ok := g.types[id.Name]
	if !ok {
		return false
	}
	_, isMap := t.(*ast.MapType)
	if isMap {
		return true
	}
	if alias, ok := t.(*ast.Ident); ok {
		if target, ok := g.aliases[alias.Name]; ok {
			_, isMap := target.(*ast.MapType)
			return isMap
		}
	}
	return false
}

func (g *Generator) emitSwitch(st *ast.SwitchStmt) {
	// Lower to a goto-based switch so that string tags and fallthrough work.
	g.switchCounter++
	n := g.switchCounter
	tag := "true"
	if st.Tag != nil {
		tag = g.emitExpr(st.Tag)
	}
	g.writeln("{")
	g.indent++
	if st.Init != nil {
		g.writeln("%s;", g.inlineStmt(st.Init))
	}
	g.writeln("auto&& _sw%d = %s;", n, tag)
	// dispatch
	for i, c := range st.Body {
		if c.List == nil {
			g.writeln("goto _sw%d_default;", n)
			continue
		}
		for _, e := range c.List {
			g.writeln("if (_sw%d == %s) goto _sw%d_case%d;", n, g.emitExpr(e), n, i)
		}
	}
	g.writeln("goto _sw%d_end;", n)
	// bodies
	for i, c := range st.Body {
		if c.List == nil {
			g.writeln("_sw%d_default:", n)
		} else {
			g.writeln("_sw%d_case%d:", n, i)
		}
		g.writeln("{")
		g.indent++
		for _, s := range c.Body {
			if b, ok := s.(*ast.BranchStmt); ok && b.Tok == "fallthrough" {
				next := g.nextCaseLabel(st, i)
				g.writeln("goto %s;", next)
				continue
			}
			g.emitStmt(s)
		}
		g.writeln("goto _sw%d_end;", n)
		g.indent--
		g.writeln("}")
	}
	g.writeln("_sw%d_end:;", n)
	g.indent--
	g.writeln("}")
}

func (g *Generator) nextCaseLabel(st *ast.SwitchStmt, i int) string {
	n := g.switchCounter
	if i+1 < len(st.Body) {
		next := st.Body[i+1]
		if next.List == nil {
			return fmt.Sprintf("_sw%d_default", n)
		}
		return fmt.Sprintf("_sw%d_case%d", n, i+1)
	}
	return fmt.Sprintf("_sw%d_end", n)
}

func (g *Generator) emitBranch(b *ast.BranchStmt) {
	switch b.Tok {
	case "break":
		g.writeln("break;")
	case "continue":
		g.writeln("continue;")
	case "goto":
		g.writeln("goto %s;", b.Label)
	case "fallthrough":
		g.fail("fallthrough outside switch")
	}
}

// emitSelect lowers `select` to a try-each-case loop. Each case is attempted
// non-blockingly; when nothing is ready the iteration yields and retries. A
// `default` clause runs immediately. This is not the most efficient scheduling
// but it is correct for the common patterns and needs no central scheduler.
func (g *Generator) emitSelect(st *ast.SelectStmt) {
	g.needRuntime = true
	g.selectCounter++
	n := g.selectCounter
	g.writeln("{")
	g.indent++
	g.writeln("bool _sel%d = false;", n)
	g.writeln("while (!_sel%d)", n)
	g.writeln("{")
	g.indent++
	g.writeln("do")
	g.writeln("{")
	g.indent++
	for i, c := range st.Body {
		g.emitCommClause(c, n, i)
	}
	g.indent--
	g.writeln("} while (false);")
	g.writeln("if (!_sel%d) std::this_thread::yield();", n)
	g.indent--
	g.writeln("}")
	g.indent--
	g.writeln("}")
}

func (g *Generator) emitCommClause(c *ast.CommClause, n, i int) {
	finish := func() {
		g.emitStmts(c.Body)
		g.writeln("_sel%d = true;", n)
		g.writeln("break;")
	}
	if c.Comm == nil {
		// default clause
		finish()
		return
	}
	switch comm := c.Comm.(type) {
	case *ast.SendStmt:
		g.writeln("if ((%s).try_send(%s))", g.emitExpr(comm.Chan), g.emitExpr(comm.Value))
		g.writeln("{")
		g.indent++
		finish()
		g.indent--
		g.writeln("}")
	case *ast.AssignStmt:
		if len(comm.Rhs) == 1 {
			if u, ok := comm.Rhs[0].(*ast.UnaryExpr); ok && u.Op == "<-" {
				ch := g.emitExpr(u.X)
				g.writeln("std::remove_reference_t<decltype(%s)>::value_type _selv%d_%d;", ch, n, i)
				g.writeln("if ((%s).try_recv(_selv%d_%d))", ch, n, i)
				g.writeln("{")
				g.indent++
				if id, ok := comm.Lhs[0].(*ast.Ident); ok && id.Name != "_" {
					g.writeln("auto %s = _selv%d_%d;", id.Name, n, i)
				}
				finish()
				g.indent--
				g.writeln("}")
				return
			}
		}
		g.fail("unsupported select assignment")
	case *ast.ExprStmt:
		if u, ok := comm.X.(*ast.UnaryExpr); ok && u.Op == "<-" {
			ch := g.emitExpr(u.X)
			g.writeln("std::remove_reference_t<decltype(%s)>::value_type _selv%d_%d;", ch, n, i)
			g.writeln("if ((%s).try_recv(_selv%d_%d))", ch, n, i)
			g.writeln("{")
			g.indent++
			finish()
			g.indent--
			g.writeln("}")
			return
		}
		g.fail("unsupported select expression")
	default:
		g.fail("unsupported select case %T", c.Comm)
	}
}

// emitTypeSwitch lowers `switch v := x.(type)` to an is<T>()/as<T>() if-chain.
func (g *Generator) emitTypeSwitch(st *ast.TypeSwitchStmt) {
	g.needAny = true
	varName := ""
	var X ast.Expr
	if as, ok := st.Assign.(*ast.AssignStmt); ok && len(as.Rhs) == 1 {
		if ta, ok := as.Rhs[0].(*ast.TypeAssertExpr); ok {
			X = ta.X
		}
		if len(as.Lhs) == 1 {
			if id, ok := as.Lhs[0].(*ast.Ident); ok {
				varName = id.Name
			}
		}
	}
	if X == nil {
		g.fail("unsupported type switch")
		return
	}
	g.writeln("{")
	g.indent++
	g.writeln("auto _tsw = %s;", g.emitExpr(X))
	for i, c := range st.Body {
		cond := ""
		if len(c.List) > 0 {
			var conds []string
			for _, te := range c.List {
				if id, ok := te.(*ast.Ident); ok && id.Name == "nil" {
					conds = append(conds, "!_tsw.has_value()")
					continue
				}
				conds = append(conds, fmt.Sprintf("_tsw.is<%s>()", g.cppType(te)))
			}
			cond = strings.Join(conds, " || ")
		}
		switch {
		case i == 0 && cond == "":
			g.writeln("{")
		case i == 0:
			g.writeln("if (%s)", cond)
			g.writeln("{")
		case cond == "":
			g.writeln("else")
			g.writeln("{")
		default:
			g.writeln("else if (%s)", cond)
			g.writeln("{")
		}
		g.indent++
		if varName != "" && varName != "_" {
			if len(c.List) == 1 {
				if id, ok := c.List[0].(*ast.Ident); !ok || id.Name != "nil" {
					g.writeln("auto %s = _tsw.as<%s>();", varName, g.cppType(c.List[0]))
				} else {
					g.writeln("auto %s = _tsw;", varName)
				}
			} else {
				g.writeln("auto %s = _tsw;", varName)
			}
		}
		g.emitStmts(c.Body)
		g.indent--
		g.writeln("}")
	}
	g.indent--
	g.writeln("}")
}

func (g *Generator) emitDefer(d *ast.DeferStmt) {
	g.deferCounter++
	body := g.deferBody(d.Call)
	g.writeln("auto _defer%d = std::shared_ptr<void>(nullptr, [&](...) { %s });", g.deferCounter, body)
}

func (g *Generator) deferBody(call ast.Expr) string {
	if fl, ok := call.(*ast.FuncLit); ok {
		var b strBuilder
		sub := &Generator{
			sb:           b,
			structNames:  g.structNames,
			structFields: g.structFields,
			aliases:      g.aliases,
			types:        g.types,
			indent:       0,
		}
		sub.emitStmts(fl.Body.List)
		return strings.TrimSpace(sub.sb.String())
	}
	return g.emitExpr(call) + ";"
}

// ------------------------------------------------------------ expressions

func (g *Generator) emitExpr(e ast.Expr) string {
	switch t := e.(type) {
	case *ast.Ident:
		if g.recvName != "" && t.Name == g.recvName {
			if g.recvPtr {
				return "this"
			}
			return "(*this)"
		}
		return t.Name
	case *ast.BasicLit:
		return g.emitLit(t)
	case *ast.ParenExpr:
		return "(" + g.emitExpr(t.X) + ")"
	case *ast.BinaryExpr:
		op := t.Op
		if op == "&^" {
			return fmt.Sprintf("(%s & ~%s)", g.emitExpr(t.X), g.emitExpr(t.Y))
		}
		return fmt.Sprintf("(%s %s %s)", g.emitExpr(t.X), op, g.emitExpr(t.Y))
	case *ast.UnaryExpr:
		if t.Op == "<-" {
			g.needRuntime = true
			return "(" + g.emitExpr(t.X) + ").recv()"
		}
		if t.Op == "&" {
			if _, ok := t.X.(*ast.CompositeLit); ok {
				return "new " + g.emitExpr(t.X)
			}
		}
		if t.Op == "^" {
			return "(~" + g.emitExpr(t.X) + ")"
		}
		return "(" + t.Op + g.emitExpr(t.X) + ")"
	case *ast.StarExpr:
		return "(*" + g.emitExpr(t.X) + ")"
	case *ast.SelectorExpr:
		if id, ok := t.X.(*ast.Ident); ok {
			base, sep := g.selBase(id)
			return base + sep + t.Sel
		}
		return g.emitExpr(t.X) + "." + t.Sel
	case *ast.IndexExpr:
		return fmt.Sprintf("%s[%s]", g.emitExpr(t.X), g.emitExpr(t.Index))
	case *ast.SliceExpr:
		return g.emitSlice(t)
	case *ast.CallExpr:
		return g.emitCall(t)
	case *ast.CompositeLit:
		return g.emitComposite(t)
	case *ast.KeyValueExpr:
		return fmt.Sprintf("{ %s, %s }", g.emitExpr(t.Key), g.emitExpr(t.Value))
	case *ast.FuncLit:
		return g.emitFuncLit(t)
	case *ast.TypeAssertExpr:
		if t.Type == nil {
			return g.emitExpr(t.X)
		}
		g.needAny = true
		return fmt.Sprintf("(%s).as<%s>()", g.emitExpr(t.X), g.cppType(t.Type))
	}
	g.fail("unsupported expression %T", e)
	return "/*?*/"
}

func (g *Generator) emitLit(l *ast.BasicLit) string {
	if strings.HasPrefix(l.Value, "`") {
		inner := strings.TrimSuffix(strings.TrimPrefix(l.Value, "`"), "`")
		return `R"(` + inner + `)"`
	}
	return l.Value
}

func (g *Generator) emitSlice(s *ast.SliceExpr) string {
	x := g.emitExpr(s.X)
	if g.isStringExpr(s.X) {
		low := "0"
		if s.Low != nil {
			low = g.emitExpr(s.Low)
		}
		if s.High != nil {
			return fmt.Sprintf("(%s).substr(%s, (%s) - (%s))", x, low, g.emitExpr(s.High), low)
		}
		return fmt.Sprintf("(%s).substr(%s)", x, low)
	}
	low := "0"
	if s.Low != nil {
		low = g.emitExpr(s.Low)
	}
	high := fmt.Sprintf("%s.size()", x)
	if s.High != nil {
		high = g.emitExpr(s.High)
	}
	return fmt.Sprintf("_slice(%s, %s, %s)", x, low, high)
}

func (g *Generator) isStringExpr(e ast.Expr) bool {
	if id, ok := e.(*ast.Ident); ok {
		if t, ok := g.types[id.Name]; ok {
			if tid, ok := t.(*ast.Ident); ok && tid.Name == "string" {
				return true
			}
		}
	}
	return false
}

func (g *Generator) emitCall(c *ast.CallExpr) string {
	// builtins
	if id, ok := c.Fun.(*ast.Ident); ok {
		switch id.Name {
		case "len":
			g.needLen()
			return fmt.Sprintf("static_cast<int>(std::size(%s))", g.emitExpr(c.Args[0]))
		case "println", "print":
			g.usesBuiltinOut = true
			return g.emitPrint(c.Args, "std::cerr", id.Name == "println")
		case "string":
			return fmt.Sprintf("std::string(%s)", g.emitExpr(c.Args[0]))
		case "int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32", "uint64", "byte", "rune", "float32", "float64", "bool",
			"make", "append", "new", "cap", "copy":
			return g.emitBuiltinConv(id.Name, c)
		}
		if g.isTypeName(id.Name) {
			return fmt.Sprintf("%s(%s)", g.cppType(id), g.emitArgs(c.Args))
		}
	}
	// selectors: fmt.*, strings.*, strconv.*
	if sel, ok := c.Fun.(*ast.SelectorExpr); ok {
		if pkg, ok := sel.X.(*ast.Ident); ok {
			switch pkg.Name {
			case "fmt":
				return g.emitFmt(sel.Sel, c)
			case "strings":
				return g.emitStrings(sel.Sel, c)
			case "strconv":
				return g.emitStrconv(sel.Sel, c)
			}
		}
	}
	// selector calls: methods and function values
	if sel, ok := c.Fun.(*ast.SelectorExpr); ok {
		if id, ok := sel.X.(*ast.Ident); ok {
			base, sep := g.selBase(id)
			return base + sep + sel.Sel + "(" + g.emitArgs(c.Args) + ")"
		}
		return g.emitExpr(sel.X) + "." + sel.Sel + "(" + g.emitArgs(c.Args) + ")"
	}
	return fmt.Sprintf("%s(%s)", g.emitExpr(c.Fun), g.emitArgs(c.Args))
}

func (g *Generator) emitArgs(args []ast.Expr) string {
	var parts []string
	for _, a := range args {
		parts = append(parts, g.emitExpr(a))
	}
	return strings.Join(parts, ", ")
}

func (g *Generator) isTypeName(name string) bool {
	if g.structNames[name] {
		return true
	}
	if _, ok := g.aliases[name]; ok {
		return true
	}
	return false
}

func (g *Generator) emitBuiltinConv(name string, c *ast.CallExpr) string {
	switch name {
	case "make":
		// make([]T, n) -> std::vector<T>(n)
		if len(c.Args) >= 1 {
			if sl, ok := c.Args[0].(*ast.SliceType); ok {
				n := ""
				if len(c.Args) >= 2 {
					n = g.emitExpr(c.Args[1])
				}
				return fmt.Sprintf("std::vector<%s>(%s)", g.cppType(sl.Elem), n)
			}
			if mt, ok := c.Args[0].(*ast.MapType); ok {
				return fmt.Sprintf("std::unordered_map<%s, %s>{}", g.cppType(mt.Key), g.cppType(mt.Value))
			}
			if ct, ok := c.Args[0].(*ast.ChanType); ok {
				g.needRuntime = true
				n := ""
				if len(c.Args) >= 2 {
					n = g.emitExpr(c.Args[1])
				}
				return fmt.Sprintf("go2cpp::channel<%s>(%s)", g.cppType(ct.Value), n)
			}
		}
		return "{}"
	case "append":
		// approximately: append(vec, x) -> helper
		g.needLen()
		var parts []string
		for _, a := range c.Args {
			parts = append(parts, g.emitExpr(a))
		}
		return fmt.Sprintf("_append(%s)", strings.Join(parts, ", "))
	case "new":
		return "new " + g.cppType(c.Args[0]) + "()"
	case "cap":
		return fmt.Sprintf("static_cast<int>(std::size(%s))", g.emitExpr(c.Args[0]))
	case "copy":
		return "0"
	}
	// numeric / bool conversion
	return fmt.Sprintf("static_cast<%s>(%s)", g.cppType(&ast.Ident{Name: name}), g.emitExpr(c.Args[0]))
}

func (g *Generator) needLen() {}

func (g *Generator) emitFmt(sel string, c *ast.CallExpr) string {
	switch sel {
	case "Print":
		g.usesPrint = true
		return g.emitPrint(c.Args, "std::cout", false)
	case "Println":
		g.usesPrint = true
		return g.emitPrint(c.Args, "std::cout", true)
	case "Printf":
		g.needPrintf = true
		g.usesPrint = true
		return g.emitPrintfCall(c.Args)
	case "Sprintf":
		g.needFmtSprintf = true
		var parts []string
		for _, a := range c.Args {
			parts = append(parts, g.emitExpr(a))
		}
		return fmt.Sprintf("fmtSprintf(%s)", strings.Join(parts, ", "))
	case "Fprintln", "Fprint":
		g.usesPrint = true
		return g.emitPrint(c.Args[1:], g.emitExpr(c.Args[0]), sel == "Fprintln")
	}
	g.fail("unsupported fmt.%s", sel)
	return "/*?*/"
}

// emitPrint builds a sequence of `stream << ...` statements. fmt.Println
// inserts a space between every operand and a trailing newline.
func (g *Generator) emitPrint(args []ast.Expr, stream string, newline bool) string {
	g.usesPrint = true
	var parts []string
	for i, a := range args {
		if i > 0 {
			parts = append(parts, stream+` << " "`)
		}
		parts = append(parts, "_format_output("+stream+", "+g.emitExpr(a)+")")
	}
	if newline {
		parts = append(parts, stream+" << std::endl")
	}
	if len(parts) == 0 {
		return stream
	}
	return strings.Join(parts, "; ")
}

func (g *Generator) emitPrintfCall(args []ast.Expr) string {
	if len(args) == 0 {
		return "printf(\"\")"
	}
	fmtLit := ""
	if bl, ok := args[0].(*ast.BasicLit); ok {
		fmtLit = strings.Trim(bl.Value, "\"")
		fmtLit = strings.ReplaceAll(fmtLit, "\\n", "\\n")
	}
	var rest []string
	for _, a := range args[1:] {
		rest = append(rest, g.cstr(g.emitExpr(a)))
	}
	if len(rest) == 0 {
		return fmt.Sprintf("printf(\"%s\")", fmtLit)
	}
	return fmt.Sprintf("printf(\"%s\", %s)", fmtLit, strings.Join(rest, ", "))
}

func (g *Generator) cstr(s string) string {
	if strings.HasPrefix(s, `"`) || strings.HasPrefix(s, `R"`) {
		return s
	}
	return s + ".c_str()"
}

func (g *Generator) emitStrings(sel string, c *ast.CallExpr) string {
	args := g.emitArgs(c.Args)
	switch sel {
	case "Contains":
		g.needContains = true
		return "stringsContains(" + args + ")"
	case "HasPrefix":
		g.needHasPrefix = true
		return "stringsHasPrefix(" + args + ")"
	case "HasSuffix":
		g.needHasSuffix = true
		return "stringsHasSuffix(" + args + ")"
	case "TrimSpace":
		g.needTrimSpace = true
		return "stringsTrimSpace(" + args + ")"
	case "Split":
		g.needSplit = true
		return "stringsSplit(" + args + ")"
	case "SplitN":
		g.needSplit = true
		return "stringsSplitN(" + args + ")"
	case "Join":
		g.needSplit = true
		return "stringsJoin(" + args + ")"
	case "Index":
		g.needIndex = true
		return "stringsIndex(" + args + ")"
	case "Replace":
		g.needReplace = true
		return "stringsReplace(" + args + ")"
	}
	g.fail("unsupported strings.%s", sel)
	return "/*?*/"
}

func (g *Generator) emitStrconv(sel string, c *ast.CallExpr) string {
	g.needStrconv = true
	switch sel {
	case "ParseFloat":
		return "strconvParseFloat(" + g.emitArgs(c.Args) + ")"
	case "ParseInt":
		return "strconvParseInt(" + g.emitArgs(c.Args) + ")"
	case "Itoa":
		return "std::to_string(" + g.emitArgs(c.Args) + ")"
	}
	g.fail("unsupported strconv.%s", sel)
	return "/*?*/"
}

func (g *Generator) emitFuncLit(fl *ast.FuncLit) string {
	var b strBuilder
	b.WriteString("[&](")
	b.WriteString(g.emitParams(fl.Type.Params))
	b.WriteString(") -> ")
	b.WriteString(g.resultType(fl.Type.Results))
	b.WriteString(" {\n")
	sub := &Generator{
		structNames:  g.structNames,
		structFields: g.structFields,
		aliases:      g.aliases,
		types:        g.types,
		indent:       1,
		curResults:   fl.Type.Results,
	}
	sub.emitStmts(fl.Body.List)
	b.WriteString(sub.sb.String())
	b.WriteString("}")
	return b.String()
}

func (g *Generator) emitComposite(c *ast.CompositeLit) string {
	switch t := c.Type.(type) {
	case *ast.MapType:
		var elts []string
		for _, e := range c.Elts {
			elts = append(elts, g.emitExpr(e))
		}
		return fmt.Sprintf("std::unordered_map<%s, %s>{ %s }", g.cppType(t.Key), g.cppType(t.Value), strings.Join(elts, ", "))
	case *ast.SliceType:
		var elts []string
		for _, e := range c.Elts {
			elts = append(elts, g.emitExpr(e))
		}
		return fmt.Sprintf("std::vector<%s>{ %s }", g.cppType(t.Elem), strings.Join(elts, ", "))
	case *ast.ArrayType:
		var elts []string
		for _, e := range c.Elts {
			elts = append(elts, g.emitExpr(e))
		}
		return fmt.Sprintf("std::vector<%s>{ %s }", g.cppType(t.Elem), strings.Join(elts, ", "))
	case *ast.Ident:
		var elts []string
		for _, e := range c.Elts {
			if kv, ok := e.(*ast.KeyValueExpr); ok {
				if name, ok := kv.Key.(*ast.Ident); ok && !g.structNames[kv.Key.(*ast.Ident).Name] {
					elts = append(elts, "."+name.Name+" = "+g.emitExpr(kv.Value))
					continue
				}
			}
			elts = append(elts, g.emitExpr(e))
		}
		if g.structNames[t.Name] || g.isTypeName(t.Name) {
			return fmt.Sprintf("%s{ %s }", t.Name, strings.Join(elts, ", "))
		}
		return fmt.Sprintf("%s{ %s }", t.Name, strings.Join(elts, ", "))
	case nil:
		var elts []string
		for _, e := range c.Elts {
			elts = append(elts, g.emitExpr(e))
		}
		return "{ " + strings.Join(elts, ", ") + " }"
	}
	// e.g. pkg.Type{...}
	var elts []string
	for _, e := range c.Elts {
		elts = append(elts, g.emitExpr(e))
	}
	return fmt.Sprintf("%s{ %s }", g.cppType(c.Type), strings.Join(elts, ", "))
}

// ---------------------------------------------------------------- preamble

func (g *Generator) preamble() string {
	var b strBuilder
	b.WriteString("// Generated by go2cpp (AST front-end).\n")
	b.WriteString("#include <iostream>\n")
	b.WriteString("#include <string>\n")
	b.WriteString("#include <sstream>\n")
	b.WriteString("#include <vector>\n")
	b.WriteString("#include <unordered_map>\n")
	b.WriteString("#include <tuple>\n")
	b.WriteString("#include <memory>\n")
	b.WriteString("#include <iterator>\n")
	b.WriteString("#include <type_traits>\n")
	b.WriteString("#include <experimental/type_traits>\n")
	b.WriteString("#include <cstdint>\n")
	if g.needPrintf || g.needFmtSprintf {
		b.WriteString("#include <cstdio>\n")
	}
	b.WriteString("#include <functional>\n")
	b.WriteString("#include <algorithm>\n")
	if g.needAny {
		b.WriteString("#include <typeindex>\n")
		b.WriteString("#include <typeinfo>\n")
	}
	if g.needRuntime {
		b.WriteString("#include <thread>\n")
		b.WriteString("#include <mutex>\n")
		b.WriteString("#include <condition_variable>\n")
		b.WriteString("#include <deque>\n")
		b.WriteString("#include <stdexcept>\n")
	}
	b.WriteString("\n")

	if g.needAny {
		b.WriteString("#include <memory>\n")
	}
	if g.usesPrint || g.needAny {
		b.WriteString(strconvHelperFormatOutput)
	}
	if g.needAny {
		b.WriteString(helperAny)
	}
	if g.needRuntime {
		b.WriteString(helperConcurrency)
	}
	if g.needContains {
		b.WriteString(helperContains)
	}
	if g.needHasPrefix {
		b.WriteString(helperHasPrefix)
	}
	if g.needHasSuffix {
		b.WriteString(helperHasSuffix)
	}
	if g.needTrimSpace {
		b.WriteString(helperTrimSpace)
	}
	if g.needSplit {
		b.WriteString(helperSplit)
	}
	if g.needIndex {
		b.WriteString(helperIndex)
	}
	if g.needReplace {
		b.WriteString(helperReplace)
	}
	if g.needFmtSprintf {
		b.WriteString(helperFmtSprintf)
	}
	if g.needStrconv {
		b.WriteString(helperStrconv)
	}
	b.WriteString(helperSlice)
	b.WriteString("\n")
	return b.String()
}

const strconvHelperFormatOutput = `
template <typename T>
using _str_t = decltype(std::declval<T&>()._str());
template <typename T>
using _p_str_t = decltype(std::declval<T&>()->_str());

template <typename T>
void _format_output(std::ostream& out, T x)
{
    if constexpr (std::is_same<T, bool>::value) {
        out << std::boolalpha << x << std::noboolalpha;
    } else if constexpr (std::is_integral<T>::value) {
        out << static_cast<int>(x);
    } else if constexpr (!std::is_pointer<T>::value && std::experimental::is_detected_v<_str_t, T>) {
        out << x._str();
    } else if constexpr (std::is_pointer<T>::value && std::experimental::is_detected_v<_p_str_t, T>) {
        out << "&" << x->_str();
    } else {
        out << x;
    }
}
`

const helperContains = `
inline bool stringsContains(const std::string& h, const std::string& n) { return h.find(n) != std::string::npos; }
`

const helperHasPrefix = `
inline bool stringsHasPrefix(const std::string& h, const std::string& p) { return h.rfind(p, 0) == 0; }
`

const helperHasSuffix = `
inline bool stringsHasSuffix(const std::string& h, const std::string& s) {
    return h.size() >= s.size() && h.compare(h.size() - s.size(), s.size(), s) == 0;
}
`

const helperTrimSpace = `
inline std::string stringsTrimSpace(const std::string& s) {
    const char* ws = " \t\n\v\f\r";
    size_t a = s.find_first_not_of(ws);
    if (a == std::string::npos) return "";
    size_t b = s.find_last_not_of(ws);
    return s.substr(a, b - a + 1);
}
`

const helperSplit = `
inline std::vector<std::string> stringsSplit(const std::string& s, const std::string& sep) {
    std::vector<std::string> out;
    if (sep.empty()) { out.push_back(s); return out; }
    size_t start = 0, pos;
    while ((pos = s.find(sep, start)) != std::string::npos) {
        out.push_back(s.substr(start, pos - start));
        start = pos + sep.size();
    }
    out.push_back(s.substr(start));
    return out;
}
inline std::string stringsJoin(const std::vector<std::string>& parts, const std::string& sep) {
    std::string out;
    for (size_t i = 0; i < parts.size(); i++) { if (i) out += sep; out += parts[i]; }
    return out;
}
inline std::vector<std::string> stringsSplitN(const std::string& s, const std::string& sep, int n) {
    std::vector<std::string> out;
    if (n <= 0) { out.push_back(s); return out; }
    if (sep.empty()) {
        for (char ch : s) { if ((int)out.size() == n) { out.back() += ch; } else { out.push_back(std::string(1, ch)); } }
        return out;
    }
    size_t start = 0, pos; int c = 1;
    while (c < n && (pos = s.find(sep, start)) != std::string::npos) {
        out.push_back(s.substr(start, pos - start)); start = pos + sep.size(); c++;
    }
    out.push_back(s.substr(start));
    return out;
}
`

const helperIndex = `
inline int stringsIndex(const std::string& h, const std::string& n) {
    auto p = h.find(n);
    return p == std::string::npos ? -1 : static_cast<int>(p);
}
`

const helperReplace = `
inline std::string stringsReplace(const std::string& s, const std::string& oldS, const std::string& newS, int n) {
    if (oldS.empty() || n == 0) return s;
    std::string out = s; std::string::size_type pos = 0; int c = 0;
    while ((n < 0 || c < n) && (pos = out.find(oldS, pos)) != std::string::npos) {
        out.replace(pos, oldS.size(), newS); pos += newS.size(); c++;
    }
    return out;
}
`

const helperFmtSprintf = `
inline std::string _to_str(const std::string& s) { return s; }
inline std::string _to_str(const char* s) { return std::string(s); }
template <typename T> std::string _to_str(T v) { return std::to_string(v); }

inline void _replace_first_spec(std::string& s, const std::string& val) {
    size_t best = std::string::npos;
    for (const char* spec : { "%v", "%d", "%s", "%f" }) {
        size_t q = s.find(spec);
        if (q != std::string::npos && (best == std::string::npos || q < best)) best = q;
    }
    if (best != std::string::npos) s.replace(best, 2, val);
}

template <typename... Args>
std::string fmtSprintf(const std::string& fmt, Args... args) {
    std::string out = fmt;
    int _dummy[] = { 0, (_replace_first_spec(out, _to_str(args)), 0)... };
    (void)_dummy;
    return out;
}
`

const helperStrconv = `
inline std::tuple<double, bool> strconvParseFloat(const std::string& s, int) {
    try { return { std::stod(s), true }; } catch (...) { return { 0.0, false }; }
}
inline std::tuple<long, bool> strconvParseInt(const std::string& s, int, int) {
    try { return { std::stol(s, nullptr, 0), true }; } catch (...) { return { 0, false }; }
}
`

const helperAny = `
namespace go2cpp {

// any is the runtime representation of interface{}: a type-erased, printable
// box with dynamic type identity, backing type assertions and type switches.
class any {
    struct base {
        virtual ~base() {}
        virtual void print(std::ostream&) const = 0;
        virtual std::type_index type() const = 0;
    };
    template <typename T> struct holder : base {
        T v;
        holder(T x) : v(std::move(x)) {}
        void print(std::ostream& o) const override { _format_output(o, v); }
        std::type_index type() const override { return std::type_index(typeid(T)); }
    };
    std::shared_ptr<base> p_;
public:
    any() {}
    template <typename T>
    using stored_t = std::conditional_t<
        std::is_same<std::decay_t<T>, const char*>::value || std::is_same<std::decay_t<T>, char*>::value,
        std::string, std::decay_t<T>>;
    template <typename T, typename = std::enable_if_t<!std::is_same<std::decay_t<T>, any>::value>>
    any(T x) : p_(std::make_shared<holder<stored_t<T>>>(stored_t<T>(std::move(x)))) {}
    template <typename T, typename = std::enable_if_t<!std::is_same<std::decay_t<T>, any>::value>>
    any& operator=(T x) { p_ = std::make_shared<holder<stored_t<T>>>(stored_t<T>(std::move(x))); return *this; }
    bool has_value() const { return static_cast<bool>(p_); }
    template <typename T> bool is() const { return std::dynamic_pointer_cast<holder<T>>(p_) != nullptr; }
    template <typename T> T as() const {
        auto h = std::dynamic_pointer_cast<holder<T>>(p_);
        if (!h) throw std::runtime_error("interface conversion: bad type assertion");
        return h->v;
    }
    std::type_index type() const { return p_ ? p_->type() : std::type_index(typeid(void)); }
    friend std::ostream& operator<<(std::ostream& o, const any& a) { if (a.p_) a.p_->print(o); return o; }
};

} // namespace go2cpp
`

const helperConcurrency = `
namespace go2cpp {

// channel is a Go-like channel: cap()==0 gives an unbuffered rendezvous,
// cap()>0 a bounded buffer.
template <typename T>
class channel {
public:
    using value_type = T;
    explicit channel(std::size_t cap = 0) : cap_(cap) {}

    void send(const T& v) {
        std::unique_lock<std::mutex> lk(m_);
        while (true) {
            if (closed_) throw std::runtime_error("send on closed channel");
            if (recv_waiting_ > 0 || (cap_ > 0 && buf_.size() < cap_)) {
                buf_.push_back(v);
                cv_.notify_all();
                return;
            }
            cv_.wait(lk);
        }
    }

    T recv() {
        std::unique_lock<std::mutex> lk(m_);
        recv_waiting_++;
        cv_.notify_all();
        cv_.wait(lk, [&] { return !buf_.empty() || closed_; });
        recv_waiting_--;
        if (buf_.empty()) return T{};
        T v = buf_.front();
        buf_.pop_front();
        cv_.notify_all();
        return v;
    }

    bool try_recv(T& out) {
        std::unique_lock<std::mutex> lk(m_);
        if (buf_.empty()) return false;
        out = buf_.front();
        buf_.pop_front();
        cv_.notify_all();
        return true;
    }

    bool try_send(const T& v) {
        std::unique_lock<std::mutex> lk(m_);
        if (closed_) return false;
        if (recv_waiting_ > 0 || (cap_ > 0 && buf_.size() < cap_)) {
            buf_.push_back(v);
            cv_.notify_all();
            return true;
        }
        return false;
    }

    void close() {
        std::unique_lock<std::mutex> lk(m_);
        closed_ = true;
        cv_.notify_all();
    }

private:
    std::mutex m_;
    std::condition_variable cv_;
    std::deque<T> buf_;
    std::size_t cap_;
    bool closed_ = false;
    int recv_waiting_ = 0;
};

template <typename F>
void go(F f) { std::thread(std::move(f)).detach(); }

} // namespace go2cpp
`

const helperSlice = `
template <typename T>
std::vector<T> _slice(const std::vector<T>& v, std::size_t lo, std::size_t hi) {
    if (hi < lo) hi = lo;
    return std::vector<T>(v.begin() + lo, v.begin() + hi);
}

template <typename T, typename... Rest>
std::vector<T> _append(std::vector<T> v, Rest... rest) {
    (v.push_back(rest), ...);
    return v;
}
`
