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

// Package is one Go package's declarations, for whole-program generation.
type Package struct {
	Name  string
	Decls []ast.Decl
}

// Generator accumulates generated C++ source.
type Generator struct {
	sb     strBuilder
	indent int

	structNames      map[string]bool
	structFields     map[string][]string
	structFieldTypes map[string][]ast.Expr
	aliases          map[string]ast.Expr
	types            map[string]ast.Expr
	methods          map[string][]*ast.FuncDecl
	embedOf          map[string][]string
	ptrVars          map[string]bool
	recvName         string
	recvPtr          bool

	ifaces      map[string]*ast.InterfaceType
	ifaceOrder  []string
	ifaceOwn    map[string][]ifaceMeth
	ifaceEmbeds map[string][]string

	pkgNames map[string]bool
	ptrFuncs map[string]bool

	fieldTypes  map[string]ast.Expr
	funcResults map[string]ast.Expr

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
	needError      bool
	needUnicode    bool
	needUtf8       bool
	needGoString   bool

	switchCounter int
	tmpCounter    int
	selectCounter int
	deferCounter  int
	iotaIndex     int

	curResults []*ast.Field
	err        error
}

// ifaceMeth is one method in an interface's method set.
type ifaceMeth struct {
	name string
	ft   *ast.FuncType
}

// Generate compiles a parsed file to C++ source text.
func Generate(f *ast.File) (string, error) {
	g := &Generator{
		structNames:      map[string]bool{},
		structFields:     map[string][]string{},
		structFieldTypes: map[string][]ast.Expr{},
		aliases:          map[string]ast.Expr{},
		types:            map[string]ast.Expr{},
		methods:          map[string][]*ast.FuncDecl{},
		embedOf:          map[string][]string{},
		ptrVars:          map[string]bool{},
		ifaces:           map[string]*ast.InterfaceType{},
		ifaceOwn:         map[string][]ifaceMeth{},
		ifaceEmbeds:      map[string][]string{},
		pkgNames:         map[string]bool{},
		ptrFuncs:         map[string]bool{},
		fieldTypes:       map[string]ast.Expr{},
		funcResults:      map[string]ast.Expr{},
	}
	g.collectDecls(f.Decls)
	g.buildInterfaces()
	g.emitDecls(f.Decls)
	if g.err != nil {
		return "", g.err
	}
	return g.preamble() + g.sb.String(), nil
}

// GenerateProgram lowers several packages into a single translation unit, one
// C++ namespace per package ("main" stays at global scope).
func GenerateProgram(pkgs []*Package) (string, error) {
	g := &Generator{
		structNames:      map[string]bool{},
		structFields:     map[string][]string{},
		structFieldTypes: map[string][]ast.Expr{},
		aliases:          map[string]ast.Expr{},
		types:            map[string]ast.Expr{},
		methods:          map[string][]*ast.FuncDecl{},
		embedOf:          map[string][]string{},
		ptrVars:          map[string]bool{},
		ifaces:           map[string]*ast.InterfaceType{},
		ifaceOwn:         map[string][]ifaceMeth{},
		ifaceEmbeds:      map[string][]string{},
		pkgNames:         map[string]bool{},
		ptrFuncs:         map[string]bool{},
		fieldTypes:       map[string]ast.Expr{},
		funcResults:      map[string]ast.Expr{},
	}
	for _, p := range pkgs {
		if p.Name != "main" {
			g.pkgNames[p.Name] = true
		}
	}
	for _, p := range pkgs {
		g.collectDecls(p.Decls)
	}
	g.buildInterfaces()
	for _, p := range pkgs {
		g.emitPackage(p)
	}
	if g.err != nil {
		return "", g.err
	}
	return g.preamble() + g.sb.String(), nil
}

// emitPackage emits one package's declarations, namespaced unless it is `main`.
func (g *Generator) emitPackage(p *Package) {
	ns := p.Name != "main"
	if ns {
		g.writeln("namespace %s {", p.Name)
		g.indent++
	}
	g.emitDecls(p.Decls)
	if ns {
		g.indent--
		g.writeln("} // namespace %s", p.Name)
		g.writeln("")
	}
}

func (g *Generator) fail(format string, args ...interface{}) {
	if g.err == nil {
		g.err = fmt.Errorf("codegen: "+format, args...)
	}
}

// ---------------------------------------------------------------- pre-scan

func (g *Generator) collectDecls(decls []ast.Decl) {
	for _, d := range decls {
		if fd, ok := d.(*ast.FuncDecl); ok {
			if len(fd.Type.Results) == 1 && g.isPtrType(fd.Type.Results[0].Type) {
				g.ptrFuncs[fd.Name] = true
			}
			if len(fd.Type.Results) == 1 {
				g.funcResults[fd.Name] = fd.Type.Results[0].Type
			}
			if fd.Recv != nil && len(fd.Recv.List) > 0 {
				name, _ := baseTypeName(fd.Recv.List[0].Type)
				g.methods[name] = append(g.methods[name], fd)
				continue
			}
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
				var ftypes []ast.Expr
				for _, fld := range st.Fields {
					if len(fld.Names) == 0 {
						// embedded field: member named after its base type
						base, _ := baseTypeName(fld.Type)
						g.embedOf[ts.Name] = append(g.embedOf[ts.Name], base)
						continue
					}
					for _, n := range fld.Names {
						fields = append(fields, n)
						ftypes = append(ftypes, fld.Type)
						g.fieldTypes[ts.Name+"."+n] = fld.Type
					}
				}
				g.structFields[ts.Name] = fields
				g.structFieldTypes[ts.Name] = ftypes
			} else if it, ok := ts.Type.(*ast.InterfaceType); ok {
				g.ifaces[ts.Name] = it
				g.ifaceOrder = append(g.ifaceOrder, ts.Name)
			} else {
				g.aliases[ts.Name] = ts.Type
			}
		}
	}
}

// buildInterfaces computes each interface's method set, flattening embedded
// interfaces (which must be declared earlier in the file).
func (g *Generator) buildInterfaces() {
	for _, name := range g.ifaceOrder {
		it := g.ifaces[name]
		var methods []ifaceMeth
		for _, f := range it.Methods {
			if len(f.Names) == 0 {
				continue
			}
			mname := f.Names[0]
			if f.Type == nil {
				// embedded interface
				g.ifaceEmbeds[name] = append(g.ifaceEmbeds[name], mname)
				methods = append(methods, g.ifaceOwn[mname]...)
				continue
			}
			if ft, ok := f.Type.(*ast.FuncType); ok {
				methods = append(methods, ifaceMeth{name: mname, ft: ft})
			}
		}
		g.ifaceOwn[name] = methods
	}
}

// methodSet is the set of method names of a type, including promoted methods.
func (g *Generator) methodSet(typeName string) map[string]bool {
	out := map[string]bool{}
	for _, fd := range g.methods[typeName] {
		out[fd.Name] = true
	}
	for _, base := range g.embedOf[typeName] {
		for k := range g.methodSet(base) {
			out[k] = true
		}
	}
	return out
}

// implementsOf lists the named interfaces a type satisfies, reduced to the
// maximal ones (an interface embedded by another satisfied interface is
// dropped to avoid a diamond).
func (g *Generator) implementsOf(typeName string) []string {
	ms := g.methodSet(typeName)
	var satisfied []string
	for _, iname := range g.ifaceOrder {
		need := g.ifaceOwn[iname]
		if len(need) == 0 {
			continue
		}
		ok := true
		for _, m := range need {
			if !ms[m.name] {
				ok = false
				break
			}
		}
		if ok {
			satisfied = append(satisfied, iname)
		}
	}
	var out []string
	for _, i := range satisfied {
		redundant := false
		for _, j := range satisfied {
			if i == j {
				continue
			}
			if g.ifaceDerives(j, i) {
				redundant = true
				break
			}
		}
		if !redundant {
			out = append(out, i)
		}
	}
	return out
}

// ifaceDerives reports whether interface j embeds i transitively.
func (g *Generator) ifaceDerives(j, i string) bool {
	for _, b := range g.ifaceEmbeds[j] {
		if b == i || g.ifaceDerives(b, i) {
			return true
		}
	}
	return false
}

// recordParams records the declared types of named parameters/locals so later
// stages can tell interface values from concrete ones.
func (g *Generator) recordParams(fields []*ast.Field) {
	for _, fld := range fields {
		for _, n := range fld.Names {
			g.types[n] = fld.Type
			if g.isPtrType(fld.Type) {
				g.ptrVars[n] = true
			}
		}
	}
}

// isPtrType reports whether a type is represented as a C++ pointer (a named
// interface or an explicit pointer type).
func (g *Generator) isPtrType(e ast.Expr) bool {
	switch t := e.(type) {
	case *ast.PtrType, *ast.StarExpr:
		return true
	case *ast.SelectorExpr:
		return g.isIfaceName(t.Sel)
	case *ast.Ident:
		if t.Name == "error" {
			return true
		}
		if _, ok := g.ifaces[t.Name]; ok {
			if len(g.ifaceOwn[t.Name]) > 0 || len(g.ifaceEmbeds[t.Name]) > 0 {
				return true
			}
		}
	}
	return false
}

// namedIface reports whether an expression's static type is a named interface.
func (g *Generator) namedIface(e ast.Expr) bool {
	id, ok := e.(*ast.Ident)
	if !ok {
		return false
	}
	t, ok := g.types[id.Name]
	if !ok {
		return false
	}
	tid, ok := t.(*ast.Ident)
	if !ok {
		if sel, ok2 := t.(*ast.SelectorExpr); ok2 {
			return g.isIfaceName(sel.Sel)
		}
		return false
	}
	_, ok = g.ifaces[tid.Name]
	return ok
}

// isIfaceName reports whether a bare type name denotes a named interface.
func (g *Generator) isIfaceName(name string) bool {
	if name == "error" {
		return true
	}
	if _, ok := g.ifaces[name]; ok {
		return len(g.ifaceOwn[name]) > 0 || len(g.ifaceEmbeds[name]) > 0
	}
	return false
}

// exprType is a best-effort static type of an expression.
func (g *Generator) exprType(e ast.Expr) ast.Expr {
	switch t := e.(type) {
	case *ast.Ident:
		return g.types[t.Name]
	case *ast.ParenExpr:
		return g.exprType(t.X)
	case *ast.SelectorExpr:
		if id, ok := t.X.(*ast.Ident); ok && g.pkgNames[id.Name] {
			return nil
		}
		if ft, ok := g.fieldTypes[typeNameOf(g.exprType(t.X))+"."+t.Sel]; ok {
			return ft
		}
		return nil
	case *ast.IndexExpr:
		switch b := g.exprType(t.X).(type) {
		case *ast.SliceType:
			return b.Elem
		case *ast.ArrayType:
			return b.Elem
		case *ast.MapType:
			return b.Value
		}
		return nil
	case *ast.StarExpr:
		return g.exprType(t.X)
	case *ast.UnaryExpr:
		if t.Op == "*" {
			return g.exprType(t.X)
		}
		if t.Op == "&" {
			return &ast.PtrType{Elem: g.exprType(t.X)}
		}
		return nil
	case *ast.CallExpr:
		return g.funcResults[calleeNameOf(t.Fun)]
	}
	return nil
}

// isIfaceExpr reports whether an expression's static type is a named interface.
func (g *Generator) isIfaceExpr(e ast.Expr) bool {
	if g.namedIface(e) {
		return true
	}
	switch t := g.exprType(e).(type) {
	case *ast.Ident:
		return g.isIfaceName(t.Name)
	case *ast.SelectorExpr:
		return g.isIfaceName(t.Sel)
	}
	return false
}

// emitCommaOK emits `v, ok := x.(T)` as a checked dynamic cast.
func (g *Generator) emitCommaOK(lhs0, lhs1, rhs ast.Expr) bool {
	ta, ok := rhs.(*ast.TypeAssertExpr)
	if !ok || ta.Type == nil {
		return false
	}
	vName := identName(lhs0)
	if vName == "" || vName == "_" {
		vName = "_tv"
	}
	okName := identName(lhs1)
	ct := g.cppType(ta.Type)
	var expr string
	if strings.HasSuffix(ct, "*") {
		expr = fmt.Sprintf("dynamic_cast<%s>(%s)", ct, g.emitExpr(ta.X))
		g.ptrVars[vName] = true
	} else if g.isIfaceExpr(ta.X) {
		expr = fmt.Sprintf("(*dynamic_cast<%s*>(%s))", ct, g.emitExpr(ta.X))
	} else {
		g.needAny = true
		expr = fmt.Sprintf("(%s).as<%s>()", g.emitExpr(ta.X), ct)
	}
	g.writeln("auto %s = %s;", vName, expr)
	if ta.Type != nil {
		g.types[vName] = ta.Type
	}
	if okName != "" && okName != "_" {
		if strings.HasSuffix(ct, "*") {
			g.writeln("bool %s = (%s != nullptr);", okName, vName)
		} else {
			g.writeln("bool %s = true;", okName)
		}
	}
	return true
}

// emitMapCommaOK emits `v, ok := m[k]` using find().
func (g *Generator) emitMapCommaOK(lhs0, lhs1, rhs ast.Expr) bool {
	ix, ok := rhs.(*ast.IndexExpr)
	if !ok {
		return false
	}
	mt, ok := g.exprType(ix.X).(*ast.MapType)
	if !ok {
		return false
	}
	g.tmpCounter++
	tmp := fmt.Sprintf("_mfind%d", g.tmpCounter)
	vName := identName(lhs0)
	if vName == "" || vName == "_" {
		vName = "_mv"
	}
	okName := identName(lhs1)
	m := g.emitExpr(ix.X)
	g.writeln("auto %s = (%s).find(%s);", tmp, m, g.emitExpr(ix.Index))
	g.writeln("%s %s = (%s != (%s).end()) ? %s->second : %s;", g.cppType(mt.Value), vName, tmp, m, tmp, g.zeroValue(mt.Value))
	if okName != "" && okName != "_" {
		g.writeln("bool %s = (%s != (%s).end());", okName, tmp, m)
	}
	return true
}

// recordElem records the element type of a range-over-slice variable.
func (g *Generator) recordElem(name string, x ast.Expr) {
	switch t := g.exprType(x).(type) {
	case *ast.SliceType:
		g.types[name] = t.Elem
		if g.isPtrType(t.Elem) {
			g.ptrVars[name] = true
		}
	case *ast.ArrayType:
		g.types[name] = t.Elem
		if g.isPtrType(t.Elem) {
			g.ptrVars[name] = true
		}
	case *ast.MapType:
		g.types[name] = t.Value
		if g.isPtrType(t.Value) {
			g.ptrVars[name] = true
		}
	}
}

// subGenerator returns a Generator sharing all collected state, for emitting
// nested function bodies (closures, deferred calls).
func (g *Generator) subGenerator() *Generator {
	return &Generator{
		structNames:      g.structNames,
		structFields:     g.structFields,
		structFieldTypes: g.structFieldTypes,
		aliases:          g.aliases,
		types:            g.types,
		methods:          g.methods,
		embedOf:          g.embedOf,
		ptrVars:          map[string]bool{},
		ifaces:           g.ifaces,
		ifaceOrder:       g.ifaceOrder,
		ifaceOwn:         g.ifaceOwn,
		ifaceEmbeds:      g.ifaceEmbeds,
		pkgNames:         g.pkgNames,
		ptrFuncs:         g.ptrFuncs,
		fieldTypes:       g.fieldTypes,
		funcResults:      g.funcResults,
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
			g.needError = true
			return "go2cpp::error*"
		}
		if _, ok := g.ifaces[t.Name]; ok {
			if len(g.ifaceOwn[t.Name]) > 0 || len(g.ifaceEmbeds[t.Name]) > 0 {
				return t.Name + "*"
			}
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
	case *ast.UnaryExpr:
		if t.Op == "*" {
			// `*T` in type position (e.g. a type-switch case)
			return g.cppType(t.X) + "*"
		}
		return "auto"
	case *ast.SelectorExpr:
		if id, ok := t.X.(*ast.Ident); ok && g.pkgNames[id.Name] {
			base := id.Name + "::" + t.Sel
			if g.isIfaceName(t.Sel) {
				return base + "*"
			}
			return base
		}
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
		case "error":
			return "nullptr"
		case "int", "int8", "int16", "int32", "int64",
			"uint", "uint8", "uint16", "uint32", "uint64",
			"byte", "float32", "float64", "rune":
			return "0"
		}
		if g.structNames[t.Name] {
			return "{}"
		}
		if _, ok := g.ifaces[t.Name]; ok {
			if len(g.ifaceOwn[t.Name]) > 0 || len(g.ifaceEmbeds[t.Name]) > 0 {
				return "nullptr"
			}
		}
		if target, ok := g.aliases[t.Name]; ok {
			return g.zeroValue(target)
		}
		return "0"
	case *ast.SliceType:
		return "std::vector<" + g.cppType(t.Elem) + ">{}"
	case *ast.ArrayType:
		return "std::vector<" + g.cppType(t.Elem) + ">{}"
	case *ast.MapType:
		return "std::unordered_map<" + g.cppType(t.Key) + ", " + g.cppType(t.Value) + ">{}"
	case *ast.PtrType, *ast.StarExpr:
		return "nullptr"
	case *ast.SelectorExpr:
		if g.isIfaceName(t.Sel) {
			return "nullptr"
		}
		return "{}"
	case *ast.InterfaceType:
		return "{}"
	}
	return "{}"
}

// ------------------------------------------------------------------ decls

func (g *Generator) emitDecls(decls []ast.Decl) {
	// Forward-declare named types so interfaces can name types declared later.
	for _, d := range decls {
		if gd, ok := d.(*ast.GenDecl); ok && gd.Tok == "type" {
			for _, s := range gd.Specs {
				ts, ok := s.(*ast.TypeSpec)
				if !ok {
					continue
				}
				switch ts.Type.(type) {
				case *ast.StructType:
					g.writeln("class %s;", ts.Name)
				case *ast.InterfaceType:
					// only when it becomes a class (empty interfaces alias `any`)
					if len(g.ifaceOwn[ts.Name]) > 0 || len(g.ifaceEmbeds[ts.Name]) > 0 {
						g.writeln("class %s;", ts.Name)
					}
				}
			}
		}
	}
	g.writeln("")
	// 0a. Type aliases (`using X = Y;`) before constants and variables.
	for _, d := range decls {
		if gd, ok := d.(*ast.GenDecl); ok && gd.Tok == "type" {
			for _, s := range gd.Specs {
				ts, ok := s.(*ast.TypeSpec)
				if !ok {
					continue
				}
				switch ts.Type.(type) {
				case *ast.StructType, *ast.InterfaceType:
				default:
					g.emitTypeSpec(ts)
				}
			}
		}
	}
	// 0. Free-function prototypes, so class methods can call them.
	for _, d := range decls {
		if fd, ok := d.(*ast.FuncDecl); ok && fd.Recv == nil && fd.Name != "main" {
			g.emitFuncProto(fd)
		}
	}
	// 1. Package-level constants and variables.
	for _, d := range decls {
		if gd, ok := d.(*ast.GenDecl); ok && gd.Tok != "type" {
			g.emitGenDecl(gd, true)
		}
	}
	// 2. Interfaces: abstract classes that structs later derive from.
	for _, d := range decls {
		if gd, ok := d.(*ast.GenDecl); ok && gd.Tok == "type" {
			for _, s := range gd.Specs {
				if ts, ok := s.(*ast.TypeSpec); ok {
					if _, isIface := ts.Type.(*ast.InterfaceType); isIface {
						g.emitTypeSpec(ts)
					}
				}
			}
		}
	}
	// 3. The remaining type declarations.
	for _, d := range decls {
		if gd, ok := d.(*ast.GenDecl); ok && gd.Tok == "type" {
			for _, s := range gd.Specs {
				ts, ok := s.(*ast.TypeSpec)
				if !ok {
					continue
				}
				if _, isStruct := ts.Type.(*ast.StructType); isStruct {
					g.emitTypeSpec(ts)
				}
			}
		}
	}
	// 4. Function bodies (types, consts, vars already emitted).
	for _, d := range decls {
		if fd, ok := d.(*ast.FuncDecl); ok {
			if fd.Recv == nil {
				g.emitFunc(fd)
			}
		}
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
	sig := fmt.Sprintf("auto %s(%s) -> %s", name, params, ret)
	if hasVariadic(fd.Type.Params) {
		sig = "template <typename... _A>\n" + sig
	}
	return sig
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
		if _, ok := fld.Type.(*ast.Ellipsis); ok {
			// Go variadic -> C++ parameter pack
			if len(fld.Names) == 0 {
				parts = append(parts, "_A...")
				continue
			}
			for _, n := range fld.Names {
				parts = append(parts, "_A... "+n)
			}
			continue
		}
		typ := g.cppType(fld.Type)
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

// hasVariadic reports whether a parameter list ends in a variadic parameter.
func hasVariadic(fields []*ast.Field) bool {
	if len(fields) == 0 {
		return false
	}
	_, ok := fields[len(fields)-1].Type.(*ast.Ellipsis)
	return ok
}

// emitCallArgs renders call arguments, expanding a trailing `x...` spread.
func (g *Generator) emitCallArgs(c *ast.CallExpr) string {
	var parts []string
	for _, a := range c.Args {
		parts = append(parts, g.emitExpr(a))
	}
	if c.Ellipsis && len(parts) > 0 {
		parts[len(parts)-1] += "..."
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
	saved := g.ptrVars
	g.ptrVars = map[string]bool{}
	defer func() { g.ptrVars = saved }()
	savedTypes := g.types
	g.types = map[string]ast.Expr{}
	defer func() { g.types = savedTypes }()
	g.recordParams(fd.Type.Params)
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
	sig := fmt.Sprintf("auto %s(%s) -> %s", fd.Name, params, ret)
	if hasVariadic(fd.Type.Params) {
		sig = "template <typename... _A>\n" + sig
	}
	return sig
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
		g.types[g.recvName] = rf.Type
	}
	saved := g.ptrVars
	g.ptrVars = map[string]bool{}
	defer func() { g.ptrVars = saved }()
	savedTypes := g.types
	g.types = map[string]ast.Expr{}
	defer func() { g.types = savedTypes }()
	g.recordParams(fd.Type.Params)
	g.emitStmts(fd.Body.List)
	g.recvName, g.recvPtr = savedRecv, savedPtr
	g.indent--
	g.writeln("}")
}

// emitPromoted forwards a method of an embedded type.
func (g *Generator) emitPromoted(fd *ast.FuncDecl, base string) {
	params := g.emitParams(fd.Type.Params)
	var names []string
	for _, fld := range fd.Type.Params {
		names = append(names, fld.Names...)
	}
	// Return type is deduced, so the embedded member (named after its type)
	// cannot shadow it.
	g.writeln("auto %s(%s) -> %s", fd.Name, params, g.resultType(fd.Type.Results))
	g.writeln("{")
	g.indent++
	g.writeln("return %s_.%s(%s);", base, fd.Name, strings.Join(names, ", "))
	g.indent--
	g.writeln("}")
}

// memberName maps a composite-literal key to its C++ member name.
func (g *Generator) memberName(structName, key string) string {
	for _, b := range g.embedOf[structName] {
		if b == key {
			return key + "_"
		}
	}
	return key
}

// emitFieldsCtor emits a constructor taking every field (all defaulted), for
// types that are polymorphic and therefore not aggregates.
func (g *Generator) emitFieldsCtor(typeName string) {
	names := g.structFields[typeName]
	ftypes := g.structFieldTypes[typeName]
	var params, body []string
	for i, n := range names {
		pn := fmt.Sprintf("_f%d", i)
		params = append(params, fmt.Sprintf("%s %s = %s", g.cppType(ftypes[i]), pn, g.zeroValue(ftypes[i])))
		body = append(body, fmt.Sprintf("%s = %s;", n, pn))
	}
	g.writeln("%s(%s) { %s }", typeName, strings.Join(params, ", "), strings.Join(body, " "))
}

// calleeNameOf returns the bare name of a called function or method.
func calleeNameOf(e ast.Expr) string {
	switch t := e.(type) {
	case *ast.Ident:
		return t.Name
	case *ast.SelectorExpr:
		return t.Sel
	}
	return ""
}

// callsPtrFunc reports whether an expression calls a pointer-returning func.
func (g *Generator) callsPtrFunc(e ast.Expr) bool {
	c, ok := e.(*ast.CallExpr)
	if !ok {
		return false
	}
	return g.ptrFuncs[calleeNameOf(c.Fun)]
}

// typeNameOf returns the bare name of a named type expression.
func typeNameOf(t ast.Expr) string {
	switch tt := t.(type) {
	case *ast.Ident:
		return tt.Name
	case *ast.SelectorExpr:
		return tt.Sel
	case *ast.StarExpr:
		return typeNameOf(tt.X)
	case *ast.PtrType:
		return typeNameOf(tt.Elem)
	}
	return ""
}

// emitTypedComposite emits an untyped composite literal with a known element
// type (designated init for a struct, `new` for a pointer element).
func (g *Generator) emitTypedComposite(t ast.Expr, elts []ast.Expr) string {
	ptr := false
	base := t
	if st, ok := t.(*ast.StarExpr); ok {
		ptr = true
		base = st.X
	} else if pt, ok := t.(*ast.PtrType); ok {
		ptr = true
		base = pt.Elem
	}
	baseName := typeNameOf(base)
	if baseName == "" || !g.structNames[baseName] {
		lit := &ast.CompositeLit{Type: t, Elts: elts}
		return g.emitExpr(lit)
	}
	if len(g.implementsOf(baseName)) > 0 {
		// polymorphic: use the generated constructor
		call := g.emitCtorCall(g.cppType(base), elts)
		if ptr {
			return "new " + call
		}
		return call
	}
	var parts []string
	for _, e := range elts {
		if kv, ok := e.(*ast.KeyValueExpr); ok {
			if id, ok := kv.Key.(*ast.Ident); ok {
				parts = append(parts, "."+g.memberName(baseName, id.Name)+" = "+g.emitExpr(kv.Value))
				continue
			}
		}
		parts = append(parts, g.emitExpr(e))
	}
	expr := fmt.Sprintf("%s{ %s }", g.cppType(base), strings.Join(parts, ", "))
	if ptr {
		return "new " + expr
	}
	return expr
}

// emitCtorCall builds a constructor call for a composite literal of a
// non-aggregate struct type, in field-declaration order.
func (g *Generator) emitCtorCall(typeName string, elts []ast.Expr) string {
	bare := typeName
	if i := strings.LastIndex(bare, "::"); i >= 0 {
		bare = bare[i+2:]
	}
	names := g.structFields[bare]
	ftypes := g.structFieldTypes[bare]
	provided := map[string]string{}
	var positional []string
	for _, e := range elts {
		if kv, ok := e.(*ast.KeyValueExpr); ok {
			if id, ok := kv.Key.(*ast.Ident); ok {
				provided[id.Name] = g.emitExpr(kv.Value)
				continue
			}
		}
		positional = append(positional, g.emitExpr(e))
	}
	var args []string
	last := -1
	for i, n := range names {
		if _, ok := provided[n]; ok {
			last = i
		} else if i < len(positional) {
			last = i
		}
	}
	for i := 0; i <= last; i++ {
		n := names[i]
		if v, ok := provided[n]; ok {
			args = append(args, v)
			continue
		}
		if i < len(positional) {
			args = append(args, positional[i])
			continue
		}
		args = append(args, g.zeroValue(ftypes[i]))
	}
	return fmt.Sprintf("%s(%s)", typeName, strings.Join(args, ", "))
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
	if _, ok := ts.Type.(*ast.InterfaceType); ok {
		bases := g.ifaceEmbeds[ts.Name]
		methods := g.ifaceOwn[ts.Name]
		if len(bases) == 0 && len(methods) == 0 {
			g.writeln("using %s = go2cpp::any;", ts.Name)
			return
		}
		base := ""
		if len(bases) > 0 {
			var parts []string
			for _, b := range bases {
				parts = append(parts, "public "+b)
			}
			base = " : " + strings.Join(parts, ", ")
		}
		g.writeln("class %s%s { public:", ts.Name, base)
		g.indent++
		g.writeln("virtual ~%s() {}", ts.Name)
		for _, m := range methods {
			params := g.emitParams(m.ft.Params)
			ret := g.resultType(m.ft.Results)
			g.writeln("virtual %s %s(%s) = 0;", ret, m.name, params)
		}
		g.indent--
		g.writeln("};")
		g.writeln("")
		return
	}
	if st, ok := ts.Type.(*ast.StructType); ok {
		base := ""
		bases := g.implementsOf(ts.Name)
		if len(bases) > 0 {
			var parts []string
			for _, b := range bases {
				parts = append(parts, "public "+b)
			}
			base = " : " + strings.Join(parts, ", ")
		}
		g.writeln("class %s%s { public:", ts.Name, base)
		g.indent++
		if len(bases) > 0 {
			// A polymorphic type is not an aggregate, so provide a constructor.
			g.emitFieldsCtor(ts.Name)
		}
		for _, fld := range st.Fields {
			if len(fld.Names) == 0 {
				// embedded field: an object member named after its type, with a
				// trailing underscore so it does not shadow the type name.
				base, _ := baseTypeName(fld.Type)
				g.writeln("%s %s_;", base, base)
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
			if g.isPtrType(vs.Type) {
				g.ptrVars[n] = true
			}
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
			if g.isPtrType(vs.Type) {
				g.ptrVars[n] = true
			}
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
		if len(a.Lhs) == 1 && len(a.Rhs) == 1 {
			if id, ok := a.Rhs[0].(*ast.Ident); ok && id.Name == "nil" {
				if lt := g.exprType(a.Lhs[0]); lt != nil {
					switch lt.(type) {
					case *ast.SliceType, *ast.MapType:
						g.writeln("%s = %s;", g.emitExpr(a.Lhs[0]), g.zeroValue(lt))
						return
					}
				}
			}
		}
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
		if g.emitCommaOK(a.Lhs[0], a.Lhs[1], a.Rhs[0]) {
			return
		}
		if g.emitMapCommaOK(a.Lhs[0], a.Lhs[1], a.Rhs[0]) {
			return
		}
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
		if isPointerExpr(a.Rhs[i]) || g.callsPtrFunc(a.Rhs[i]) {
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
	flat := g.flattenResults(g.curResults)
	if len(r.Results) == 1 {
		g.writeln("return %s;", g.returnValue(r.Results[0], flat, 0))
		return
	}
	var vals []string
	for i, e := range r.Results {
		vals = append(vals, g.returnValue(e, flat, i))
	}
	types := g.resultTypes(g.curResults)
	g.writeln("return std::tuple<%s>(%s);", strings.Join(types, ", "), strings.Join(vals, ", "))
}

// returnValue renders a returned expression, mapping a bare `nil` to the zero
// value of the corresponding result type (a slice is `{}`, not `nullptr`).
func (g *Generator) returnValue(e ast.Expr, flat []ast.Expr, i int) string {
	if id, ok := e.(*ast.Ident); ok && id.Name == "nil" && i < len(flat) {
		return g.zeroValue(flat[i])
	}
	return g.emitExpr(e)
}

// flattenResults expands grouped named results into one type per value.
func (g *Generator) flattenResults(results []*ast.Field) []ast.Expr {
	var out []ast.Expr
	for _, fld := range results {
		if len(fld.Names) == 0 {
			out = append(out, fld.Type)
			continue
		}
		for range fld.Names {
			out = append(out, fld.Type)
		}
	}
	return out
}

// identName returns an identifier's name, or "".
func identName(e ast.Expr) string {
	if id, ok := e.(*ast.Ident); ok {
		return id.Name
	}
	return ""
}

// emitInitStmt emits an if/for initialiser as ordinary statements. This
// handles the comma-ok form `v, ok := x.(T)`.
func (g *Generator) emitInitStmt(s ast.Stmt) {
	as, ok := s.(*ast.AssignStmt)
	if !ok {
		g.emitStmt(s)
		return
	}
	if as.Op == ":=" && len(as.Lhs) == 2 && len(as.Rhs) == 1 {
		if g.emitCommaOK(as.Lhs[0], as.Lhs[1], as.Rhs[0]) {
			return
		}
	}
	g.emitAssign(as)
}

func (g *Generator) emitIf(st *ast.IfStmt) {
	wrap := st.Init != nil
	if wrap {
		g.writeln("{")
		g.indent++
		g.emitInitStmt(st.Init)
	}
	g.writeln("if (%s)", g.emitExpr(st.Cond))
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
	if wrap {
		g.indent--
		g.writeln("}")
	}
}

func (g *Generator) emitIfInline(st *ast.IfStmt) {
	if st.Init != nil {
		g.emitInitStmt(st.Init)
	}
	g.writeln("if (%s)", g.emitExpr(st.Cond))
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
		if init == "" {
			g.emitInitStmt(st.Init)
		}
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
		g.recordElem(st.Value.(*ast.Ident).Name, st.X)
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
	} else if es, ok := st.Assign.(*ast.ExprStmt); ok {
		if ta, ok := es.X.(*ast.TypeAssertExpr); ok {
			X = ta.X
		}
	}
	if X == nil {
		g.fail("unsupported type switch")
		return
	}
	if g.isIfaceExpr(X) {
		g.emitIfaceTypeSwitch(st, varName, X)
		return
	}
	g.needAny = true
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

// emitIfaceTypeSwitch lowers a type switch on a named interface value to a
// chain of dynamic_cast tests.
func (g *Generator) emitIfaceTypeSwitch(st *ast.TypeSwitchStmt, varName string, X ast.Expr) {
	g.writeln("{")
	g.indent++
	g.writeln("auto _tsw = %s;", g.emitExpr(X))
	for i, c := range st.Body {
		cond := ""
		if len(c.List) > 0 {
			var conds []string
			for _, te := range c.List {
				if id, ok := te.(*ast.Ident); ok && id.Name == "nil" {
					conds = append(conds, "_tsw == nullptr")
					continue
				}
				conds = append(conds, fmt.Sprintf("dynamic_cast<%s>(_tsw) != nullptr", g.cppType(te)))
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
		if varName != "" && varName != "_" && len(c.List) == 1 {
			if id, ok := c.List[0].(*ast.Ident); !ok || id.Name != "nil" {
				g.writeln("auto %s = dynamic_cast<%s>(_tsw);", varName, g.cppType(c.List[0]))
				g.ptrVars[varName] = true
			}
		}
		g.emitStmts(c.Body)
		g.indent--
		g.writeln("}")
	}
	g.indent--
	g.writeln("}")
}

// emitDefer emits a deferred call as a scope guard.
func (g *Generator) emitDefer(d *ast.DeferStmt) {
	g.deferCounter++
	body := g.deferBody(d.Call)
	g.writeln("auto _defer%d = std::shared_ptr<void>(nullptr, [&](...) { %s });", g.deferCounter, body)
}

func (g *Generator) deferBody(call ast.Expr) string {
	if fl, ok := call.(*ast.FuncLit); ok {
		sub := g.subGenerator()
		sub.emitStmts(fl.Body.List)
		return strings.TrimSpace(sub.sb.String())
	}
	return g.emitExpr(call) + ";"
}

// ------------------------------------------------------------ expressions

func (g *Generator) emitExpr(e ast.Expr) string {
	switch t := e.(type) {
	case *ast.Ident:
		if t.Name == "nil" {
			return "nullptr"
		}
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
			if g.pkgNames[id.Name] {
				return id.Name + "::" + t.Sel
			}
			base, sep := g.selBase(id)
			return base + sep + t.Sel
		}
		if c, ok := t.X.(*ast.CallExpr); ok {
			sep := "."
			if g.ptrFuncs[calleeNameOf(c.Fun)] {
				sep = "->"
			}
			return g.emitExpr(c) + sep + t.Sel
		}
		sep := "."
		if g.isPtrType(g.exprType(t.X)) {
			sep = "->"
		}
		return g.emitExpr(t.X) + sep + t.Sel
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
		ct := g.cppType(t.Type)
		if strings.HasSuffix(ct, "*") {
			return fmt.Sprintf("dynamic_cast<%s>(%s)", ct, g.emitExpr(t.X))
		}
		if g.isIfaceExpr(t.X) {
			return fmt.Sprintf("(*dynamic_cast<%s*>(%s))", ct, g.emitExpr(t.X))
		}
		g.needAny = true
		return fmt.Sprintf("(%s).as<%s>()", g.emitExpr(t.X), ct)
	}
	g.fail("unsupported expression %T", e)
	return "/*?*/"
}

func (g *Generator) emitLit(l *ast.BasicLit) string {
	if strings.HasPrefix(l.Value, "`") {
		inner := strings.TrimSuffix(strings.TrimPrefix(l.Value, "`"), "`")
		return cppQuote(inner)
	}
	return l.Value
}

// cppQuote renders a string as a C++ string literal with escapes, so that any
// content (including `R"(`) is safe.
func cppQuote(s string) string {
	var b strings.Builder
	b.WriteByte('"')
	for i := 0; i < len(s); i++ {
		switch s[i] {
		case '\\':
			b.WriteString("\\\\")
		case '"':
			b.WriteString("\\\"")
		case '\n':
			b.WriteString("\\n")
		case '\t':
			b.WriteString("\\t")
		case '\r':
			b.WriteString("\\r")
		default:
			b.WriteByte(s[i])
		}
	}
	b.WriteByte('"')
	return b.String()
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
			g.needGoString = true
			return fmt.Sprintf("go2cpp::_goString(%s)", g.emitExpr(c.Args[0]))
		case "int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32", "uint64", "byte", "rune", "float32", "float64", "bool",
			"make", "append", "new", "cap", "copy":
			return g.emitBuiltinConv(id.Name, c)
		}
		if g.isTypeName(id.Name) {
			return fmt.Sprintf("%s(%s)", g.cppType(id), g.emitCallArgs(c))
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
			case "unicode":
				return g.emitUnicode(sel.Sel, c)
			case "utf8":
				return g.emitUtf8(sel.Sel, c)
			}
		}
	}
	// selector calls: methods and function values
	if sel, ok := c.Fun.(*ast.SelectorExpr); ok {
		if id, ok := sel.X.(*ast.Ident); ok {
			if g.pkgNames[id.Name] {
				return id.Name + "::" + sel.Sel + "(" + g.emitCallArgs(c) + ")"
			}
			base, sep := g.selBase(id)
			return base + sep + sel.Sel + "(" + g.emitCallArgs(c) + ")"
		}
		return g.emitExpr(sel.X) + "." + sel.Sel + "(" + g.emitCallArgs(c) + ")"
	}
	return fmt.Sprintf("%s(%s)", g.emitExpr(c.Fun), g.emitCallArgs(c))
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
		g.needLen()
		if c.Ellipsis && len(c.Args) == 2 {
			return fmt.Sprintf("_appendAll(%s, %s)", g.emitExpr(c.Args[0]), g.emitExpr(c.Args[1]))
		}
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
		return fmt.Sprintf("fmtSprintf(%s)", g.emitCallArgs(c))
	case "Errorf":
		g.needFmtSprintf = true
		g.needError = true
		return fmt.Sprintf("go2cpp::newError(fmtSprintf(%s))", g.emitCallArgs(c))
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
	args := g.emitCallArgs(c)
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
	case "TrimSuffix":
		g.needTrimSpace = true
		return "stringsTrimSuffix(" + args + ")"
	case "TrimPrefix":
		g.needTrimSpace = true
		return "stringsTrimPrefix(" + args + ")"
	case "Trim":
		g.needTrimSpace = true
		return "stringsTrim(" + args + ")"
	case "TrimLeft":
		g.needTrimSpace = true
		return "stringsTrimLeft(" + args + ")"
	case "TrimRight":
		g.needTrimSpace = true
		return "stringsTrimRight(" + args + ")"
	case "Fields":
		g.needSplit = true
		return "stringsFields(" + args + ")"
	case "Repeat":
		g.needSplit = true
		return "stringsRepeat(" + args + ")"
	case "ToLower":
		g.needSplit = true
		return "stringsToLower(" + args + ")"
	case "ToUpper":
		g.needSplit = true
		return "stringsToUpper(" + args + ")"
	case "Count":
		g.needIndex = true
		return "stringsCount(" + args + ")"
	case "ReplaceAll":
		g.needReplace = true
		return "stringsReplace(" + args + ", -1)"
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
	case "LastIndex":
		g.needIndex = true
		return "stringsLastIndex(" + args + ")"
	case "Replace":
		g.needReplace = true
		return "stringsReplace(" + args + ")"
	}
	g.fail("unsupported strings.%s", sel)
	return "/*?*/"
}

func (g *Generator) emitUnicode(sel string, c *ast.CallExpr) string {
	g.needUnicode = true
	switch sel {
	case "IsLetter":
		return "go2cpp::unicodeIsLetter(" + g.emitCallArgs(c) + ")"
	case "IsDigit":
		return "go2cpp::unicodeIsDigit(" + g.emitCallArgs(c) + ")"
	case "IsSpace":
		return "go2cpp::unicodeIsSpace(" + g.emitCallArgs(c) + ")"
	}
	g.fail("unsupported unicode.%s", sel)
	return "/*?*/"
}

func (g *Generator) emitUtf8(sel string, c *ast.CallExpr) string {
	g.needUtf8 = true
	switch sel {
	case "DecodeRuneInString":
		return "go2cpp::utf8DecodeRuneInString(" + g.emitCallArgs(c) + ")"
	}
	g.fail("unsupported utf8.%s", sel)
	return "/*?*/"
}

func (g *Generator) emitStrconv(sel string, c *ast.CallExpr) string {
	g.needStrconv = true
	switch sel {
	case "ParseFloat":
		return "strconvParseFloat(" + g.emitCallArgs(c) + ")"
	case "ParseInt":
		return "strconvParseInt(" + g.emitCallArgs(c) + ")"
	case "Itoa":
		return "std::to_string(" + g.emitCallArgs(c) + ")"
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
	sub := g.subGenerator()
	sub.indent = 1
	sub.curResults = fl.Type.Results
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
			if inner, ok := e.(*ast.CompositeLit); ok && inner.Type == nil {
				elts = append(elts, g.emitTypedComposite(t.Elem, inner.Elts))
				continue
			}
			elts = append(elts, g.emitExpr(e))
		}
		return fmt.Sprintf("std::vector<%s>{ %s }", g.cppType(t.Elem), strings.Join(elts, ", "))
	case *ast.ArrayType:
		var elts []string
		for _, e := range c.Elts {
			if inner, ok := e.(*ast.CompositeLit); ok && inner.Type == nil {
				elts = append(elts, g.emitTypedComposite(t.Elem, inner.Elts))
				continue
			}
			elts = append(elts, g.emitExpr(e))
		}
		return fmt.Sprintf("std::vector<%s>{ %s }", g.cppType(t.Elem), strings.Join(elts, ", "))
	case *ast.Ident:
		if len(g.implementsOf(t.Name)) > 0 {
			return g.emitCtorCall(t.Name, c.Elts)
		}
		var elts []string
		for _, e := range c.Elts {
			if kv, ok := e.(*ast.KeyValueExpr); ok {
				if name, ok := kv.Key.(*ast.Ident); ok && !g.structNames[name.Name] {
					elts = append(elts, "."+g.memberName(t.Name, name.Name)+" = "+g.emitExpr(kv.Value))
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
	// pkg.Type{...} and other qualified types: designated init.
	if sel, ok := c.Type.(*ast.SelectorExpr); ok {
		var elts []string
		if len(g.implementsOf(sel.Sel)) > 0 {
			return g.emitCtorCall(g.cppType(sel), c.Elts)
		}
		for _, e := range c.Elts {
			if kv, ok := e.(*ast.KeyValueExpr); ok {
				if name, ok := kv.Key.(*ast.Ident); ok && !g.structNames[name.Name] {
					elts = append(elts, "."+g.memberName(sel.Sel, name.Name)+" = "+g.emitExpr(kv.Value))
					continue
				}
			}
			elts = append(elts, g.emitExpr(e))
		}
		return fmt.Sprintf("%s{ %s }", g.cppType(sel), strings.Join(elts, ", "))
	}
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
	if g.needPrintf {
		b.WriteString("#include <cstdio>\n")
	}
	b.WriteString("#include <functional>\n")
	b.WriteString("#include <algorithm>\n")
	// Disable C macros that clash with Go identifiers.
	b.WriteString("#ifdef EOF\n#undef EOF\n#endif\n")
	b.WriteString("#ifdef NULL\n#undef NULL\n#endif\n")
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
	if g.needError {
		b.WriteString(helperError)
	}
	if g.needUnicode {
		b.WriteString(helperUnicode)
	}
	if g.needUtf8 {
		b.WriteString(helperUtf8)
	}
	if g.needGoString {
		b.WriteString(helperGoString)
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

template <typename T>
void _format_output(std::ostream& out, const std::vector<T>& v)
{
    out << "[";
    for (std::size_t i = 0; i < v.size(); i++) {
        if (i) out << " ";
        _format_output(out, v[i]);
    }
    out << "]";
}

template <typename K, typename V>
void _format_output(std::ostream& out, const std::unordered_map<K, V>& m)
{
    out << "map[";
    bool _first = true;
    for (const auto& _kv : m) {
        if (!_first) out << " ";
        _first = false;
        _format_output(out, _kv.first);
        out << ":";
        _format_output(out, _kv.second);
    }
    out << "]";
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
inline std::string stringsTrimSuffix(const std::string& s, const std::string& suf) {
    if (s.size() >= suf.size() && s.compare(s.size() - suf.size(), suf.size(), suf) == 0)
        return s.substr(0, s.size() - suf.size());
    return s;
}
inline std::string stringsTrimPrefix(const std::string& s, const std::string& pre) {
    if (s.rfind(pre, 0) == 0) return s.substr(pre.size());
    return s;
}
inline std::string stringsTrim(const std::string& s, const std::string& cut) {
    size_t a = s.find_first_not_of(cut);
    if (a == std::string::npos) return "";
    size_t b = s.find_last_not_of(cut);
    return s.substr(a, b - a + 1);
}
inline std::string stringsTrimLeft(const std::string& s, const std::string& cut) {
    size_t a = s.find_first_not_of(cut);
    if (a == std::string::npos) return "";
    return s.substr(a);
}
inline std::string stringsTrimRight(const std::string& s, const std::string& cut) {
    size_t b = s.find_last_not_of(cut);
    if (b == std::string::npos) return "";
    return s.substr(0, b + 1);
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
inline int stringsLastIndex(const std::string& h, const std::string& n) {
    auto p = h.rfind(n);
    return p == std::string::npos ? -1 : static_cast<int>(p);
}
inline int stringsCount(const std::string& s, const std::string& sub) {
    if (sub.empty()) return static_cast<int>(s.size()) + 1;
    int c = 0; size_t p = 0;
    while ((p = s.find(sub, p)) != std::string::npos) { c++; p += sub.size(); }
    return c;
}
inline std::vector<std::string> stringsFields(const std::string& s) {
    std::vector<std::string> out; std::string cur;
    for (char ch : s) {
        if (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\v' || ch == '\f' || ch == '\r') {
            if (!cur.empty()) { out.push_back(cur); cur.clear(); }
        } else { cur += ch; }
    }
    if (!cur.empty()) out.push_back(cur);
    return out;
}
inline std::string stringsRepeat(const std::string& s, int n) {
    std::string out;
    for (int i = 0; i < n; i++) out += s;
    return out;
}
inline std::string stringsToLower(const std::string& s) {
    std::string out = s;
    for (auto& c : out) if (c >= 'A' && c <= 'Z') c = static_cast<char>(c - 'A' + 'a');
    return out;
}
inline std::string stringsToUpper(const std::string& s) {
    std::string out = s;
    for (auto& c : out) if (c >= 'a' && c <= 'z') c = static_cast<char>(c - 'a' + 'A');
    return out;
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
template <typename T> std::string _to_str(T v) {
    std::ostringstream _o;
    _format_output(_o, v);
    return _o.str();
}

inline void _replace_first_spec(std::string& s, const std::string& val) {
    size_t best = std::string::npos;
    std::string which;
    for (const char* spec : { "%v", "%d", "%s", "%f", "%q" }) {
        size_t q = s.find(spec);
        if (q != std::string::npos && (best == std::string::npos || q < best)) { best = q; which = spec; }
    }
    if (best != std::string::npos) {
        std::string rep = val;
        if (which == "%q") rep = "\"" + val + "\"";
        s.replace(best, 2, rep);
    }
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

const helperError = `
namespace go2cpp {
struct error {
    virtual ~error() {}
    virtual std::string Error() const = 0;
};
struct _errString : error {
    std::string msg;
    _errString(const std::string& m) : msg(m) {}
    std::string Error() const override { return msg; }
};
inline error* newError(const std::string& m) { return new _errString(m); }
}
`

const helperGoString = `
namespace go2cpp {
inline std::string _goString(const std::string& s) { return s; }
inline std::string _goString(const char* s) { return std::string(s); }
inline std::string _goString(std::int32_t r) {
    std::string out;
    unsigned int c = static_cast<unsigned int>(r);
    if (c < 0x80) {
        out += static_cast<char>(c);
    } else if (c < 0x800) {
        out += static_cast<char>(0xC0 | (c >> 6));
        out += static_cast<char>(0x80 | (c & 0x3F));
    } else if (c < 0x10000) {
        out += static_cast<char>(0xE0 | (c >> 12));
        out += static_cast<char>(0x80 | ((c >> 6) & 0x3F));
        out += static_cast<char>(0x80 | (c & 0x3F));
    } else {
        out += static_cast<char>(0xF0 | (c >> 18));
        out += static_cast<char>(0x80 | ((c >> 12) & 0x3F));
        out += static_cast<char>(0x80 | ((c >> 6) & 0x3F));
        out += static_cast<char>(0x80 | (c & 0x3F));
    }
    return out;
}
}
`

const helperUnicode = `
namespace go2cpp {
inline bool unicodeIsLetter(std::int32_t r) {
    return (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || r == '_' || r > 127;
}
inline bool unicodeIsDigit(std::int32_t r) { return r >= '0' && r <= '9'; }
inline bool unicodeIsSpace(std::int32_t r) {
    return r == ' ' || r == '\t' || r == '\n' || r == '\v' || r == '\f' || r == '\r';
}
}
`

const helperUtf8 = `
namespace go2cpp {
inline std::tuple<std::int32_t, int> utf8DecodeRuneInString(const std::string& s) {
    if (s.empty()) return std::tuple<std::int32_t, int>{ 0, 0 };
    unsigned char c = static_cast<unsigned char>(s[0]);
    int n = 1;
    std::int32_t r = c;
    if (c >= 0xF0) { n = 4; r = c & 0x07; }
    else if (c >= 0xE0) { n = 3; r = c & 0x0F; }
    else if (c >= 0xC0) { n = 2; r = c & 0x1F; }
    if (n > static_cast<int>(s.size())) return std::tuple<std::int32_t, int>{ 0xFFFD, 1 };
    for (int i = 1; i < n; i++) {
        r = static_cast<std::int32_t>((r << 6) | (static_cast<unsigned char>(s[i]) & 0x3F));
    }
    return std::tuple<std::int32_t, int>{ r, n };
}
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
    bool operator==(std::nullptr_t) const { return !has_value(); }
    bool operator!=(std::nullptr_t) const { return has_value(); }
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
    if (lo > v.size()) lo = v.size();
    if (hi > v.size()) hi = v.size();
    return std::vector<T>(v.begin() + lo, v.begin() + hi);
}

inline std::string _slice(const std::string& s, std::size_t lo, std::size_t hi) {
    if (hi < lo) hi = lo;
    if (lo > s.size()) lo = s.size();
    if (hi > s.size()) hi = s.size();
    return s.substr(lo, hi - lo);
}

template <typename T, typename... Rest>
std::vector<T> _append(std::vector<T> v, Rest... rest) {
    (v.push_back(rest), ...);
    return v;
}

template <typename T>
std::vector<T> _appendAll(std::vector<T> v, const std::vector<T>& rest) {
    v.insert(v.end(), rest.begin(), rest.end());
    return v;
}
`
