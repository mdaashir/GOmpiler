// Package ast defines the syntax tree produced by the parser.
//
// Every node embeds Pos, which satisfies the Node interface. Concrete node
// kinds are separated into expressions, statements, declarations and types so
// that later stages can reject malformed trees at compile time where practical.
package ast

// Pos is a source position.
type Pos struct {
	Line int
	Col  int
}

// Pos returns the node position.
func (p Pos) Pos() Pos { return p }

// Node is anything in the tree.
type Node interface {
	Pos() Pos
}

// Expr, Stmt, Decl and Type tag the categories of node.
type (
	Expr interface {
		Node
		exprNode()
	}
	Stmt interface {
		Node
		stmtNode()
	}
	Decl interface {
		Node
		declNode()
	}
	Spec interface {
		Node
		specNode()
	}
	// TypeNode marks a node that denotes a type.
	TypeNode interface {
		Node
		typeNode()
	}
)

// File is a parsed source file.
type File struct {
	Pos
	Package string
	Imports []*ImportSpec
	Decls   []Decl
}

// ---------------------------------------------------------------- declarations

// ImportSpec is one `import` entry.
type ImportSpec struct {
	Pos
	Name string // optional alias, may be "_" or "."
	Path string
}

func (*ImportSpec) specNode() {}

// GenDecl is a var/const/type block.
type GenDecl struct {
	Pos
	Tok   string // "var" | "const" | "type"
	Specs []Spec
}

func (*GenDecl) declNode() {}

// ValueSpec is `var a, b T = x, y` or `const c = 1`.
type ValueSpec struct {
	Pos
	Names  []string
	Type   Expr // may be nil
	Values []Expr
}

func (*ValueSpec) specNode() {}

// TypeSpec is `type Name T` or `type Name = T` or `type Name struct {...}`.
type TypeSpec struct {
	Pos
	Name   string
	Assign bool
	Type   Expr
}

func (*TypeSpec) specNode() {}

// FuncDecl is a function or method declaration.
type FuncDecl struct {
	Pos
	Recv   *FieldList // non-nil for methods
	Name   string
	Type   *FuncType
	Body   *BlockStmt
}

func (*FuncDecl) declNode() {}

// Field is a name/type pair in a parameter list, struct or interface.
type Field struct {
	Pos
	Names []string
	Type  Expr
}

// FieldList groups fields (parameters, results, struct/interface members).
type FieldList struct {
	Pos
	List []*Field
}

// ------------------------------------------------------------------- statements

// BlockStmt is `{ ... }`.
type BlockStmt struct {
	Pos
	List []Stmt
}

func (*BlockStmt) stmtNode() {}

// ExprStmt is an expression used as a statement.
type ExprStmt struct {
	Pos
	X Expr
}

func (*ExprStmt) stmtNode() {}

// AssignStmt covers `=`, `:=` and the compound assignments.
type AssignStmt struct {
	Pos
	Lhs []Expr
	Op  string
	Rhs []Expr
}

func (*AssignStmt) stmtNode() {}

// IncDecStmt is `x++` or `x--`.
type IncDecStmt struct {
	Pos
	X  Expr
	Op string
}

func (*IncDecStmt) stmtNode() {}

// IfStmt is `if [init;] cond { ... } [else ...]`.
type IfStmt struct {
	Pos
	Init Stmt
	Cond Expr
	Body *BlockStmt
	Else Stmt
}

func (*IfStmt) stmtNode() {}

// ForStmt is a three-clause or bare `for` loop.
type ForStmt struct {
	Pos
	Init Stmt
	Cond Expr
	Post Stmt
	Body *BlockStmt
}

func (*ForStmt) stmtNode() {}

// RangeStmt is `for k, v := range x { ... }`.
type RangeStmt struct {
	Pos
	Key   Expr
	Value Expr
	Tok   string // ":=" or "="
	X     Expr
	Body  *BlockStmt
}

func (*RangeStmt) stmtNode() {}

// SwitchStmt is `switch [init;] [tag] { ... }`.
type SwitchStmt struct {
	Pos
	Init Stmt
	Tag  Expr
	Body []*CaseClause
}

func (*SwitchStmt) stmtNode() {}

// TypeSwitchStmt is `switch x := v.(type) { ... }`.
type TypeSwitchStmt struct {
	Pos
	Init   Stmt
	Assign Stmt
	Body   []*CaseClause
}

func (*TypeSwitchStmt) stmtNode() {}

// CaseClause is one `case ...:` / `default:` arm.
type CaseClause struct {
	Pos
	List []Expr // nil for default
	Body []Stmt
}

// SelectStmt is `select { ... }`.
type SelectStmt struct {
	Pos
	Body []*CommClause
}

func (*SelectStmt) stmtNode() {}

// CommClause is one `case ...:` arm inside a select.
type CommClause struct {
	Pos
	Comm Stmt // SendStmt, ExprStmt, AssignStmt or nil for default
	Body []Stmt
}

// ReturnStmt is `return [exprs]`.
type ReturnStmt struct {
	Pos
	Results []Expr
}

func (*ReturnStmt) stmtNode() {}

// BranchStmt is break/continue/fallthrough/goto.
type BranchStmt struct {
	Pos
	Tok   string
	Label string
}

func (*BranchStmt) stmtNode() {}

// DeferStmt is `defer call()`.
type DeferStmt struct {
	Pos
	Call Expr
}

func (*DeferStmt) stmtNode() {}

// GoStmt is `go call()`.
type GoStmt struct {
	Pos
	Call Expr
}

func (*GoStmt) stmtNode() {}

// SendStmt is `ch <- v`.
type SendStmt struct {
	Pos
	Chan  Expr
	Value Expr
}

func (*SendStmt) stmtNode() {}

// LabeledStmt is `label: stmt`.
type LabeledStmt struct {
	Pos
	Label string
	Stmt  Stmt
}

func (*LabeledStmt) stmtNode() {}

// DeclStmt wraps a declaration used as a statement.
type DeclStmt struct {
	Pos
	Decl Decl
}

func (*DeclStmt) stmtNode() {}

// ------------------------------------------------------------------ expressions

// Ident is a name.
type Ident struct {
	Pos
	Name string
}

func (*Ident) exprNode()    {}
func (*Ident) typeNode()    {}

// BasicLit is a literal.
type BasicLit struct {
	Pos
	Kind  string // INT | FLOAT | IMAG | STRING | CHAR
	Value string
}

func (*BasicLit) exprNode() {}

// BinaryExpr is `X op Y`.
type BinaryExpr struct {
	Pos
	Op string
	X  Expr
	Y  Expr
}

func (*BinaryExpr) exprNode() {}

// UnaryExpr is `op X` (`-x`, `!x`, `&x`, `*x`, `<-x`, `^x`).
type UnaryExpr struct {
	Pos
	Op string
	X  Expr
}

func (*UnaryExpr) exprNode() {}

// CallExpr is `Fun(args...)`.
type CallExpr struct {
	Pos
	Fun      Expr
	Args     []Expr
	Ellipsis bool
}

func (*CallExpr) exprNode() {}

// SelectorExpr is `X.Sel`.
type SelectorExpr struct {
	Pos
	X   Expr
	Sel string
}

func (*SelectorExpr) exprNode() {}

// IndexExpr is `X[Index]`.
type IndexExpr struct {
	Pos
	X     Expr
	Index Expr
}

func (*IndexExpr) exprNode() {}

// SliceExpr is `X[Low:High:Max]` (any of the three may be nil).
type SliceExpr struct {
	Pos
	X    Expr
	Low  Expr
	High Expr
	Max  Expr
}

func (*SliceExpr) exprNode() {}

// StarExpr is `*X`: a dereference in expression position, a pointer in type
// position.
type StarExpr struct {
	Pos
	X Expr
}

func (*StarExpr) exprNode() {}
func (*StarExpr) typeNode() {}

// CompositeLit is `T{...}` or `{...}` (Type may be nil).
type CompositeLit struct {
	Pos
	Type Expr
	Elts []Expr
}

func (*CompositeLit) exprNode() {}

// KeyValueExpr is `Key: Value` inside a composite literal.
type KeyValueExpr struct {
	Pos
	Key   Expr
	Value Expr
}

func (*KeyValueExpr) exprNode() {}

// ParenExpr is `(X)`.
type ParenExpr struct {
	Pos
	X Expr
}

func (*ParenExpr) exprNode() {}

// FuncLit is a function literal.
type FuncLit struct {
	Pos
	Type *FuncType
	Body *BlockStmt
}

func (*FuncLit) exprNode() {}

// TypeAssertExpr is `X.(T)` or `X.(type)` (Type nil in a type switch).
type TypeAssertExpr struct {
	Pos
	X    Expr
	Type Expr
}

func (*TypeAssertExpr) exprNode() {}

// ------------------------------------------------------------------------- types

// PtrType is `*T`.
type PtrType struct {
	Pos
	Elem Expr
}

func (*PtrType) typeNode() {}

// SliceType is `[]T`.
type SliceType struct {
	Pos
	Elem Expr
}

func (*SliceType) typeNode() {}

// ArrayType is `[N]T` (Len nil for `[...]T`).
type ArrayType struct {
	Pos
	Len  Expr
	Elem Expr
}

func (*ArrayType) typeNode() {}

// MapType is `map[K]V`.
type MapType struct {
	Pos
	Key   Expr
	Value Expr
}

func (*MapType) typeNode() {}

// ChanType is `chan T`, `<-chan T` or `chan<- T`.
type ChanType struct {
	Pos
	Dir   string // "both" | "recv" | "send"
	Value Expr
}

func (*ChanType) typeNode() {}

// StructType is `struct { ... }`.
type StructType struct {
	Pos
	Fields []*Field
}

func (*StructType) typeNode() {}

// InterfaceType is `interface { ... }`.
type InterfaceType struct {
	Pos
	Methods []*Field
}

func (*InterfaceType) typeNode() {}

// FuncType is `func(params) results`.
type FuncType struct {
	Pos
	Params  []*Field
	Results []*Field
}

func (*FuncType) typeNode() {}

// Ellipsis is `...T`.
type Ellipsis struct {
	Pos
	Elem Expr
}

func (*Ellipsis) typeNode() {}
