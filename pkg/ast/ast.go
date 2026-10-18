// Package ast defines the syntax tree produced by the parser.
//
// Every node embeds Pos, which satisfies the Node interface. Concrete node
// kinds are separated into expressions, statements, declarations and types so
// that later stages can reject malformed trees at compile time where practical.
package ast

// Position is a source position.
type Position struct {
	Line int
	Col  int
}

// Pos returns the node position.
func (p Position) Pos() Position { return p }

// Node is anything in the tree.
type Node interface {
	Pos() Position
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
	Position
	Package string
	Imports []*ImportSpec
	Decls   []Decl
}

// ---------------------------------------------------------------- declarations

// ImportSpec is one `import` entry.
type ImportSpec struct {
	Position
	Name string // optional alias, may be "_" or "."
	Path string
}

func (*ImportSpec) specNode() {}

// GenDecl is a var/const/type block.
type GenDecl struct {
	Position
	Tok   string // "var" | "const" | "type"
	Specs []Spec
}

func (*GenDecl) declNode() {}

// ValueSpec is `var a, b T = x, y` or `const c = 1`.
type ValueSpec struct {
	Position
	Names  []string
	Type   Expr // may be nil
	Values []Expr
}

func (*ValueSpec) specNode() {}

// TypeSpec is `type Name T` or `type Name = T` or `type Name struct {...}`.
type TypeSpec struct {
	Position
	Name   string
	Assign bool
	Type   Expr
}

func (*TypeSpec) specNode() {}

// FuncDecl is a function or method declaration.
type FuncDecl struct {
	Position
	Recv *FieldList // non-nil for methods
	Name string
	Type *FuncType
	Body *BlockStmt
}

func (*FuncDecl) declNode() {}

// Field is a name/type pair in a parameter list, struct or interface.
type Field struct {
	Position
	Names []string
	Type  Expr
}

// FieldList groups fields (parameters, results, struct/interface members).
type FieldList struct {
	Position
	List []*Field
}

// ------------------------------------------------------------------- statements

// BlockStmt is `{ ... }`.
type BlockStmt struct {
	Position
	List []Stmt
}

func (*BlockStmt) stmtNode() {}

// ExprStmt is an expression used as a statement.
type ExprStmt struct {
	Position
	X Expr
}

func (*ExprStmt) stmtNode() {}

// AssignStmt covers `=`, `:=` and the compound assignments.
type AssignStmt struct {
	Position
	Lhs []Expr
	Op  string
	Rhs []Expr
}

func (*AssignStmt) stmtNode() {}

// IncDecStmt is `x++` or `x--`.
type IncDecStmt struct {
	Position
	X  Expr
	Op string
}

func (*IncDecStmt) stmtNode() {}

// IfStmt is `if [init;] cond { ... } [else ...]`.
type IfStmt struct {
	Position
	Init Stmt
	Cond Expr
	Body *BlockStmt
	Else Stmt
}

func (*IfStmt) stmtNode() {}

// ForStmt is a three-clause or bare `for` loop.
type ForStmt struct {
	Position
	Init Stmt
	Cond Expr
	Post Stmt
	Body *BlockStmt
}

func (*ForStmt) stmtNode() {}

// RangeStmt is `for k, v := range x { ... }`.
type RangeStmt struct {
	Position
	Key   Expr
	Value Expr
	Tok   string // ":=" or "="
	X     Expr
	Body  *BlockStmt
}

func (*RangeStmt) stmtNode() {}

// SwitchStmt is `switch [init;] [tag] { ... }`.
type SwitchStmt struct {
	Position
	Init Stmt
	Tag  Expr
	Body []*CaseClause
}

func (*SwitchStmt) stmtNode() {}

// TypeSwitchStmt is `switch x := v.(type) { ... }`.
type TypeSwitchStmt struct {
	Position
	Init   Stmt
	Assign Stmt
	Body   []*CaseClause
}

func (*TypeSwitchStmt) stmtNode() {}

// CaseClause is one `case ...:` / `default:` arm.
type CaseClause struct {
	Position
	List []Expr // nil for default
	Body []Stmt
}

// SelectStmt is `select { ... }`.
type SelectStmt struct {
	Position
	Body []*CommClause
}

func (*SelectStmt) stmtNode() {}

// CommClause is one `case ...:` arm inside a select.
type CommClause struct {
	Position
	Comm Stmt // SendStmt, ExprStmt, AssignStmt or nil for default
	Body []Stmt
}

// ReturnStmt is `return [exprs]`.
type ReturnStmt struct {
	Position
	Results []Expr
}

func (*ReturnStmt) stmtNode() {}

// BranchStmt is break/continue/fallthrough/goto.
type BranchStmt struct {
	Position
	Tok   string
	Label string
}

func (*BranchStmt) stmtNode() {}

// DeferStmt is `defer call()`.
type DeferStmt struct {
	Position
	Call Expr
}

func (*DeferStmt) stmtNode() {}

// GoStmt is `go call()`.
type GoStmt struct {
	Position
	Call Expr
}

func (*GoStmt) stmtNode() {}

// SendStmt is `ch <- v`.
type SendStmt struct {
	Position
	Chan  Expr
	Value Expr
}

func (*SendStmt) stmtNode() {}

// LabeledStmt is `label: stmt`.
type LabeledStmt struct {
	Position
	Label string
	Stmt  Stmt
}

func (*LabeledStmt) stmtNode() {}

// DeclStmt wraps a declaration used as a statement.
type DeclStmt struct {
	Position
	Decl Decl
}

func (*DeclStmt) stmtNode() {}

// ------------------------------------------------------------------ expressions

// Ident is a name.
type Ident struct {
	Position
	Name string
}

func (*Ident) exprNode() {}
func (*Ident) typeNode() {}

// BasicLit is a literal.
type BasicLit struct {
	Position
	Kind  string // INT | FLOAT | IMAG | STRING | CHAR
	Value string
}

func (*BasicLit) exprNode() {}

// BinaryExpr is `X op Y`.
type BinaryExpr struct {
	Position
	Op string
	X  Expr
	Y  Expr
}

func (*BinaryExpr) exprNode() {}

// UnaryExpr is `op X` (`-x`, `!x`, `&x`, `*x`, `<-x`, `^x`).
type UnaryExpr struct {
	Position
	Op string
	X  Expr
}

func (*UnaryExpr) exprNode() {}

// CallExpr is `Fun(args...)`.
type CallExpr struct {
	Position
	Fun      Expr
	Args     []Expr
	Ellipsis bool
}

func (*CallExpr) exprNode() {}

// SelectorExpr is `X.Sel`.
type SelectorExpr struct {
	Position
	X   Expr
	Sel string
}

func (*SelectorExpr) exprNode() {}

// IndexExpr is `X[Index]`.
type IndexExpr struct {
	Position
	X     Expr
	Index Expr
}

func (*IndexExpr) exprNode() {}

// SliceExpr is `X[Low:High:Max]` (any of the three may be nil).
type SliceExpr struct {
	Position
	X    Expr
	Low  Expr
	High Expr
	Max  Expr
}

func (*SliceExpr) exprNode() {}

// StarExpr is `*X`: a dereference in expression position, a pointer in type
// position.
type StarExpr struct {
	Position
	X Expr
}

func (*StarExpr) exprNode() {}
func (*StarExpr) typeNode() {}

// CompositeLit is `T{...}` or `{...}` (Type may be nil).
type CompositeLit struct {
	Position
	Type Expr
	Elts []Expr
}

func (*CompositeLit) exprNode() {}

// KeyValueExpr is `Key: Value` inside a composite literal.
type KeyValueExpr struct {
	Position
	Key   Expr
	Value Expr
}

func (*KeyValueExpr) exprNode() {}

// ParenExpr is `(X)`.
type ParenExpr struct {
	Position
	X Expr
}

func (*ParenExpr) exprNode() {}

// FuncLit is a function literal.
type FuncLit struct {
	Position
	Type *FuncType
	Body *BlockStmt
}

func (*FuncLit) exprNode() {}

// TypeAssertExpr is `X.(T)` or `X.(type)` (Type nil in a type switch).
type TypeAssertExpr struct {
	Position
	X    Expr
	Type Expr
}

func (*TypeAssertExpr) exprNode() {}

// ------------------------------------------------------------------------- types

// PtrType is `*T`.
type PtrType struct {
	Position
	Elem Expr
}

func (*PtrType) typeNode() {}
func (*PtrType) exprNode() {}

// SliceType is `[]T`.
type SliceType struct {
	Position
	Elem Expr
}

func (*SliceType) typeNode() {}
func (*SliceType) exprNode() {}

// ArrayType is `[N]T` (Len nil for `[...]T`).
type ArrayType struct {
	Position
	Len  Expr
	Elem Expr
}

func (*ArrayType) typeNode() {}
func (*ArrayType) exprNode() {}

// MapType is `map[K]V`.
type MapType struct {
	Position
	Key   Expr
	Value Expr
}

func (*MapType) typeNode() {}
func (*MapType) exprNode() {}

// ChanType is `chan T`, `<-chan T` or `chan<- T`.
type ChanType struct {
	Position
	Dir   string // "both" | "recv" | "send"
	Value Expr
}

func (*ChanType) typeNode() {}
func (*ChanType) exprNode() {}

// StructType is `struct { ... }`.
type StructType struct {
	Position
	Fields []*Field
}

func (*StructType) typeNode() {}
func (*StructType) exprNode() {}

// InterfaceType is `interface { ... }`.
type InterfaceType struct {
	Position
	Methods []*Field
}

func (*InterfaceType) typeNode() {}
func (*InterfaceType) exprNode() {}

// FuncType is `func(params) results`.
type FuncType struct {
	Position
	Params  []*Field
	Results []*Field
}

func (*FuncType) typeNode() {}
func (*FuncType) exprNode() {}

// Ellipsis is `...T`.
type Ellipsis struct {
	Position
	Elem Expr
}

func (*Ellipsis) typeNode() {}
func (*Ellipsis) exprNode() {}
