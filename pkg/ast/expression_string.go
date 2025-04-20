package ast

import "fmt"

// String methods for all expression types

func (ae *AssignmentExpression) String() string {
	return fmt.Sprintf("(%v %s %v)", ae.Left, ae.Operator, ae.Right)
}

func (ue *UnaryExpression) String() string {
	return fmt.Sprintf("%s%v", ue.Operator, ue.Right)
}

func (ce *CallExpression) String() string {
	args := ""
	for i, arg := range ce.Arguments {
		if i > 0 {
			args += ", "
		}
		args += fmt.Sprintf("%v", arg)
	}
	return fmt.Sprintf("%v(%s)", ce.Function, args)
}

func (me *MemberExpression) String() string {
	return fmt.Sprintf("%v.%v", me.Object, me.Property)
}

func (ie *IndexExpression) String() string {
	return fmt.Sprintf("%v[%v]", ie.Array, ie.Index)
}

func (i *Identifier) String() string {
	return i.Value
}

func (nl *NumberLiteral) String() string {
	return nl.Value
}

func (sl *StringLiteral) String() string {
	return sl.Value
}

func (cl *CharLiteral) String() string {
	return cl.Value
}

func (bl *BooleanLiteral) String() string {
	if bl.Value {
		return "true"
	}
	return "false"
}

func (ne *NullExpression) String() string {
	return "null"
}
