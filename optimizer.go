package main

import "strconv"

type Optimizer struct {
	blocks []*BasicBlock
}

func NewOptimizer(blocks []*BasicBlock) *Optimizer {
	return &Optimizer{blocks: blocks}
}

func (o *Optimizer) Optimize() {
	o.constantFolding()
	o.deadCodeElimination()
	o.commonSubexpressionElimination()
}

func (o *Optimizer) constantFolding() {
	for _, block := range o.blocks {
		for i, inst := range block.Instructions {
			switch inst.Op {
			case ADD, SUB, MUL, DIV:
				// Check if both operands are constants
				if isConstant(inst.Arg1) && isConstant(inst.Arg2) {
					result := evaluateConstantExpr(inst)
					// Replace with assignment of constant
					block.Instructions[i] = IRInstruction{
						Op:   ASSIGN,
						Dest: inst.Dest,
						Arg1: result,
					}
				}
			}
		}
	}
}

func (o *Optimizer) deadCodeElimination() {
	for _, block := range o.blocks {
		used := make(map[string]bool)

		// Mark variables that are used
		for _, inst := range block.Instructions {
			if inst.Arg1 != "" {
				used[inst.Arg1] = true
			}
			if inst.Arg2 != "" {
				used[inst.Arg2] = true
			}
		}

		// Remove assignments to unused variables
		newInsts := make([]IRInstruction, 0)
		for _, inst := range block.Instructions {
			if inst.Op == ASSIGN && !used[inst.Dest] {
				continue
			}
			newInsts = append(newInsts, inst)
		}
		block.Instructions = newInsts
	}
}

func (o *Optimizer) commonSubexpressionElimination() {
	for _, block := range o.blocks {
		exprMap := make(map[string]string) // expression -> result variable

		for i, inst := range block.Instructions {
			switch inst.Op {
			case ADD, SUB, MUL, DIV:
				exprKey := string(inst.Op) + inst.Arg1 + inst.Arg2
				if result, exists := exprMap[exprKey]; exists {
					// Replace with assignment of previous result
					block.Instructions[i] = IRInstruction{
						Op:   ASSIGN,
						Dest: inst.Dest,
						Arg1: result,
					}
				} else {
					exprMap[exprKey] = inst.Dest
				}
			}
		}
	}
}

func isConstant(s string) bool {
	// Simple check if string represents a number
	_, err := strconv.Atoi(s)
	return err == nil
}

func evaluateConstantExpr(inst IRInstruction) string {
	a, _ := strconv.Atoi(inst.Arg1)
	b, _ := strconv.Atoi(inst.Arg2)
	var result int

	switch inst.Op {
	case ADD:
		result = a + b
	case SUB:
		result = a - b
	case MUL:
		result = a * b
	case DIV:
		if b != 0 {
			result = a / b
		}
	}

	return strconv.Itoa(result)
}
