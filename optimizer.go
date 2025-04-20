package main
package main

import (
	"fmt"
)

// OptimizationPass defines an interface for optimization passes
type OptimizationPass interface {
	Run(program *IRProgram) bool
}

// Optimizer applies various optimization passes to the IR
type Optimizer struct {
	passes []OptimizationPass
}

// NewOptimizer creates a new optimizer with standard optimization passes
func NewOptimizer() *Optimizer {
	return &Optimizer{
		passes: []OptimizationPass{
			&ConstantFoldingPass{},
			&DeadCodeEliminationPass{},
			&CommonSubexpressionEliminationPass{},
		},
	}
}

// Optimize runs the optimization passes on the IR
func (o *Optimizer) Optimize(program IRProgram) IRProgram {
	// Apply optimization passes until no further changes
	changed := true
	iterationCount := 0
	maxIterations := 10 // Limit iterations to avoid infinite loops

	for changed && iterationCount < maxIterations {
		changed = false
		iterationCount++

		for _, pass := range o.passes {
			if pass.Run(&program) {
				changed = true
			}
		}
	}

	return program
}

// ConstantFoldingPass implements constant folding optimization
type ConstantFoldingPass struct{}

func (p *ConstantFoldingPass) Run(program *IRProgram) bool {
	changed := false

	for i := range program.Functions {
		function := &program.Functions[i]
		for j := range function.Blocks {
			block := function.Blocks[j]
			
			// Create a new instruction list
			var newInstructions []IRInstruction
			constantMap := make(map[string]string)
			
			// Process instructions
			for _, inst := range block.Instructions {
				// Skip instructions that will be folded
				if skipInstruction := p.foldConstant(&inst, constantMap, &newInstructions); skipInstruction {
					changed = true
					continue
				}
				
				// Add unchanged instruction
				newInstructions = append(newInstructions, inst)
			}
			
			block.Instructions = newInstructions
		}
	}

	return changed
}

func (p *ConstantFoldingPass) foldConstant(inst *IRInstruction, constantMap map[string]string, newInstructions *[]IRInstruction) bool {
	switch inst.Type {
	case IRConst:
		// Track constants
		constantMap[inst.Dest] = inst.Value
		return false
		
	case IRAdd, IRSub, IRMul, IRDiv:
		// If both operands are constants, fold them
		val1, ok1 := constantMap[inst.Op1]
		val2, ok2 := constantMap[inst.Op2]
		
		if ok1 && ok2 {
			// Parse values
			v1, errV1 := strconv.ParseFloat(val1, 64)
			v2, errV2 := strconv.ParseFloat(val2, 64)
			
			if errV1 == nil && errV2 == nil {
				// Compute result
				var result float64
				switch inst.Type {
				case IRAdd:
					result = v1 + v2
				case IRSub:
					result = v1 - v2
				case IRMul:
					result = v1 * v2
				case IRDiv:
					if v2 != 0 {
						result = v1 / v2
					} else {
						return false // Avoid division by zero
					}
				}
				
				// Add folded constant instruction
				*newInstructions = append(*newInstructions, IRInstruction{
					Type:  IRConst,
					Dest:  inst.Dest,
					Value: fmt.Sprintf("%g", result),
				})
				
				// Update constant map
				constantMap[inst.Dest] = fmt.Sprintf("%g", result)
				return true
			}
		}
	}
	
	return false
}

// DeadCodeEliminationPass implements dead code elimination
type DeadCodeEliminationPass struct{}

func (p *DeadCodeEliminationPass) Run(program *IRProgram) bool {
	changed := false

	for i := range program.Functions {
		function := &program.Functions[i]
		for j := range function.Blocks {
			block := function.Blocks[j]
			
			// Track variable usages
			varUsages := make(map[string]int)
			
			// First pass: count variable usages
			for _, inst := range block.Instructions {
				// Count uses of variables
				if inst.Op1 != "" && !p.isConstant(inst.Op1) {
					varUsages[inst.Op1]++
				}
				if inst.Op2 != "" && !p.isConstant(inst.Op2) {
					varUsages[inst.Op2]++
				}
			}
			
			// Second pass: remove unused variable assignments
			var newInstructions []IRInstruction
			for _, inst := range block.Instructions {
				// Skip instructions that assign to unused variables
				if inst.Dest != "" && varUsages[inst.Dest] == 0 && 
				   inst.Type != IRStore && inst.Type != IRRet && 
				   inst.Type != IRCall && inst.Type != IRJmp && 
				   inst.Type != IRJe && inst.Type != IRJne && 
				   inst.Type != IRJl && inst.Type != IRJle && 
				   inst.Type != IRJg && inst.Type != IRJge {
					changed = true
					continue
				}
				
				// Keep other instructions
				newInstructions = append(newInstructions, inst)
			}
			
			block.Instructions = newInstructions
		}
	}

	return changed
}

func (p *DeadCodeEliminationPass) isConstant(val string) bool {
	// Check if the value is a numeric constant (starts with a digit or negative sign)
	if len(val) == 0 {
		return false
	}
	return unicode.IsDigit(rune(val[0])) || (val[0] == '-' && len(val) > 1 && unicode.IsDigit(rune(val[1])))
}

// CommonSubexpressionEliminationPass implements CSE optimization
type CommonSubexpressionEliminationPass struct{}

func (p *CommonSubexpressionEliminationPass) Run(program *IRProgram) bool {
	changed := false

	for i := range program.Functions {
		function := &program.Functions[i]
		for j := range function.Blocks {
			block := function.Blocks[j]
			
			// Track expressions and their results
			exprMap := make(map[string]string)
			
			// Create a new instruction list
			var newInstructions []IRInstruction
			
			// Process instructions
			for _, inst := range block.Instructions {
				// Check if this is a binary operation that can be eliminated
				if p.canEliminate(inst.Type) {
					// Create a key for this expression
					exprKey := fmt.Sprintf("%s_%s_%s", inst.Type, inst.Op1, inst.Op2)
					
					// If we've seen this expression before, reuse the result
					if existingDest, found := exprMap[exprKey]; found {
						// Add a load instruction to copy the existing result
						*newInstructions = append(newInstructions, IRInstruction{
							Type:  IRLoad,
							Dest:  inst.Dest,
							Op1:   existingDest,
							Value: "CSE",
						})
						changed = true
						continue
					}
					
					// Otherwise, remember this expression
					exprMap[exprKey] = inst.Dest
				}
				
				// Add the original instruction
				newInstructions = append(newInstructions, inst)
			}
			
			block.Instructions = newInstructions
		}
	}

	return changed
}

func (p *CommonSubexpressionEliminationPass) canEliminate(opType IRType) bool {
	return opType == IRAdd || opType == IRSub || opType == IRMul || opType == IRDiv
}
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
