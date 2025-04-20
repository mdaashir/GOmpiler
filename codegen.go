package main

type Register string

const (
	RAX Register = "rax"
	RBX Register = "rbx"
	RCX Register = "rcx"
	RDX Register = "rdx"
	RSI Register = "rsi"
	RDI Register = "rdi"
	RSP Register = "rsp"
	RBP Register = "rbp"
)

type X86Instruction struct {
	Op        string
	Dest      string
	Src       string
	Immediate int64
	Label     string
}

type CodeGenerator struct {
	registers     []Register
	freeRegisters map[Register]bool
	instructions  []X86Instruction
	labelCounter  int
}

func NewCodeGenerator() *CodeGenerator {
	regs := []Register{RAX, RBX, RCX, RDX, RSI, RDI}
	freeRegs := make(map[Register]bool)
	for _, reg := range regs {
		freeRegs[reg] = true
	}

	return &CodeGenerator{
		registers:     regs,
		freeRegisters: freeRegs,
		instructions:  make([]X86Instruction, 0),
		labelCounter:  0,
	}
}

func (g *CodeGenerator) allocateRegister() Register {
	for reg := range g.freeRegisters {
		if g.freeRegisters[reg] {
			g.freeRegisters[reg] = false
			return reg
		}
	}
	return ""
}

func (g *CodeGenerator) freeRegister(reg Register) {
	g.freeRegisters[reg] = true
}

func (g *CodeGenerator) emit(inst X86Instruction) {
	g.instructions = append(g.instructions, inst)
}

func (g *CodeGenerator) GenerateCode(block *BasicBlock) {
	// Emit label for the block
	g.emit(X86Instruction{Op: "label", Label: block.Label})

	// Generate code for each IR instruction
	for _, irInst := range block.Instructions {
		switch irInst.Op {
		case ADD:
			destReg := g.allocateRegister()
			g.emit(X86Instruction{Op: "mov", Dest: string(destReg), Src: irInst.Arg1})
			g.emit(X86Instruction{Op: "add", Dest: string(destReg), Src: irInst.Arg2})
			g.emit(X86Instruction{Op: "mov", Dest: irInst.Dest, Src: string(destReg)})
			g.freeRegister(destReg)

		case SUB:
			destReg := g.allocateRegister()
			g.emit(X86Instruction{Op: "mov", Dest: string(destReg), Src: irInst.Arg1})
			g.emit(X86Instruction{Op: "sub", Dest: string(destReg), Src: irInst.Arg2})
			g.emit(X86Instruction{Op: "mov", Dest: irInst.Dest, Src: string(destReg)})
			g.freeRegister(destReg)

		case MUL:
			g.emit(X86Instruction{Op: "mov", Dest: string(RAX), Src: irInst.Arg1})
			g.emit(X86Instruction{Op: "mul", Src: irInst.Arg2})
			g.emit(X86Instruction{Op: "mov", Dest: irInst.Dest, Src: string(RAX)})

		case DIV:
			g.emit(X86Instruction{Op: "mov", Dest: string(RAX), Src: irInst.Arg1})
			g.emit(X86Instruction{Op: "xor", Dest: string(RDX), Src: string(RDX)})
			g.emit(X86Instruction{Op: "div", Src: irInst.Arg2})
			g.emit(X86Instruction{Op: "mov", Dest: irInst.Dest, Src: string(RAX)})

		case ASSIGN:
			g.emit(X86Instruction{Op: "mov", Dest: irInst.Dest, Src: irInst.Arg1})

		case JMP:
			g.emit(X86Instruction{Op: "jmp", Label: irInst.Label})

		case JZ:
			g.emit(X86Instruction{Op: "test", Dest: irInst.Arg1, Src: irInst.Arg1})
			g.emit(X86Instruction{Op: "jz", Label: irInst.Label})

		case CALL:
			g.emit(X86Instruction{Op: "call", Label: irInst.Label})

		case RET:
			g.emit(X86Instruction{Op: "ret"})
		}
	}

	// Generate code for successor blocks
	for _, succ := range block.Successors {
		g.GenerateCode(succ)
	}
}
