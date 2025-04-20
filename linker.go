package main

import (
	"encoding/binary"
	"fmt"
	"os"
)

type Symbol struct {
	Name     string
	Address  uint64
	Size     uint64
	IsGlobal bool
}

type Section struct {
	Name        string
	Data        []byte
	Size        uint64
	BaseAddr    uint64
	Relocations []Relocation
}

type Relocation struct {
	Offset uint64
	Symbol string
	Type   string
	Addend int64
}

type ObjectFile struct {
	Symbols  map[string]Symbol
	Sections map[string]Section
}

type Linker struct {
	objectFiles []ObjectFile
	symbols     map[string]Symbol
	sections    map[string]Section
}

func NewLinker() *Linker {
	return &Linker{
		objectFiles: make([]ObjectFile, 0),
		symbols:     make(map[string]Symbol),
		sections:    make(map[string]Section),
	}
}

func (l *Linker) AddObjectFile(obj ObjectFile) {
	l.objectFiles = append(l.objectFiles, obj)
}

func (l *Linker) ResolveSymbols() error {
	// First pass: collect all global symbols
	for _, obj := range l.objectFiles {
		for name, sym := range obj.Symbols {
			if sym.IsGlobal {
				if existing, exists := l.symbols[name]; exists {
					return fmt.Errorf("duplicate symbol: %s", name)
				}
				l.symbols[name] = sym
			}
		}
	}

	// Second pass: resolve all symbol references
	for _, obj := range l.objectFiles {
		for _, section := range obj.Sections {
			for _, reloc := range section.Relocations {
				if _, exists := l.symbols[reloc.Symbol]; !exists {
					return fmt.Errorf("undefined symbol: %s", reloc.Symbol)
				}
			}
		}
	}

	return nil
}

func (l *Linker) Layout() {
	baseAddr := uint64(0x400000) // Default base address for executables

	// Layout sections
	for _, obj := range l.objectFiles {
		for name, section := range obj.Sections {
			if _, exists := l.sections[name]; !exists {
				section.BaseAddr = baseAddr
				baseAddr += section.Size
				l.sections[name] = section
			}
		}
	}

	// Update symbol addresses
	for name, sym := range l.symbols {
		if section, exists := l.sections[sym.Name]; exists {
			sym.Address += section.BaseAddr
			l.symbols[name] = sym
		}
	}
}

func (l *Linker) ApplyRelocations() {
	for _, section := range l.sections {
		for _, reloc := range section.Relocations {
			targetSymbol := l.symbols[reloc.Symbol]
			relocAddr := section.BaseAddr + reloc.Offset

			switch reloc.Type {
			case "R_X86_64_PC32":
				// PC-relative 32-bit relocation
				value := targetSymbol.Address - relocAddr + uint64(reloc.Addend)
				writeUint32(section.Data[reloc.Offset:], uint32(value))

			case "R_X86_64_64":
				// Absolute 64-bit relocation
				value := targetSymbol.Address + uint64(reloc.Addend)
				writeUint64(section.Data[reloc.Offset:], value)
			}
		}
	}
}

func (l *Linker) GenerateExecutable(outputPath string) error {
	// Create ELF header
	header := createELFHeader()

	// Write sections
	file, err := os.Create(outputPath)
	if err != nil {
		return err
	}
	defer file.Close()

	// Write ELF header
	if err := binary.Write(file, binary.LittleEndian, header); err != nil {
		return err
	}

	// Write sections
	for _, section := range l.sections {
		if _, err := file.WriteAt(section.Data, int64(section.BaseAddr)); err != nil {
			return err
		}
	}

	return nil
}

func writeUint32(data []byte, value uint32) {
	binary.LittleEndian.PutUint32(data, value)
}

func writeUint64(data []byte, value uint64) {
	binary.LittleEndian.PutUint64(data, value)
}

func createELFHeader() []byte {
	// Simplified ELF header creation
	// In a real implementation, this would create a proper ELF header
	return make([]byte, 64) // Placeholder
}
