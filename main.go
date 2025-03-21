package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <filename>")
		os.Exit(1)
	}

	filename := os.Args[1]
	file, err := os.Open(filename)
	if err != nil {
		fmt.Println("Error reading file:", err)
		os.Exit(1)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var content strings.Builder
	for scanner.Scan() {
		content.WriteString(scanner.Text() + "\n")
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error scanning file:", err)
		os.Exit(1)
	}

	lexer := NewLexer(content.String())
	tokens := lexer.Lex()
	fmt.Println("Lexing Done, Token Count:", len(tokens))
	parser := NewParser(tokens)
	ast := parser.Parse()
	fmt.Println("Parsing Done")
	astFile, err := os.Create("ast.txt")
	if err != nil {
		fmt.Println("Error creating AST file:", err)
		os.Exit(1)
	}
	defer astFile.Close()
	astWriter := bufio.NewWriter(astFile)
	writeAST(ast, astWriter, 0)
	astWriter.Flush()

	fmt.Println("AST written to ast.txt")
}

func writeAST(node *ASTNode, writer *bufio.Writer, depth int) {
	for i := 0; i < depth; i++ {
		writer.WriteString("  ")
	}
	writer.WriteString(fmt.Sprintf("%s : %s\n", node.Type, node.Value))
	for _, child := range node.Children {
		writeAST(child, writer, depth+1)
	}
}
