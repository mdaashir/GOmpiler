package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	// Parse command line flags
	outputFlag := flag.String("o", "a.out", "Output file name")
	optimizeFlag := flag.Bool("O", false, "Enable optimizations")
	verboseFlag := flag.Bool("v", false, "Verbose output")
	flag.Parse()

	// Check for input file argument
	args := flag.Args()
	if len(args) < 1 {
		fmt.Println("Usage: gompiler [options] <input-file>")
		fmt.Println("Options:")
		flag.PrintDefaults()
		os.Exit(1)
	}

	inputFile := args[0]
	outputFile := *outputFlag

	// Validate file extension
	ext := filepath.Ext(inputFile)
	if ext != ".cpp" && ext != ".c" {
		fmt.Println("Error: Input file must have .cpp or .c extension")
		os.Exit(1)
	}
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <filename>")
		os.Exit(1)
	}

	filename := os.Args[1]
	// Validate file extension
	if !strings.HasSuffix(filename, ".txt") && !strings.HasSuffix(filename, ".cpp") {
		fmt.Println("Error: Input file must have .cpp or .txt extension")
		os.Exit(1)
	}

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

	outputFile, err := os.Create("tokens.txt")
	if err != nil {
		fmt.Println("Error creating output file:", err)
		os.Exit(1)
	}
	defer outputFile.Close()

	writer := bufio.NewWriter(outputFile)
	for _, token := range tokens {
		fmt.Fprintf(writer, "Type: %-12s Value: %s\n", token.Type, token.Value)
	}
	writer.Flush()

	fmt.Println("Tokens written to tokens.txt")

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
