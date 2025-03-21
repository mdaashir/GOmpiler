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
	parser := NewParser(tokens)
	parser.Parse()
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
}
