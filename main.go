package main

import (
	"fmt"
	"os"

	"github.com/mdaashir/GOmpiler-2/cmd"
)

func main() {
	if err := cmd.Execute(); err != nil {
		_, err := fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		if err != nil {
			return
		}
		os.Exit(1)
	}
}
