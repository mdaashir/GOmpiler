package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"github.com/mdaashir/GOmpiler-2/pkg/compiler"
)

var (
	outputFile string
	optimize   bool
)

var rootCmd = &cobra.Command{
	Use:   "cppcompiler [source files]",
	Short: "A C++ compiler written in Go",
	Long:  `A complete C++ compiler implementation using Go. Compile C++ code to executables.`,
	Args:  cobra.MinimumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		for _, sourceFile := range args {
			if err := compiler.CompileFile(sourceFile, outputFile, optimize); err != nil {
				return fmt.Errorf("compiling %s: %w", sourceFile, err)
			}
		}
		return nil
	},
}

// Execute executes the root command
func Execute() error {
	return rootCmd.Execute()
}

func init() {
	rootCmd.Flags().StringVarP(&outputFile, "output", "o", "a.out", "Output file name")
	rootCmd.Flags().BoolVarP(&optimize, "optimize", "O", false, "Enable optimizations")
}
