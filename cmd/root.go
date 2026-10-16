package cmd

import (
	"fmt"

	"github.com/mdaashir/GOmpiler-2/pkg/compiler"
	"github.com/spf13/cobra"
)

var (
	outputFile string
	optimize   bool
)

var rootCmd = &cobra.Command{
	Use:   "gompiler [source files]",
	Short: "A C++ compiler written in Go",
	Long:  `GOmpiler compiles C++ source files to native executables through a tokenize -> parse -> analyze -> x86-64 code generation pipeline.`,
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
	rootCmd.Flags().BoolVarP(&optimize, "optimize", "O", false, "Enable constant folding")
}
