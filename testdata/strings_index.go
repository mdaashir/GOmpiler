package main

import (
	"fmt"
	"strings"
)

func main() {
	fmt.Println(strings.Index("chicken", "ken"))
	fmt.Println(strings.Index("chicken", "x"))
	fmt.Println(strings.Index("chicken", "chicken"))
}
