package main

import (
	"fmt"
	"strings"
)

func main() {
	fmt.Println(strings.HasSuffix("ammu", "mu"))
	fmt.Println(strings.HasSuffix("ammu", "x"))
	fmt.Println(strings.HasPrefix("asdfqwerty", "asdf"))
}
