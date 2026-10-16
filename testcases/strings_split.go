package main

import (
	"fmt"
	"strings"
)

func main() {
	parts := strings.Split("a,b,c", ",")
	fmt.Println(strings.Join(parts, "-"))
	parts2 := strings.SplitN("a,b,c,d", ",", 3)
	fmt.Println(strings.Join(parts2, "|"))
}
