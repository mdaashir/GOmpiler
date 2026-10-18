package main

import "fmt"

func describe(x interface{}) {
	switch v := x.(type) {
	case int:
		fmt.Println("int", v)
	case string:
		fmt.Println("string", v)
	default:
		fmt.Println("other")
	}
}

func main() {
	describe(1)
	describe("hi")
	describe(3.5)
}
