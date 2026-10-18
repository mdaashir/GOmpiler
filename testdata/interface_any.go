package main

import "fmt"

func main() {
	var x interface{} = 42
	fmt.Println(x)
	x = "hello"
	fmt.Println(x)
	x = 3.5
	fmt.Println(x)
}
