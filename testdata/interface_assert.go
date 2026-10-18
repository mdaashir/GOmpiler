package main

import "fmt"

func main() {
	var x interface{} = 7
	n := x.(int)
	fmt.Println(n + 1)
}
