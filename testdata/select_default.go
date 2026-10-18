package main

import "fmt"

func main() {
	c := make(chan int, 1)
	select {
	case v := <-c:
		fmt.Println("got", v)
	default:
		fmt.Println("empty")
	}
}
