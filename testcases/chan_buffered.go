package main

import "fmt"

func main() {
	ch := make(chan string, 2)
	ch <- "a"
	ch <- "b"
	fmt.Println(<-ch)
	fmt.Println(<-ch)
}
