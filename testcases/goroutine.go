package main

import "fmt"

func main() {
	sum := make(chan int)
	done := make(chan bool)
	go func() {
		sum <- 1 + 2
		done <- true
	}()
	s := <-sum
	<-done
	fmt.Println(s)
}
