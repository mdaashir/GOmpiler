package main

import "fmt"

type Animal interface {
	Speak() string
}

type Dog struct {
	name string
}

func (d *Dog) Speak() string {
	return d.name + " says woof"
}

type Cat struct {
	name string
}

func (c *Cat) Speak() string {
	return c.name + " says meow"
}

func describe(a Animal) string {
	return a.Speak()
}

func main() {
	d := &Dog{name: "Rex"}
	c := &Cat{name: "Tom"}
	fmt.Println(describe(d))
	fmt.Println(describe(c))
}
