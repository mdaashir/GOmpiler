package main

import (
	"fmt"
)

type Counter struct {
	n int
}

func (c *Counter) Inc() {
	c.n++
}

func (c *Counter) Add(d int) {
	c.n = c.n + d
}

func (c Counter) Get() int {
	return c.n
}

type Point struct {
	X int
	Y int
}

func (p Point) Sum() int {
	return p.X + p.Y
}

func main() {
	c := &Counter{n: 0}
	c.Inc()
	c.Inc()
	c.Add(10)
	fmt.Println(c.Get())

	p := Point{X: 3, Y: 4}
	fmt.Println(p.Sum())
}
