package main

import (
	"fmt"
)

type HelloWorlder struct {
	message string
}

func (hw *HelloWorlder) greet() {
	fmt.Println(hw.message)
}

func main() {
	helloWorlder := &HelloWorlder{
		message: "Hello, world!",
	}

	helloWorlder.greet()
}