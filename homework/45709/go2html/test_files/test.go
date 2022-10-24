package main

//import fmt package
import "fmt"

type person struct {
	name string
	age  uint8
}

func main() {
	martin := person{"Martin Kostov", 22}

	fmt.Println(martin)
}
