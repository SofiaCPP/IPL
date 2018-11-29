package main

import "github.com/geosteffanov/bfinterpreter/bf"

func main() {
	repl := bf.NewRepl()
	repl.Start()
}
