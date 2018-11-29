package bf

import (
	"bufio"
	"fmt"
	"os"
)

type repl struct {
	interpreter *state
	reader      *bufio.Reader
	writer      *bufio.Writer
}

func NewRepl() repl {
	intepreter := NewInterpreter("")
	return repl{
		interpreter: &intepreter,
		reader:      bufio.NewReader(os.Stdin),
		writer:      bufio.NewWriter(os.Stdout),
	}
}

func (r *repl) read() {
	fmt.Print(">>")
	instructions, err := r.reader.ReadString('\n')
	instructions = instructions[:len(instructions)-1]
	if err != nil {
		panic("not implemented")
	}

	r.interpreter.addInstructions(instructions)
}

func (r *repl) eval() {
	r.interpreter.Run()
}

func (r *repl) Start() {
	for {
		r.read()
		r.eval()
	}
}
