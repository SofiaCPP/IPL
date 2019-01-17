package bf

import (
	"bytes"
	"io"
)

type token int

const (
	unkwn token = iota
	movR
	movL
	incD
	decD
	outD
	inD
	loopS
	loopE
)

func tokenize(symbol string) token {
	switch symbol {
	case ">":
		return movR
	case "<":
		return movL
	case ".":
		return outD
	case ",":
		return inD
	case "+":
		return incD
	case "-":
		return decD
	case "[":
		return loopS
	case "]":
		return loopE
	}

	return 0
}

type tokenizingStream struct {
	reader io.Reader
}

func (s *tokenizingStream) Read(p []byte) (int, error) {
	buffer := make([]byte, len(p))

	count, err := s.reader.Read(buffer)
	if err != nil {
		return 0, err
	}

	for i := 0; i < count; i++ {
		p[i] = byte(tokenize(string(buffer[i])))
	}

	return count, nil
}

func tokenizeInput(input string) []instruction {
	byteBuff := []byte(input)
	reader := bytes.NewReader(byteBuff)
	tokenizingReader := tokenizingStream{
		reader: reader,
	}

	bytesRead := make([]byte, 1)
	instructions := make([]instruction, 0)

	for {
		_, ok := tokenizingReader.Read(bytesRead)
		if ok == io.EOF {
			break
		}
		if ok == nil {
			inst := instruction{
				instructionType: token(bytesRead[0]),
			}

			instructions = append(instructions, inst)
		}
	}

	return instructions
}

func parseInput(instructions []instruction) []instruction {
	stack := make([]instruction, 0)

	for idx, inst := range instructions {
		instructions[idx].idx = uint(idx)

		if inst.instructionType != loopS && inst.instructionType != loopE {
			continue
		}

		if inst.instructionType == loopS {
			inst.idx = uint(idx)
			stack = append(stack, inst)

			continue
		}

		if len(stack) < 1 {
			panic("input data invalid!")
		}

		prevInst := stack[len(stack)-1]
		prevInst.matchingBracketIdx = uint(idx)
		instructions[prevInst.idx] = prevInst

		inst.idx = uint(idx)
		inst.matchingBracketIdx = prevInst.idx
		instructions[idx] = inst

		stack = stack[:len(stack)-1]
	}

	return instructions
}
