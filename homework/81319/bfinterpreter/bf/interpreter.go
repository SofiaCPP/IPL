package bf

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"strconv"
)

type instruction struct {
	idx             uint
	instructionType token

	matchingBracketIdx uint
}

type state struct {
	mem            memoryState
	src            []instruction
	instructionPtr uint

	buffer []byte
	reader io.Reader
	writer io.Writer
}

func (s *state) moveRight() {
	s.mem.incrementPtr()
}

func (s *state) moveLeft() {
	s.mem.decrementPtr()
}

func (s *state) incrementValue() {
	s.mem.incrementCell()
}

func (s *state) decrementValue() {
	s.mem.decerementCell()
}

func (s *state) readCellValue() error {
	count, err := s.reader.Read(s.buffer)
	if err == io.EOF {
		return nil
	}

	if err != nil {
		return errors.New("couldn't read from input")
	}

	if count < 1 {
		return errors.New("couldn't read from input")
	}

	s.mem.setCell(s.buffer[0])

	return nil
}

func (s *state) writeCellValue() error {
	count, err := s.writer.Write([]byte{s.mem.getCell()})

	if err != nil {
		return errors.New("couldn't write to output")
	}

	if count != 1 {
		return errors.New("couldn't write to output")
	}

	return nil
}

func (s *state) interpretInstruction() {
	instr := s.src[s.instructionPtr]

	switch instr.instructionType {
	case movR:
		s.moveRight()
	case movL:
		s.moveLeft()
	case incD:
		s.incrementValue()
	case decD:
		s.decrementValue()
	case outD:
		s.writeCellValue()
	case inD:
		s.readCellValue()
	case loopS:
		if s.mem.getCell() == 0 {
			s.instructionPtr = instr.matchingBracketIdx
		}
	case loopE:
		if s.mem.getCell() != 0 {
			s.instructionPtr = instr.matchingBracketIdx
		}
	}
	s.instructionPtr += 1
}

func (s *state) addInstructions(src string) {
	instructions := parseInput(tokenizeInput(src))

	s.src = append(s.src, instructions...)
}

type flushingWriter struct {
	w *bufio.Writer
}

func newFlushingWriter() io.Writer {
	writer := &flushingWriter{
		w: bufio.NewWriter(os.Stdout),
	}

	return writer
}

func (w *flushingWriter) Write(p []byte) (int, error) {
	fmt.Print("#=> ")
	count, err := w.w.Write(p)
	if err != nil {
		return 0, err
	}
	w.w.Flush()

	fmt.Println()

	return count, nil
}

type lineReader struct {
	r *bufio.Reader
}

func newLineReader() io.Reader {
	reader := &lineReader{
		r: bufio.NewReader(os.Stdin),
	}

	return reader
}

func (r *lineReader) Read(buffer []byte) (int, error) {
	stringVal, err := r.r.ReadString('\n')

	if err != nil {
		return 0, err
	}

	result, err := strconv.ParseInt(stringVal[:len(stringVal)-1], 10, 8)

	if err != nil {
		return 0, err
	}

	buffer[0] = byte(result)

	return 1, nil
}

func NewInterpreter(src string) state {
	instructions := parseInput(tokenizeInput(src))

	state := state{
		instructionPtr: 0,
		src:            instructions,
		buffer:         make([]byte, 1),
		writer:         newFlushingWriter(),
		reader:         newLineReader(),
	}

	return state
}

func (s *state) Run() {
	for {
		if int(s.instructionPtr) == len(s.src) {
			return
		}

		s.interpretInstruction()
	}
}
