package bf

const (
	MEMORY_SIZE uint = 1024
)

type memoryState struct {
	memory         [MEMORY_SIZE]cell
	currentCellIdx uint
}

func (m *memoryState) incrementPtr() {
	if m.currentCellIdx == MEMORY_SIZE {
		return
	}
	m.currentCellIdx += 1
}

func (m *memoryState) decrementPtr() {
	if m.currentCellIdx == 0 {
		return
	}
	m.currentCellIdx -= 1
}

func (m *memoryState) incrementCell() {
	m.memory[m.currentCellIdx].increment()
}

func (m *memoryState) decerementCell() {
	m.memory[m.currentCellIdx].decrement()
}

func (m *memoryState) setCell(value byte) {
	m.memory[m.currentCellIdx].setValue(value)
}

func (m *memoryState) getCell() byte {
	return m.memory[m.currentCellIdx].getValue()
}
