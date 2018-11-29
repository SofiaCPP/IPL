package bf

type cell struct {
	value byte
}

func (c *cell) increment() {
	c.value = byte(int(c.value) + 1)
}

func (c *cell) decrement() {
	c.value = byte(int(c.value) - 1)
}

func (c *cell) getValue() byte {
	return c.value
}

func (c *cell) setValue(value byte) {
	c.value = value
}
