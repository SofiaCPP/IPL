package main

import (
	"encoding/json"
	"errors"
	"io/ioutil"
	"net/http"
	"net/url"
	"regexp"
	"strconv"
	"sync"
	//"fmt"
)

type Cell struct {
	X int64 `json:"x"`
	Y int64 `json:"y"`
}

type GameOfLifeHandler struct {
	Generation      int
	Board           map[Cell]bool
	Neighbours      map[Cell]int
	RespWriter      http.ResponseWriter
	BoardMutex      sync.Mutex
	NeighboursMutex sync.Mutex
	RequestMutex    sync.RWMutex
}

/* util functions */
func (g *GameOfLifeHandler) sendError(err error) {
	g.RespWriter.Header().Set("Error", err.Error())
	g.RespWriter.WriteHeader(http.StatusBadRequest)
}

func (g *GameOfLifeHandler) parseXYQuery(r *http.Request) (result Cell, err error) {
	values, err := url.ParseQuery(r.URL.RawQuery)
	if err != nil {
		g.sendError(err)
		return
	}

	// validate
	if len(values) != 2 {
		err = errors.New("Invalid parameters query - invalid number of query parameters")
		g.sendError(err)
		return
	}

	xString := values.Get("x")
	yString := values.Get("y")

	if xString == "" || yString == "" {
		err = errors.New("Invalid parameters query - x or y not set")
		g.sendError(err)
		return
	}

	x, err := strconv.ParseInt(xString, 10, 64)
	if err != nil {
		g.sendError(err)
		return
	}
	y, err := strconv.ParseInt(yString, 10, 64)
	if err != nil {
		g.sendError(err)
		return
	}

	result.X = x
	result.Y = y

	return
}

func updateNeighbours(cell Cell, neighbours *map[Cell]int) {
	var neighbour Cell

	for i := cell.X - 1; i <= cell.X+1; i++ {
		for j := cell.Y - 1; j <= cell.Y+1; j++ {

			if i == cell.X && j == cell.Y {
				continue
			}

			neighbour.X = i
			neighbour.Y = j

			(*neighbours)[neighbour]++
		}
	}
}

/* url handlers */
func (g *GameOfLifeHandler) addNewCells(r *http.Request) (err error) {
	var cellSlice []Cell
	cellSlice = make([]Cell, 0, 2)

	jsonBytes, err := ioutil.ReadAll(r.Body)
	if err != nil {
		g.sendError(err)
		return
	}

	err = json.Unmarshal(jsonBytes, &cellSlice)
	if err != nil {
		g.sendError(err)
		return
	}

	for _, cell := range cellSlice {
		g.BoardMutex.Lock()
		g.Board[cell] = true
		g.BoardMutex.Unlock()

		g.NeighboursMutex.Lock()
		updateNeighbours(cell, &g.Neighbours)
		g.NeighboursMutex.Unlock()
	}

	return err
}

// writes json as a response
func (g *GameOfLifeHandler) checkCell(w http.ResponseWriter, cell Cell) (err error) {
	alive := make(map[string]bool)
	alive["alive"] = g.Board[cell]

	output, err := json.Marshal(alive)
	if err != nil {
		g.sendError(err)
		return
	}

	w.WriteHeader(http.StatusOK)
	w.Write(output)
	return
}

// writes json as a response
func (g *GameOfLifeHandler) generation(w http.ResponseWriter) {
	generation := make([]Cell, 0, len(g.Board))

	for cell := range g.Board {
		generation = append(generation, cell)
	}

	currentGeneration := make(map[string]interface{})
	currentGeneration["generation"] = strconv.Itoa(g.Generation)
	currentGeneration["living"] = generation

	output, err := json.Marshal(currentGeneration)
	if err != nil {
		g.sendError(err)
		return
	}

	w.WriteHeader(http.StatusOK)
	w.Write(output)
}

func (g *GameOfLifeHandler) evolve(w http.ResponseWriter) {
	newBoard := make(map[Cell]bool)
	newNeighbourBoard := make(map[Cell]int)

	for cell, neighbours := range g.Neighbours {
		if g.Board[cell] {
			if neighbours == 2 || neighbours == 3 {
				newBoard[cell] = true

				updateNeighbours(cell, &newNeighbourBoard)
			}
		} else {
			if neighbours == 3 {
				newBoard[cell] = true

				updateNeighbours(cell, &newNeighbourBoard)
			}
		}
	}

	g.BoardMutex.Lock()
	g.Board = newBoard
	g.BoardMutex.Unlock()

	g.NeighboursMutex.Lock()
	g.Neighbours = newNeighbourBoard
	g.NeighboursMutex.Unlock()

	g.Generation++

	w.WriteHeader(http.StatusNoContent)
}

func (g *GameOfLifeHandler) reset(w http.ResponseWriter) {
	g.Generation = 0

	g.BoardMutex.Lock()
	g.Board = make(map[Cell]bool)
	g.BoardMutex.Unlock()

	w.WriteHeader(http.StatusNoContent)
}

func (g *GameOfLifeHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	path := r.URL.Path

	switch {
	case regexp.MustCompile(`/generation/evolve/`).MatchString(path):
		// POST
		if r.Method != "POST" {
			w.WriteHeader(http.StatusMethodNotAllowed)
		} else {
			g.RequestMutex.Lock()

			g.evolve(w)

			g.RequestMutex.Unlock()
		}

	case regexp.MustCompile(`/generation/`).MatchString(path):
		// GET
		if r.Method != "GET" {
			w.WriteHeader(http.StatusMethodNotAllowed)
		} else {
			g.RequestMutex.RLock()

			g.generation(w)

			g.RequestMutex.RUnlock()
		}

	case regexp.MustCompile(`/cell/status/`).MatchString(path):
		// GET
		if r.Method != "GET" {
			w.WriteHeader(http.StatusMethodNotAllowed)
		} else {
			g.RequestMutex.RLock()

			cell, err := g.parseXYQuery(r)
			if err == nil {
				g.checkCell(w, cell)
			}

			g.RequestMutex.RUnlock()
		}

	case regexp.MustCompile(`/cells/`).MatchString(path):
		// POST
		if r.Method != "POST" {
			w.WriteHeader(http.StatusMethodNotAllowed)
		} else {
			g.RequestMutex.Lock()

			if err := g.addNewCells(r); err == nil {
				w.WriteHeader(http.StatusCreated)
			}

			g.RequestMutex.Unlock()
		}

	case regexp.MustCompile(`/reset/`).MatchString(path):
		// POST
		if r.Method != "POST" {
			w.WriteHeader(http.StatusMethodNotAllowed)
		} else {
			g.RequestMutex.Lock()

			g.reset(w)

			g.RequestMutex.Unlock()
		}

	default:
		w.WriteHeader(http.StatusNotFound)
	}
}

func NewGameOfLifeHandler(cells [][2]int64) *GameOfLifeHandler {
	board := make(map[Cell]bool)
	neighbours := make(map[Cell]int)

	var newCell Cell

	for _, cell := range cells {
		newCell.X = cell[0]
		newCell.Y = cell[1]

		board[newCell] = true

		updateNeighbours(newCell, &neighbours)
	}

	gameOfLifeHandler := &GameOfLifeHandler{Generation: 0, Board: board, Neighbours: neighbours}

	return gameOfLifeHandler
}

func main() {
	gofh := NewGameOfLifeHandler([][2]int64{
		{2, 3},
		{2, 4},
		{-20, 4},
		{-42, 0},
	})

	http.ListenAndServe(":8080", gofh)
}
