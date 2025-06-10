package main

import (
	"fmt"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

const (
	NrOfTravelers           = 15
	MinSteps, MaxSteps      = 10, 100
	MinDelay, MaxDelay      = 0.01, 0.05
	BoardHeight, BoardWidth = 15, 15
)

type Position struct{ x, y int }

type MoveRequest struct {
	from, to  Position
	replyCh   chan string
	traveler  int
	time      time.Duration
	reloq     bool
	trapLeave bool
}

type BoardCell struct {
	occupied bool
	reqCh    chan MoveRequest
	pos      Position
	ghost    bool
	updater  chan *BoardCell
	trap     bool
}

type Trace struct {
	timeStamp time.Duration
	Id        int
	pos       Position
	symbol    rune
}

var (
	nrOfGhosts, nrOfTraps int
	closing               bool
	startTime             = time.Now()
	outCh                 = make(chan []Trace)
	board                 [][]*BoardCell
	ghostId                     = 500
	travelersRemained     int32 = NrOfTravelers
	busy                  [BoardWidth][BoardHeight]bool
	busyLocks             [BoardWidth][BoardHeight]sync.Mutex
)

func createBoard() [][]*BoardCell {
	r := rand.New(rand.NewSource(time.Now().UnixNano()))
	b := make([][]*BoardCell, BoardWidth)
	for i := range b {
		b[i] = make([]*BoardCell, BoardHeight)
		for j := range b[i] {
			cell := &BoardCell{reqCh: make(chan MoveRequest), pos: Position{i, j}}
			if r.Float64() < 0.1 {
				cell.trap = true
				nrOfTraps++
			}
			b[i][j] = cell
			go cell.run()
			if !cell.trap {
				go simulateGhostTraffic(cell)
			}
		}
	}
	return b
}

func simulateGhostTraffic(cell *BoardCell) {
	time.Sleep(100 * time.Millisecond)
	r := rand.New(rand.NewSource(time.Now().UnixNano()))
	for travelersRemained > 0 {
		time.Sleep(10 * time.Millisecond)
		if r.Float64() < 0.001 {
			reply := make(chan string)
			cell.reqCh <- MoveRequest{traveler: -1, time: 150 * time.Millisecond, replyCh: reply}
			<-reply
		}
	}
}

func (cell *BoardCell) tryMove(req MoveRequest) string {
	cell.reqCh <- req
	return <-req.replyCh
}

func (cell *BoardCell) tryRelocate() bool {
	req := MoveRequest{reloq: true, replyCh: make(chan string)}
	for _, d := range []Position{{0, 1}, {0, -1}, {1, 0}, {-1, 0}} {
		nx := (cell.pos.x + d.x + BoardWidth) % BoardWidth
		ny := (cell.pos.y + d.y + BoardHeight) % BoardHeight
		if neighbor := board[nx][ny]; neighbor.tryMove(req) == "true" {
			neighbor.updater = cell.updater
			cell.updater <- neighbor
			cell.occupied, neighbor.occupied = false, true
			return true
		}
	}
	return false
}

func (cell *BoardCell) run() {
	id := 100 + cell.pos.x*BoardWidth + cell.pos.y
	var relocator int32 = 0
	var tracer []Trace
	if cell.trap {
		tracer = append(tracer, *traceCreator(cell.pos, time.Since(startTime), '#', id))
	}
	for req := range cell.reqCh {
		if closing {
			break
		}
		switch {
		case req.trapLeave:
			tracer = append(tracer, *traceCreator(cell.pos, time.Since(startTime), '#', id))
		case req.traveler == -1:
			if relocator == 0 {
				cell.updater = make(chan *BoardCell)
				if !cell.occupied {
					cell.occupied = true
					go ghost('1', outCh, req.time, cell, cell.pos, cell.updater)
					nrOfGhosts++
				}
				req.replyCh <- "true"
			} else {
				req.replyCh <- "false"
			}
		case req.reloq:
			if relocator == 0 {
				req.replyCh <- map[bool]string{true: "true", false: "false"}[!cell.occupied]
				if !cell.occupied {
					cell.occupied = true
				}
			} else {
				req.replyCh <- "false"
			}
		case !cell.occupied:
			if relocator == 0 {
				cell.occupied = true
				req.replyCh <- "true"
			} else {
				req.replyCh <- "false"
			}
		case cell.ghost:
			if relocator == 0 {
				atomic.AddInt32(&relocator, 1)
				go func() {
					var channel = req.replyCh
					if cell.tryRelocate() {
						channel <- "true"
					} else {
						channel <- "false"
					}
					atomic.AddInt32(&relocator, -1)
				}()
			} else {
				req.replyCh <- "false"
			}
		default:
			req.replyCh <- "false"
		}
	}
	outCh <- tracer
}

func releaseCell(cell *BoardCell) {
	busyLocks[cell.pos.x][cell.pos.y].Lock()
	busy[cell.pos.x][cell.pos.y] = false
	busyLocks[cell.pos.x][cell.pos.y].Unlock()

	cell.occupied = false
}

func ghost(symbol rune, outCh chan []Trace, d time.Duration, cell *BoardCell, pos Position, reloqChan chan *BoardCell) {
	cell.ghost = true
	id := ghostId
	ghostId++
	tracer := []Trace{*traceCreator(pos, time.Since(startTime), symbol, id)}
	terminator := time.After(d)
	for {
		select {
		case <-terminator:
			tracer = append(tracer, *traceCreator(Position{BoardWidth, BoardHeight}, time.Since(startTime), symbol, id))
			releaseCell(cell)
			cell.ghost = false
			outCh <- tracer
			return
		case newCell := <-reloqChan:
			pos, cell = newCell.pos, newCell
			tracer = append(tracer, *traceCreator(pos, time.Since(startTime), symbol, id))
			if newCell.trap {
				goto trapped
			}
		}
	}
trapped:
	tracer = append(tracer, *traceCreator(pos, time.Since(startTime), '*', id))
	time.Sleep(time.Duration(MaxDelay*1000) * time.Millisecond)
	tracer = append(tracer, *traceCreator(Position{BoardWidth, BoardHeight}, time.Since(startTime), symbol, id))
	outCh <- tracer
	releaseCell(cell)
	cell.ghost = false
}

func randomMove(pos Position, r *rand.Rand) Position {
	switch r.Intn(4) {
	case 0:
		return Position{pos.x, (pos.y + 1) % BoardHeight}
	case 1:
		return Position{pos.x, (pos.y - 1 + BoardHeight) % BoardHeight}
	case 2:
		return Position{(pos.x + 1) % BoardWidth, pos.y}
	default:
		return Position{(pos.x - 1 + BoardWidth) % BoardWidth, pos.y}
	}
}

func traceCreator(pos Position, t time.Duration, symbol rune, id int) *Trace {
	return &Trace{t, id, pos, symbol}
}

func traveler(id int, symbol rune, board [][]*BoardCell, outCh chan []Trace) {
	r := rand.New(rand.NewSource(time.Now().UnixNano() + int64(id)))
	steps := r.Intn(MaxSteps-MinSteps) + MinSteps
	pos := Position{r.Intn(BoardWidth), r.Intn(BoardHeight)}
	replyCh := make(chan string)
	var tracer []Trace

	busyLocks[pos.x][pos.y].Lock()
	if busy[pos.x][pos.y] {
		busyLocks[pos.x][pos.y].Unlock()
		outCh <- tracer
		atomic.AddInt32(&travelersRemained, -1)
		return
	}
	busy[pos.x][pos.y] = true
	busyLocks[pos.x][pos.y].Unlock()

	board[pos.x][pos.y].reqCh <- MoveRequest{to: pos, replyCh: replyCh, traveler: id}
	if <-replyCh != "true" {
		releaseCell(board[pos.x][pos.y])
		outCh <- tracer
		atomic.AddInt32(&travelersRemained, -1)
		return
	}

	timer := time.Now()
	for i := 0; i < steps; i++ {
		time.Sleep(time.Duration(MinDelay*1000+float64(r.Intn(int((MaxDelay-MinDelay)*1000)))) * time.Millisecond)
		newPos := randomMove(pos, r)

		busyLocks[newPos.x][newPos.y].Lock()
		if busy[newPos.x][newPos.y] {
			busyLocks[newPos.x][newPos.y].Unlock()
			continue
		}
		busy[newPos.x][newPos.y] = true
		busyLocks[newPos.x][newPos.y].Unlock()

		board[newPos.x][newPos.y].reqCh <- MoveRequest{from: pos, to: newPos, replyCh: replyCh, traveler: id}
		if <-replyCh == "true" {
			releaseCell(board[pos.x][pos.y])
			pos = newPos
			tracer = append(tracer, *traceCreator(pos, time.Since(startTime), symbol, id))
			if board[pos.x][pos.y].trap {
				tracer = append(tracer, *traceCreator(pos, time.Since(startTime), symbol+32, id))
				time.Sleep(time.Duration(MaxDelay*1000) * time.Millisecond)
				tracer = append(tracer, *traceCreator(Position{BoardWidth, BoardHeight}, time.Since(startTime), symbol, id))
				board[pos.x][pos.y].reqCh <- MoveRequest{trapLeave: true}
				releaseCell(board[pos.x][pos.y])
				break
			}
			timer = time.Now()
		} else {
			releaseCell(board[newPos.x][newPos.y])
		}
		if time.Since(timer) > time.Duration(MaxDelay*float64(time.Second)) {
			break
		}
	}

	releaseCell(board[pos.x][pos.y])
	outCh <- tracer
	atomic.AddInt32(&travelersRemained, -1)
}

func shutdownBoard() {
	for i := range board {
		for _, cell := range board[i] {
			close(cell.reqCh)
		}
	}
}

func printer(ch chan []Trace) {
	for i := 0; i < NrOfTravelers+nrOfGhosts; i++ {
		for _, t := range <-ch {
			fmt.Printf("%d %d %d %d %c\n", t.timeStamp.Milliseconds(), t.Id, t.pos.x, t.pos.y, t.symbol)

		}
	}
	closing = true
	shutdownBoard()
	for i := 0; i < BoardHeight*BoardWidth; i++ {
		for _, t := range <-ch {
			fmt.Printf("%d %d %d %d %c\n", t.timeStamp.Milliseconds(), t.Id, t.pos.x, t.pos.y, t.symbol)
		}
	}
}

func main() {
	fmt.Printf("-1 %d %d %d\n", NrOfTravelers, BoardWidth, BoardHeight)
	board = createBoard()
	for i := 0; i < NrOfTravelers; i++ {
		go traveler(i, rune('A'+i), board, outCh)
	}
	printer(outCh)
}
