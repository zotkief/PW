package main

import (
	"fmt"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

const (
	NrOfProcesses = 15
	MinSteps      = 50
	MaxSteps      = 100
	MinDelay      = 10 * time.Millisecond
	MaxDelay      = 50 * time.Millisecond
)

type ProcessState int

const (
	LocalSection ProcessState = iota
	EntryProtocol
	CriticalSection
	ExitProtocol
)

var (
	BoardWidth  = NrOfProcesses
	BoardHeight = int(ExitProtocol) + 1
	StartTime   = time.Now()
	wg          sync.WaitGroup
	printCh     = make(chan TracesSequenceType, NrOfProcesses)
	Choosing    [NrOfProcesses]atomic.Bool
	Number      [NrOfProcesses]atomic.Int32
	maxLabel    atomic.Int32
)

type Position struct {
	X, Y int
}

type Trace struct {
	TimeStamp time.Duration
	ID        int
	Position  Position
	Symbol    rune
}

type TracesSequenceType struct {
	Last       int
	TraceArray []Trace
}

type Process struct {
	ID       int
	Symbol   rune
	Position Position
}

func storeTrace(traces *TracesSequenceType, trace Trace) {
	traces.Last++
	traces.TraceArray = append(traces.TraceArray, trace)
}

func changeState(state ProcessState, p *Process, traces *TracesSequenceType) {
	p.Position.Y = int(state)
	timestamp := time.Since(StartTime)
	storeTrace(traces, Trace{
		TimeStamp: timestamp,
		ID:        p.ID,
		Position:  p.Position,
		Symbol:    p.Symbol,
	})
}

func delayRand(r *rand.Rand) {
	d := MinDelay + time.Duration(r.Intn(int(MaxDelay-MinDelay)))
	time.Sleep(d)
}

func lock(i int) int {
	var maxNum int32 = 0
	Choosing[i].Store(true)
	for j := 0; j < len(Number); j++ {
		maxNum = max(maxNum, Number[j].Load())
	}

	Number[i].Store(int32(maxNum + 1))
	Choosing[i].Store(false)

	for j := 0; j < len(Choosing); j++ {
		if j != i {
			for Choosing[j].Load() {
			}
			for Number[j].Load() != 0 &&
				(Number[j].Load() < Number[i].Load() || (Number[j].Load() == Number[i].Load() && j < i)) {
			}
		}
	}
	return int(Number[i].Load())
}

func unlock(i int) {
	Number[i].Store(0)
}

func process(id int, seed int64, symbol rune) {
	var maxTicket = 0
	defer wg.Done()

	r := rand.New(rand.NewSource(seed))
	p := Process{
		ID:       id,
		Symbol:   symbol,
		Position: Position{X: id, Y: int(LocalSection)},
	}

	traces := TracesSequenceType{
		Last:       -1,
		TraceArray: make([]Trace, 0, MaxSteps),
	}
	timestamp := time.Since(StartTime)
	storeTrace(&traces, Trace{
		TimeStamp: timestamp,
		ID:        id,
		Position:  p.Position,
		Symbol:    symbol,
	})

	steps := MinSteps + r.Intn(MaxSteps-MinSteps)

	for i := 0; i < steps; i++ {
		delayRand(r)

		changeState(EntryProtocol, &p, &traces)

		var local = lock(id)
		if local > maxTicket {
			maxTicket = local
		}

		changeState(CriticalSection, &p, &traces)
		delayRand(r)

		changeState(ExitProtocol, &p, &traces)

		unlock(id)

		changeState(LocalSection, &p, &traces)
	}
	if maxLabel.Load() < int32(maxTicket) {
		maxLabel.Store(int32(maxTicket))
	}
	printCh <- traces
}

func printer() {
	defer wg.Done()
	for i := 0; i < NrOfProcesses; i++ {
		traces := <-printCh
		printTraces(traces)
	}
	close(printCh)

	fmt.Printf("-1 %d %d %d ", NrOfProcesses, BoardWidth, BoardHeight)
	for _, s := range []string{"LocalSection", "EntryProtocol", "CriticalSection", "ExitProtocol"} {
		fmt.Print(s + ";")
	}
	fmt.Printf("maxTicket=%d;", maxLabel.Load())
}

func printTraces(traces TracesSequenceType) {
	for _, t := range traces.TraceArray {
		fmt.Printf("%v %d %d %d %c\n",
			t.TimeStamp.Seconds(), t.ID, t.Position.X, t.Position.Y, t.Symbol)
	}
}

func main() {
	maxLabel.Store(0)
	go printer()
	wg.Add(1)

	symbol := 'A'
	for i := 0; i < NrOfProcesses; i++ {
		wg.Add(1)
		go process(i, time.Now().UnixNano()+int64(i), symbol)
		symbol++
	}

	wg.Wait()
}
