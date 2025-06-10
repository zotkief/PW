package main

import (
	"fmt"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

const (
	NrOfProcesses = 2
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
	Number      atomic.Int32
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

func process(id int, seed int64, symbol rune) {
	var other = (id + 1) % 2
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

		Choosing[id].Store(true)
		Number.Store(int32(other))
		for Choosing[other].Load() && (Number.Load() == int32(other)) {
		}

		changeState(CriticalSection, &p, &traces)
		delayRand(r)

		Choosing[id].Store(false)

		changeState(ExitProtocol, &p, &traces)

		changeState(LocalSection, &p, &traces)
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
	fmt.Println("EXTRA_LABEL;")
}

func printTraces(traces TracesSequenceType) {
	for _, t := range traces.TraceArray {
		fmt.Printf("%v %d %d %d %c\n",
			t.TimeStamp.Seconds(), t.ID, t.Position.X, t.Position.Y, t.Symbol)
	}
}

func main() {
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
