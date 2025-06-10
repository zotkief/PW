package main

import (
	"fmt"
	"math/rand"
	"sync"
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
	EntryProtocol_1
	EntryProtocol_2
	EntryProtocol_3
	EntryProtocol_4
	CriticalSection
	ExitProtocol
)

var (
	BoardWidth  = NrOfProcesses
	BoardHeight = int(ExitProtocol) + 1
	StartTime   = time.Now()
	wg          sync.WaitGroup
	printCh     = make(chan TracesSequenceType, NrOfProcesses)
)
var (
	flag      [NrOfProcesses]int
	flagMutex sync.Mutex
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
func allIn(states []int, allowed map[int]bool) bool {
	for _, state := range states {
		if !allowed[state] {
			return false
		}
	}
	return true
}

func anyIn(states []int, wanted int) bool {
	for _, state := range states {
		if state == wanted {
			return true
		}
	}
	return false
}

func snapshotFlags() []int {
	flagMutex.Lock()
	defer flagMutex.Unlock()
	snap := make([]int, NrOfProcesses)
	copy(snap, flag[:])
	return snap
}

func setFlag(id, value int) {
	flagMutex.Lock()
	flag[id] = value
	flagMutex.Unlock()
}

func delayRand(r *rand.Rand) {
	d := MinDelay + time.Duration(r.Intn(int(MaxDelay-MinDelay)))
	time.Sleep(d)
}

func process(id int, seed int64, symbol rune) {
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
		fmt.Println(i, " ", steps)
		delayRand(r)
		//fmt.Println("1")
		// Entry Protocol – Etap 1
		setFlag(p.ID, 1)
		changeState(EntryProtocol_1, &p, &traces)
		//fmt.Println("2")

		// Entry Protocol – Etap 2
		for {
			//fmt.Println("3")
			snap := snapshotFlags()
			//fmt.Println("snapshot:", snap)

			if allIn(snap, map[int]bool{0: true, 1: true, 2: true}) {
				break
			}
			time.Sleep(1 * time.Millisecond)
		}
		//fmt.Println("4")
		setFlag(p.ID, 3)
		changeState(EntryProtocol_2, &p, &traces)

		// Entry Protocol – Etap 3
		snap := snapshotFlags()
		//fmt.Println("5")
		waiting := false
		for j, f := range snap {
			if j != p.ID && f == 1 {
				waiting = true
				break
			}
		}
		if waiting {
			//fmt.Println("6")
			//fmt.Println("snapshot:", snap)
			setFlag(p.ID, 2)
			changeState(EntryProtocol_3, &p, &traces)

			for {
				snap = snapshotFlags()
				if anyIn(snap, 4) {
					break
				}
				time.Sleep(1 * time.Millisecond)
			}
			//fmt.Println("7")
		}
		setFlag(p.ID, 4)
		changeState(EntryProtocol_4, &p, &traces)

		for {
			//fmt.Println("8")
			snap = snapshotFlags()
			//fmt.Println("snapshot:", snap)
			ok := true
			for j := 0; j < p.ID; j++ {
				if snap[j] != 0 && snap[j] != 1 {
					ok = false
					break
				}
			}
			if ok {
				break
			}
			time.Sleep(1 * time.Millisecond)
			//fmt.Println("9")
		}

		// Critical section
		changeState(CriticalSection, &p, &traces)
		delayRand(r)

		// Exit Protocol
		changeState(ExitProtocol, &p, &traces)

		for {
			//fmt.Println("10")
			snap = snapshotFlags()
			ok := true
			for j := p.ID + 1; j < NrOfProcesses; j++ {
				if snap[j] != 0 && snap[j] != 1 && snap[j] != 4 {
					ok = false
					break
				}
			}
			if ok {
				break
			}
			time.Sleep(1 * time.Millisecond)
		}
		//fmt.Println("11")

		setFlag(p.ID, 0)

		// Local section
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
	for _, s := range []string{"LocalSection", "EntryProtocol_1", "EntryProtocol_2", "EntryProtocol_3", "EntryProtocol_4", "CriticalSection", "ExitProtocol"} {
		fmt.Print(s + ";")
	}
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
