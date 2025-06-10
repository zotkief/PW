package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

const (
	Readers       = 10
	Writers       = 5
	NrOfProcesses = Readers + Writers
	MinSteps      = 20
	MaxSteps      = 50
	MinDelay      = 10 * time.Millisecond
	MaxDelay      = 50 * time.Millisecond
)

type ProcessState int

const (
	LocalSection ProcessState = iota
	Start
	CriticalSection
	Stop
)

var (
	BoardWidth  = NrOfProcesses
	BoardHeight = int(Stop) + 1
	StartTime   = time.Now()
	wg          sync.WaitGroup
	printCh     = make(chan TracesSequenceType, NrOfProcesses)
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

//Monitor

type ReaderWriterMonitor struct {
	mutex          sync.Mutex
	okToRead       *sync.Cond
	okToWrite      *sync.Cond
	readers        int
	writers        int
	waitingWriters int
	queue          []rune
}

func NewReaderWriterMonitor() *ReaderWriterMonitor {
	m := &ReaderWriterMonitor{}
	m.okToRead = sync.NewCond(&m.mutex)
	m.okToWrite = sync.NewCond(&m.mutex)
	m.queue = make([]rune, 0)
	return m
}

func (m *ReaderWriterMonitor) ReaderEnter(p *Process, traces *TracesSequenceType) {
	m.mutex.Lock()
	m.queue = append(m.queue, 'R')
	for m.writers > 0 || m.firstInQueue() != 'R' {
		m.okToRead.Wait()
	}
	m.readers++
	changeState(CriticalSection, p, traces)
	m.mutex.Unlock()
}

func (m *ReaderWriterMonitor) ReaderLeave(p *Process, traces *TracesSequenceType) {
	m.mutex.Lock()
	m.readers--
	m.removeFirst('R')
	changeState(Stop, p, traces)
	if m.readers == 0 {
		m.okToWrite.Signal()
	}
	m.okToRead.Broadcast()
	m.mutex.Unlock()
}

func (m *ReaderWriterMonitor) WriterEnter(p *Process, traces *TracesSequenceType) {
	m.mutex.Lock()
	m.queue = append(m.queue, 'W')
	m.waitingWriters++
	for m.readers > 0 || m.writers > 0 || m.firstInQueue() != 'W' {
		m.okToWrite.Wait()
	}
	m.removeFirst('W')
	m.waitingWriters--
	m.writers++
	changeState(CriticalSection, p, traces)
	m.mutex.Unlock()
}

func (m *ReaderWriterMonitor) WriterLeave(p *Process, traces *TracesSequenceType) {
	m.mutex.Lock()
	m.writers--
	changeState(Stop, p, traces)
	m.okToRead.Broadcast()
	m.okToWrite.Signal()
	m.mutex.Unlock()
}

func (m *ReaderWriterMonitor) firstInQueue() rune {
	if len(m.queue) == 0 {
		return 0
	}
	return m.queue[0]
}

func (m *ReaderWriterMonitor) removeFirst(kind rune) {
	for i, k := range m.queue {
		if k == kind {
			m.queue = append(m.queue[:i], m.queue[i+1:]...)
			break
		}
	}
}

//Main

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

func process(id int, symbol rune, monitor *ReaderWriterMonitor) {
	defer wg.Done()
	r := rand.New(rand.NewSource(time.Now().UnixNano() + int64(id)))

	p := Process{ID: id, Symbol: symbol, Position: Position{X: id, Y: int(LocalSection)}}
	traces := TracesSequenceType{Last: -1, TraceArray: make([]Trace, 0, MaxSteps)}

	changeState(LocalSection, &p, &traces)
	steps := MinSteps + r.Intn(MaxSteps-MinSteps)

	for i := 0; i < steps; i++ {
		delayRand(r)

		changeState(Start, &p, &traces)
		if symbol == 'R' {
			monitor.ReaderEnter(&p, &traces)
		} else {
			monitor.WriterEnter(&p, &traces)
		}

		delayRand(r)

		if symbol == 'R' {
			monitor.ReaderLeave(&p, &traces)
		} else {
			monitor.WriterLeave(&p, &traces)
		}

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
	for _, s := range []string{"LocalSection", "Entry", "CriticalSection", "Exit"} {
		fmt.Print(s + ";")
	}
	fmt.Println()
}

func printTraces(traces TracesSequenceType) {
	for _, t := range traces.TraceArray {
		fmt.Printf("%.10f %d %d %d %c\n",
			t.TimeStamp.Seconds(), t.ID, t.Position.X, t.Position.Y, t.Symbol)
	}
}

func main() {
	go printer()
	wg.Add(1)

	monitor := NewReaderWriterMonitor()

	for i := 0; i < NrOfProcesses; i++ {
		wg.Add(1)
		var symbol rune
		if i < Readers {
			symbol = 'R'
		} else {
			symbol = 'W'
		}
		go process(i, symbol, monitor)
	}

	wg.Wait()
}
