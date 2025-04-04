package main

import (
	"fmt"
	"math"
	"math/rand"
	"strconv"
	"sync"
	"time"
)

const Nr_of_travelers = 20
const Min_Steps = 10
const Max_Steps = 100
const Min_Delay = 0.01
const Max_Delay = 0.05
const Board_Height = 15
const Board_Width = 15

const seedLimit = 1000000

var start_time = time.Now()
var seeds = [Nr_of_travelers]uint32{}

type Position struct {
	x int
	y int
}

type Trace struct {
	timeStamp time.Duration
	Id        int
	pos       Position
	symbol    rune
}

func traceCreator(position Position, time time.Duration, symbol rune, id int) *Trace {
	p := new(Trace)
	p.pos = position
	p.symbol = symbol
	p.timeStamp = time
	p.Id = id
	return p
}

var Symbol = 'A'

func up(pos *Position) Position {
	var retPos Position
	retPos.x = pos.x
	retPos.y = (pos.y + 1) % Board_Height
	return retPos
}
func down(pos *Position) Position {
	var retPos Position
	retPos.x = pos.x
	retPos.y = (pos.y - 1 + Board_Height) % Board_Height
	return retPos
}
func left(pos *Position) Position {
	var retPos Position
	retPos.x = (pos.x + 1) % Board_Width
	retPos.y = pos.y
	return retPos
}
func right(pos *Position) Position {
	var retPos Position
	retPos.x = (pos.x - 1 + Board_Width) % Board_Width
	retPos.y = pos.y
	return retPos
}

func make_step(pos *Position, gen rand.Rand, mu *[][]sync.Mutex, time_stamp *time.Duration, tracer *[]Trace, symbol rune, Id int) bool {
	var n = int(math.Floor(gen.Float64() * 4.0))
	var tempPos Position
	switch n {
	case 0:
		tempPos = up(pos)
	case 1:
		tempPos = down(pos)
	case 2:
		tempPos = left(pos)
	case 3:
		tempPos = right(pos)
	default:
		fmt.Println(" ?????????????? " + strconv.Itoa(n))
	}
	var delay = 2 * Max_Delay * 1000

	terminator := make(chan string)
	continuer := make(chan string)

	go func() {
		time.Sleep(time.Duration(delay) * time.Millisecond)
		terminator <- "end"
	}()
	go func() {
		(*mu)[tempPos.x][tempPos.y].Lock()
		continuer <- "con"
	}()
	select {
	case <-terminator:
		(*mu)[pos.x][pos.y].Unlock()
		return false
	case <-continuer:
		(*mu)[pos.x][pos.y].Unlock()
		*time_stamp = time.Since(start_time)
		*tracer = append(*tracer, *traceCreator(*pos, *time_stamp, symbol, Id))
	}

	pos.x = tempPos.x
	pos.y = tempPos.y
	return true
}

func traveler(symbol rune, Id int, seed int64, wg *sync.WaitGroup, ch chan []Trace, mu *[][]sync.Mutex) {
	defer wg.Done()
	var gen = rand.New(rand.NewSource(seed))
	var pos Position
	var time_stamp = time.Since(start_time)
	var nr_of_steps = (gen.Int() % (Max_Steps - Min_Steps)) + Min_Steps
	pos.x = gen.Int() % Board_Width
	pos.y = gen.Int() % Board_Height
	tracer := []Trace{}

	terminator := make(chan string)
	continuer := make(chan string)
	go func() {
		var delay = 2 * Max_Delay
		time.Sleep(time.Duration(delay) * time.Second)
		terminator <- "end"
	}()
	go func() {
		(*mu)[pos.x][pos.y].Lock()
		continuer <- "con"
	}()
	select {
	case <-terminator:
		symbol = symbol + 32
		tracer = append(tracer, *traceCreator(pos, time_stamp, symbol, Id))
		ch <- tracer
		return
	case <-continuer:
	}

	//==================================

	for i := 0; i < nr_of_steps; i++ {
		var milis = Min_Delay + (Max_Delay-Min_Delay)*gen.Float32()*1000
		time.Sleep(time.Duration(milis) * time.Millisecond)
		var state bool = make_step(&pos, *gen, mu, &time_stamp, &tracer, symbol, Id)
		if !state {
			symbol = symbol + 32
			tracer = append(tracer, *traceCreator(pos, time_stamp, symbol, Id))
			break
		}
	}
	ch <- tracer
}

func printer(ch chan []Trace, wg *sync.WaitGroup) {
	for i := 0; i < Nr_of_travelers; i++ {
		traces := <-ch
		for j := 0; j < len(traces); j++ {
			trace := traces[j]
			fmt.Print(trace.timeStamp, " ",
				trace.Id, " ",
				trace.pos.x, " ",
				trace.pos.y)
			fmt.Printf(" %c\n", trace.symbol)
		}
	}
	wg.Done()
}

func main() {
	//mutexes
	mutexes := make([][]sync.Mutex, Board_Width)
	for i := range mutexes {
		mutexes[i] = make([]sync.Mutex, Board_Height)
	}
	//mutexes
	for i := 0; i < Nr_of_travelers; i++ {
		seeds[i] = uint32(seedLimit * rand.Float64())
	}
	fmt.Println("-1 " +
		strconv.Itoa(Nr_of_travelers) + " " +
		strconv.Itoa(Board_Width) + " " +
		strconv.Itoa(Board_Height))
	ch := make(chan []Trace)
	var wg sync.WaitGroup
	for i := 0; i < Nr_of_travelers; i++ {
		go traveler(Symbol, i, int64(seeds[i]), &wg, ch, &mutexes)
		wg.Add(1)
		Symbol = Symbol + 1
	}
	go printer(ch, &wg)
	wg.Add(1)
	wg.Wait()
}
