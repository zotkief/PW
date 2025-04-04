package main

import (
	"fmt"
	"math"
	"math/rand"
	"strconv"
	"sync"
	"time"
)

const Nr_of_travelers = 15
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

func up(pos *Position) {
	pos.y = (pos.y + 1) % Board_Height
}
func down(pos *Position) {
	pos.y = (pos.y - 1 + Board_Height) % Board_Height
}
func left(pos *Position) {
	pos.x = (pos.x + 1) % Board_Width
}
func right(pos *Position) {
	pos.x = (pos.x - 1 + Board_Width) % Board_Width
}

func make_step(pos *Position, gen rand.Rand) {
	var n = int(math.Floor(gen.Float64() * 4.0))
	switch n {
	case 0:
		up(pos)
	case 1:
		down(pos)
	case 2:
		left(pos)
	case 3:
		right(pos)
	default:
		fmt.Println(" ?????????????? " + strconv.Itoa(n))
	}
}

func traveler(symbol rune, Id int, seed int64, wg *sync.WaitGroup, ch chan []Trace) {
	var gen = rand.New(rand.NewSource(seed))
	var pos Position
	var time_stamp = time.Since(start_time)
	var nr_of_steps = (gen.Int() % (Max_Steps - Min_Steps)) + Min_Steps
	pos.x = gen.Int() % Board_Width
	pos.y = gen.Int() % Board_Height

	tracer := []Trace{}

	//==================================

	for i := 0; i < nr_of_steps; i++ {
		time.Sleep(time.Duration(Min_Delay+(Max_Delay-Min_Delay)*gen.Float32()*1000) * time.Millisecond)
		make_step(&pos, *gen)
		tracer = append(tracer, *traceCreator(pos, time_stamp, symbol, Id))
		time_stamp = time.Since(start_time)
	}
	ch <- tracer
	defer wg.Done()
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
		go traveler(Symbol, i, int64(seeds[i]), &wg, ch)
		wg.Add(1)
		Symbol = Symbol + 1
	}
	go printer(ch, &wg)
	wg.Add(1)
	wg.Wait()
}
