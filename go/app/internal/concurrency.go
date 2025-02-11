// This is a test doc
//
//  func() f {
//  	var x = 1
//  }
//
// List:
//   1. abc
//   		def
//   2. def
package internal

import (
	"fmt"
	"math"
	"time"
	. "github.com/ynishiza/myapp/internal/utils"
)

func sleepSeconds[N ~float64](n N) {
	var m = time.Duration(math.Round(1000 * float64(n)))
	time.Sleep(m * time.Millisecond)
}

func TestGoroutineSimple() {
	PrintBanner("TestGoroutine")
	var c = make(chan string)
	go func() {
		Logln("Routine start")
		sleepSeconds(2.0)
		Logln("Routine done")
		c <- "done"
		Logln(cap(c), len(c))
	}()

	var res string
	Logln(cap(c), len(c))
	res = <-c
	Logln(res, cap(c), len(c))
}

func TestLocks() {
	PrintBanner("TestLocks")
	type Lock struct{ name string }
	var locks = make(chan Lock, 3)
	locks <- Lock{"a"}
	locks <- Lock{"b"}
	locks <- Lock{"c"}
	var use = func(x int) {
		Logln("waiting for lock", x)
		l := <-locks
		defer func() { locks <- l }()
		Logln("start lock", x, l)
		sleepSeconds(1.0)
		Logln("done", x)
	}

	for i := range 10 {
		go use(i)
	}

	sleepSeconds(0.1)
	v := <-locks
	fmt.Println("All done", v)
}

func TestChannelSelect() {
	var (
		c1     chan int = make(chan int, 1)
		c2              = make(chan int, 1)
		c3              = make(chan int, 1)
		status          = make(chan bool, 1)
	)

	var count = 0
	c1 <- 0
	status <- false
	var done = false

	for !done {
		select {
		case t := <-c1:
			go func() {
				fmt.Printf("c1:%d\n", t)
				sleepSeconds(0.3)
				c2 <- t + 1
				Logln(cap(c2), len(c2))
			}()
		case t := <-c2:
			go func() {
				fmt.Printf("c2:%d\n", t)
				sleepSeconds(0.3)
				c3 <- t + 1
			}()
		case t := <-c3:
			go func() {
				fmt.Printf("c3:%d\n", t)
				c1 <- t + 1
				status <- t >= 10
			}()
		case done = <-status:
			fmt.Printf("status:%t", done)
		default:
			count++
			if count > 40 {
				panic("FATAL. Taking too long.")
			}

			sleepSeconds(0.5)
			if done {
				Logln("default: done")
			} else {
				Logln("default: not done")
			}
		}
	}
}
