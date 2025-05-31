package internal

import (
	"fmt"
	. "github.com/ynishiza/myapp/internal/utils"
	"golang.org/x/mod/semver"
	"log"
)

func TestBasics() {
	var x int = 1
	var w = uint(1)
	fmt.Println(x, w)
	var xx bool = true
	fmt.Println(xx)

	// multiple
	var (
		aa      = 1
		bb uint = 2
	)
	cc, dd := 1, 2
	var aaa, aaaa int = 1, 2
	fmt.Println(aa, aaa, aaaa, bb, cc, dd)

}

func TestScratch() {
	var list []int
	fmt.Println(len(list))
}

func TestSemver() {
	var m = semver.Major("v112345.1")
	fmt.Println("Hello" + m)
}

func TestCompositeLiteral() {
	PrintBanner("TestCompositeLiteral")
	type A struct{ x int }
	type B struct{ A }
	var (
		a  = [2]int{1, 2}
		a1 = [2]int{}
		// a [2]int = {1, 2}
		aa  = [...]int{1, 2}
		aaa = struct{ x [2]int }{[2]int{1, 2}}
		// aaa = struct { x [2]int } { {1, 2 } }

		b = [][]int{{1}}

		bb   = B{A{1}}
		bbb  = []A{{1}}
		bbb2 = []B{{A{1}}}
		bbb3 = map[string]A{"a": {1}}
		bbb4 = map[A]string{{1}: "a"}
	)

	a[0] = 2
	a[1] = 2

	fmt.Println(
		a, a1, aa, aaa, b, bb, bbb, bbb2, bbb3, bbb4,
	)
}

func TestStatements() {
	// panic(*new(error))
	var f = func(x int) {
		defer func() { fmt.Println(x) }()
		defer func() { fmt.Println(x) }()
		if x == 2 {
			return
		}
		x += 10
		return
	}
	f(1)
	f(2)

	// loop
	for i, v := range [10]int{1, 2, 3} {
		fmt.Println(i, v)
	}
	for i, v := range "abc" {
		fmt.Println(i, v)
	}
	var fib = func(maxValue int) int {
		var yield = func(v int) bool { return v < maxValue }
		var current, next = 0, 1
		for yield(next) {
			var prev = current
			current = next
			next += prev
		}
		return current
	}
	fmt.Println(fib(10))
	fmt.Println(fib(20))
	fmt.Println(fib(30))
}

type C1 int

func (c C1) A() {}
func (c C1) B() {}
func (c C1) DoSomething() {
	fmt.Println("this is C1", c)
	log.Println("test")
}

