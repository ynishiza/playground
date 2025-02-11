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

func TestString() {
	PrintBanner("TestString")
	var v = "abc"[0]
	fmt.Println(
		v,
		"abc"[0],
		"abc"[2],
	)
}

func TestScratch() {
	var list []int
	fmt.Println(len(list))
}

func TestLiteral() {
	fmt.Println("rune", 'a')
	fmt.Println("\u3042\nい")
	fmt.Println("escape: \\n")
	fmt.Println(`\u3042\n
	い`)
}

func TestSlice() {
	PrintBanner("TestSlice")
	var a [5]int = [5]int{1, 2, 3, 4, 5}
	var b []int = a[0:3]
	var c = b[0:2:2]
	fmt.Println(
		a, b, c, cap(b), cap(c),
	)
	c[0] = 100
	fmt.Println(
		a, b, c,
	)

	var s = "abcd"
	var s2 = s[0:2]
	var s3 = s[:2]
	fmt.Println(s, s2, s3)
}

func TestMap() {
	var d = make(map[string]int)
	d["a"] = 1
	d["b"] = 2
	delete(d, "c")
	fmt.Println(d, len(d))
	fmt.Println(d["c"])
}

func TestZeroValue() {
	type IF interface{}
	PrintBanner("TestZeroValue")
	var z1 [8]int
	var z2 []int = nil
	var z3 IF
	fmt.Println(
		z1, z2, nil, z3,
	)
}

func TestNil() {
	PrintBanner("TestNil")
	var uninitInt int
	var uninitIntPtr *int
	var newIntPtr *int = new(int)

	var uninitIntSlice []int
	var emptyIntSlice []int = []int{}
	var uninitIntSlicePtr *[]int
	var newIntSlicePtr *[]int = new([]int)
	var makeIntSlice []int = make([]int, 5)

	var uninitMap map[string]int
	var emptyMap map[string]int = map[string]int{}
	var newMap = new(map[string]int)

	// uninitMap["a"] = 1
	emptyMap["a"] = 1
	// (*newMap)["a"] = 1
	fmt.Println(
		"uninitialized int:", &uninitInt, uninitInt,
		"\nuninitialized *int", uninitIntPtr,
		// *uninitIntPtr,
		"\nnew int:", newIntPtr, *newIntPtr,
		"\nunint []int:", &uninitIntSlice, uninitIntSlice, &uninitIntSlice == nil, uninitIntSlice == nil, append(uninitIntSlice, 1),
		"\nempty []int:", &emptyIntSlice, emptyIntSlice, &emptyIntSlice == nil, emptyIntSlice == nil, append(emptyIntSlice, 1),
		"\nuninitialized pointer []int:", uninitIntSlicePtr,
		// *uninitIntSlicePtr,
		// *uninitializedPointer,
		"\nnew []int:", newIntSlicePtr, *newIntSlicePtr, newIntSlicePtr == nil, *newIntSlicePtr == nil, append(*newIntSlicePtr, 1),
		"\nmake []int:", &makeIntSlice, makeIntSlice,
		"\nuninitialized map", &uninitMap, uninitMap, uninitMap["a"],
		"\nemptyp map", &emptyMap, emptyMap, (*&emptyMap)["a"],
		"\nnew map", newMap, *newMap, (*newMap)["a"],
	)
}

type Obj struct{ x int }
type ObjInterface interface{ DoSomething() }

func (o *Obj) DoSomething() {
	o.x *= 11
}
func TestMutation() {
	// Objects: pass by value
	var o = Obj{1}
	var oCopy = o
	var oPtr = &o
	var f = func(o Obj) {
		o.x += 10
	}
	var f2 = func(o *Obj) {
		o.x += 10
	}
	var f3 = func(o ObjInterface) {
		o.DoSomething()
	}
	var check = func() {
		fmt.Println(o, oCopy, *oPtr)
	}
	o.x += 10
	oCopy.x += 1000
	oPtr.x += 30
	check()
	f(o)
	check()
	f2(&o)
	check()
	f3(&o)
	check()

	// Arrays: pass by value
	var a1 [2]int
	var a2 = a1
	var a3 = &a1
	var checkArray = func() {
		fmt.Println(a1, a2, *a3)
	}
	checkArray()
	a1[0] = 2
	checkArray()

	// Slices: pass by value
	var sl []int = []int{1, 2, 3}
	var sl2 = sl
	var sl3 = sl[:]
	sl[0] += 100
	var g = func(x []int) {
		x[0] += 123
	}
	var g2 = func(x *[]int) {
		(*x)[0] += 20
	}
	var checkSlice = func() {
		fmt.Println(sl, sl2, sl3)
	}

	checkSlice()
	g(sl)
	checkSlice()
	g2(&sl)
	checkSlice()
}

// Recursive
type ARec struct {
	// ARec
	// l [10]ARec
	*ARec
}

func TestEmbedded() {
	// Embedded
	type MyTypeA struct {
		int
		string
		*bool
		rune
		x, y int
	}
	type MyTypeB struct{ MyTypeA }

	var b = true
	fmt.Println("MyTypeB", MyTypeB{})
	fmt.Println("MyTypeB", MyTypeB{MyTypeA{int: 10, string: "abc", bool: &b, x: 1, y: 2}})
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

func (UUU) fn3() {}

func TestError() {
	PrintBanner("TestError")
	var f = func() (int, error) {
		return 0, MyError{"OOPS"}
	}

	if _, e := f(); e != nil {
		fmt.Printf("error:%s", e.Error())
	}
	_, ee := f()
	if e, ok := ee.(MyError); ok {
		fmt.Printf("error:%s", e.name)
	}

	switch _, e := f(); m := e.(type) {
	case MyError:
		fmt.Printf("MyError:%s", m.name)
	default:
		panic("Unknown")
	}
}

type MyError struct {
	name string
}

func (m MyError) Error() string {
	return fmt.Sprintf("name:%s", m.name)
}
