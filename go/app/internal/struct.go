package internal

import (
	"fmt"
	. "github.com/ynishiza/myapp/internal/utils"
)

func TestStruct() {
	// var c struct { x int; y string } = struct { x int; y string } { x:1, y:"" }
	var a struct {
		x int
		y string
	} = struct {
		x int
		y string
	}{
		x: 1,
		y: "a",
	}

	var b struct {
		x int
		y string
	} = struct {
		x int
		y string
	}{x: 1, y: "a"}
	// embedded
	b = struct {
		x int
		y string
	}{2, ""}
	// empty
	b = struct {
		x int
		y string
	}{}
	// partial
	b = struct {
		x int
		y string
	}{x: 2}
	var c struct {
		x int
		y string
	}

	var embedded = struct {
		int
		string
	}{1, "a"}
	fmt.Println(a, b, c, embedded)
}

func TestEmbedded2() {
	PrintBanner("TestEmbedded2")
	var y = YYY{XXX: XXX{1}}
	y.x = 100
	print(y.x)
	print(y.XXX.x)
	y.f()
	y.XXX.f()
	y.g()
	fmt.Print(y)

	var y2 = YYY2{new(XXX)}
	y2.x = 2000
	fmt.Print(y2)
	y2.f()
	fmt.Println(y2, y2.x)
	y2.x = 3000
	y2.g()
	fmt.Println(y2, y2.x)
}

type XXX struct{ x int }
type YYY struct{ XXX }
type YYY2 struct{ *XXX }

func (x XXX) f()  { fmt.Println("A!", x); x.x = 23903 }
func (x *XXX) g() { fmt.Println("B!", x); x.x = 123 }

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

type Obj struct{ x int }
type ObjInterface interface{ DoSomething() }

func (o *Obj) DoSomething() {
	o.x *= 11
}
