package internal

import (
	"fmt"
	. "github.com/ynishiza/myapp/internal/utils"
)

func TestTypeIdentity() {
	type MyString = string

	// var x MyString = string("a")
	var x MyString = string("a")
	var y string = MyString("a")
	var z struct {
		x int `b:"a"`
	} = struct {
		x int `b:"a"`
	}{1}

	// var xx MyPoint = MyPoint2 { 1, 2 }
	fmt.Print(x, y, z)
}

func TestType() {
	PrintBanner("TestType")
	type ABC struct {
		x int
		y string
	}
	type A = string
	type B string
	type C struct{}
	type D = struct{}

	fmt.Println(ABC{})
	fmt.Println(ABC{x: 10})
	fmt.Println(ABC{x: 10, y: "abc"})
	fmt.Println(ABC{
		x: 10,
		y: "abc",
	})
}

func TestEnum() {
	PrintBanner("TestEnum")
	const (
		d = iota
		e
		f
		g
	)

	const (
		s = iota * 3
		t
		u
	)

	const a, b = 1, d
	fmt.Println(d, e, f, g)

	const x = d
	switch x {
	case a:
		fmt.Printf("a")
	default:
		fmt.Printf("default")
	}

	fmt.Println(s, t, u)
}

func TestConversion() {
	type MyString string
	type MyPoint struct{ x, y int }
	type MyPoint2 struct{ x, y int }

	var x any = 1
	fmt.Println(
		x,
		// int(true),
		// int("a"),
		// int(1.1),

		// string(true),
		// string(1),
		// string(1.1),

		// bool("true"),
		// bool(1),
		// bool(1.1),

		// float32(true),
		// float32("a"),
		float32(1),

		MyString(string("a")),
		MyPoint(struct{ x, y int }{1, 2}),
	)
}

func TestFunctionType() {
	PrintBanner("TestFunctionType")
	var a func(x int)
	a = func(x int) {
		fmt.Println(x)
	}
	a(1)

	// rest args
	var b = func(x ...int) {
		x[0] = 2
		fmt.Println(x[0])
		fmt.Println(x[1])
		fmt.Println(x[1])
		fmt.Println(x)
		fmt.Println(len(x))
	}
	b(1, 2, 3)

	// labeled
	var fn = func() (success bool, value int) {
		success = true
		return
	}
	success, v := fn()
	fmt.Println(success, v)

	var fn2 = func(_ int) {
	}
	fn2(1)

	// higher order
	var h = func(x int) func() int {
		return func() int { return x }
	}
	h(1)()

	var h2 = func(f func() int) func() int {
		return f
	}
	h2(func() int { return 1 })()

	// nested
	var inner = func() (int, string, string) {
		return 1, "a", "b"
	}
	var outer = func(x int, y, z string) {
		fmt.Println(x, y, z)
	}
	outer(inner())
}

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

func TestGeneric() {
	PrintBanner("TestGeneric")
	type MyType[T ~int] struct {
		x T
	}

	var x = MyType[int]{1}
	fmt.Println(x)
}

func TestInterface() {
	PrintBanner("TestInterface")
	type A interface{}
	type B interface {
		A
		int
	}

	type C interface {
		DoSomething()
	}

	var ccc = func(c C) {
		c.DoSomething()
	}
	var c2 C = C1(3)

	ccc(C1(1))
	ccc(c2)

	// Embedded
	type A1 interface {
		A()
		DoSomething()
	}
	type B1 interface {
		B()
		DoSomething()
	}
	type C2 interface {
		A1
		B1
		DoSomething()
	}

	var c3 C2 = C1(4)
	c3.DoSomething()

	type I1 int
	type I2 = int
	type UI = uint
	type UI2 uint
	type II interface {
		~int | uint
	}
	type MyType[T II] struct{ value T }
	fmt.Println(MyType[I1]{-2})
	fmt.Println(MyType[I2]{1})
	fmt.Println(MyType[UI]{3})
	fmt.Println(MyType[int]{4})
	fmt.Println(MyType[uint]{5})
	// fmt.Println(I3[UI2]{3})

	type R interface {
		~int
		I1
		// ~I1
		I2
		~I2
		I1 | I2
		[10]int
		*int
		~int | ~float64
		int
		float32
	}
}

func TestArraySliceMap() {
	PrintBanner("TestArraySliceMap")
	var a = [...]int{}
	var b = []int{1, 2}
	fmt.Println(
		a,
		b,
		[2]int{},
		[...]int{1, 2},
		[...]int{},
		[]int{},
	)

	var s = make([]int, 2, 10)
	s1 := append(s, 1, 2)
	s1[0] = 1
	fmt.Println(s, s1)
	s1 = make([]int, 10)
	copy(s1, s[:])
	s1[0] = 100
	fmt.Println(s, s1)
	clear(s1)
	fmt.Println(s, s1)

	var m = make(map[string]int)
	m["a"] = 1
	if x, ok := m["a"]; ok {
		Logln(x)
	} else {
		Logln("no a")
	}
	if x, ok := m["b"]; ok {
		Logln(x)
	} else {
		Logln("no b")
	}
}

func TestTypeAssertion() {
	PrintBanner("TestTypeAssertion")
	type MI interface{}
	type M2 interface{ t() }
	var m MI = "abc"
	fmt.Println(
		m.(string),
		// m.(int), ERROR
		m.(MI),
		// m.(M2),
		// m.(M2),
		m,
	)

	v, s := m.(string)
	fmt.Println(v, s)
	v1, s := m.(int)
	fmt.Println(v1, s)

	var match = func(x any) {
		switch y := x.(type) {
		case int:
			fmt.Println("int", y)
		default:
			fmt.Println("unknown", y)
		}
	}
	match(1)
	match(true)
}

type T1 struct{}
type T2 struct{ T1 }
func (T1) fn() {}

func TestPointerTypeAssertion() {
	PrintBanner("TestTypeAssertion")
	type B interface{ fn() }
	var v interface{} = &T1{}
	var v2 interface{} = &T2{}

	switch v.(type) {
	case T1:
		panic("")
	case *T1:
		fmt.Println("v is *T1")
	default:
		panic("")
	}
	switch v2.(type) {
	case T1:
		panic("")
	case *T1:
		panic("")
	case T2:
	case *T2:
		fmt.Println("v is *T2")
	default:
		panic("")
	}
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
