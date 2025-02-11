package main

import (
	"fmt"
	"github.com/ynishiza/lib"
	. "github.com/ynishiza/myapp/internal"
	p "github.com/ynishiza/myapp/internal/packages"
	"github.com/ynishiza/mymodule"
	"github.com/ynishiza/mymodule/a"
	"golang.org/x/net/html"
	"golang.org/x/net/icmp"
	"log"
)

func main() {
	defer func() {
		if e := recover(); e != nil {
			if v, s := e.(error); s {
				fmt.Println("ERROR", v.Error())
				return
			}

			fmt.Println("PANIC!", e)
		}

		log.Println("DONE")
	}()

	log.Default()
	log.SetPrefix("app:")
	log.SetFlags(log.Ldate | log.Llongfile | log.Lmicroseconds)
	log.Print("Hello")
	log.SetFlags(log.LstdFlags | log.Lshortfile | log.LUTC)

	log.Printf("Test: %s", "Start app")
	fmt.Println("Hello, World!")
	// log.Fatal("Hello, World!")
	// log.Panic("Hello, World!")

	mymodule.Hello()
	mymodule.Goodbye()
	a.Hello()

	html.EscapeString("a")
	lib.Hello(1)
	var x = [1]byte{1}
	icmp.ParseIPv4Header(x[:])

	// internal.TestBasics()
	// internal.TestSlice()
	// internal.TestInterface()
	// internal.TestTypeAssertion()
	// internal.TestFunctionType()
	TestMutation()
	TestStatements()
	// internal.TestEnum()
	// internal.TestLiteral()
	// internal.TestSemver()
	TestArraySliceMap()

	// internal.TestFunctionType()
	// internal.TestMap()
	// internal.TestGeneric()
	// internal.TestInterface()
	// internal.TestMethods()
	// internal.TestCompositeLiteral()
	// internal.TestZeroValue()
	// internal.TestNil()
	// internal.TestString()
	TestEmbedded2()
	// p.TestValidator()
	// TestGoroutineSimple()
	// TestLocks()
	// TestChannelSelect()
	// TestError()
	p.TestContext()

	var s = atype{1}
	s.y = -1000
	var s2 = s

	println(s.y, s2 == s)
	s.f()
	println(s.y)
	s.f2()
	println(s.y)
	g(&s)
	println(s.y)
	h(s)
	println(s.y)
	s2.y = 3
	println(s.y, s2 == s)

	var t = [...]int{1, 2, 3}
	t[0] = 1000
	var t2 = t
	t2[0] = 100
	log.Print(t)
	log.Print(t2)
}

type atype struct{ y int }

func (x atype) f() {
	x.y = 2
}
func (x *atype) f2() {
	x.y = 200000
}
func g(x *atype) {
	x.y *= 2
}
func h(x atype) {
	x.y *= 2
}
