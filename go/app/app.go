package main

import (
	"fmt"
	"github.com/ynishiza/lib"
	. "github.com/ynishiza/myapp/internal"
	// p "github.com/ynishiza/myapp/internal/packages"
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

	// Packages
	// p.TestContext()
	// p.TestGorm()
	// p.TestGormTransacion()
	// p.GormExample()
	// p.TestGormError()

	TestErrorWrap()
}
