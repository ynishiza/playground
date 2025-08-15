package main

import (
	"fmt"
	"log"
	"os"

	"github.com/ynishiza/lib"
	. "github.com/ynishiza/myapp/internal"
	p "github.com/ynishiza/myapp/internal/packages"
	"github.com/ynishiza/myapp/internal/std"
	"github.com/ynishiza/myapp/internal/std/ast"
	"github.com/ynishiza/mymodule"
	"github.com/ynishiza/mymodule/a"
	"golang.org/x/net/html"
	"golang.org/x/net/icmp"
)

func main() {
	log.Default()
	log.SetPrefix("app:")
	log.SetFlags(log.Ldate | log.Llongfile | log.Lmicroseconds)
	log.Print("Hello")
	log.SetFlags(log.LstdFlags | log.Lshortfile | log.LUTC)

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

	if len(os.Args) == 1 {
		log.Printf("%s all|default", os.Args[0])
		os.Exit(1)
	}

	log.Printf("Test: %s", "Start app")
	fmt.Println("Hello, World!")

	switch os.Args[1] {
	case "all":
		runAll()
	case "default":
		runDefault()
	default:
		log.Panic("Should never happen")
	}
}

func runDefault() {
	mymodule.Hello()
	mymodule.Goodbye()
	a.Hello()

	html.EscapeString("a")
	lib.Hello(1)
	var x = [1]byte{1}
	icmp.ParseIPv4Header(x[:])

	// p.TestValidator()
	// p.TestGorm()
	// std.TestJSON()
	// TestSlice()
	// p.TestGormArray()
	ast.Test()
}

func runAll() {
	TestBasics()
	TestSlice()
	TestInterface()
	TestTypeAssertion()
	TestFunctionType()
	TestMutation()
	TestStatements()
	TestLiteral()
	TestSemver()
	TestArraySliceMap()

	// Types
	TestTypeIdentity()
	TestType()
	TestEnum()
	TestFunctionType()
	TestMap()
	TestGeneric()
	TestInterface()
	TestMethods()
	TestCompositeLiteral()
	TestZeroValue()
	TestNil()
	TestString()
	TestEmbedded2()

	// Struct

	// concurrency
	TestGoroutineSimple()
	TestLocks()
	TestChannelSelect()

	// error
	TestError()

	// Packages
	p.TestValidator()
	p.TestGorm()
	p.TestGormTransacion()
	p.GormExample()
	p.TestGormError()
	p.TestValidator()

	TestErrorWrap()
	TestPointerTypeAssertion()

	// Std
	std.TestReflectNew()
	p.TestContext()
}
