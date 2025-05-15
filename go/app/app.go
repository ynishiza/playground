package main

import (
	"fmt"
	"log"
	"reflect"

	"github.com/ynishiza/lib"
	. "github.com/ynishiza/myapp/internal"
	"github.com/ynishiza/mymodule"
	"github.com/ynishiza/mymodule/a"
	"golang.org/x/net/html"
	"golang.org/x/net/icmp"
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
	// TestMutation()
	// TestStatements()
	// internal.TestEnum()
	// internal.TestLiteral()
	// internal.TestSemver()
	// TestArraySliceMap()

	// internal.TestFunctionType()
	// internal.TestMap()
	// internal.TestGeneric()
	// internal.TestInterface()
	// internal.TestMethods()
	// internal.TestCompositeLiteral()
	// internal.TestZeroValue()
	// internal.TestNil()
	// internal.TestString()
	// TestEmbedded2()
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

	// TestErrorWrap()
	TestPointerTypeAssertion()
	TestReflectNew()
}

func TestReflectNew() {
	var x []*int = []*int{}

	var t = reflect.TypeOf(x)
	fmt.Printf("%v\n", t.Kind() == reflect.Slice)
	var tElem = t.Elem()
	fmt.Printf("%v\n", tElem.Kind() == reflect.Pointer)
	fmt.Printf("%v\n", tElem.Elem().Kind() == reflect.Int)
	fmt.Printf("%v\n", reflect.New(tElem.Elem()).Elem().Int())
	fmt.Printf("%v\n", *reflect.New(tElem.Elem()).Interface().(*int))
	fmt.Printf("%v\n", reflect.TypeOf(8))

	type T struct{}
	var y = T{}
	var t2 = reflect.TypeOf(&y)
	fmt.Printf("%v\n", t2.Elem().Kind() == reflect.Struct)
	var y2base = reflect.New(t2.Elem()).Interface()
	fmt.Printf("%v\n", y2base)
	var y2 = y2base.(*T)
	fmt.Printf("%v\n", y2)
}
