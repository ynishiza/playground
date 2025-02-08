package main

import (
  "fmt"
  "golang.org/x/mod/semver"
  "golang.org/x/net/icmp"
  "golang.org/x/net/html"
  "github.com/ynishiza/lib"
)

func main() {
	fmt.Println("Hello, World!")

  html.EscapeString("a")
  lib.Hello(1)
  var x = [1]byte{1}
  icmp.ParseIPv4Header(x[:])
  fmt.Printf("1.1.1 %s", semver.IsValid("1.1.1"))
  testEnum()
  testEmbedded()
  testEmbedded()
  testEnum()
}

func testBasics() {
  var x int = 1
  var y = d
  z := d
	fmt.Println(x, d, y, z)
  var xx bool = true
	fmt.Println(xx)

  var list []int
  fmt.Println(len(list))
}

func testType() {
  fmt.Println(ABC {})
  fmt.Println(ABC { x: 10 })
  fmt.Println(ABC { x: 10, y: "abc" })
  fmt.Println(ABC { 
    x: 10,
    y: "abc",
  })
}
type ABC struct {
  x int
  y string
}

func testEnum() {
  const a, b = 1, 2
	fmt.Println(d, e, f, g)

  const x = d
  switch x {
  case a:
    fmt.Printf("a")
  default: 
    fmt.Printf("default")
  }
}
const (
	d = iota
	e
	f
	g
)


// Recursive
type ARec struct {
  // ARec
  // l [10]ARec
  *ARec
}

// Embedded
type MyTypeA struct { int; string }
type MyTypeB struct { MyTypeA }
func testEmbedded() {
  fmt.Println("MyTypeB", MyTypeB{}) 
  fmt.Println("MyTypeB", MyTypeB{ MyTypeA{ 10, "abc" } }) 
}
