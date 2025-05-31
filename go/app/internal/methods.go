package internal

import (
	"fmt"
	. "github.com/ynishiza/myapp/internal/utils"
)

func TestMethods() {
	PrintBanner("TestMethods")
	var (
		u1 UUU  = UUU{}
		u2 *UUU = &u1
	)
	// no mutation
	u1.fn()
	u1.fn()
	u1.fn()
	// mutation
	u1.fn2()
	u1.fn2()
	u1.fn2()
	u1.fn3()

	u2.fn()
	u2.fn2()
	u2.fn3()

	var x UUU
	fmt.Println("x")
	x.fn2()
	x.fn2()
	x.fn2()

	var m = u2.fn2
	fmt.Println("m")
	m()
	m()
	fmt.Println(u2.x)
	(*UUU).fn2(&u1)

	fmt.Println("m2")
	var m2 = u1.fn2
	m2()
	m2()
	m2()
	fmt.Println(u2.x)

	UUU.fn(u1)
	UUU.fn(*u2)
	(*UUU).fn2(&u1)
	(*UUU).fn2(u2)
}

type UUU struct{ x int }
type VVV struct{}

func (u UUU) fn() {
	u.x += 1
	fmt.Println("uuu immutable", u.x)
}
func (u *UUU) fn2() {
	u.x += 1
	fmt.Println("uuu mutable", u.x)
}

// func (*UUU) fn() { }
// func (UUU) fn(x int) { }
func (VVV) fn(x int) {}

func (UUU) fn3() {}
