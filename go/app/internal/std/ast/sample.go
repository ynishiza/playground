package ast

import "fmt"

// MyPoint data
//
//  x  x value
//  y  y value
type MyPoint struct {
	// x cood
	x int
	// y coord
	y int // y is int
}

type MyValue string

// Prints
func (m MyPoint) string() string {
	return fmt.Sprintf("(%d, %d)", m.x, m.y)
}
