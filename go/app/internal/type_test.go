package internal

import (
	"github.com/stretchr/testify/assert"
	. "github.com/ynishiza/myapp/internal/utils"
	"testing"
)

func myAdd[T interface { ~float64 | ~int64 | ~uint64 }](t T) T {
	return t + t
}

func TestGeneralInterface(t *testing.T) {
	type MyInt int64
	var assert = assert.New(t)
	assert.Equal(myAdd(int64(-1)), int64(-2))
	assert.Equal(myAdd(MyInt(-1)), int64(-2))
	assert.Equal(myAdd(float64(1.1)), float64(2.2))
	assert.Equal(myAdd(uint64(1)), uint64(2))
}

func TestSliceCapacityBasic(t *testing.T) {
	var x = [5]int{1}
	var xs = x[:3]
	var xss = xs[:2]
	result := append(xss, 2)
	result = append(result, 3)
	result = append(result, 4)
	result = append(result, 5) 	// capacity full. New memory
	assert.Equal(t, 5, cap(xs))
	assert.Less(t, 5, cap(result))
	assert.Equal(t, x, [5]int{1, 0, 2, 3, 4})
	assert.Equal(t, xs, []int{1, 0, 2})
	assert.Equal(t, xss, []int{1, 0})
	assert.Equal(t, result, []int{1, 0, 2, 3, 4, 5})

	var x2 = [5]int{1}
	var xs2 = x2[:3:4]
	result = append(xs2, 2)
	result = append(result, 3) // capacity full. New memory.
	result = append(result, 4)
	assert.Equal(t, 4, cap(xs2))
	assert.Less(t, 4, cap(result))
	assert.Equal(t, x2, [5]int{1, 0, 0, 2})
	assert.Equal(t, xs2, []int{1, 0, 0})
	assert.Equal(t, result, []int{1, 0, 0, 2, 3, 4})

	var x3 = [5]int{1}
	var xs3 = x3[:4]
	var xss3 = xs3[:2]
	result = append(xss3, 2)
	result = append(result, 3, 4, 5) // capacity full. 
	assert.Equal(t, 5, cap(xs3))
	assert.Less(t, 5, cap(result))
	assert.Equal(t, x3, [5]int{1, 0, 2, 0, 0}) 			// 3,4,5 NOT included
	assert.Equal(t, xs3, []int{1, 0, 2, 0})
	assert.Equal(t, xss3, []int{1, 0})
	assert.Equal(t, result, []int{1, 0, 2, 3, 4, 5})
}

func TestSliceCapacity(t *testing.T) {
	// append
	var src = [10]int{1}
	var slice1 = src[:4:6]
	var slice2 = slice1[:3:6]
	Logln(src, slice1, slice2)
	s3 := append(slice2, 100)
	Logln(src, slice1, slice2, s3)
	s3 = append(s3, 200)
	Logln(src, slice1, slice2, s3)
	s3 = append(s3, 300)
	// src = [1, 0, 0, 100, 200, 300, 0, ...]
	Logln(src, slice1, slice2, s3)
	s3 = append(s3, 400)
	// src = [1, 0, 0, 100, 200, 300, 0, ...]
	Logln(src, slice1, slice2, s3)
	s3 = append(s3, 500)
	Logln(src, slice1, slice2, s3)
	assert.Equal(t, cap(src), 10)
	assert.Equal(t, cap(slice1), 6)
	assert.Equal(t, cap(slice2), 6)
	assert.Equal(t, [10]int{1, 0, 0, 100, 200, 300}, src)
	assert.Equal(t, []int{1, 0, 0, 100}, slice1)
	assert.Equal(t, []int{1, 0, 0}, slice2)
	assert.Equal(t, []int{1, 0, 0, 100, 200, 300, 400, 500}, s3)

	var src2 = [10]int{1}
	var shortSlice = src2[:3:3]
	s4 := append(shortSlice, 100)
	Logln(src2, shortSlice, s4)
	s4 = append(shortSlice, 200)
	// src2= [1, 0, 0, 100, 200, 300, 0, ...]
	Logln(src2, shortSlice, s4)
	assert.Equal(t, cap(src2), 10)
	assert.Equal(t, cap(shortSlice), 3)
	assert.Equal(t, src2, [10]int{1, 0, 0})
}
