package std

import (
	"fmt"
	"reflect"
)

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
