package std

import (
	"fmt"
	"github.com/stretchr/testify/assert"
	rf "reflect"
	"testing"
)

type MyType struct {
	x int
	y string
}

func (MyType) DoSomethingA()  {}
func (*MyType) DoSomethingB() {}

func TestType(t *testing.T) {
	var assert = assert.New(t)
	var x int = 1
	var tint = rf.TypeFor[int]()
	var tpint = rf.TypeOf(&x)

	printTypeInfo(rf.TypeFor[bool]())
	printTypeInfo(rf.TypeFor[string]())
	printTypeInfo(rf.TypeFor[int]())
	printTypeInfo(rf.TypeFor[int64]())
	printTypeInfo(rf.TypeFor[float64]())
	printTypeInfo(rf.TypeFor[*int]())
	printTypeInfo(rf.TypeFor[[10]int]())
	printTypeInfo(rf.TypeFor[map[string]bool]())
	printTypeInfo(rf.TypeFor[[]int]())
	printTypeInfo(rf.TypeOf(struct{ x int }{}))
	printTypeInfo(rf.TypeFor[func() bool]())
	printTypeInfo(rf.TypeFor[fmt.State]())
	printTypeInfo(rf.TypeOf(MyType{}))

	assert.Equal(tint.Name(), "int")
	assert.Equal(tint.Kind(), rf.Int)
	assert.Equal(tint.NumMethod(), 0)
	assert.Equal(tpint.Kind(), rf.Pointer)
}

func printTypeInfo(t rf.Type) {
	var _, exists = t.MethodByName("DoSomethingA")
	fmt.Printf("name %s\tstring %s\tkind %d\tsize %d\tpkgPath: %s\tNumMethod %d\talign %d\tfield align %d \tassignable %v\tDoSomething %v\n",
		t.Name(),
		t.String(),
		t.Kind(),
		t.Size(),
		t.PkgPath(),
		t.NumMethod(),
		t.Align(),
		t.FieldAlign(),
		t.AssignableTo(rf.TypeFor[any]()),
		exists,
	)
}

type MyTestType struct {
	X int
	Y bool   `a:"A"`
	z string `b:"B"`
	w struct{ a string }
}

func (MyTestType) DoSomethingA(x int) (bool, error) { return true, nil }
func (MyTestType) DoSomethingB(x ...int)            {}
func (*MyTestType) DoSomethingC()                   {}
func (MyTestType) doSomething()                     {}

func TestStructType(t *testing.T) {
	var assert = assert.New(t)
	var tp = rf.TypeFor[MyTestType]()

	assert.Equal(tp.NumMethod(), 2)
	assert.Equal(tp.Method(0).Name, "DoSomethingA")
	assert.Equal(tp.Method(0).PkgPath, "")
	assert.Equal(tp.Method(1).Name, "DoSomethingB")
	assert.Equal(tp.Method(1).PkgPath, "")
	m, exists := tp.MethodByName("doSomething")
	assert.Equal(m.Name, "")
	assert.Equal(m.PkgPath, "")
	m, exists = tp.MethodByName("AAAA")
	assert.False(exists)

	assert.Equal(tp.NumField(), 4)
	assert.Equal(len(rf.VisibleFields(tp)), 4)
	assert.Equal(tp.Field(0).Name, "X")
	assert.Equal(tp.FieldByIndex([]int{0}).Name, "X")
	assert.Equal(tp.Field(0).PkgPath, "")
	assert.Equal(tp.Field(0).Type, rf.TypeFor[int]())
	assert.Equal(tp.Field(0).Tag, rf.StructTag(""))

	assert.Equal(tp.Field(1).Name, "Y")
	assert.Equal(tp.FieldByIndex([]int{1}).Name, "Y")
	assert.Equal(tp.Field(1).PkgPath, "")
	assert.Equal(tp.Field(1).Type, rf.TypeFor[bool]())
	assert.Equal(tp.Field(1).Tag, rf.StructTag(`a:"A"`))

	assert.Equal(tp.Field(2).Name, "z")
	assert.Equal(tp.FieldByIndex([]int{2}).Name, "z")
	assert.NotEmpty(tp.Field(2).PkgPath)
	assert.Equal(tp.Field(2).Type, rf.TypeFor[string]())
	assert.Equal(tp.Field(2).Tag, rf.StructTag(`b:"B"`))
	f, _ := tp.FieldByName("z")
	assert.Equal(f.Name, "z")
	assert.Equal(tp.Field(3).Name, "w")
	assert.Equal(tp.FieldByIndex([]int{3, 0}).Name, "a")
	f, exists = tp.FieldByName("zzzz")
	assert.False(exists)

	assert.Equal(rf.TypeFor[testing.T]().NumField(), 3)
	assert.Equal(rf.TypeFor[testing.T]().Field(0).Name, "common")
	assert.Equal(rf.TypeFor[testing.T]().Field(1).Name, "isEnvSet")
	assert.Equal(rf.TypeFor[testing.T]().Field(2).Name, "context")
}

func TestReflectValue(t *testing.T) {
	var assert = assert.New(t)

	type User struct {
		Id   int
		Name string
	}

	var x = struct {
		User   User
		Value  int
		secret bool
	}{
		User{0, "a"},
		1,
		false,
	}

	var vr = rf.ValueOf(x)
	var userValue = rf.ValueOf(x.User)
	var numValue = rf.ValueOf(x.Value)
	var secretValue = rf.ValueOf(x.secret)
	assert.True(vr.Field(0).Equal(userValue))
	assert.True(vr.Field(1).Equal(numValue))
	assert.True(vr.Field(2).Equal(secretValue))
	assert.Panics(func() { vr.Field(3).Equal(secretValue) })
	assert.True(vr.FieldByName("User").Equal(userValue))
	assert.True(vr.FieldByName("Value").Equal(numValue))
	assert.True(vr.FieldByName("secret").Equal(secretValue))
	assert.False(vr.FieldByName("a").IsValid())

	// Interface
	assert.Equal(vr.Interface(), x)
	assert.Equal(vr.FieldByName("User").Interface(), x.User)
	assert.Equal(vr.FieldByName("Value").Interface(), x.Value)
	assert.Equal(vr.FieldByName("secret").Bool(), false)
	assert.Equal(rf.ValueOf(1).Interface(), 1)
	assert.Panics(func() { vr.FieldByName("secret").Interface() })

	assert.True(vr.FieldByIndex([]int{0, 0}).Equal(rf.ValueOf(x.User.Id)))
	assert.True(vr.FieldByIndex([]int{0, 1}).Equal(rf.ValueOf(x.User.Name)))
	assert.Panics(func() { vr.FieldByIndex([]int{0, 2}).Equal(rf.ValueOf(x.User.Name)) })
}

func TestPointer(t *testing.T) {
	var assert = assert.New(t)
	var x = 1
	var tp = rf.TypeFor[*int]()
	var tp2 = rf.TypeFor[**int]()
	var v = rf.ValueOf(&x)

	assert.Equal(tp.Elem(), rf.TypeFor[int]())
	assert.Equal(tp2.Elem(), tp)

	assert.Equal(v.Type(), tp)
	assert.Equal(v.CanSet(), false)
	assert.Equal(v.Elem().CanSet(), true)
	assert.False(rf.TypeFor[int]().AssignableTo(v.Type()))
	assert.True(rf.TypeFor[int]().AssignableTo(v.Elem().Type()))

	v.Elem().Set(rf.ValueOf(10))
	assert.Equal(x, 10)

	var y interface{} = int(1)
	y = uint(1)
	y = float32(1)
	y = bool(true)
	y = [1000]int{}
	y = &x
	fmt.Println(y)
}

type ABC struct {
	X int
	Y *int
	z int
}

func TestValueSet(t *testing.T) {
	var assert = assert.New(t)

	var x = 1
	var v = struct {
		X int
		Y *int
		z int
	}{1, new(int), 2}
	// var vi interface {} = v
	var sl = make([]int, 2, 10)
	var m = map[string]int{ "a": 1 }
	var mKeys = rf.ValueOf(m).MapRange()
	mKeys.Next()
	assert.False(rf.ValueOf(1).CanAddr())
	assert.False(rf.ValueOf(x).CanAddr())
	assert.False(rf.ValueOf(&x).CanAddr())
	assert.False(rf.ValueOf(v).CanAddr())
	assert.False(rf.ValueOf(v).Field(0).CanAddr())
	assert.False(rf.ValueOf(v).Field(1).CanAddr())
	assert.True(rf.ValueOf(&x).Elem().CanAddr())
	assert.True(rf.ValueOf(&v).Elem().Field(0).CanAddr())
	assert.True(rf.ValueOf(&v).Elem().Field(1).CanAddr())
	assert.True(rf.ValueOf(&v).Elem().Field(2).CanAddr())
	assert.False(rf.ValueOf(sl).CanAddr())
	assert.True(rf.ValueOf(sl).Index(0).CanAddr())

	// assert.True(rf.ValueOf(vi).CanAddr())

	assert.False(rf.ValueOf(1).CanSet())
	assert.False(rf.ValueOf(x).CanSet())
	assert.False(rf.ValueOf(&x).CanSet())
	assert.True(rf.ValueOf(&x).Elem().CanSet())
	assert.True(rf.ValueOf(&v).Elem().Field(0).CanSet())
	assert.True(rf.ValueOf(&v).Elem().Field(1).CanSet())
	assert.False(rf.ValueOf(&v).Elem().Field(2).CanSet())
	assert.True(rf.ValueOf(sl).Index(0).CanSet())
	assert.Equal(mKeys.Key().String(), "a")
	assert.False(rf.ValueOf(m).MapIndex(mKeys.Key()).CanSet())
	// assert.True(rf.ValueOf(vi).Elem().CanAddr())

	rf.ValueOf(&x).Elem().SetInt(10)
	assert.Equal(x, 10)
	rf.ValueOf(&v).Elem().Field(0).SetInt(-10)
	assert.Equal(v.X, -10)
	rf.ValueOf(sl).Index(0).SetInt(10)
	assert.Equal(sl, []int{10, 0})
}
