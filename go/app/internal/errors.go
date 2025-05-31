package internal

import (
	"errors"
	"fmt"
	. "github.com/ynishiza/myapp/internal/utils"
)

var ErrorA = errors.New("SomeInnerError")

func TestErrorWrap() {
	PrintBanner("TestErrorWrap")

	// var outerError = myError { childError: ErrorA }
	var topError = fmt.Errorf("OOPS : %w", ErrorA)
	var topError2 = fmt.Errorf("OOPS : %w", myError {})

	fmt.Printf("\n top Is errorA: %v", errors.Is(topError, ErrorA))
	fmt.Printf("\n topError3 Is errorA: %v", errors.Is(ErrorA, ErrorA))
	var unwrapped myError
	if errors.As(topError2, &unwrapped) {
		fmt.Printf("\nWrapped error : %v", unwrapped)
	} else {
		panic("OOPS")
	}
	if errors.As(myError{}, &unwrapped) {
		fmt.Printf("\nWrapped error : %v", unwrapped)
	} else {
		panic("OOPS")
	}
}

type myError struct {
}

func (myError) Error() string {
	return "myError"
}

func TestError() {
	PrintBanner("TestError")
	var f = func() (int, error) {
		return 0, MyError{"OOPS"}
	}

	if _, e := f(); e != nil {
		fmt.Printf("error:%s", e.Error())
	}
	_, ee := f()
	if e, ok := ee.(MyError); ok {
		fmt.Printf("error:%s", e.name)
	}

	switch _, e := f(); m := e.(type) {
	case MyError:
		fmt.Printf("MyError:%s", m.name)
	default:
		panic("Unknown")
	}
}

type MyError struct {
	name string
}

func (m MyError) Error() string {
	return fmt.Sprintf("name:%s", m.name)
}
