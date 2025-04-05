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

