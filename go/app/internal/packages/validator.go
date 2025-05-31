package packages

import (
	"errors"
	"fmt"
	"log"

	validator "github.com/go-playground/validator/v10"
	. "github.com/ynishiza/myapp/internal/utils"
)

var val = validator.New(validator.WithRequiredStructEnabled())

type MyStruct struct {
	A int `validate:"required,min=10,max=20"`
}

func TestValidator() {
	log.Println("TestValidator")
	PrintBanner("TestValidator")
	fmt.Println(
		"\nempty:",
		val.Struct(MyStruct{}),
		"\nmin:",
		val.Struct(MyStruct{1}),
		"\nok:",
		val.Struct(MyStruct{10}),
		"\nmax:",
		val.Struct(MyStruct{100}),
	)

	var check = func(data any) {
		var es validator.ValidationErrors
		if errors.As(val.Struct(data), &es) {
			for _, e := range es {
				fmt.Printf(`
%s

Field %s
Tag %s
Actual %s
StructField %s
StructNamespace %s
Type %s
Value %v
				`,
					e,
					e.Field(),
					e.Tag(),
					e.ActualTag(),
					e.StructField(),
					e.StructNamespace(),
					e.Type(),
					e.Value(),
				)
			}
		}
	}

	check(struct {
		A int `json:"a" validate:"required,min=10"`
	}{})
	check(struct {
		A int `json:"a" validate:"required,min=10"`
	}{
		A: 1,
	})

  // Step 1: validate
  err := val.Struct(MyStruct {})

  // Step 2: check errors
  var verrs validator.ValidationErrors
  if errors.As(err, &verrs) {
    for _, verr := range verrs {
     fmt.Printf("%s field: %s tag: %s value: %v", verr, verr.Field(), verr.Tag(), verr.Value())
    }
  }
}
