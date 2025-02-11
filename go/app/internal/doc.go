/*
This is a doc test

# List:

 1. A
 2. B
 3. C

# Bullets:
 - a
  - b
  - c

# Code block

abc

    struct {
      x int
    }

a b c

A 
B

# Links


BUG(1): bug 

NOTE(a): a

[internal.MyFn]

[MyFn]

[packages.TestValidator]

[fmt.Println]

[RFC 7159]: https:tools.ietf.org/html/rfc7159
[google]: http:www.google.com 

*/
package internal

import (
	"github.com/ynishiza/myapp/internal/packages"
	"fmt"
)

// MyFn
//
// [RFC 7159]: https://tools.ietf.org/html/rfc7159
// [google]: http://www.google.com 
//
func MyFn() {
	packages.TestValidator()
	fmt.Println()
}	
