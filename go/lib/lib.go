package lib

import (
  "fmt"
  "log"
)

func Hello(x int) {
  _, err := fmt.Printf("Hello from module %d\n", x)
  if err != nil {
    log.Fatal("ERROR")
  }
  log.Printf("done")
}
