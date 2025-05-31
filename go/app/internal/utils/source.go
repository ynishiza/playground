package utils

import (
	"fmt"
	"log"
	"math"
	"time"
)

func SleepSeconds[N ~float64](n N) {
	var m = time.Duration(math.Round(1000 * float64(n)))
	time.Sleep(m * time.Millisecond)
}

func PrintBanner(name string) {
	fmt.Printf("\n===== %s =====\n", name)
}

var Logln = log.Println

