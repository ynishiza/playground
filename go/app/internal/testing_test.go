package internal

import (
	"github.com/stretchr/testify/assert"
	"log"
	"testing"
)

func TestMain(m *testing.M) {
	log.SetFlags(log.Ldate | log.Llongfile)
	log.Println("Start test")
	m.Run()
	log.Println("End test")
	log.Printf("fuzzed %d", count)
}

func TestHello2(t *testing.T) {
	t.SkipNow()
	t.Error("Hello2 failed")
}

func TestA(t *testing.T) {
	// t.Fatalf("OOPS")
	t.Log("TestA")
}

func TestB(t *testing.T) {
	t.SkipNow()
	t.Error("TestB")
	// t.Fatalf("OOPS")
}

var count = 0

func FuzzTestA(f *testing.F) {
	f.Add("a")
	f.Add("b")
	f.Fuzz(func(t *testing.T, s string) {
		var assert = assert.New(t)
		assert.True(len(s) >= 0)
		t.Log(s)
	})
}

func FuzzTestB(f *testing.F) {
	f.Fuzz(func(t *testing.T,  i int ) {
		var assert = assert.New(t)
		assert.True(i > -100000)
		t.Log(i)
	})
}
