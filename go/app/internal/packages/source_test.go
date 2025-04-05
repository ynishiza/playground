package packages

import (
	"fmt"
	"github.com/stretchr/testify/assert"
	"golang.org/x/text/language"
	"golang.org/x/xerrors"
	"regexp"
	"testing"
)

func init() {
	fmt.Println("Begin test ")
}

func TestXErrors(t *testing.T) {
	var assert = assert.New(t)

	var e = xerrors.New("OOPS")
	var e2 = xerrors.Errorf("OOPS %d", 1)
	var e3 = fmt.Errorf("OOPS %d", 2)

	fmt.Println(e)
	fmt.Println(e2)
	fmt.Println(e3)
	assert.Equal(e.Error(), "OOPS")
	assert.Equal(e2.Error(), "OOPS 1")
	assert.Equal(e3.Error(), "OOPS 2")
	assert.Equal(fmt.Sprint(e), "OOPS")
	assert.Equal(fmt.Sprint(e2), "OOPS 1")
}

type MyInt int

func (m MyInt) String() string   { return fmt.Sprintf("MyInt[%d]", m) }
func (m MyInt) GoString() string { return "MYINT" }

func TestFmtInterfaces(t *testing.T) {
	var assert = assert.New(t)
	assert.Equal("MyInt[19] MYINT", fmt.Sprintf("%[1]v %#[1]v", MyInt(19)))
}

func TestAcceptLanguage(t *testing.T) {
	var m = map[string][]int{"a": []int{1}}
	print(len(m["a"]))
	print(len(m["b"]))
	var assert = assert.New(t)
	tags, quals, error := language.ParseAcceptLanguage("en-gb;q=0.8")
	if error != nil {
		assert.Fail("", error)
	}
	assert.Equal("", quals[0])
	x, y, z := tags[0].Raw()
	assert.Equal("", x.String())
	assert.Equal("", y.String())
	assert.Equal("", z.String())
	assert.Equal("", tags[0].String())

}

func TestRegex(t *testing.T) {
	var assert = assert.New(t)

	var r = regexp.MustCompile("(?P<N>[a-zA-Z]+)(?P<M>[0-9]+)")
	assert.Equal([]string{"UQJP10", "UQJP", "10"}, r.FindStringSubmatch("UQJP10"))
}
