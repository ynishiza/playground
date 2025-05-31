package std

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/ynishiza/myapp/internal/utils"
)

func TestJSON() {
	utils.PrintBanner("TestJSON")
	var list = []int{1, 2, 3, 4}
	bytes, err := json.Marshal(list)
	if err != nil {
		panic(err)
	}

	var list1 []int
	var list2 *[]int
	err = json.Unmarshal(bytes, &list1)
	if err != nil {
		panic(err)
	}
	fmt.Printf("list1: %v\n", list1)

	err = json.Unmarshal(bytes, &list2)
	if err != nil {
		panic(err)
	}

	fmt.Printf("list2: %v\n", list2)

	r := strings.NewReader(string(bytes))
	decoder := json.NewDecoder(r)
	decoder.DisallowUnknownFields()
	err = decoder.Decode(&list1)
	if err != nil {
		panic(err)
	}
	fmt.Printf("list1: %v\n", list1)
}
