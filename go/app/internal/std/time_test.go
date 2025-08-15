package std

import (
	"testing"

	"time"
	"github.com/stretchr/testify/assert"
)

func TestTimeEquality(t *testing.T) {
	assert := assert.New(t)

	// t1 := time.Now()
	var t1 time.Time
	t1, _ = time.Parse("2006-01-01", "2006-01-10")
	t2, _ := time.Parse("2006-01-01", "2006-01-11")
	assert.Equal(&t1, &t2)
}
