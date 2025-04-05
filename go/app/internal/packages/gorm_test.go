package packages

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/ynishiza/myapp/internal/utils"
	"gorm.io/driver/postgres"
	"gorm.io/gorm"
)

var db *gorm.DB

func TestMain(m *testing.M) {
	utils.PrintBanner("TestGrom")

	var err error
	db, err = gorm.Open(postgres.Open(gormSettings), &gormConfig)
	if err != nil {
		fmt.Printf("Failed to connect to DB. Skipping test. %v", err.Error())
		// return
	}

	cleanupTable(db, MyTestDataA{}.TableName())
	cleanupTable(db, "bad_data")
	cleanupTable(db, "my_simple_test_data")
	db.AutoMigrate(&MyTestDataA{})
	m.Run()
}

func TestGormBasic(t *testing.T) {
	var assert = assert.New(t)
	var err error

	// db = db.Where("true").Session(&gorm.Session{})
	var allValues = []MyTestDataA{
		{1, "a", valuePointer(-2.3)},
		{2, "e", valuePointer(-2.3)},
		{3, "aa", valuePointer(1.1)},
		{4, "bb", valuePointer(1.1)},
		{31, "dd", valuePointer(1.1)},
	}
	value1, value2 := allValues[0], allValues[1]
	db.Create(&value1)
	db.Create(&value2)
	db.Create(allValues[2:])
	db.Create(&MyTestDataA{100, "f", nil})
	allValues = append(allValues, MyTestDataA{100, "f", nil})

	var result *MyTestDataA
	err = db.
		Where("id = ?", value1.MyID).
		First(&result).Error
	assert.Nil(err)
	assert.Equal(value1, *result)

	result = nil
	err = db.
		Where("id = ?", value2.MyID).
		Where("id > ? AND id > ?", 0, -1).
		First(&result).Error
	assert.Nil(err)
	assert.Equal(value2, *result)

	result = nil
	err = db.
		First(&result, "id=?", allValues[5].MyID).Error
	assert.Nil(err)
	assert.Equal(allValues[5], *result)

	var v *MyTestDataA = &MyTestDataA{MyID: 100}
	err = db.First(&v, "id=1").Error
	assert.Equal("record not found", err.Error())
	v = nil
	err = db.First(&v, "id=1").Error
	assert.Nil(err)

	var results []MyTestDataA
	err = db.Find(&results).Error
	assert.Nil(err)
	assert.Equal(allValues, results)
}

type MyTestDataA struct {
	MyID int      `gorm:"primaryKey;column:id"`
	Name string   `gorm:"column:name;not null"`
	Num  *float64 `gorm:"column:num;default:null"`
}

func (MyTestDataA) TableName() string {
	return "MyTestDataA"
}

func TestGormWrite(t *testing.T) {
	var assert = assert.New(t)
	var err error
	var result *MySimpleWriteTest
	var results []MySimpleWriteTest

	var values = []MySimpleWriteTest{
		{},
		{},
		{},
		{},
	}

	cleanupTable(db, "my_simple_write_tests")
	cleanupTable(db, "my_assoc_tests")
	db.AutoMigrate(&MySimpleWriteTest{})
	db.Create(&values)

	db.Find(&results)
	assert.Equal([]MySimpleWriteTest{
		{ID: 1, X: true, Y: 1.2, Z: nil},
		{ID: 2, X: true, Y: 1.2, Z: nil},
		{ID: 3, X: true, Y: 1.2, Z: nil},
		{ID: 4, X: true, Y: 1.2, Z: nil},
	}, results)
	fmt.Printf("%v", values)

	res := db.Create(&[]MySimpleWriteTest{
		{ID: 1},
		{ID: 1000},
	})
	assert.Equal("ERROR: duplicate key value violates unique constraint \"my_simple_write_tests_pkey\" (SQLSTATE 23505)", res.Error.Error())
	assert.Equal(res.RowsAffected, int64(0))

	var v1 = &MySimpleWriteTest{ID: 100, Y: 1.2345}
	db.Select("ID").Create(&v1)
	db.First(&result, "id = 100")
	assert.Equal(MySimpleWriteTest{ID: 100, X: false, Y: 1.2345}, *v1)
	assert.Equal(MySimpleWriteTest{ID: 100, X: true, Y: 1.2, Z: nil}, *result)

	// BeforeCreate
	var v2 = &MySimpleWriteTest{ID: 200}
	db.Create(&v2)
	assert.Equal(v2.ID, -1)

	// Primary key
	v2 = &MySimpleWriteTest{}
	db.Create(&v2)
	assert.Equal(5, v2.ID)
	v2.ID = 0
	db.Create(&v2)
	assert.Equal(6, v2.ID)

	// Defaults
	v2 = &MySimpleWriteTest{Y: 0}
	db.Create(&v2)
	assert.Equal(MySimpleWriteTest{ID: 7, X: true, Y: 1.2, Z: nil}, *v2)
	v2 = &MySimpleWriteTest{Y: 1}
	db.Create(&v2)
	assert.Equal(MySimpleWriteTest{ID: 8, X: true, Y: 1, Z: nil}, *v2)

	// by map
	err = db.Table("my_simple_write_tests").Create(map[string]any {
		"id": 123,
	}).Error
	assert.Nil(err)

	db.Model(&MySimpleWriteTest{ ID: 1 }).Updates(MySimpleWriteTest { X: true, Z: valuePointer(1.5) })

	db.AutoMigrate(&MyAssocTest{})
	var v3 = &MyAssocTest{}
	err = db.Create(&v3).Error
	assert.Nil(err)
	assert.Equal(MyAssocTest{
		Secret: 1,
		V:      MySimpleWriteTest{ID: 0, X: true, Y: 1.2, Z: nil},
	}, *v3)
}

type MySimpleWriteTest struct {
	ID int      `gorm:"type:integer not null primary key"`
	X  bool     `gorm:"default:true"`
	Y  float64  `gorm:"default:1.2"`
	Z  *float64 `gorm:"default:null"`
}

type MyAssocTest struct {
	Secret int               `gorm:"primarykey"`
	V      MySimpleWriteTest `gorm:"embedded"`
}

func (d *MySimpleWriteTest) BeforeCreate(tx *gorm.DB) error {
	if d.ID == 200 {
		d.ID = -1
	}
	return nil
}

func TestGormTableType(t *testing.T) {
	var assert = assert.New(t)
	var err error

	db.AutoMigrate(&BadData{})
	db.Create(&BadData{1})
	var d *BadData
	err = db.First(&d).Error
	assert.Equal(BadData{0}, *d)

	err = db.AutoMigrate(&MyTableTest{})
	assert.Nil(err)
}

func valuePointer[T any](t T) *T { return &t }

type MySimpleTestData struct {
	MyID   int
	MyName *bool
}

type MyTableTest struct {
	MyID int16 `gorm:"type:bigint not null primary key;column:id"`
}

type BadData struct {
	myid int
}
