package packages

import (
	"fmt"
	"log"
	"os"
	"strings"

	"gorm.io/driver/postgres"
	"gorm.io/gorm"
	"gorm.io/gorm/logger"
)

func GormExample() {
	var out *gorm.DB
	var err error
	var settings = strings.Join([]string{
		"host=localhost",
		"user=yuinishizawa",
		"dbname=postgres",
	}, " ")

	// Custom logger
	var config = gorm.Config{
		Logger: logger.New(
			log.New(os.Stdout, "\n", log.LstdFlags),
			logger.Config{
				LogLevel: logger.Info,
			}),
	}
	db, err := gorm.Open(postgres.Open(settings), &config)
	if err != nil {
		panic(err.Error())
	}

	if db.Migrator().HasTable(&MyGormData{}) {
		db.Migrator().DropTable(&MyGormData{})
	}
	if err = db.Migrator().CreateTable(&MyGormData{}); err != nil {
		panic(err.Error())
	}

	if err = db.Create(&MyGormData{}).Error; err != nil {
		panic(err.Error())
	}
	if err = db.Create(&MyGormData{
		MyID: -123,
	}).Error; err != nil {
		panic(err.Error())
	}

	var result []MyGormData
	out = db.Find(&result)
	if out.Error != nil {
		panic(err.Error())
	}
	fmt.Printf("%v", result)
}

type MyGormData struct {
	MyID int64    `gorm:"primaryKey;column:dataid"`
	X    bool     `gorm:"column:xvalue;default:true;not null"`
	Y    *float64 `gorm:"column:yvalue;default:123;not null"`
}

func (MyGormData) TableName() string {
	return "gorm_test"
}
