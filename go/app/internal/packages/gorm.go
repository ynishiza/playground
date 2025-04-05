package packages

import (
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/jackc/pgx/v5/pgconn"
	"github.com/ynishiza/myapp/internal/utils"
	"gorm.io/driver/postgres"
	"gorm.io/gorm"
	"gorm.io/gorm/logger"
)

// Create db with
//
// $ createdb gormtest
var gormSettings = strings.Join([]string{
	"host=localhost",
	"user=gormtest",
	"dbname=gormtest",
}, " ")

var gormConfig = gorm.Config{
	TranslateError: true,
	Logger: logger.New(
		log.New(os.Stdout, "\n", log.LstdFlags),
		logger.Config{
			LogLevel: logger.Info,
		}),
}

func cleanupTable(db *gorm.DB, tableName string) {
	if db.Migrator().HasTable(tableName) {
		db.Migrator().DropTable(tableName)
	}
}

func TestGorm() {
	utils.PrintBanner("TestGrom")
	db, err := gorm.Open(postgres.Open(gormSettings), &gormConfig)
	if err != nil {
		panic(err.Error())
	}

	cleanupTable(db, "gormtable")

	fmt.Println("Connected")

	db.AutoMigrate(&MyEntry{})

	var value *MyEntry
	if err = db.
		Table("gormtable").
		First(&value).Error; err != nil {
		fmt.Printf("error %v", err.Error())
	}
	fmt.Println(*value)

	var values []MyEntry
	if err = db.
		Table("gormtable").
		Find(&values).Error; err != nil {
		fmt.Printf("error %v", err.Error())
	}
	fmt.Println(values)

	var maxId int64
	for _, v := range values {
		maxId = max(v.Id, maxId)
	}

	if err = db.Create(&MyEntry{
		Id:      maxId + 1,
		MyName:  "aa",
		MyValue: 1.234,
	}).Error; err != nil {
		fmt.Printf("error %v", err.Error())
	}
	if err = db.Create(&MyEntry{}).Error; err != nil {
		printf("%v %v", err == gorm.ErrDuplicatedKey, err.Error())
	}
	db.Create(&MyEntry{})
	db.Create(&MyEntry{})

	db2 := db.Table("gormtable").Session(&gorm.Session{})
	values = nil
	db2.
		Limit(2).
		Find(&values)
	printf("\n%v\n", values)

	// db2 = db2.Session(&gorm.Session{}).Debug()
	values = nil
	db2.
		Find(&values)
	printf("\n%v\n", values)

	// Raw
	if err = db.Exec(`
	INSERT INTO gormtable values ($1, $2)
	`, 2345, "KLDJF").Error; err != nil {
		panic(err.Error())
	}
	if err = db.Raw(`
	SELECT * FROM gormtable WHERE id > $1
	`, 0).Scan(&values).Error; err != nil {
		panic(err.Error())
	}
	printf("raw: %v", values)
}

func TestGormTransacion() {
	db, err := gorm.Open(postgres.Open(gormSettings), &gormConfig)
	if err != nil {
		panic(err.Error())
	}

	// ErrInvalidTransaction
	// if err = db.Commit().Error; err == gorm.ErrInvalidTransaction {
	// 	panic(err.Error())
	// }
	tx := db.Begin()
	tx.Create(&MyEntry{Id: 1234})
	tx.Rollback()
	tx = db.Begin()
	tx.Create(&MyEntry{Id: 2345})
	tx.Commit()
	if err = tx.Commit().Error; err != nil {
		printf("%T", err)
		panic(err.Error())
	}

	err = db.Transaction(func(tx *gorm.DB) error {
		tx.Create(&MyEntry{Id: -134})
		return fmt.Errorf("OOPS")
	})
	printf("%v", err)
	err = db.Transaction(func(tx *gorm.DB) error {
		tx.Create(&MyEntry{Id: -235})
		return nil
	})
	printf("%v", err)
}

func TestGormError() {
	utils.PrintBanner("TestGormError")
	db, err := gorm.Open(postgres.Open(gormSettings), &gormConfig)
	if err != nil {
		panic(err.Error())
	}

	if err = db.Exec(`
		INSERT INTO gormtable (id) VALUES (1)
	`).Error; err != nil {
		if pgerror, ok := err.(*pgconn.PgError); ok {
			printf("pgerror[%v]: %v", pgerror.Code, pgerror.Error())
		} else {
			printf(err.Error())
		}
	}
}

type MyEntry struct {
	// myID    int64   `gorm:"column:id"`
	Id      int64   `gorm:"column:id;not null"`
	MyName  string  `gorm:"column:name"`
	MyValue float64 `gorm:"column:value"`
}

func (MyEntry) TableName() string {
	return "gormtable"
}
