module github.com/ynishiza/myapp

go 1.23

toolchain go1.24.3

require (
	github.com/go-playground/validator/v10 v10.25.0
	github.com/jackc/pgx/v5 v5.7.2
	github.com/ynishiza/lib v1.0.0
	github.com/ynishiza/mymodule v1.0.0
	golang.org/x/mod v0.23.0
	golang.org/x/net v0.34.0
	golang.org/x/xerrors v0.0.0-20240903120638-7835f813f4da
	gorm.io/driver/postgres v1.5.11
	gorm.io/gorm v1.25.12
)

require (
	github.com/stretchr/testify v1.8.4
	golang.org/x/text v0.21.0
)

require (
	github.com/davecgh/go-spew v1.1.1 // indirect
	github.com/gabriel-vasile/mimetype v1.4.8 // indirect
	github.com/go-playground/locales v0.14.1 // indirect
	github.com/go-playground/universal-translator v0.18.1 // indirect
	github.com/jackc/pgpassfile v1.0.0 // indirect
	github.com/jackc/pgservicefile v0.0.0-20240606120523-5a60cdf6a761 // indirect
	github.com/jackc/puddle/v2 v2.2.2 // indirect
	github.com/jinzhu/inflection v1.0.0 // indirect
	github.com/jinzhu/now v1.1.5 // indirect
	github.com/kr/text v0.2.0 // indirect
	github.com/leodido/go-urn v1.4.0 // indirect
	github.com/pmezard/go-difflib v1.0.0 // indirect
	github.com/rogpeppe/go-internal v1.14.1 // indirect
	golang.org/x/crypto v0.32.0 // indirect
	golang.org/x/sync v0.10.0 // indirect
	golang.org/x/sys v0.29.0 // indirect
	gopkg.in/yaml.v3 v3.0.1 // indirect
)

replace github.com/ynishiza/lib => ../lib

replace github.com/ynishiza/mymodule => ../mymodule
