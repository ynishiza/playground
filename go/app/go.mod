module github.com/ynishiza/myapp

go 1.22.10

toolchain go1.23.6

require (
	github.com/go-playground/validator/v10 v10.25.0
	github.com/ynishiza/lib v1.0.0
	github.com/ynishiza/mymodule v1.0.0
	golang.org/x/mod v0.23.0
	golang.org/x/net v0.34.0
	golang.org/x/xerrors v0.0.0-20240903120638-7835f813f4da
)

require (
	github.com/davecgh/go-spew v1.1.1 // indirect
	github.com/gabriel-vasile/mimetype v1.4.8 // indirect
	github.com/go-playground/locales v0.14.1 // indirect
	github.com/go-playground/universal-translator v0.18.1 // indirect
	github.com/leodido/go-urn v1.4.0 // indirect
	github.com/pmezard/go-difflib v1.0.0 // indirect
	github.com/stretchr/testify v1.8.4
	golang.org/x/crypto v0.32.0 // indirect
	golang.org/x/sys v0.29.0 // indirect
	golang.org/x/text v0.21.0
	gopkg.in/yaml.v3 v3.0.1 // indirect
)

replace github.com/ynishiza/lib => ../lib

replace github.com/ynishiza/mymodule => ../mymodule
