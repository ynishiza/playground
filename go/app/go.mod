module github.com/ynishiza/hello

go 1.22.0

require (
	github.com/ynishiza/lib v0.0.0-00010101000000-000000000000
	golang.org/x/net v0.34.0
	golang.org/x/mod v0.23.0
	golang.org/x/sys v0.29.0 // indirect
)

replace github.com/ynishiza/lib => ../lib
