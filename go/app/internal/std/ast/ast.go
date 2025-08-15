package ast

import (
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"log"
	"os"
	"path/filepath"
	"strings"
)

func Test() {
	workDir, err := os.Getwd()
	if err != nil {
		log.Panicf("%w", err)
	}
	codeDir := filepath.Join(workDir, "internal", "std", "ast")
	log.Printf("workDir %v", workDir)

	fs := token.NewFileSet()
	parsed, err := parser.ParseDir(fs, codeDir, nil, parser.ParseComments)
	if err != nil {
		log.Panicf("%w", err)
	}

	for pkgName, pkg := range parsed {
		for fileName, fileAst := range pkg.Files {
			var builder strings.Builder
			err = format.Node(&builder, fs, fileAst)
			if err != nil {
				log.Panicf("%w", err)
			}

			cmts := ast.NewCommentMap(fs, fileAst, fileAst.Comments)

			log.Printf("[package name: %s, file name: %s]\n %s\n",
				pkgName,
				fileName,
				builder.String(),
			)

			for _, d := range fileAst.Decls {
				typeComments := cmts[d]
				var typeCommentText string
				for _, c := range typeComments {
					typeCommentText += c.Text()
				}

				ast.Inspect(d, func(n ast.Node) bool {
					switch m := n.(type) {
					case *ast.TypeSpec:
						var builder strings.Builder
						err = format.Node(&builder, fs, m)
						if err != nil {
							log.Panicf("%w", err)
						}
						log.Printf("[package name: %s, file name: %s  type: %s]\n ```%s```\n ```%v```\n",
							pkgName,
							fileName,
							m.Name,
							typeCommentText,
							builder.String(),
						)
						log.Printf(cmts.String())

						// struct types
						if str, ok := m.Type.(*ast.StructType); ok {
							for _, f := range str.Fields.List {
								log.Printf("field:%s  doc: %s  comment: %s", f.Names[0].Name, f.Doc.Text(), f.Comment.Text())
							}
						}
					}
					return true
				})
			}
		}
	}

}
