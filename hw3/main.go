package main

import (
	"fmt"
	"html/template"
	"strings"
)

type DocumentGenerator interface {
	PrepareData() string
	FormatContent(data string) string
	Save(content string) string
}

type BaseGenerator struct{}

func (b *BaseGenerator) Generate(doc DocumentGenerator) string {
	data := doc.PrepareData()
	formatted := doc.FormatContent(data)
	return doc.Save(formatted)
}

type TextDocument struct{}

func (td TextDocument) PrepareData() string {
	return "This is the raw text data"
}

func (td TextDocument) FormatContent(data string) string {
	return fmt.Sprintf("Formatted Text: %s", data)
}

func (td TextDocument) Save(content string) string {
	return fmt.Sprintf("Saving text document: %s", content)
}

type HTMLDocument struct{}

func (hd HTMLDocument) PrepareData() string {
	return "<html><body>This is raw HTML data.</body></html>"
}

func (hd HTMLDocument) FormatContent(data string) string {
	out := &strings.Builder{}
	t := template.Must(template.New("doc").Parse(`<div>{{.}}</div>`))
	err := t.Execute(out, data)
	if err != nil {
		panic(err)
	}
	return out.String()
}

func (hd HTMLDocument) Save(content string) string {
	return fmt.Sprintf("Saving text document: %s", content)
}
