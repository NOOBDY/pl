package main

import (
	"fmt"
	"strings"
	"testing"
)

func TestTextDocumentPrepareData(t *testing.T) {
	td := TextDocument{}
	data := td.PrepareData()
	if data != "This is the raw text data" {
		t.Errorf("Expected 'This is the raw text data', got '%s'", data)
	}
}

func TestTextDocumentFormatContent(t *testing.T) {
	td := TextDocument{}
	data := "raw data"
	formatted := td.FormatContent(data)
	if formatted != fmt.Sprintf("Formatted Text: %s", data) {
		t.Errorf("Expected 'Formatted Text: %s', got '%s'", data, formatted)
	}
}

func TestTextDocumentSave(t *testing.T) {
	td := TextDocument{}
	content := "formatted content"
	saved := td.Save(content)
	if saved != fmt.Sprintf("Saving text document: %s", content) {
		t.Errorf("Expected 'Saving text document: %s', got '%s'", content, saved)
	}
}

func TestHTMLDocumentPrepareData(t *testing.T) {
	hd := HTMLDocument{}
	data := hd.PrepareData()
	if data != "<html><body>This is raw HTML data.</body></html>" {
		t.Errorf("Expected '<html><body>This is raw HTML data.</body></html>', got '%s'", data)
	}
}

func TestHTMLDocumentFormatContent(t *testing.T) {
	hd := HTMLDocument{}
	data := "<html><body>This is raw HTML data.</body></html>"
	formatted := hd.FormatContent(data)
	if !strings.HasPrefix(formatted, "<div>") && !strings.HasSuffix(formatted, "</div>") {
		t.Errorf("Expected formatted content to be wrapped in <div>, got '%s'", formatted)
	}
}

func TestHTMLDocumentSave(t *testing.T) {
	hd := HTMLDocument{}
	content := "formatted content"
	saved := hd.Save(content)
	if saved != fmt.Sprintf("Saving text document: %s", content) {
		t.Errorf("Expected 'Saving text document: %s', got '%s'", content, saved)
	}
}

func TestBaseGeneratorGenerateTextDocument(t *testing.T) {
	bg := &BaseGenerator{}
	td := TextDocument{}
	result := bg.Generate(td)
	if !strings.HasPrefix(result, "Saving text document: Formatted Text: ") {
		t.Errorf("Expected result to start with 'Saving text document: Formatted Text: ', got '%s'", result)
	}
}

func TestBaseGeneratorGenerateHTMLDocument(t *testing.T) {
	bg := &BaseGenerator{}
	hd := HTMLDocument{}
	result := bg.Generate(hd)
	if !strings.HasPrefix(result, "Saving text document: <div>") {
		t.Errorf("Expected result to start with 'Saving text document: <div>', got '%s'", result)
	}
}
