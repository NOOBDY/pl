package main

import (
	"bytes"
	"io"
	"os"
	"reflect"
	"testing"
)

func TestReadFile(t *testing.T) {
	// Create a temporary file
	tmpFile, err := os.CreateTemp("", "test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmpFile.Name())

	// Write some content to the file
	content := "Hello, World!"
	_, err = tmpFile.WriteString(content)
	if err != nil {
		t.Fatal(err)
	}
	err = tmpFile.Close()
	if err != nil {
		t.Fatal(err)
	}

	// Read the file
	readContent, err := readFile(tmpFile.Name())
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}

	if readContent != content {
		t.Errorf("Expected content %q, got %q", content, readContent)
	}
}

func TestFilterCharsAndNormalize(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"Hello, World!", "hello world "},
		{"This is a test.", "this is a test "},
		{"123 Test!", "123 test "},
	}

	for _, test := range tests {
		result := filterCharsAndNormalize(test.input)
		if result != test.expected {
			t.Errorf("Expected %q, got %q", test.expected, result)
		}
	}
}

func TestScan(t *testing.T) {
	tests := []struct {
		input    string
		expected []string
	}{
		{"hello world", []string{"hello", "world"}},
		{"this is a test", []string{"this", "is", "a", "test"}},
		{"single", []string{"single"}},
	}

	for _, test := range tests {
		result := scan(test.input)
		if !reflect.DeepEqual(result, test.expected) {
			t.Errorf("Expected %v, got %v", test.expected, result)
		}
	}
}

func TestRemoveStopWords(t *testing.T) {
	// Create a temporary stop words file
	tmpStopWordsFile, err := os.CreateTemp("", "stopwords")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmpStopWordsFile.Name())

	// Write some stop words to the file
	stopWords := "the,a,an"
	_, err = tmpStopWordsFile.WriteString(stopWords)
	if err != nil {
		t.Fatal(err)
	}
	err = tmpStopWordsFile.Close()
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		input    []string
		expected []string
	}{
		{[]string{"hello", "the", "world"}, []string{"hello", "world"}},
		{[]string{"this", "is", "a", "test"}, []string{"this", "is", "test"}},
		{[]string{"single"}, []string{"single"}},
	}

	for _, test := range tests {
		result, err := removeStopWords(test.input, tmpStopWordsFile.Name())
		if err != nil {
			t.Errorf("Expected no error, got %v", err)
		}

		if !reflect.DeepEqual(result, test.expected) {
			t.Errorf("Expected %v, got %v", test.expected, result)
		}
	}
}

func TestFrequencies(t *testing.T) {
	tests := []struct {
		input    []string
		expected map[string]int
	}{
		{[]string{"hello", "world", "hello"}, map[string]int{"hello": 2, "world": 1}},
		{[]string{"this", "is", "a", "test", "this", "test"}, map[string]int{"this": 2, "is": 1, "a": 1, "test": 2}},
		{[]string{"single"}, map[string]int{"single": 1}},
	}

	for _, test := range tests {
		result := frequencies(test.input)
		if !reflect.DeepEqual(result, test.expected) {
			t.Errorf("Expected %v, got %v", test.expected, result)
		}
	}
}

func TestSortWordFreqs(t *testing.T) {
	tests := []struct {
		input    map[string]int
		expected []WordFreqPair
	}{
		{
			map[string]int{"hello": 2, "world": 1},
			[]WordFreqPair{{Word: "hello", Freq: 2}, {Word: "world", Freq: 1}},
		},
		{
			map[string]int{"this": 4, "is": 2, "a": 1, "test": 3},
			[]WordFreqPair{{Word: "this", Freq: 4}, {Word: "test", Freq: 3}, {Word: "is", Freq: 2}, {Word: "a", Freq: 1}},
		},
		{
			map[string]int{"single": 1},
			[]WordFreqPair{{Word: "single", Freq: 1}},
		},
	}

	for _, test := range tests {
		result := sortWordFreqs(test.input)
		if !reflect.DeepEqual(result, test.expected) {
			t.Errorf("Expected %v, got %v", test.expected, result)
		}
	}
}

func TestPrintAll(t *testing.T) {
	// This function is difficult to test directly because it prints to stdout.
	// However, we can test that it doesn't panic by calling it with some data.
	wordFreqs := []WordFreqPair{{Word: "test", Freq: 1}}
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	printAll(wordFreqs)

	w.Close()
	out, _ := io.ReadAll(r)
	os.Stdout = oldStdout

	if !bytes.Contains(out, []byte("test")) {
		t.Errorf("Expected output to contain 'test', got %s", out)
	}
}
