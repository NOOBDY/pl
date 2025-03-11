package main

import (
	"bytes"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

func TestNewDataStorageManager(t *testing.T) {
	tmpFile, err := os.CreateTemp("", "test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmpFile.Name())

	_, err = tmpFile.WriteString("Hello, World!")
	if err != nil {
		t.Fatal(err)
	}
	tmpFile.Close()

	dataStorageManager, err := NewDataStorageManager(tmpFile.Name())
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}

	pattern := regexp.MustCompile(`[\W_]+`)
	res := strings.ToLower(pattern.ReplaceAllString("Hello, World!", " "))

	if dataStorageManager.data != res {
		t.Errorf("Expected data to be '%s', got '%s'", res, dataStorageManager.data)
	}
}

func TestDataStorageManagerWords(t *testing.T) {
	dataStorageManager := DataStorageManager{
		data: "hello world",
	}

	words := dataStorageManager.Words()
	if len(words) != 2 || words[0] != "hello" || words[1] != "world" {
		t.Errorf("Expected words to be ['hello', 'world'], got %v", words)
	}
}

func TestNewStopWordManager(t *testing.T) {
	tmpFile, err := os.CreateTemp("", "stop_words")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmpFile.Name())

	_, err = tmpFile.WriteString("a,b,c")
	if err != nil {
		t.Fatal(err)
	}
	tmpFile.Close()

	tmpDir, err := os.MkdirTemp("", "stop_words_dir")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	err = os.Rename(tmpFile.Name(), filepath.Join(tmpDir, "stop_words.txt"))
	if err != nil {
		t.Fatal(err)
	}

	// Change the current directory to the temp directory
	origDir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	defer os.Chdir(origDir)
	err = os.Chdir(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	stopWordManager, err := NewStopWordManager()
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}

	if len(stopWordManager.stopWords) != 29 {
		t.Errorf("Expected 29 stop words, got %d", len(stopWordManager.stopWords))
	}

	for _, letter := range "abcdefghijklmnopqrstuvwxyz" {
		if !contains(stopWordManager.stopWords, string(letter)) {
			t.Errorf("Expected '%s' to be in stop words", string(letter))
		}
	}
}

func TestStopWordManagerIsStopWord(t *testing.T) {
	stopWordManager, err := NewStopWordManager()
	if err != nil {
		t.Fatal(err)
	}

	if !stopWordManager.IsStopWord("a") {
		t.Errorf("Expected 'a' to be a stop word")
	}

	if stopWordManager.IsStopWord("hello") {
		t.Errorf("Expected 'hello' not to be a stop word")
	}
}

func TestNewWordFrequencyManager(t *testing.T) {
	wordFreqManager := NewWordFrequencyManager()
	if len(wordFreqManager.wordFreqs) != 0 {
		t.Errorf("Expected empty map, got %v", wordFreqManager.wordFreqs)
	}
}

func TestWordFrequencyManagerIncrementCount(t *testing.T) {
	wordFreqManager := NewWordFrequencyManager()
	wordFreqManager.IncrementCount("hello")
	wordFreqManager.IncrementCount("hello")
	wordFreqManager.IncrementCount("world")

	if wordFreqManager.wordFreqs["hello"] != 2 {
		t.Errorf("Expected 'hello' count to be 2, got %d", wordFreqManager.wordFreqs["hello"])
	}

	if wordFreqManager.wordFreqs["world"] != 1 {
		t.Errorf("Expected 'world' count to be 1, got %d", wordFreqManager.wordFreqs["world"])
	}
}

func TestWordFrequencyManagerSorted(t *testing.T) {
	wordFreqManager := NewWordFrequencyManager()
	wordFreqManager.IncrementCount("hello")
	wordFreqManager.IncrementCount("hello")
	wordFreqManager.IncrementCount("world")

	sorted := wordFreqManager.Sorted()
	if len(sorted) != 2 {
		t.Errorf("Expected 2 word frequency pairs, got %d", len(sorted))
	}

	if sorted[0].Word != "hello" || sorted[0].Freq != 2 {
		t.Errorf("Expected first pair to be {hello, 2}, got {%s, %d}", sorted[0].Word, sorted[0].Freq)
	}

	if sorted[1].Word != "world" || sorted[1].Freq != 1 {
		t.Errorf("Expected second pair to be {world, 1}, got {%s, %d}", sorted[1].Word, sorted[1].Freq)
	}
}

func TestNewWordFrequencyController(t *testing.T) {
	tmpFile, err := os.CreateTemp("", "test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmpFile.Name())

	_, err = tmpFile.WriteString("Hello, World!")
	if err != nil {
		t.Fatal(err)
	}
	tmpFile.Close()

	controller, err := NewWordFrequencyController(tmpFile.Name())
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}

	if controller.storageManager.data == "" {
		t.Errorf("Expected storage manager to have data")
	}

	if len(controller.stopWordmanger.stopWords) == 0 {
		t.Errorf("Expected stop word manager to have stop words")
	}

	if len(controller.wordFreqManager.wordFreqs) != 0 {
		t.Errorf("Expected word frequency manager to have empty map")
	}
}

func TestWordFrequencyControllerRun(t *testing.T) {
	tmpFile, err := os.CreateTemp("", "test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmpFile.Name())

	_, err = tmpFile.WriteString("hello hello world")
	if err != nil {
		t.Fatal(err)
	}
	tmpFile.Close()

	controller, err := NewWordFrequencyController(tmpFile.Name())
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}

	// Capture output
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	controller.run()

	w.Close()
	out, _ := io.ReadAll(r)
	os.Stdout = oldStdout

	if !bytes.Contains(out, []byte("hello - 2")) {
		t.Errorf("Expected output to contain 'hello - 2', got %s", out)
	}

	if !bytes.Contains(out, []byte("world - 1")) {
		t.Errorf("Expected output to contain 'world - 1', got %s", out)
	}
}

func contains(s []string, e string) bool {
	for _, a := range s {
		if a == e {
			return true
		}
	}
	return false
}
