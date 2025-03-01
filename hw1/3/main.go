package main

import (
	"fmt"
	"os"
	"regexp"
	"sort"
	"strings"
)

// Function to read the entire contents of a file as a string
func readFile(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	return string(data), nil
}

// Function to replace non-alphanumeric characters with spaces and convert to lowercase
func filterCharsAndNormalize(strData string) string {
	re := regexp.MustCompile(`[\W_]+`)
	return re.ReplaceAllString(strings.ToLower(strData), " ")
}

// Function to split a string into words
func scan(strData string) []string {
	return strings.Fields(strData)
}

// Function to read stop words from a file and remove them from a word list
func removeStopWords(wordList []string, stopWordsPath string) ([]string, error) {
	stopWordsData, err := os.ReadFile(stopWordsPath)
	if err != nil {
		return nil, err
	}
	stopWords := strings.Split(string(stopWordsData), ",")

	// Add single-letter words
	for _, letter := range "abcdefghijklmnopqrstuvwxyz" {
		stopWords = append(stopWords, string(letter))
	}

	filteredWords := make([]string, 0)
	for _, word := range wordList {
		found := false
		for _, stopWord := range stopWords {
			if word == strings.TrimSpace(stopWord) {
				found = true
				break
			}
		}
		if !found {
			filteredWords = append(filteredWords, word)
		}
	}
	return filteredWords, nil
}

// Function to count word frequencies
func frequencies(wordList []string) map[string]int {
	wordFreqs := make(map[string]int)
	for _, word := range wordList {
		wordFreqs[word]++
	}
	return wordFreqs
}

// Function to sort word frequencies by frequency in descending order
func sortWordFreqs(wordFreq map[string]int) []WordFreqPair {
	var pairs []WordFreqPair
	for word, freq := range wordFreq {
		pairs = append(pairs, WordFreqPair{Word: word, Freq: freq})
	}
	sort.Slice(pairs, func(i, j int) bool {
		return pairs[i].Freq > pairs[j].Freq
	})
	return pairs
}

// Function to print word frequencies recursively
func printAll(wordFreqs []WordFreqPair) {
	if len(wordFreqs) > 0 {
		fmt.Printf("%s - %d\n", wordFreqs[0].Word, wordFreqs[0].Freq)
		printAll(wordFreqs[1:])
	}
}

// Helper struct for word frequency pairs
type WordFreqPair struct {
	Word string
	Freq int
}

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage:", os.Args[0], "filename")
		return
	}

	filePath := os.Args[1]
	stopWordsPath := "../stop_words.txt"

	// Read file
	data, err := readFile(filePath)
	if err != nil {
		fmt.Println(err)
		return
	}

	// Normalize and scan for words
	normalizedData := filterCharsAndNormalize(data)
	words := scan(normalizedData)

	// Remove stop words
	filteredWords, err := removeStopWords(words, stopWordsPath)
	if err != nil {
		fmt.Println(err)
		return
	}

	// Count frequencies and sort
	wordFreqs := frequencies(filteredWords)
	sortedWordFreqs := sortWordFreqs(wordFreqs)

	// Print top 25 words
	if len(sortedWordFreqs) > 25 {
		printAll(sortedWordFreqs[:25])
	} else {
		printAll(sortedWordFreqs[:])
	}
}
