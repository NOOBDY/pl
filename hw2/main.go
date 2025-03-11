package main

import (
	"fmt"
	"log"
	"os"
	"regexp"
	"sort"
	"strings"
)

type DataStorageManager struct {
	data string
}

func NewDataStorageManager(pathToFile string) (DataStorageManager, error) {
	data, err := os.ReadFile(pathToFile)
	if err != nil {
		return DataStorageManager{}, err
	}

	pattern := regexp.MustCompile(`[\W_]+`)
	res := strings.ToLower(pattern.ReplaceAllString(string(data), " "))

	return DataStorageManager{
		data: res,
	}, nil
}

func (d DataStorageManager) Words() []string {
	return strings.Split(d.data, " ")
}

type StopWordManager struct {
	stopWords []string
}

func NewStopWordManager() (StopWordManager, error) {
	data, err := os.ReadFile("./stop_words.txt")
	if err != nil {
		return StopWordManager{}, err
	}

	words := strings.Split(string(data), ",")
	for _, letter := range "abcdefghijklmnopqrstuvwxyz" {
		words = append(words, string(letter))
	}

	return StopWordManager{
		stopWords: words,
	}, nil
}

func (s StopWordManager) IsStopWord(word string) bool {
	for _, stopWord := range s.stopWords {
		if word == stopWord {
			return true
		}
	}
	return false
}

type WordFrequencyManager struct {
	wordFreqs map[string]int
}

func NewWordFrequencyManager() WordFrequencyManager {
	return WordFrequencyManager{
		wordFreqs: make(map[string]int),
	}
}

func (w WordFrequencyManager) IncrementCount(word string) {
	_, ok := w.wordFreqs[word]
	if ok {
		w.wordFreqs[word] += 1
	} else {
		w.wordFreqs[word] = 1
	}
}

func (w WordFrequencyManager) Sorted() []WordFreqPair {
	pairs := make([]WordFreqPair, len(w.wordFreqs))
	i := 0
	for k, v := range w.wordFreqs {
		pairs[i] = WordFreqPair{Word: k, Freq: v}
		i++
	}

	sort.SliceStable(pairs, func(i, j int) bool {
		return pairs[i].Freq > pairs[j].Freq
	})

	return pairs
}

type WordFreqPair struct {
	Word string
	Freq int
}

type WordFrequencyController struct {
	storageManager  DataStorageManager
	stopWordmanger  StopWordManager
	wordFreqManager WordFrequencyManager
}

func NewWordFrequencyController(pathToFile string) (WordFrequencyController, error) {
	storageManager, err := NewDataStorageManager(pathToFile)
	if err != nil {
		return WordFrequencyController{}, err
	}
	stopWordManager, err := NewStopWordManager()
	if err != nil {
		return WordFrequencyController{}, err

	}
	wordFreqManager := NewWordFrequencyManager()

	return WordFrequencyController{
		storageManager:  storageManager,
		stopWordmanger:  stopWordManager,
		wordFreqManager: wordFreqManager,
	}, nil
}

func (w WordFrequencyController) run() {
	for _, word := range w.storageManager.Words() {
		if !w.stopWordmanger.IsStopWord(word) {
			w.wordFreqManager.IncrementCount(word)
		}
	}
	wordFreqs := w.wordFreqManager.Sorted()

	printAll := func(wordFreqs []WordFreqPair) {
		for _, f := range wordFreqs {
			fmt.Printf("%s - %d", f.Word, f.Freq)
		}
	}
	if len(wordFreqs) > 25 {
		printAll(wordFreqs[:25])
	} else {
		printAll(wordFreqs)
	}
}

func main() {
	controller, err := NewWordFrequencyController(os.Args[1])
	if err != nil {
		log.Fatalln(err)
	}
	controller.run()
}
