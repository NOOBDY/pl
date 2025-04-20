# pl2025s_HW4

In this homework, you will implement the word frequency counter program by using Prolog. 

## Specification

The program has the following functor:
```prolog
word_frequencies(+File, +StopWordsFile)

read_stop_words(+File, -StopWords)

filter_chars_and_normalize(+Text, -FilteredText)

scan(+Text, -WordList)

remove_stop_words(+WordList, +StopWords, -FilteredWordList)

frequencies(+WordList, -WordFreq)

sorted(+WordList, -SortedWordList)
```

- `word_frequencies(+File, +StopWordsFile)` reads the text file and stop words file, counts the word frequencies, and prints the result of the top 25.

    - Example:
        ```txt
        input.txt:
        This is a test, only a test.
        ```

        ```txt
        tmp_stop_words.txt:
        a,is,the
        ```

        ```prolog
        ?- word_frequencies('input.txt', 'tmp_stop_words.txt').
        test: 2
        only: 1
        ```

- `read_stop_words(+File, -StopWords)` reads the stop words from the file and returns a list of stop words containing alphabets a-z.

    - Example:
        ```prolog
        ?- read_stop_words('tmp_stop_words.txt', StopWords).
        StopWords = ["a","is","the","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]
        ```

- `filter_chars_and_normalize(+Text, -FilteredText)` filters the text, removes punctuation, and normalizes it to lowercase.

    - Example:
        ```prolog
        ?- filter_chars_and_normalize('This is a test, only a test.', FilteredText).
        FilteredText = "this is a test only a test "
        ```

- `scan(+Text, -WordList)` scans the filtered text and returns a list of words.

    - Example:
        ```prolog
        ?- scan("this is a test only a test ", WordList).
        WordList = ["this","is","a","test","only","a","test"]
        ```

- `remove_stop_words(+WordList, +StopWords, -FilteredWordList)` removes the stop words from the word list.

    - Example:
        ```prolog
        ?- remove_stop_words(["this","is","a","test","only","a","test"], ["a","is","the"], FilteredWordList).
        FilteredWordList = ["this", "test", "only", "test"]
        ```

- `frequencies(+WordList, -WordFreq)` counts the frequency of each word in the list and returns a list of pairs [\<word\>-\<frequency\>].

    - Example:
        ```prolog
        ?- frequencies(["test","only","test"], WordFreq).
        WordFreq = [only-1, test-2]
        ```

- `sorted(+WordList, -SortedWordList)` sorts the word list in descending order of frequency.

    - Example:
        ```prolog
        ?- sorted([only-1, test-2], SortedWordList).
        SortedWordList = [test-2, only-1]
        ```

## Installation
- Install SWI-Prolog (https://www.swi-prolog.org/download/devel).

## Execution

### Run Tests
```
swipl -f word_freq.pl word_freq_test.pl
?- run_tests.
```

Output:
```
[8/8] word_freq_test:word_frequencies_output ............................................................ passed (0.002 sec)
% All 8 tests passed in 0.033 seconds (0.018 cpu)
```

### Run Program
```
swipl -f word_freq.pl
?- word_frequencies('pride-and-prejudice.txt', 'stop_words.txt').
```
Output:
```
mr - 786
elizabeth - 635
very - 488
darcy - 418
such - 395
mrs - 343
much - 329
more - 327
bennet - 323
bingley - 306
jane - 295
miss - 283
one - 275
know - 239
before - 229
herself - 227
though - 226
well - 224
never - 220
sister - 218
soon - 216
think - 211
now - 209
time - 203
good - 201
true.
```

## Folder Structure
```
學號_姓名_hw4/
├── pride-and-prejudice.txt
├── stop_words.txt
├── word_freq.pl
├── word_freq_test.pl
└── <additional files required by your program>
```
## Grading Criteria
- Source code: (25%)
- Test code: (25%)
- Tests from TA: (30%)
- Main program: (20%)