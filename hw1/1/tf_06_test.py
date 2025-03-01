import pytest
from tf_06 import (
    read_file,
    filter_chars_and_normalize,
    scan,
    remove_stop_words,
    frequencies,
    sort,
    print_all,
)


def test_read_file(tmp_path):
    # Create a temporary file
    file_path = tmp_path / "test.txt"
    with open(file_path, "w") as f:
        f.write("Hello, World!")

    # Test reading the file
    content = read_file(file_path)
    assert content == "Hello, World!"


def test_read_file_nonexistent_file():
    # Test reading a nonexistent file
    with pytest.raises(FileNotFoundError):
        read_file("nonexistent_file.txt")


def test_filter_chars_and_normalize():
    # Test filtering and normalizing
    input_str = "Hello, World! How's it going?"
    expected_output = "hello world how s it going "
    assert filter_chars_and_normalize(input_str) == expected_output


def test_scan():
    # Test scanning for words
    input_str = "Hello World this is a test"
    expected_output = ["Hello", "World", "this", "is", "a", "test"]
    assert scan(input_str) == expected_output


def test_remove_stop_words(tmp_path):
    # Create a temporary file with stop words
    stop_words_file_path = tmp_path / "stop_words.txt"
    with open(stop_words_file_path, "w") as f:
        f.write("the,a,an")

    # Test removing stop words
    word_list = ["the", "quick", "brown", "fox", "a", "jumps"]
    expected_output = ["quick", "brown", "fox", "jumps"]
    assert remove_stop_words(word_list)(stop_words_file_path) == expected_output


def test_frequencies():
    # Test calculating word frequencies
    word_list = ["apple", "banana", "apple", "orange", "banana", "banana"]
    expected_output = {"apple": 2, "banana": 3, "orange": 1}
    assert frequencies(word_list) == expected_output


def test_sort():
    # Test sorting word frequencies
    word_freq = {"apple": 2, "banana": 3, "orange": 1}
    expected_output = [("banana", 3), ("apple", 2), ("orange", 1)]
    assert sort(word_freq) == expected_output


def test_print_all(capsys):
    # Test printing word frequencies
    word_freqs = [("banana", 3), ("apple", 2), ("orange", 1)]
    print_all(word_freqs)
    captured = capsys.readouterr()
    expected_output = "banana - 3\napple - 2\norange - 1\n"
    assert captured.out == expected_output


def test_print_all_empty_list(capsys):
    # Test printing an empty list
    print_all([])
    captured = capsys.readouterr()
    assert captured.out == ""
