from tf_05 import (
    read_file,
    scan,
    frequencies,
)
import tf_05  # for global variables


def test_read_file_not_idempotent(tmp_path):
    tf_05.data = []

    # Create a temporary file
    file_path = tmp_path / "test_file.txt"
    with open(file_path, "w") as f:
        f.write("Hello, World!")

    # Initial state
    read_file(file_path)
    assert tf_05.data == list("Hello, World!")

    # Second call should append to data, not replace it
    read_file(file_path)
    assert tf_05.data == list(
        "Hello, World!Hello, World!"
    )  # Not idempotent because it appends


def test_scan_not_idempotent():
    tf_05.data = list("hello world ")
    tf_05.words = []
    scan()
    assert tf_05.words == ["hello", "world"]

    # Second call should append to words, not replace them
    scan()
    assert tf_05.words == [
        "hello",
        "world",
        "hello",
        "world",
    ]  # Not idempotent because it appends


def test_frequencies_not_idempotent():
    tf_05.words = ["hello", "world", "hello"]
    tf_05.word_freqs = []
    frequencies()
    assert tf_05.word_freqs == [["hello", 2], ["world", 1]]

    # Second call should increment frequencies, not replace them
    frequencies()
    assert tf_05.word_freqs == [
        ["hello", 4],
        ["world", 2],
    ]  # Not idempotent because it increments
