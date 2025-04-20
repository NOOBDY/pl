:- use_module(library(plunit)).
:- use_module(word_freq).

:- begin_tests(word_freq_test).

test(read_stop_words,
     [ setup(setup_call_cleanup(
         open('tmp_stop_words.txt', write, S),
         write(S, "a,is,the"),
         close(S)
       )),
       cleanup(delete_file('tmp_stop_words.txt'))
     ]) :-
    read_stop_words('tmp_stop_words.txt', StopWords),
    memberchk("a", StopWords),
    memberchk("is", StopWords),
    memberchk("the", StopWords),
    memberchk("z", StopWords).

test(filter_chars_and_normalize) :-
    filter_chars_and_normalize("This is a test, only a test.", Out),
    Out == "this is a test  only a test ".

test(scan) :-
    scan("this is a test only a test ", WordList),
    WordList == ["this","is","a","test","only","a","test"].

test(remove_stop_words) :-
    remove_stop_words(["this","is","a","test","only","a","test"], ["a","is","the"], Filtered),
    Filtered == ["this", "test", "only", "test"].

test(frequencies) :-
    frequencies(["test","only","test"], Freq),
    memberchk("test"-2, Freq),
    memberchk("only"-1, Freq).

test(sorted) :-
    sorted(["only"-1, "test"-2], Sorted),
    Sorted == ["test"-2, "only"-1].

test(word_frequencies_output, [setup(
    (
        open('input.txt', write, S1),
        write(S1, 'This is a test, only a test.'),
        close(S1),
        open('tmp_stop_words.txt', write, S2),
        write(S2, 'a,is,the'),
        close(S2)
    )
), true]) :-
    word_frequencies('input.txt', 'tmp_stop_words.txt').

:- end_tests(word_freq_test).
