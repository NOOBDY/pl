word_frequencies(File, StopWordsFile) :-
    read_file_to_string(File, Text, []),
    read_stop_words(StopWordsFile, StopWords),
    filter_chars_and_normalize(Text, FilteredText),
    scan(FilteredText, WordList),
    remove_stop_words(WordList, StopWords, FilteredWordList),
    frequencies(FilteredWordList, WordFreq),
    sorted(WordFreq, SortedWordFreq),
    print_top_25(SortedWordFreq).

read_stop_words(File, StopWords) :-
    read_file_to_string(File, Content, []),
    split_string(Content, ",", ",", Words),
    findall(_, (between(97, 122, C), char_code(Char, C), atom_string(Char, S)), Letters),
    append(Words, Letters, StopWords).

filter_chars_and_normalize(Text, FilteredText) :-
    string_lower(Text, Lower),
    string_chars(Lower, Chars),
    maplist(normalize_char, Chars, NormalizedChars),
    atomic_list_concat(NormalizedChars, '', Filtered0),
    atom_string(Filtered0, FilteredText).

normalize_char(C, R) :-
    char_type(C, alnum), !, R = C.
normalize_char(_, ' ').

scan(Text, WordList) :-
    split_string(Text, " ", " ", Words),
    exclude(==( ""), Words, WordList).

remove_stop_words([], _, []).
remove_stop_words([H|T], StopWords, Result) :-
    member(H, StopWords), !,
    remove_stop_words(T, StopWords, Result).
remove_stop_words([H|T], StopWords, [H|Rest]) :-
    remove_stop_words(T, StopWords, Rest).

frequencies(WordList, WordFreq) :-
    frequencies_helper(WordList, [], WordFreq).

frequencies_helper([], Acc, Acc).
frequencies_helper([H|T], Acc, Result) :-
    ( select(H-N, Acc, Rest) ->
        N1 is N + 1,
        NewAcc = [H-N1 | Rest]
    ;
        NewAcc = [H-1 | Acc]
    ),
    frequencies_helper(T, NewAcc, Result).

sorted(WordFreq, Sorted) :-
    predsort(compare_freq, WordFreq, Sorted).

compare_freq(Delta, _-Count1, _-Count2) :-
    compare(C, Count2, Count1),
    Delta = C.

print_top_25(List) :-
    take(25, List, Top),
    print_list(Top).

take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [H|T], [H|Rest]) :-
    N1 is N - 1,
    take(N1, T, Rest).

print_list([]).
print_list([Word-Freq | T]) :-
    format("~w: ~d~n", [Word, Freq]),
    print_list(T).

