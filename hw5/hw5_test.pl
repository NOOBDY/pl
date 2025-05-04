:- use_module(library(plunit)).

:- begin_tests(hw5_test).

:- dynamic instance/2.
:- dynamic child_of/2.

:- consult('hw5.pl').

% --- Setup & Teardown ---
setup :-
    retractall(instance(_, _)),
    retractall(child_of(_, _)).

% --- Test Instance Creation ---
test(new_instance_success, [setup(setup)]) :-
    once(new_instance(i1, stage)),
    instance(i1, stage).

test(new_instance_fail_abstract_class, [fail, setup(setup)]) :-
    new_instance(i2, lane).

% --- Test Add Child Valid ---
test(add_valid_child_same_class, [setup(setup)]) :-
    once(new_instance(p1, stage)),
    once(new_instance(c1, stage)),
    add_child(p1, c1),
    child_of(c1, p1).

% --- Test Children Retrieval ---
test(children_list, [setup(setup)]) :-
    once(new_instance(p2, stage)),
    once(new_instance(c2a, stage)),
    once(new_instance(c2b, stage)),
    add_child(p2, c2a),
    add_child(p2, c2b),
    children(p2, L),
    sort(L, Sorted),
    sort([c2a, c2b], Expected),
    Sorted == Expected.

% --- Test Descendants ---
test(descendants_deep, [setup(setup)]) :-
    once(new_instance(s1, stage)),
    once(new_instance(s2, stage)),
    once(new_instance(s3, stage)),
    once(add_child(s1, s2)),
    once(add_child(s2, s3)),
    descendants(s1, D),
    sort(D, Sorted),
    sort([s2, s3], Sorted).

% --- Test Cycles Prevention ---
test(add_child_self, [fail, setup(setup)]) :-
    new_instance(a1, stage),
    \+ add_child(a1, a1).

test(add_child_to_ancestor, [fail, setup(setup)]) :-
    new_instance(a2, stage),
    new_instance(a3, stage),
    add_child(a2, a3),
    \+ add_child(a3, a2).

% --- Test Only One Parent ---
test(add_child_already_has_parent, [fail, setup(setup)]) :-
    new_instance(p3, stage),
    new_instance(p4, stage),
    new_instance(c3, stage),
    add_child(p3, c3),
    \+ add_child(p4, c3),
    children(p3, [c3]),
    children(p4, []).

% --- Test Valid Children ---
test(valid_children_true, [setup(setup)]) :-
    once(new_instance(p5, swimlane)),
    once(new_instance(c4, swimlane)),
    once(new_instance(c5, swimlane)),
    once(add_child(p5, c4)),
    once(add_child(p5, c5)),
    valid_children(p5).

test(valid_children_empty, [setup(setup)]) :-
    once(new_instance(p6, stage)),
    valid_children(p6).

:- end_tests(hw5_test).

