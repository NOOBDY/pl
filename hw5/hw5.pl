% ---------- Class Hierarchy ----------
class(lane).
class(stage).
class(swimlane).

subclass(stage, lane).
subclass(swimlane, lane).

concrete_class(stage).
concrete_class(swimlane).

% ---------- Dynamic Facts ----------
:- dynamic instance/2.     % instance(Name, Class)
:- dynamic child_of/2.     % child_of(Child, Parent)

% ---------- Instance Creation ----------
new_instance(Name, Class) :-
    concrete_class(Class),
    \+ instance(Name, _),
    assertz(instance(Name, Class)).

new_instance(_, Class) :-
    \+ concrete_class(Class),
    false.

% ---------- Class Retrieval ----------
get_class(Instance, Class) :-
    instance(Instance, Class).

% ---------- Validity Checks ----------
same_class(Parent, Child) :-
    get_class(Parent, Class),
    get_class(Child, Class).

same_concrete_class(Parent, Children) :-
    get_class(Parent, Class),
    forall(member(C, Children), get_class(C, Class)).

% ---------- Child Addition ----------
add_child(Parent, Child) :-
    (Parent = Child ->
        writeln('Child not added: cannot add a child to itself'), !
    ; is_descendant(Child, Parent) ->
        writeln('Child not added: cannot add an ancestor as a new child'), !
    ; is_descendant(Parent, Child) ->
        writeln('Child not added: cannot add a descendant as a new child'), !
    ; child_of(Child, OtherParent), OtherParent \= Parent ->
        writeln('Child not added: cannot add a child that is already a child of another parent'), !
    ; \+ same_class(Parent, Child) ->
        writeln('Child not added: cannot add a child with different class'), !
    ; assertz(child_of(Child, Parent))
    ).

% ---------- Children and Descendants ----------
children(Parent, Children) :-
    findall(Child, child_of(Child, Parent), Children).

descendants(Parent, Descendants) :-
    descendants_helper(Parent, [], Descendants).

descendants_helper(Parent, Visited, Descendants) :-
    children(Parent, Children),
    subtract(Children, Visited, NewChildren),
    foldl(descendants_helper, NewChildren, NewChildren, MoreDescendants),
    append(NewChildren, MoreDescendants, All),
    list_to_set(All, Descendants).

% ---------- Cycle Detection ----------
is_descendant(Ancestor, Descendant) :-
    child_of(Descendant, Ancestor).
is_descendant(Ancestor, Descendant) :-
    child_of(Descendant, Intermediate),
    is_descendant(Ancestor, Intermediate).

% ---------- Valid Children Check ----------
valid_children(Instance) :-
    children(Instance, Children),
    (Children == [] -> true;
     get_class_list(Children, Classes),
     list_to_set(Classes, [_])).

get_class_list([], []).
get_class_list([H|T], [C|CT]) :-
    get_class(H, C),
    get_class_list(T, CT).

