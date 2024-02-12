:- use_module(library(semweb/rdf11)).

%Load schema
:- rdf_load('ontology_test.rdf').

%Register schema IRI
:- rdf_register_prefix(onto, 'http://www.semanticweb.org/test_children#').

/*
Query for starting the rule engine
    start_engine.
    or
    assert_all([deduce_is_child, deduce_same_age_as, deduce_symmetrical_property]).

Query for saving inferred ontology
    rdf_save('inferred_onto.rdf').
*/

/*********[Rule engine]***********/
%Calls every rule in the list
assert_all([Rule|Rest]) :-
    assert(Rule),
    assert_all(Rest).
%End case
assert_all([]).

%Saves in the rdf schema, triples inferred by called "function" (goal)
assert(Function) :-
    call(Function, Subject, Predicate, Object),
    \+ rdf(Subject, Predicate, Object),
    rdf_assert(Subject, Predicate, Object),
    format('asserted ~w ~w ~w~n', [Subject, Predicate, Object]).

%To continue asserting other rules when one fails to assert anything
assert(_).
/*********************************/



/************[Rules]**************/
% Person(X) ^ age(X, Age) ^ LessThan(Age, 18) -> Child(X) 
deduce_is_child(Person, Predicate, Object) :-
    Predicate = rdf:type,
    rdf_canonical_literal(onto:'Child', Object), 
    rdf(Person, Predicate, onto:'Person'),
    rdf(Person, onto:'age', Age),
    arg(1, Age, AgeValue),
    AgeValue < 18.

%Person(X) ^ same_age_as(X,Y) ^ Person(Y) ^ age(Y, Age) -> age(X, Age)
deduce_same_age_as(Person, Predicate, Age) :-
    Predicate = onto:'age',
    rdf(Person, onto:'same_age_as', OtherPerson),
    rdf(OtherPerson, Predicate, Age).

% This is to recreate owl symmetric property description
deduce_symmetrical_property(Subject, Property, Object) :-
    rdf(Property, rdf:type, owl:'SymmetricProperty'),
    rdf(Object, Property, Subject).
/*********************************/

% Declare all the rules in the rule list
rules([deduce_is_child, deduce_same_age_as, deduce_symmetrical_property]).

start_engine :-
    print('running'),
    rules(RuleList),
    assert_all(RuleList).

