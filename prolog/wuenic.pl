:- module(wuenic, [ test_wuenic/0 ]).

test_wuenic :-
  fail.

:- begin_tests(wuenic).
% :- use_module(library(wuenic)).

test(wuenic) :-
  test_wuenic.

:- end_tests(wuenic).
