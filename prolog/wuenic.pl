:- module(wuenic, [ test_wuenic/0 ]).

test_wuenic :-
  true.

:- begin_tests(wuenic).
:- use_module(library(wuenic)).

test(test_wuenic) :-
  test_wuenic.

:- end_tests(wuenic).
