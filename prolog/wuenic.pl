:- module(wuenic, [ test_wuenic/0 ]).

:- use_module(wuenic_bgd).

test_wuenic :-
  true.

:- begin_tests(wuenic).
% :- use_module(library(wuenic)).

test(wuenic) :-
  test_wuenic.

:- end_tests(wuenic).
