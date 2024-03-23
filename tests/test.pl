file_search_path(purity, '../src').

:- multifile(test/2).

:- ensure_loaded(test_purity).
:- ensure_loaded(test_plist).
:- ensure_loaded(pure_meta_interpretter).

report(true, Test) :-
    format('Test ~w ok~n', Test).
report(false, Test) :- 
    format('Test ~w FAILED~n', Test).

do_tests :-
    mi(test(Test, Result)),
    report(Result, Test),
    fail.
do_tests.

    