file_search_path(purity, '../prolog').

:- ensure_loaded(test_purity).
:- ensure_loaded(pure_meta_interpretter).

:- multifile(test/2).

do_tests :-
    mi(test(Test, Result)),
    writeln(Test:Result),
    fail.
do_tests.

    