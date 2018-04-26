erl -pa _build/test/lib/*/ebin \
    -pa _build/test/lib/*/test \
    -noshell \
    -eval 'foil_bench:run()' \
    -eval 'init:stop()'
