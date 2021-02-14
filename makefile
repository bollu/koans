# https://clojure.org/guides/deps_and_cli
.PHONY: run

tensor.out: 1-tensor.cpp
	clang++ 1-tensor.cpp -o tensor.out -Wall -Werror -fsanitize=address -fsanitize=undefined

repl:
	clj -m nrepl.cmdline \
    --middleware "[cider.nrepl/cider-middleware]" \
    --interactive

run:
	clj -X dg/run
test: 
	clj -Atest
