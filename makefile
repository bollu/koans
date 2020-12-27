# https://clojure.org/guides/deps_and_cli
.PHONY: run

repl:
	clj -m nrepl.cmdline \
    --middleware "[cider.nrepl/cider-middleware]" \
    --interactive

run:
	clj -X dg/run
test: 
	clj -Atest
