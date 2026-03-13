# This makefile assumes that you have "lbm" installed on your path. You can
# install it by running `make install` in ../repl.

LBM=lbm

all: lbmref.md displayref.md runtimeref.md dynref.md ttfref.md mathref.md stringref.md arrayref.md dspref.md mutexref.md randomref.md setref.md cryptref.md

doclib.env: doclib.lisp
	$(LBM) -H 100000 -M 512000 --src="doclib.lisp" --store_env=doclib.env --terminate

lbmref.md: doclib.env lbmref.lisp
	$(LBM) -H 100000 -M 512000 --src="lbmref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

displayref.md: doclib.env displayref.lisp
	$(LBM) -H 150000 -M 640000 --src="displayref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

ttfref.md: doclib.env ttfref.lisp
	$(LBM) -H 100000 -M 1048576 --src="ttfref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

runtimeref.md: doclib.env runtimeref.lisp
	$(LBM) -H 100000 -M 512000 --src="runtimeref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

dynref.md: doclib.env dynref.lisp
	$(LBM) -H 100000 -M 512000 --src="dynref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

mathref.md: doclib.env mathref.lisp
	$(LBM) -H 100000 -M 512000 --src="mathref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

stringref.md: doclib.env stringref.lisp
	$(LBM) -H 100000 -M 512000 --src="stringref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

arrayref.md: doclib.env arrayref.lisp
	$(LBM) -H 100000 -M 512000 --src="arrayref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

dspref.md: doclib.env dspref.lisp
	$(LBM) -H 100000 -M 512000 --src="dspref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

mutexref.md: doclib.env mutexref.lisp
	$(LBM) -H 100000 -M 512000 --src="mutexref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

randomref.md: doclib.env randomref.lisp
	$(LBM) -H 100000 -M 512000 --src="randomref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

setref.md: doclib.env setref.lisp
	$(LBM) -H 100000 -M 512000 --src="setref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

cryptref.md: doclib.env cryptref.lisp
	$(LBM) -H 100000 -M 512000 --src="cryptref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

clean:
	rm -f doclib.env
	rm -f displayref.md
	rm -f lbmref.md
	rm -f runtimeref.md
	rm -f dynref.md
	rm -f mathref.md
	rm -f stringref.md
	rm -f arrayref.md
	rm -f dspref.md
	rm -f mutexref.md
	rm -f randomref.md
	rm -f setref.md
	rm -f cryptref.md
