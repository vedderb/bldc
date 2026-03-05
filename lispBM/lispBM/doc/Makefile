# This makefile assumes that you have "lbm" installed on your path. You can
# install it by running `make install` in ../repl.

LBM=lbm

all: lbmref.md displayref.md runtimeref.md dynref.md ttfref.md

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

clean:
	rm -f doclib.env
	rm -f displayref.md
	rm -f lbmref.md
	rm -f runtimeref.md
	rm -f dynref.md
