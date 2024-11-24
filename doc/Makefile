# This makefile assumes that you have "lbm" installed on your path. You can
# install it by running `make install` in ../repl.


all: lbmref.md displayref.md runtimeref.md strange.md efficient.md

doclib.env: doclib.lisp
	lbm -H 100000 -M 11 --src="doclib.lisp" --store_env=doclib.env --terminate

lbmref.md: doclib.env lbmref.lisp
	lbm -H 10000000 -M 11 --src="lbmref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

displayref.md: doclib.env displayref.lisp
	lbm -H 10000000 -M 11 --src="displayref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

runtimeref.md: doclib.env runtimeref.lisp
	lbm -H 10000000 -M 11 --src="runtimeref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

strange.md: doclib.env strange.lisp
	lbm -H 10000000 -M 11 --src="strange.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

efficient.md: doclib.env efficient.lisp
	lbm -H 10000000 -M 11 --src="efficient.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

clean:
	rm -f doclib.env
	rm -f displayref.md
	rm -f lbmref.md
	rm -f runtimeref.md
