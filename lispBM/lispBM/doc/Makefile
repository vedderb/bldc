# This makefile assumes that you have "lbm" installed on your path. You can
# install it by running `make install` in ../repl.


all: lbmref.md displayref.md runtimeref.md

doclib.env: doclib.lisp
	lbm -H 100000 --src="doclib.lisp" --store_env=doclib.env --terminate

lbmref.md: doclib.env lbmref.lisp
	lbm -H 10000000 --src="lbmref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

displayref.md: doclib.env displayref.lisp
	lbm -H 10000000 --src="displayref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

runtimeref.md: doclib.env runtimeref.lisp
	lbm -H 10000000 --src="runtimeref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

clean:
	rm doclib.env
	rm displayref.md
	rm lbmref.md
	rm runtimeref.md
