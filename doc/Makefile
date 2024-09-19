# This makefile assumes that you have "lbm" installed on your path. You can
# install it by running `make install` in ../repl.


all: lbmref.md displayref.md

doclib.env: doclib.lisp
	lbm --src="doclib.lisp" --store_env=doclib.env --terminate

lbmref.md: doclib.env lbmref.lisp
	lbm -H 10000000 --src="lbmref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

displayref.md: doclib.env displayref.lisp
	lbm -H 10000000 --src="displayref.lisp" --eval="(render-manual)" --load_env="doclib.env" --terminate

clean:
	rm doclib.env
	displayref.lisp
	rm lbmref.md
