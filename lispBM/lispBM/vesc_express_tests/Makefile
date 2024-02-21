

IN=$(wildcard *.lisp)

OUT=$(IN:.lisp=.plisp)

all: $(OUT)

%.plisp : %.lisp test_utils.kit
	cat test_utils.kit "$<" > $@


clean:
	rm *.plisp
