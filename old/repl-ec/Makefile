
LISPBM := ../

LISPBM_USE_COMPRESSION = true
LISPBM_USE_EC_EVAL = true
LISPBM_USE_PRELUDE = true
LISPBM_USE_EXTENSIONS = true

include $(LISPBM)/lispbm.mk

CCFLAGS = -m32 -O2 -Wall -Wconversion -pedantic -std=c11

ifdef HEAP_VIS
	CCFLAGS += -DVISUALIZE_HEAP
endif

all: repl

debug: CCFLAGS += -g
debug: repl

repl: repl.c $(LIB)
	gcc $(CCFLAGS) $(LISPBM_SRC) $(LISPBM_FLAGS) repl.c -o repl -I../include

clean:
	rm repl
