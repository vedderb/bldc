
LISPBM := ../

include $(LISPBM)/lispbm.mk

PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c

CCFLAGS = -m32 -O2 -Wall -Wconversion -pedantic -std=c11

PICCFLAGS = -O2 -Wall -Wconversion -pedantic -std=c11

LISPBM_SRC += $(LISPBM_EVAL_CPS_SRC)

ifdef HEAP_VIS
	CCFLAGS += -DVISUALIZE_HEAP
endif

all: repl $(LISPBM_DEPS)

debug: CCFLAGS += -g
debug: repl

repl: repl.c $(LISPBM_SRC)
	gcc $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) repl.c -o repl $(LISPBM_INC) $(PLATFORM_INCLUDE) -lpthread

pirepl: repl.c $(LISPBM_SRC)
	gcc $(PICCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) repl.c -o repl $(LISPBM_INC) $(PLATFORM_INCLUDE) -lpthread

clean:
	rm repl
