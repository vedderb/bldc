
LISPBM := ../

include $(LISPBM)/lispbm.mk

PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c

CCFLAGS =  -O2 -Wall -Wconversion -Wsign-compare -pedantic -std=c11

PICCFLAGS = -O2 -Wall -Wconversion -pedantic -std=c11

LISPBM_SRC += $(LISPBM_EVAL_CPS_SRC)

ifdef HEAP_VIS
	CCFLAGS += -DVISUALIZE_HEAP
endif

all: CCFLAGS += -m32
all: repl 

all64: 	CCFLAGS += -DLBM64
all64: repl 


install: repl
	mkdir -p ~/.local/bin
	cp repl ~/.local/bin/lbm


debug: CCFLAGS += -g
debug: repl

repl: repl.c $(LISPBM_SRC) $(LISPBM_DEPS)
	gcc $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) repl.c -o repl $(LISPBM_INC) $(PLATFORM_INCLUDE) -lpthread

pirepl: repl.c $(LISPBM_SRC) $(LISPBM_DEPS)
	gcc $(PICCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) repl.c -o repl $(LISPBM_INC) $(PLATFORM_INCLUDE) -lpthread

clean:
	rm repl
