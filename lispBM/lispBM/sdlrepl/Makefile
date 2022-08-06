
LISPBM := ../

include $(LISPBM)/lispbm.mk

PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c

CCFLAGS = -g -Wall -Wconversion -Wsign-compare -pedantic -std=c11

LIBS = -lSDL2_image -lSDL2 -lm
INCS = -I/usr/include/SDL2

LISPBM_SRC += $(LISPBM_EVAL_CPS_SRC)

LISPBM_SRC += lbm_sdl.c

ifdef HEAP_VIS
	CCFLAGS += -DVISUALIZE_HEAP
endif

all: CCFLAGS += -DLBM64
all: repl 

install: repl
	mkdir -p ~/.local/bin
	cp repl ~/.local/bin/lbm

debug: CCFLAGS += -g
debug: repl

repl: repl.c $(LISPBM_SRC) $(LISPBM_DEPS)
	gcc $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) repl.c -o repl $(LISPBM_INC) $(PLATFORM_INCLUDE) -lpthread $(LIBS) $(INCS)

clean:
	rm repl
