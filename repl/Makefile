
LISPBM := ../

include $(LISPBM)/lispbm.mk

PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c

LBMFLAGS = -DFULL_RTS_LIB \
	   -DLBM_USE_DYN_MACROS \
           -DLBM_USE_DYN_LOOPS \
           -DLBM_USE_DYN_FUNS \
           -DLBM_USE_DYN_ARRAYS \
           -DLBM_USE_DYN_DEFSTRUCT \
           -DLBM_USE_TIME_QUOTA \
           -DLBM_USE_ERROR_LINENO




CCFLAGS =  -g -O2 -Wall -Wconversion -Wsign-compare -pedantic -std=c11 $(LBMFLAGS) -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast

PICCFLAGS = -O2 -Wall -Wconversion -pedantic -std=c11

LISPBM_SRC += $(LISPBM_EVAL_CPS_SRC)

REPL_SRC= repl.c \
	  repl_exts.c \
          crc.c \
          packet.c


ifdef HEAP_VIS
	CCFLAGS += -DVISUALIZE_HEAP
endif

improved_closures: CCFLAGS += -m32 -DCLEAN_UP_CLOSURES
improved_closures: repl clean_cl.h

CCFLAGS_COV = -g -coverage -O0 -Wall -Wconversion -Wsign-compare -pedantic -std=c11 -m32

LIBS = -lpthread -lreadline -lhistory -lpng

all: CCFLAGS += -m32 
all: repl

cov: CCFLAGS += -m32 
cov: repl_cov


all64: 	CCFLAGS += -DLBM64
all64: repl

sdl64: CCFLAGS += -DLBM64 -DWITH_SDL -I/usr/include/SDL2
sdl64: LIBS += -lSDL2_image -lSDL2
sdl64: REPL_SRC += lbm_sdl.c
sdl64: repl

sdl: CCFLAGS += -DWITH_SDL -m32 -I/usr/include/SDL2
sdl: LIBS += -lSDL2_image -lSDL2
sdl: REPL_SRC += lbm_sdl.c
sdl: repl


clean_cl.h: ./scripts/clean.lisp
	./repl --store_env="clean_cl.env" --src=./scripts/clean.lisp --terminate
	xxd -i clean_cl.env clean_cl.h

install: all
	mkdir -p ~/.local/bin
	cp repl ~/.local/bin/lbm

debug: CCFLAGS += -g
debug: repl

repl: $(REPL_SRC) $(LISPBM_SRC) $(LISPBM_DEPS) $(LISPBM_H)
	gcc $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) $(REPL_SRC) -o repl $(LISPBM_INC) $(PLATFORM_INCLUDE) $(LIBS)

repl_cov: $(REPL_SRC) $(LISPBM_SRC) $(LISPBM_DEPS) $(LISPBM_H)
	gcc $(CCFLAGS_COV) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) $(REPL_SRC) -o repl_cov $(LISPBM_INC) $(PLATFORM_INCLUDE) $(LIBS)

pirepl: $(REPL_SRC) $(LISPBM_SRC) $(LISPBM_DEPS) $(LISPBM_H)
	gcc $(PICCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) $(REPL_SRC) -o repl $(LISPBM_INC) $(PLATFORM_INCLUDE) $(LIBS)

clean:
	rm -f repl
	rm -f repl_cov
	rm -f *.gcda
	rm -f *.gcno
