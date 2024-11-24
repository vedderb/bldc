

LISPBM := ../

include $(LISPBM)/lispbm.mk

PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c

#CCFLAGS = -g -O2 -Wall -Wextra -Wshadow -Wconversion -Wclobbered -pedantic -std=c99

# -DLBM_ALWAYS_GC

CCFLAGS = -Wall -Wextra -Wshadow -Wconversion -Wclobbered -pedantic -std=c99 -DFULL_RTS_LIB

CCFLAGS_32 = $(CCFLAGS) -m32 -g -O2
CCFLAGS_GC = $(CCFLAGS) -m32 -DLBM_ALWAYS_GC -g -O2
CCFLAGS_REVGC = $(CCFLAGS) -DUSE_GC_PTR_REV -m32
CCFLAGS_64 = $(CCFLAGS) -DLBM64 -g -O2
CCFLAGS_COV = $(CCFLAGS) -m32 --coverage -g -O0 -DLONGER_DELAY

CC=gcc

SRC = src

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %.exe, $(SOURCES))

test_lisp_code_cps_gc: $(LISPBM_SRC) $(PLATFORM_SRC) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_GC) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_gc -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps_cov: $(LISPBM_SRC) $(PLATFORM_SRC) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_COV) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_cov -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps: $(LISPBM_SRC) $(PLATFORM_SRC) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_32) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps_64: $(LISPBM_SRC) $(PLATFORM_SRC) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_64) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_64 -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

test_lisp_code_cps_revgc: $(LISPBM_SRC) $(PLATFORM_SRC) test_lisp_code_cps.c
	$(CC) $(CCFLAGS_REVGC) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps_revgc -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

all: test_lisp_code_cps_cov test_lisp_code_cps test_lisp_code_cps_64 test_lisp_code_cps_revgc test_lisp_code_cps_gc

clean:
	rm -f *.exe
	rm -f test_lisp_code_cps
	rm -f test_lisp_code_cps_64
	rm -f test_lisp_code_cps_gc
	rm -f test_lisp_code_cps_revgc
	rm -f test_lisp_code_cps_cov
	rm -f test_heap_alloc
	rm -f *.gcda
	rm -f *.gcno

