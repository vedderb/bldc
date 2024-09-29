

LISPBM := ../

include $(LISPBM)/lispbm.mk

PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c

#CCFLAGS = -g -O2 -Wall -Wextra -Wshadow -Wconversion -Wclobbered -pedantic -std=c99
CCFLAGS = -Wall -Wextra -Wshadow -Wconversion -Wclobbered -pedantic -std=c99 -DFULL_RTS_LIB
# -DLBM_ALWAYS_GC
CC=gcc

SRC = src

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %.exe, $(SOURCES))

gc: CCFLAGS += -m32 -DLBM_ALWAYS_GC -g -O2
gc: $(EXECS)
	mv test_lisp_code_cps.exe test_lisp_code_cps


.PHONY: coverage
coverage: CCFLAGS += -m32 --coverage -g -O0 -DLONGER_DELAY
coverage: 
	$(CC) $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) test_lisp_code_cps.c -o test_lisp_code_cps -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

all: CCFLAGS += -m32 -g -O2
all: $(EXECS)
	mv test_lisp_code_cps.exe test_lisp_code_cps
#	mv test_lisp_code_cps_nc.exe test_lisp_code_cps_nc

all64: CCFLAGS += -DLBM64 -g -O2
all64: $(EXECS)
	mv test_lisp_code_cps.exe test_lisp_code_cps
#	mv test_lisp_code_cps_nc.exe test_lisp_code_cps_nc


allrev: CCFLAGS += -DUSE_GC_PTR_REV -m32
allrev: $(EXECS)
	mv test_lisp_code_cps.exe test_lisp_code_cps

%.exe: %.c $(LISPBM_DEPS)
	$(CC) $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) $< -o $@  -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm


clean:
	rm -f *.exe
	rm -f test_lisp_code_cps
	rm -f test_heap_alloc
	rm -f *.gcda
	rm -f *.gcno

