

LISPBM := ../

include $(LISPBM)/lispbm.mk

PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c

CCFLAGS = -g -O2 -Wall -Wextra -Wshadow -Wconversion -Wclobbered -pedantic -std=c99
# -DLBM_ALWAYS_GC
CC=gcc

SRC = src
OBJ = obj

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %.exe, $(SOURCES))

gc: CCFLAGS += -m32 -DLBM_ALWAYS_GC
gc: $(EXECS)
	mv test_lisp_code_cps.exe test_lisp_code_cps


all: CCFLAGS += -m32
all: $(EXECS)
	mv test_lisp_code_cps.exe test_lisp_code_cps
#	mv test_lisp_code_cps_nc.exe test_lisp_code_cps_nc

all64: CCFLAGS += -DLBM64
all64: $(EXECS)
	mv test_lisp_code_cps.exe test_lisp_code_cps
#	mv test_lisp_code_cps_nc.exe test_lisp_code_cps_nc


allrev: CCFLAGS += -DUSE_GC_PTR_REV -m32
allrev: $(EXECS)
	mv test_lisp_code_cps.exe test_lisp_code_cps

%.exe: %.c $(LISPBM_DEPS)
	$(CC) $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) $< -o $@  -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread


clean:
	rm *.exe
	rm test_lisp_code_cps

