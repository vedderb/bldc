

LISPBM := ../

include $(LISPBM)/lispbm.mk

PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c

CCFLAGS = -g -m32 -O2 -Wall -Wconversion -pedantic -std=c11 
CC=gcc

SRC = src
OBJ = obj

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %.exe, $(SOURCES))

all: $(EXECS)
	mv test_lisp_code_cps.exe test_lisp_code_cps
#	mv test_lisp_code_cps_nc.exe test_lisp_code_cps_nc

%.exe: %.c $(LISPBM_DEPS)
	$(CC) $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) $< -o $@  -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread


clean:
	rm *.exe
	rm test_lisp_code_cps

