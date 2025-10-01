

LISPBM := ../

include $(LISPBM)/lispbm.mk

PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c

#CCFLAGS = -g -O2 -Wall -Wextra -Wshadow -Wconversion -Wclobbered -pedantic -std=c99

# -DLBM_ALWAYS_GC

LBMFLAGS = -DFULL_RTS_LIB -DLBM_USE_DYN_FUNS -DLBM_USE_DYN_MACROS -DLBM_USE_DYN_LOOPS -DLBM_USE_DYN_ARRAYS

CCFLAGS =  -m32 -O0 -Wall -Wextra -Wshadow -Wconversion -Wclobbered -pedantic -std=c99 $(LBMFLAGS)

CC=gcc

SRC = src

SOURCES = $(wildcard *.c)
EXECS = $(patsubst %.c, %.exe, $(SOURCES))

SOURCES = $(wildcard *.c)
TARGETS = $(SOURCES:.c=.exe)

all: $(TARGETS)

%.exe: %.c
	$(CC) $(CCFLAGS) $(LISPBM_SRC) $(PLATFORM_SRC) $(LISPBM_FLAGS) $< -o $@ -I$(LISPBM)include $(PLATFORM_INCLUDE) -lpthread -lm

clean:
	rm -f $(TARGETS)

