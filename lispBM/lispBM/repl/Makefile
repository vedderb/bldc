
PLATFORM ?=
FEATURES ?=

LISPBM := ../
FEATURE_FILE = .build_features
.PHONY: FORCE all help install clean clean_coverage

include $(LISPBM)/lispbm.mk

LBMFLAGS = -DFULL_RTS_LIB \
	   -DLBM_USE_DYN_MACROS \
           -DLBM_USE_DYN_LOOPS \
           -DLBM_USE_DYN_FUNS \
           -DLBM_USE_DYN_ARRAYS \
           -DLBM_USE_DYN_DEFSTRUCT \
           -DLBM_USE_TIME_QUOTA \
           -DLBM_USE_ERROR_LINENO \
           -DLBM_USE_MACRO_REST_ARGS \
           -DLBM_USE_SHEBANG_COMMENTS

LDFLAGS = -lpng -lm

ifeq ($(PLATFORM), macos-arm64)
	PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include\
	                   -I/opt/homebrew/include/ \
                           -I/opt/homebrew/opt/readline/include
	LDFLAGS += -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/lib
# To make mmap work on macOS
	LBMFLAGS += -D_DARWIN_C_SOURCE
else
	PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
	LDFLAGS += -lpthread -lreadline -lhistory
endif

PLATFORM_SRC     = $(LISPBM)/platform/linux/src/platform_mutex.c \
                   $(LISPBM)/platform/linux/src/platform_timestamp.c \
	           $(LISPBM)/platform/linux/src/platform_thread.c

REPL_SRC= $(LISPBM_SRC)\
	  repl.c \
	  repl_exts.c \
          crc.c \
          packet.c \
          vesc_express_extension_stubs.c \
	  lbm_gnuplot.c \
          bldc_extension_stubs.c

## Dropped usage of pkg-config as it felt like more trouble than benefit.
## pkg-config searches for .pc files unrelated to compile flags such as -m32.
## Using just -lasound (for example) means that the linker will look for
## the alsa libraries while taking flags such as -m32 into consideration.

# Coverage build or regular
ifneq (,$(filter coverage,$(FEATURES)))
	CCFLAGS = -g -coverage -O0 -Wall -Wconversion -Wsign-compare -pedantic -std=c11 $(LBMFLAGS) -fno-pie -no-pie
else
	CCFLAGS = -O2 -Wall -Wextra -Wshadow  -Wconversion -Wsign-compare -pedantic -std=c11 $(LBMFLAGS) -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast -fno-pie -no-pie
endif

# Use 64bit
ifneq (,$(filter 64, $(FEATURES)))
	CCFLAGS += -DLBM64
else
	CCFLAGS += -m32
endif

# Use SDL
ifneq (,$(filter sdl,$(FEATURES)))
	CCFLAGS += -DWITH_SDL -I/usr/include/SDL2
	LDFLAGS += -lSDL2 -lSDL2_image
	REPL_SRC += lbm_sdl.c
endif

# Use ALSA
ifneq (,$(filter alsa, $(FEATURES)))
	CCFLAGS += -DWITH_ALSA
	LDFLAGS += -lasound
	REPL_SRC += lbm_sound.c lbm_midi.c
endif

# Use freetype
ifneq (,$(filter freetype, $(FEATURES)))
	CCFLAGS += -DLBM_TTF_USE_FREETYPE -I/usr/include/freetype2
	LDFLAGS += -lfreetype
	REPL_SRC := $(filter-out %/schrift.c,$(REPL_SRC))
	REPL_SRC += $(LISPBM)/src/extensions/ttf_backend_freetype.c
endif

# USE Debug info
ifneq (,$(filter debug, $(FEATURES)))
	CCFLAGS += -g
endif

#USE rtlsdr extensions
ifneq (,$(filter rtlsdr, $(FEATURES)))
	CCFLAGS += -DWITH_RTLSDR
	LDFLAGS += -lrtlsdr
	REPL_SRC += lbm_rtlsdr.c
endif


# -Wjump-misses-init
# -fsanitize=address

#improved_closures:
#improved_closures: repl clean_cl.h
#clean_cl.h: ./scripts/clean.lisp
#	./repl --store_env="clean_cl.env" --src=./scripts/clean.lisp --terminate
#	xxd -i clean_cl.env clean_cl.h

all: repl

help:
	@echo "Run 'make' to build the REPL"
	@echo ""
	@echo "Add a 'FEATURES' list to select REPL features:"
	@echo "64       - Build 64Bit LispBM REPL"
	@echo "alsa     - Sound and midi, builds the LispBM soft synth."
	@echo "sdl      - Adds SDL support for graphics."
	@echo "freetype - Use libfreetype for font preprocessing."
	@echo "coverage - Build with coverage information collection."
	@echo "debug    - Build with debug symbols."
	@echo ""
	@echo "Example: 'make FEATURES=\"alsa sdl\"' to build with sound and graphics support."
	@echo "Example: 'make FEATURES=\"sdl freetype\"' to build with graphics and freetype support."

install:
	@if [ ! -f repl ]; then echo "Error: repl not built. Run 'make help' for information." ; exit 1; fi
	mkdir -p ~/.local/bin
	cp repl ~/.local/bin/lbm
	cp shlbm ~/.local/bin/shlbm
	chmod a+x ~/.local/bin/shlbm

repl: $(FEATURE_FILE) $(REPL_SRC) $(PLATFORM_SRC) $(LISPBM_DEPS) $(LISPBM_H)
	gcc $(CCFLAGS) $(PLATFORM_SRC) $(LISPBM_FLAGS) $(REPL_SRC) -o repl $(LISPBM_INC) $(PLATFORM_INCLUDE) $(LDFLAGS)

clean:
	rm -f repl
	rm -f .build_features
	rm -f *.gcda
	rm -f *.gcno

clean_coverage:
	rm -f coverage/*


$(FEATURE_FILE): FORCE
	@echo "$(FEATURES)" | cmp -s - $@ || echo "$(FEATURES)" > $@


