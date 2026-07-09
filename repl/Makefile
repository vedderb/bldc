
PLATFORM ?=
LISPBM := ../
FEATURE_FILE = .build_features
FEATURES ?= $(shell cat $(FEATURE_FILE) 2>/dev/null)
.PHONY: FORCE all help install clean clean_coverage

include $(LISPBM)/lispbm.mk

CCFLAGS =

LBMFLAGS = -DFULL_RTS_LIB \
           -DLBM_USE_DYN_MACROS \
           -DLBM_USE_DYN_LOOPS \
           -DLBM_USE_DYN_FUNS \
           -DLBM_USE_DYN_ARRAYS \
           -DLBM_USE_TIME_QUOTA \
           -DLBM_USE_ERROR_LINENO \
           -DLBM_USE_MACRO_REST_ARGS \
           -DLBM_USE_SHEBANG_COMMENTS

LDFLAGS = -lpng

# Detect OS and Architecture
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

# Platform-specific paths
ifeq ($(UNAME_S),Darwin)
	HOMEBREW      = /opt/homebrew
	SDL_PATH      = $(HOMEBREW)/include/SDL2
	FREETYPE_PATH = $(HOMEBREW)/include/freetype2
	RTLSDR_PATH   = $(HOMEBREW)/include
	LIMESDR_PATH  = $(HOMEBREW)/include
	FTDI_PATH     = $(HOMEBREW)/include/libftdi1
	LIB_PATH      = $(HOMEBREW)/lib
	PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include \
	                   -I$(HOMEBREW)/include \
	                   -I$(HOMEBREW)/opt/readline/include
	LDFLAGS += -L$(HOMEBREW)/opt/readline/lib -lreadline -L$(LIB_PATH)
	LBMFLAGS += -D_DARWIN_C_SOURCE
	FEATURES += 64
else
	SDL_PATH      = /usr/include/SDL2
	FREETYPE_PATH = /usr/include/freetype2
	RTLSDR_PATH   = /usr/include
	LIMESDR_PATH  = /usr/include
	FTDI_PATH     = /usr/include/libftdi1
	PLATFORM_INCLUDE = -I$(LISPBM)/platform/linux/include
	LDFLAGS += -lpthread -lreadline -lhistory
	CCFLAGS += -fno-pie -no-pie
endif

PLATFORM_SRC = $(LISPBM)/platform/linux/src/platform_mutex.c \
               $(LISPBM)/platform/linux/src/platform_timestamp.c \
               $(LISPBM)/platform/linux/src/platform_thread.c

## If using any of the utilities.
LISPBM_INC += -I../utils -I../utils/mcp
CCFLAGS    += -DWITH_MCP

REPL_SRC = $(LISPBM_SRC)\
           repl.c \
           repl_exts.c \
           ../utils/crc.c \
           ../utils/packet.c \
           lbm_gnuplot.c \
           lbm_octave.c \
           ../utils/mcp/lbm_mcp.c \
           ../utils/mcp/cJSON.c

## Dropped usage of pkg-config as it felt like more trouble than benefit.
## pkg-config searches for .pc files unrelated to compile flags such as -m32.
## Using just -lasound (for example) means that the linker will look for
## the alsa libraries while taking flags such as -m32 into consideration.

# Coverage build or regular
ifneq (,$(filter coverage,$(FEATURES)))
	CCFLAGS += -g -coverage -O0 -Wall -Wconversion -Wsign-compare -pedantic -std=c11 $(LBMFLAGS)
else
	CCFLAGS += -O2 -Wall -Wextra -Wshadow  -Wconversion -Wsign-compare -pedantic -std=c11 $(LBMFLAGS) -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast
endif

# Use 64bit
ifneq (,$(filter 64, $(FEATURES)))
	CCFLAGS += -DLBM64
	INSTALL_NAME := lbm64
else
	CCFLAGS += -m32
	INSTALL_NAME := lbm
endif

# Use SDL
ifneq (,$(filter sdl,$(FEATURES)))
	CCFLAGS += -DWITH_SDL -I$(SDL_PATH)
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
	CCFLAGS += -DLBM_TTF_USE_FREETYPE -I$(FREETYPE_PATH)
	LDFLAGS += -lfreetype
	REPL_SRC := $(filter-out %/schrift.c,$(REPL_SRC))
	REPL_SRC += $(LISPBM)/src/extensions/ttf_backend_freetype.c
endif

# USE Debug info
ifneq (,$(filter debug, $(FEATURES)))
	CCFLAGS += -g
endif

# USE rtlsdr extensions
ifneq (,$(filter rtlsdr, $(FEATURES)))
	CCFLAGS += -DWITH_RTLSDR -I$(RTLSDR_PATH)
	LDFLAGS += -lrtlsdr
	REPL_SRC += lbm_rtlsdr.c
endif

# USE limesdr extensions
ifneq (,$(filter limesdr, $(FEATURES)))
	CCFLAGS += -DWITH_LIMESDR -I$(LIMESDR_PATH)
	LDFLAGS += -lLimeSuite
	REPL_SRC += lbm_limesdr.c
endif

# VESC platform extension stubs
ifneq (,$(filter vesc, $(FEATURES)))
	CCFLAGS  += -DWITH_VESC
	REPL_SRC += vesc_extension_stubs.c
endif

ifneq (,$(filter ft4232h_nand, $(FEATURES)))
	CCFLAGS  += -DTEST_FT4232H_NAND_DRIVER -I$(FTDI_PATH)
	LDFLAGS  += -lftdi1
	REPL_SRC += ft4232h_w25n01.c
endif

ifneq (,$(filter ft232h_nand, $(FEATURES)))
	CCFLAGS  += -DTEST_FT232H_NAND_DRIVER -I$(FTDI_PATH)
	LDFLAGS  += -lftdi1
	REPL_SRC += ft232h_w25n01.c
endif


# Qt Quick UI — requires qmake on $PATH (source ~/.bash_profile)
QT_OBJS  =
ifneq (,$(filter qt, $(FEATURES)))
	QT_PREFIX  := $(shell qmake -query QT_INSTALL_PREFIX)
	QT_BINS    := $(shell qmake -query QT_INSTALL_BINS)
	QT_LIBDIR  := $(shell qmake -query QT_INSTALL_LIBS)
	MOC_BIN    := $(QT_BINS)/moc
	QT_CFLAGS  := $(shell PKG_CONFIG_PATH=$(QT_LIBDIR)/pkgconfig \
	                pkg-config --define-variable=prefix=$(QT_PREFIX) \
	                --cflags Qt5Quick Qt5QuickControls2 Qt5Widgets Qt5PrintSupport)
	QT_LIBS    := $(shell PKG_CONFIG_PATH=$(QT_LIBDIR)/pkgconfig \
	                pkg-config --define-variable=prefix=$(QT_PREFIX) \
	                --libs Qt5Quick Qt5QuickControls2 Qt5Widgets Qt5PrintSupport)
	# Flags for g++: drop C-only flags, add C++14 and Qt includes
	CXXFLAGS  := $(filter-out -std=c11 -pedantic -fno-pie -no-pie \
	                          -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast,$(CCFLAGS)) \
	             -std=c++14 -fPIC $(QT_CFLAGS) -I../utils/qt
	CCFLAGS   += -DWITH_QT -I../utils/qt $(QT_CFLAGS)
	LDFLAGS   += $(QT_LIBS) -Wl,-rpath,$(QT_LIBDIR) -no-pie
	QT_SRC     = ../utils/qt/src
	QT_OBJS    = QLbmQuickDisplayItem.o moc_QLbmQuickDisplayItem.o QLbmValue.o qtquick_extensions.o repl_qt.o \
	             qcustomplot.o moc_qcustomplot.o QLbmPlotItem.o moc_QLbmPlotItem.o
	# MOC only accepts -D and -I flags; strip everything else from CXXFLAGS.
	MOC_FLAGS := $(filter -D% -I%,$(CXXFLAGS)) $(filter -D% -I%,$(LISPBM_INC)) \
	             $(filter -D% -I%,$(PLATFORM_INCLUDE))

# MOC: LbmSignalAdapter is defined inside qtquick_extensions.cpp and
# included via its own .moc file at the bottom of that translation unit.
qtquick_extensions.moc: $(QT_SRC)/qtquick_extensions.cpp
	$(MOC_BIN) $(MOC_FLAGS) $< -o $@

# QLbmQuickDisplayItem has Q_OBJECT in its header.
moc_QLbmQuickDisplayItem.cpp: ../utils/qt/QLbmQuickDisplayItem.h
	$(MOC_BIN) $(MOC_FLAGS) $< -o $@

QLbmQuickDisplayItem.o: $(QT_SRC)/QLbmQuickDisplayItem.cpp moc_QLbmQuickDisplayItem.cpp
	g++ $(CXXFLAGS) -c $< -o $@ $(LISPBM_INC) $(PLATFORM_INCLUDE)

moc_QLbmQuickDisplayItem.o: moc_QLbmQuickDisplayItem.cpp
	g++ $(CXXFLAGS) -c $< -o $@ $(LISPBM_INC) $(PLATFORM_INCLUDE)

QLbmValue.o: $(QT_SRC)/QLbmValue.cpp
	g++ $(CXXFLAGS) -c $< -o $@ $(LISPBM_INC) $(PLATFORM_INCLUDE)

qtquick_extensions.o: $(QT_SRC)/qtquick_extensions.cpp qtquick_extensions.moc
	g++ $(CXXFLAGS) -I$(CURDIR) -c $< -o $@ $(LISPBM_INC) $(PLATFORM_INCLUDE)

repl_qt.o: repl_qt.cpp
	g++ $(CXXFLAGS) -c $< -o $@ $(LISPBM_INC) $(PLATFORM_INCLUDE)

# qcustomplot is large; suppress warnings to keep build output readable.
moc_qcustomplot.cpp: ../utils/qt/qcustomplot.h
	$(MOC_BIN) $(MOC_FLAGS) $< -o $@

moc_qcustomplot.o: moc_qcustomplot.cpp
	g++ $(CXXFLAGS) -w -c $< -o $@

qcustomplot.o: ../utils/qt/qcustomplot.cpp moc_qcustomplot.cpp
	g++ $(CXXFLAGS) -w -c $< -o $@

moc_QLbmPlotItem.cpp: ../utils/qt/QLbmPlotItem.h
	$(MOC_BIN) $(MOC_FLAGS) $< -o $@

moc_QLbmPlotItem.o: moc_QLbmPlotItem.cpp
	g++ $(CXXFLAGS) -c $< -o $@ $(LISPBM_INC) $(PLATFORM_INCLUDE)

QLbmPlotItem.o: $(QT_SRC)/QLbmPlotItem.cpp moc_QLbmPlotItem.cpp
	g++ $(CXXFLAGS) -c $< -o $@ $(LISPBM_INC) $(PLATFORM_INCLUDE)
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
	@echo "coverage  - Build with coverage information collection."
	@echo "mcp       - Build with MCP (Model Context Protocol) server support. Adds --mcp flag."
	@echo "qt        - Adds Qt Quick UI widgets (qt-button, qt-slider, qt-plot …)."
	@echo "            Requires qmake on PATH (source ~/.bash_profile)."
	@echo "debug     - Build with debug symbols."
	@echo "test_nand - Build FT4232H + W25N01GVZEIG NAND driver test (requires libftdi1-dev)."
	@echo ""
	@echo "Example: 'make FEATURES=\"alsa sdl\"' to build with sound and graphics support."
	@echo "Example: 'make FEATURES=\"sdl freetype\"' to build with graphics and freetype support."
	@echo "Example: 'make FEATURES=\"64 qt\"' to build 64-bit REPL with Qt Quick UI."

install:
	@if [ ! -f repl ]; then echo "Error: repl not built. Run 'make help' for information." ; exit 1; fi
	mkdir -p ~/.local/bin
	cp repl ~/.local/bin/$(INSTALL_NAME)
	@echo "Installed as $(INSTALL_NAME)"
	cp shlbm ~/.local/bin/shlbm
	chmod a+x ~/.local/bin/shlbm

ifneq (,$(filter qt, $(FEATURES)))
# Qt build: compile C sources via partial link (-r) into a single relocatable
# object, compile Qt C++ sources separately with g++, then link all with g++.
repl: $(FEATURE_FILE) $(QT_OBJS) $(REPL_SRC) $(PLATFORM_SRC) $(LISPBM_DEPS) $(LISPBM_H)
	gcc $(CCFLAGS) -r -o lbm_c_objs.o $(PLATFORM_SRC) $(REPL_SRC) $(LISPBM_INC) $(PLATFORM_INCLUDE)
	g++ lbm_c_objs.o $(QT_OBJS) -o repl $(LDFLAGS)
else
repl: $(FEATURE_FILE) $(REPL_SRC) $(PLATFORM_SRC) $(LISPBM_DEPS) $(LISPBM_H)
	gcc $(CCFLAGS) $(PLATFORM_SRC) $(REPL_SRC) -o repl $(LISPBM_INC) $(PLATFORM_INCLUDE) $(LDFLAGS) $(LISPBM_FLAGS)
endif

clean:
	rm -f repl
	rm -f .build_features
	rm -f *.gcda
	rm -f *.gcno
	rm -f *.o
	rm -f *.moc moc_*.cpp

clean_coverage:
	rm -f coverage/*

$(FEATURE_FILE): FORCE
	@echo "$(FEATURES)" | cmp -s - $@ || echo "$(FEATURES)" > $@
