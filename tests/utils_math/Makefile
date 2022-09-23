WHEREAMI := $(dir $(lastword $(MAKEFILE_LIST)))
HERE     := $(realpath $(WHEREAMI)/)
TOP      := $(realpath $(WHEREAMI)/../../)
include $(TOP)/make/unittest-defs.mk

EXTRAINCDIRS += $(TOP)/
EXTRAINCDIRS += $(HERE)/


CFLAGS += -O0

# Set compiler-specific flags
GCC_CXXFLAGS = -fsingle-precision-constant
CLANG_CXXFLAGS = -cl-single-precision-constant
UNKNOWN_CXXFLAGS = -fsingle-precision-constant

# Detect if CXX is clang++ or g++, in this order.
COMPILER_VERSION := $(shell $(CXX) --version)
ifneq '' '$(findstring clang,$(COMPILER_VERSION))'
  CFLAGS += $(CLANG_CXXFLAGS)
else ifneq '' '$(findstring g++,$(COMPILER_VERSION))'
  CFLAGS += $(GCC_CXXFLAGS)
else
  $(warning Unknown compiler)
  CFLAGS += $(UNKNOWN_CXXFLAGS)
endif

CFLAGS += -Wall -Wdouble-promotion -Werror
CFLAGS += -g
CFLAGS += $(patsubst %,-I%,$(EXTRAINCDIRS)) -I.

CONLYFLAGS += -std=gnu99

SRC := $(TOP)/utils_math.c

include $(TOP)/make/unittest.mk
