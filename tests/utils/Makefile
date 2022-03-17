WHEREAMI := $(dir $(lastword $(MAKEFILE_LIST)))
HERE     := $(realpath $(WHEREAMI)/)
TOP      := $(realpath $(WHEREAMI)/../../)
include $(TOP)/make/unittest-defs.mk

EXTRAINCDIRS += $(TOP)/
EXTRAINCDIRS += $(HERE)/


CFLAGS += -O0
CFLAGS += -Wall -Werror
CFLAGS += -g
CFLAGS += $(patsubst %,-I%,$(EXTRAINCDIRS)) -I.
CFLAGS += -DUNIT_TEST

CONLYFLAGS += -std=gnu99

SRC := $(TOP)/utils.c

include $(TOP)/make/unittest.mk
