TARGET = test
LIBS = -lm
CC = gcc

TOP = ../..
# libcanard only supports 32-bit targets, on x86_64 this needs gcc-multilib (or an i386 container)
CFLAGS = -m32 -O2 -g -Wall -std=gnu99 -I. -I$(TOP)/libcanard -I$(TOP)/libcanard/dsdl
# Drop the unused DSDL functions that reference types not in canard.mk, like the firmware build does
CFLAGS += -ffunction-sections -Wl,--gc-sections
# The firmware headers are only needed to exist, main.c defines their include guards
CFLAGS += -I$(TOP) -I$(TOP)/comm -I$(TOP)/motor -I$(TOP)/hwconf -I$(TOP)/util
CFLAGS += -I$(TOP)/applications -I$(TOP)/driver/nrf -I$(TOP)/imu

include $(TOP)/libcanard/canard.mk
# canard_driver.c is included by main.c
SOURCES = main.c $(addprefix $(TOP)/,$(filter-out libcanard/canard_driver.c,$(CANARDSRC)))

.PHONY: default all clean run

default: $(TARGET)
all: default

$(TARGET): $(SOURCES) $(TOP)/libcanard/canard_driver.c ch.h hal.h
	$(CC) $(CFLAGS) $(SOURCES) $(LIBS) -o $@

clean:
	rm -f $(TARGET)

run: $(TARGET)
	./$(TARGET)
