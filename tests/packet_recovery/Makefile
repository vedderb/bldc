TARGET = test
LIBS = -lm
CC = gcc
CFLAGS = -O2 -g -Wall -Wextra -Wundef -std=gnu99 -I../../
SOURCES = main.c ../../packet.c ../../crc.c
HEADERS = ../../packet.h ../../crc.h
OBJECTS = $(notdir $(SOURCES:.c=.o))

.PHONY: default all clean

default: $(TARGET)
all: default

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@
	
%.o: ../../%.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

.PRECIOUS: $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(CC) $(OBJECTS) -Wall $(LIBS) -o $@

clean:
	rm -f $(OBJECTS) $(TARGET)
	
test2:
	echo $(OBJECTS)

run: $(TARGET)
	./$(TARGET)
