TARGET = test
LIBS = -lm -std=gnu99
CC = gcc
CFLAGS = -O2 -g -Wall -Wextra -Wundef -std=gnu99 -I../../util -I../../comm -DNO_STM32
SOURCES = main.c ../../util/utils_math.c
HEADERS = ../../util/utils_math.h
OBJECTS = $(notdir $(SOURCES:.c=.o))

.PHONY: default all clean

default: $(TARGET)
all: default

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@
	
%.o: ../../%.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@
	
%.o: ../../comm/%.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

%.o: ../../util/%.c $(HEADERS)
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
