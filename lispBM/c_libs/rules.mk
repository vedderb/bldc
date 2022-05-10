
CC = arm-none-eabi-gcc
LD = arm-none-eabi-gcc
OBJDUMP = arm-none-eabi-objdump
OBJCOPY = arm-none-eabi-objcopy

OBJECTS = $(SOURCES:.c=.so)

CFLAGS = -fpic -Os -Wall -Wextra -Wundef -std=gnu99 -I$(VESC_C_LIB_PATH)
CFLAGS += -fomit-frame-pointer -falign-functions=16 -mthumb
CFLAGS += -fsingle-precision-constant -Wdouble-promotion
CFLAGS += -mfloat-abi=hard -mfpu=fpv4-sp-d16 -mcpu=cortex-m4
CFLAGS += -fdata-sections -ffunction-sections

LDFLAGS = -nostartfiles -static -mfloat-abi=hard -mfpu=fpv4-sp-d16 -mcpu=cortex-m4
LDFLAGS += -lm -Wl,--gc-sections,--undefined=init
LDFLAGS += -T ../link.ld

.PHONY: default all clean

default: $(TARGET)
all: default

%.so: %.c
	$(CC) $(CFLAGS) -c $< -o $@

.PRECIOUS: $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(LD) $(OBJECTS) $(LDFLAGS) -o $@.elf
	$(OBJDUMP) -D $@.elf > $@.list
	$(OBJCOPY) -O binary $@.elf $@.bin --gap-fill 0x00
	python3 ../conv.py -f $@.bin -n $@ > $@.lisp

clean:
	rm -f $(OBJECTS) $(TARGET).elf $(TARGET).list $(TARGET).lisp $(TARGET).bin

