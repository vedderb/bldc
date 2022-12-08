
CC = arm-none-eabi-gcc
LD = arm-none-eabi-gcc
OBJDUMP = arm-none-eabi-objdump
OBJCOPY = arm-none-eabi-objcopy
PYTHON = python3

STLIB_PATH = $(VESC_C_LIB_PATH)/stdperiph_stm32f4/

ifeq ($(USE_STLIB),yes)
	SOURCES += \
		$(STLIB_PATH)/src/misc.c \
		$(STLIB_PATH)/src/stm32f4xx_adc.c \
		$(STLIB_PATH)/src/stm32f4xx_dma.c \
		$(STLIB_PATH)/src/stm32f4xx_exti.c \
		$(STLIB_PATH)/src/stm32f4xx_flash.c \
		$(STLIB_PATH)/src/stm32f4xx_rcc.c \
		$(STLIB_PATH)/src/stm32f4xx_syscfg.c \
		$(STLIB_PATH)/src/stm32f4xx_tim.c \
		$(STLIB_PATH)/src/stm32f4xx_iwdg.c \
		$(STLIB_PATH)/src/stm32f4xx_wwdg.c
endif

UTILS_PATH = $(VESC_C_LIB_PATH)/utils/

SOURCES += $(UTILS_PATH)/rb.c
SOURCES += $(UTILS_PATH)/utils.c

OBJECTS = $(SOURCES:.c=.so)

ifeq ($(USE_OPT),)
	USE_OPT =
endif

CFLAGS = -fpic -Os -Wall -Wextra -Wundef -std=gnu99 -I$(VESC_C_LIB_PATH)
CFLAGS += -I$(STLIB_PATH)/CMSIS/include -I$(STLIB_PATH)/CMSIS/ST -I$(UTILS_PATH)/
CFLAGS += -fomit-frame-pointer -falign-functions=16 -mthumb
CFLAGS += -fsingle-precision-constant -Wdouble-promotion
CFLAGS += -mfloat-abi=hard -mfpu=fpv4-sp-d16 -mcpu=cortex-m4
CFLAGS += -fdata-sections -ffunction-sections
CFLAGS += -DIS_VESC_LIB
CFLAGS += $(USE_OPT)

ifeq ($(USE_STLIB),yes)
	CFLAGS += -DUSE_STLIB -I$(STLIB_PATH)/inc
endif

LDFLAGS = -nostartfiles -static -mfloat-abi=hard -mfpu=fpv4-sp-d16 -mcpu=cortex-m4
LDFLAGS += -lm -Wl,--gc-sections,--undefined=init
LDFLAGS += -T $(VESC_C_LIB_PATH)/link.ld

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
	$(PYTHON) $(VESC_C_LIB_PATH)/conv.py -f $@.bin -n $@ > $@.lisp

clean:
	rm -f $(OBJECTS) $(TARGET).elf $(TARGET).list $(TARGET).lisp $(TARGET).bin

