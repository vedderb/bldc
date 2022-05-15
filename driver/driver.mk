CSRC += \
	driver/eeprom.c \
	driver/i2c_bb.c \
	driver/ledpwm.c \
	driver/servo_dec.c \
	driver/servo_simple.c \
	driver/spi_bb.c \
	driver/timer.c
	
CSRC += \
	driver/lora/SX1278.c \
	driver/lora/lora.c \
	driver/lora/SX1278_hw.c 
	
CSRC += \
	driver/nrf/spi_sw.c \
	driver/nrf/rf.c \
	driver/nrf/rfhelp.c \
	driver/nrf/nrf_driver.c

INCDIR += \
	driver \
	driver/lora \
	driver/nrf

