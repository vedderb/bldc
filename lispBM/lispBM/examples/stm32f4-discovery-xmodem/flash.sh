#!/bin/sh

openocd -f board/stm32f4discovery.cfg -c "program ./build/ch.elf verify reset exit"
