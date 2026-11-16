#!/bin/sh
openocd -f stm32f407g.cfg -c "program ./build/lisp_test.elf verify reset exit"
