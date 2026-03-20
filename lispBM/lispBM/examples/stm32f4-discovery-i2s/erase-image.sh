openocd -f interface/stlink.cfg -f target/stm32f4x.cfg -c "init" -c "reset
   halt" -c "flash erase_sector 0 11 11" -c "reset run" -c "shutdown"
