#!/bin/bash

openocd -f interface/stlink.cfg -f target/nrf52.cfg -c "init" -c "program build/zephyr/zephyr.hex verify reset exit"
