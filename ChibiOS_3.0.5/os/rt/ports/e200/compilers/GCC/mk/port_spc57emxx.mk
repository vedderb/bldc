# List of the ChibiOS/RT e200z4 SPC57EMxx port files.
PORTSRC = ${CHIBIOS}/os/rt/ports/e200/chcore.c
          
PORTASM = $(CHIBIOS)/os/common/ports/e200/devices/SPC57EMxx/boot.s \
          $(CHIBIOS)/os/common/ports/e200/compilers/GCC/vectors.s \
          $(CHIBIOS)/os/common/ports/e200/compilers/GCC/crt0.s \
          $(CHIBIOS)/os/rt/ports/e200/compilers/GCC/ivor.s

PORTINC = ${CHIBIOS}/os/common/ports/e200/compilers/GCC \
          ${CHIBIOS}/os/common/ports/e200/devices/SPC57EMxx \
          ${CHIBIOS}/os/rt/ports/e200 \
          ${CHIBIOS}/os/rt/ports/e200/compilers/GCC

PORTLD  = ${CHIBIOS}/os/common/ports/e200/compilers/GCC/ld
