/*
    ChibiOS/RT - Copyright (C) 2006,2007,2008,2009,2010,
                 2011,2012,2013,2014,2015 Giovanni Di Sirio.

    This file is part of ChibiOS/RT.

    A limited, non-exclusive license has been granted to you
    under the terms of the FOGL license, see the attached file
    EULA_FOGL.pdf.
*/

/*
 * STM32L151x6 memory setup.
 */
MEMORY
{
    flash : org = 0x08000000, len = 32k
    ram0  : org = 0x20000000, len = 10k
    ram1  : org = 0x00000000, len = 0
    ram2  : org = 0x00000000, len = 0
    ram3  : org = 0x00000000, len = 0
    ram4  : org = 0x00000000, len = 0
    ram5  : org = 0x00000000, len = 0
    ram6  : org = 0x00000000, len = 0
    ram7  : org = 0x00000000, len = 0
}

/* RAM region to be used for Main stack. This stack accommodates the processing
   of all exceptions and interrupts*/
REGION_ALIAS("MAIN_STACK_RAM", ram0);

/* RAM region to be used for the process stack. This is the stack used by
   the main() function.*/
REGION_ALIAS("PROCESS_STACK_RAM", ram0);

/* RAM region to be used for data segment.*/
REGION_ALIAS("DATA_RAM", ram0);

/* RAM region to be used for BSS segment.*/
REGION_ALIAS("BSS_RAM", ram0);

INCLUDE rules.ld
