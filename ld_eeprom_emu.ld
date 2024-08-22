/*
    ChibiOS - Copyright (C) 2006..2015 Giovanni Di Sirio.

    This file is part of ChibiOS.

    ChibiOS is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    ChibiOS is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
 * STM32F407xG memory setup.
 * Note: Use of ram1 and ram2 is mutually exclusive with use of ram0.
 * Note: flash2 length decreased by 8 bytes to use it as hardcoded CRC
 * flags.
 */
MEMORY
{
    flash : org = 0x08000000, len = 16k
    flash2 : org = 0x0800C000, len = 524288 - 16	/* NEW_APP_MAX_SIZE - CRC_INFO */
	crcinfo : org = 0x0807FFF0, len = 8		/* CRC info */
    ram0  : org = 0x20000000, len = 128k    /* SRAM1 + SRAM2 */
    ram1  : org = 0x20000000, len = 112k    /* SRAM1 */
    ram2  : org = 0x2001C000, len = 16k     /* SRAM2 */
    ram3  : org = 0x00000000, len = 0
    ram4  : org = 0x10000000, len = 62k     /* CCM SRAM */
    libif : org = 0x1000F800, len = 2k
    ram5  : org = 0x40024000, len = 4k      /* BCKP SRAM */
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

__ram0_start__          = ORIGIN(ram0);
__ram0_size__           = LENGTH(ram0);
__ram0_end__            = __ram0_start__ + __ram0_size__;
__ram1_start__          = ORIGIN(ram1);
__ram1_size__           = LENGTH(ram1);
__ram1_end__            = __ram1_start__ + __ram1_size__;
__ram2_start__          = ORIGIN(ram2);
__ram2_size__           = LENGTH(ram2);
__ram2_end__            = __ram2_start__ + __ram2_size__;
__ram3_start__          = ORIGIN(ram3);
__ram3_size__           = LENGTH(ram3);
__ram3_end__            = __ram3_start__ + __ram3_size__;
__ram4_start__          = ORIGIN(ram4);
__ram4_size__           = LENGTH(ram4);
__ram4_end__            = __ram4_start__ + __ram4_size__;
__ram5_start__          = ORIGIN(ram5);
__ram5_size__           = LENGTH(ram5);
__ram5_end__            = __ram5_start__ + __ram5_size__;
__ram6_start__          = ORIGIN(ram6);
__ram6_size__           = LENGTH(ram6);
__ram6_end__            = __ram6_start__ + __ram6_size__;
__ram7_start__          = ORIGIN(ram7);
__ram7_size__           = LENGTH(ram7);
__ram7_end__            = __ram7_start__ + __ram7_size__;

ENTRY(Reset_Handler)

SECTIONS
{
    . = 0;
    _text = .;

    startup : ALIGN(16) SUBALIGN(16)
    {
        KEEP(*(.vectors))
    } > flash

    constructors : ALIGN(4) SUBALIGN(4)
    {
        PROVIDE(__init_array_start = .);
        KEEP(*(SORT(.init_array.*)))
        KEEP(*(.init_array))
        PROVIDE(__init_array_end = .);
    } > flash2

    destructors : ALIGN(4) SUBALIGN(4)
    {
        PROVIDE(__fini_array_start = .);
        KEEP(*(.fini_array))
        KEEP(*(SORT(.fini_array.*)))
        PROVIDE(__fini_array_end = .);
    } > flash2

    .text : ALIGN(16) SUBALIGN(16)
    {
        *(.text)
        *(.text.*)
        *(.rodata)
        *(.rodata.*)
        *(.glue_7t)
        *(.glue_7)
        *(.gcc*)
    } > flash2

    .ARM.extab :
    {
        *(.ARM.extab* .gnu.linkonce.armextab.*)
    } > flash2

    .ARM.exidx : {
        PROVIDE(__exidx_start = .);
        *(.ARM.exidx* .gnu.linkonce.armexidx.*)
        PROVIDE(__exidx_end = .);
     } > flash2

    .eh_frame_hdr :
    {
        *(.eh_frame_hdr)
    } > flash2

    .eh_frame : ONLY_IF_RO
    {
        *(.eh_frame)
    } > flash2
    
    .textalign : ONLY_IF_RO
    {
        . = ALIGN(8);
    } > flash2

    . = ALIGN(4);
    _etext = .;
    _textdata = _etext;
    
    _crcinfo_start_address = 0x0807FFF0;
    
    .crcinfo _crcinfo_start_address :
	{
		KEEP(*(.crcinfo))
	}  > crcinfo

    .mstack :
    {
        . = ALIGN(8);
        __main_stack_base__ = .;
        . += __main_stack_size__;
        . = ALIGN(8);
        __main_stack_end__ = .;
    } > MAIN_STACK_RAM

    .pstack :
    {
        __process_stack_base__ = .;
        __main_thread_stack_base__ = .;
        . += __process_stack_size__;
        . = ALIGN(8);
        __process_stack_end__ = .;
        __main_thread_stack_end__ = .;
    } > PROCESS_STACK_RAM

    .data : ALIGN(4)
    {
        . = ALIGN(4);
        PROVIDE(_data = .);
        *(.data)
        *(.data.*)
        *(.ramtext)
        . = ALIGN(4);
        PROVIDE(_edata = .);
    } > DATA_RAM AT > flash2

    .bss : ALIGN(4)
    {
        . = ALIGN(4);
        PROVIDE(_bss_start = .);
        *(.bss)
        *(.bss.*)
        *(COMMON)
        . = ALIGN(4);
        PROVIDE(_bss_end = .);
        PROVIDE(end = .);
    } > BSS_RAM    

    .ram0 (NOLOAD) : ALIGN(4)
    {
        . = ALIGN(4);
        *(.ram0)
        *(.ram0.*)
        . = ALIGN(4);
        __ram0_free__ = .;
    } > ram0

    .ram1 (NOLOAD) : ALIGN(4)
    {
        . = ALIGN(4);
        *(.ram1)
        *(.ram1.*)
        . = ALIGN(4);
        __ram1_free__ = .;
    } > ram1

    .ram2 (NOLOAD) : ALIGN(4)
    {
        . = ALIGN(4);
        *(.ram2)
        *(.ram2.*)
        . = ALIGN(4);
        __ram2_free__ = .;
    } > ram2

    .ram3 (NOLOAD) : ALIGN(4)
    {
        . = ALIGN(4);
        *(.ram3)
        *(.ram3.*)
        . = ALIGN(4);
        __ram3_free__ = .;
    } > ram3

    .ram4 (NOLOAD) : ALIGN(4)
    {
        . = ALIGN(4);
        *(.ram4)
        *(.ram4.*)
        . = ALIGN(4);
        __ram4_free__ = .;
    } > ram4
    
    .libif (NOLOAD) : ALIGN(4)
    {
        . = ALIGN(4);
        *(.libif)
    } > libif

    .ram5 (NOLOAD) : ALIGN(4)
    {
        . = ALIGN(4);
        *(.ram5)
        *(.ram5.*)
        . = ALIGN(4);
        __ram5_free__ = .;
    } > ram5

    .ram6 (NOLOAD) : ALIGN(4)
    {
        . = ALIGN(4);
        *(.ram6)
        *(.ram6.*)
        . = ALIGN(4);
        __ram6_free__ = .;
    } > ram6

    .ram7 (NOLOAD) : ALIGN(4)
    {
        . = ALIGN(4);
        *(.ram7)
        *(.ram7.*)
        . = ALIGN(4);
        __ram7_free__ = .;
    } > ram7
}

/* Heap default boundaries, it is defaulted to be the non-used part
   of ram0 region.*/
__heap_base__   = __ram0_free__;
__heap_end__    = __ram0_end__;
