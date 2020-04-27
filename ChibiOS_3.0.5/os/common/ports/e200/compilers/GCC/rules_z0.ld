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

__ram_size__            = LENGTH(ram);
__ram_start__           = ORIGIN(ram);
__ram_end__             = ORIGIN(ram) + LENGTH(ram);

ENTRY(_reset_address)

SECTIONS
{
    . = ORIGIN(flash);
    .boot0 : ALIGN(16) SUBALIGN(16)
    {
        KEEP(*(.boot))
    } > flash

    .boot1 : ALIGN(16) SUBALIGN(16)
    {
        KEEP(*(.handlers))
        KEEP(*(.crt0))
        /* The vectors table requires a 2kB alignment.*/
        . = ALIGN(0x800);
        KEEP(*(.vectors))
        /* The IVPR register requires a 4kB alignment.*/
        . = ALIGN(0x1000);
        __ivpr_base__ = .;
        KEEP(*(.ivors))
    } > flash

    constructors : ALIGN(4) SUBALIGN(4)
    {
        PROVIDE(__init_array_start = .);
        KEEP(*(SORT(.init_array.*)))
        KEEP(*(.init_array))
        PROVIDE(__init_array_end = .);
    } > flash

    destructors : ALIGN(4) SUBALIGN(4)
    {
        PROVIDE(__fini_array_start = .);
        KEEP(*(.fini_array))
        KEEP(*(SORT(.fini_array.*)))
        PROVIDE(__fini_array_end = .);
    } > flash

    .text_vle : ALIGN(16) SUBALIGN(16)
    {
        *(.text_vle)
        *(.text_vle.*)
        *(.gnu.linkonce.t_vle.*)
    } > flash

    .text : ALIGN(16) SUBALIGN(16)
    {
        *(.text)
        *(.text.*)
        *(.gnu.linkonce.t.*)
    } > flash

    .rodata : ALIGN(16) SUBALIGN(16)
    {
        *(.glue_7t)
        *(.glue_7)
        *(.gcc*)
        *(.rodata)
        *(.rodata.*)
        *(.rodata1)
    } > flash

    .sdata2 : ALIGN(16) SUBALIGN(16)
    {
        __sdata2_start__ = . + 0x8000;
        *(.sdata2)
        *(.sdata2.*)
        *(.gnu.linkonce.s2.*)
        *(.sbss2)
        *(.sbss2.*)
        *(.gnu.linkonce.sb2.*)
    } > flash

    .eh_frame_hdr :
    {
        *(.eh_frame_hdr)
    } > flash

    .eh_frame : ONLY_IF_RO
    {
        *(.eh_frame)
    } > flash

    .romdata : ALIGN(16) SUBALIGN(16)
    {
        __romdata_start__ = .;
    } > flash

    .stacks : ALIGN(16) SUBALIGN(16)
    {
        . = ALIGN(8);
        __irq_stack_base__ = .;
        . += __irq_stack_size__;
        . = ALIGN(8);
        __irq_stack_end__ = .;
        __process_stack_base__ = .;
        __main_thread_stack_base__ = .;
        . += __process_stack_size__;
        . = ALIGN(8);
        __process_stack_end__ = .;
        __main_thread_stack_end__ = .;
    } > ram

    .data : AT(__romdata_start__)
    {
        . = ALIGN(4);
        __data_start__ = .;
        *(.data)
        *(.data.*)
        *(.gnu.linkonce.d.*)
        __sdata_start__ = . + 0x8000;
        *(.sdata)
        *(.sdata.*)
        *(.gnu.linkonce.s.*)
        __data_end__ = .;
    } > ram

    .sbss :
    {
        __bss_start__ = .;
        *(.sbss)
        *(.sbss.*)
        *(.gnu.linkonce.sb.*)
        *(.scommon)
    } > ram

    .bss :
    {
        *(.bss)
        *(.bss.*)
        *(.gnu.linkonce.b.*)
        *(COMMON)
        __bss_end__ = .;
    } > ram

    __heap_base__   = __bss_end__;
    __heap_end__    = __ram_end__;
}
