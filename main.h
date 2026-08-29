/*
	Copyright 2021 Benjamin Vedder	benjamin@vedder.se

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef MAIN_H_
#define MAIN_H_

bool main_init_done(void);
uint32_t main_calc_hw_crc(void);
void main_system_halt(const char *reason);
void main_fault_handler(void);

typedef struct {
	uint32_t r0;
	uint32_t r1;
	uint32_t r2;
	uint32_t r3;
	uint32_t r12;
	uint32_t lr;
	uint32_t pc;
	uint32_t psr;

	uint32_t cfsr;
	uint32_t hfsr;
	uint32_t mmfar;
	uint32_t bfar;
	uint32_t afsr;
	uint32_t shcsr;
} CrashRegisters;

typedef enum {
	CRASH_NONE = 0,   // zero so a struct wipe resets it
	CRASH_HALT,       // halt_reason is valid
	CRASH_REGISTERS,  // registers are valid
} CrashType;

typedef struct {
	uint32_t magic;
	uint32_t boot_count;   // number of boots since the struct was last wiped
	uint32_t reset_flags;  // RCC_CSR snapshot of the current boot
	uint32_t crash_streak; // consecutive preceding boots that ended in a crash, halt or watchdog reset
	CrashType type;
	uint32_t crash_boot;      // boot_count at the time the crash/halt was stored
	uint32_t pvd_dips;        // number of supply dips below the PVD threshold (2.9V)
	uint32_t pvd_last_uptime; // uptime in seconds at the last supply dip
	const char *halt_reason;
	CrashRegisters registers;
} CrashInfo;

// Marks crash_info as valid. The struct lives in noinit RAM, so its contents are
// only meaningful if RAM actually survived the reset: after a power loss (SRAM
// decay) or a firmware update that moves the section, the magic won't match and
// the struct is wiped at boot. Including the size also invalidates it on a
// firmware update that changes the layout without moving the section.
#define CRASH_INFO_MAGIC	(0xB0070000 | sizeof(CrashInfo))

extern CrashInfo crash_info;

#endif /* MAIN_H_ */
