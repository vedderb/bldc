/*
 * RegisterAccess.h
 *
 *  Created on: 12.07.2017
 *      Author: LK
 *
 *  The permission system aims to allow a general-purpose implementation for
 *  all common hardware register usages. This includes:
 *  - Trivial Cases: Read, Write, Read & Write
 *
 *  - Read & Write accesses that route to different values/functions of a chip.
 *    (e.g. serial communication, where read/write corresponds to RX/TX)
 *  - Read to clear, write to clear. This does not directly affect the access,
 *    but can be used to implement a software shadow register for flags
 *    (ORing the read value into a shadow register instead of overwriting).
 *  - Registers with default values that are not known (e.g. Factory configuration
 *    values that should not be overwritten by default).
 */

#ifndef TMC_HELPERS_REGISTERACCESS_H
#define TMC_HELPERS_REGISTERACCESS_H

// Register access bits
/* Lower nibble is used for read/write, higher nibble is used for
 * special case registers. This makes it easy to identify the read/write
 * part of the permissions in a hexadecimal permission number.
 * The dirty bit will only ever be set at runtime, so we keep the easily
 * readable lower nibble.
 */
#define TMC_ACCESS_NONE        0x00

#define TMC_ACCESS_READ        0x01
#define TMC_ACCESS_WRITE       0x02
                            // 0x04 is currently unused
#define TMC_ACCESS_DIRTY       0x08  // Register has been written since reset -> shadow register is valid for restore

// Special Register bits
#define TMC_ACCESS_RW_SPECIAL  0x10  // Read and write are independent - different values and/or different functions
#define TMC_ACCESS_FLAGS       0x20  // Register has read or write to clear flags.
#define TMC_ACCESS_HW_PRESET   0x40  // Register has hardware presets (e.g. Factory calibrations) - do not write a default value
                            // 0x80 is currently unused

// Permission combinations
#define TMC_ACCESS_RW              (TMC_ACCESS_READ  | TMC_ACCESS_WRITE)        // 0x03 - Read and write
#define TMC_ACCESS_RW_SEPARATE     (TMC_ACCESS_RW    | TMC_ACCESS_RW_SPECIAL)   // 0x13 - Read and write, with separate values/functions
#define TMC_ACCESS_R_FLAGS         (TMC_ACCESS_READ  | TMC_ACCESS_FLAGS)        // 0x21 - Read, has flags (read to clear)
#define TMC_ACCESS_RW_FLAGS        (TMC_ACCESS_RW    | TMC_ACCESS_FLAGS)        // 0x23 - Read and write, has flags (read or write to clear)
#define TMC_ACCESS_W_PRESET        (TMC_ACCESS_WRITE | TMC_ACCESS_HW_PRESET)    // 0x42 - Write, has hardware preset - skipped in reset routine
#define TMC_ACCESS_RW_PRESET       (TMC_ACCESS_RW    | TMC_ACCESS_HW_PRESET)    // 0x43 - Read and write, has hardware presets - skipped in reset routine

// Helper macros
#define TMC_IS_READABLE(x)    ((x) & TMC_ACCESS_READ)
#define TMC_IS_WRITABLE(x)    ((x) & TMC_ACCESS_WRITE)
#define TMC_IS_DIRTY(x)       ((x) & TMC_ACCESS_DIRTY)
#define TMC_IS_PRESET(x)      ((x) & TMC_ACCESS_HW_PRESET)
#define TMC_IS_RESETTABLE(x)  (((x) & (TMC_ACCESS_W_PRESET)) == TMC_ACCESS_WRITE) // Write bit set, Hardware preset bit not set
#define TMC_IS_RESTORABLE(x)  (((x) & TMC_ACCESS_WRITE) && (!(x & TMC_ACCESS_HW_PRESET) || (x & TMC_ACCESS_DIRTY))) // Write bit set, if it's a hardware preset register, it needs to be dirty

// Struct for listing registers that have constant contents which we cannot
// obtain by reading them due to the register not being read-back.
typedef struct
{
	uint8_t address;
	uint32_t value;
} TMCRegisterConstant;

// Helper define:
// Most register permission arrays are initialized with 128 values.
// In those fields its quite hard to have an easy overview of available
// registers. For that, ____ is defined to 0, since 4 underscores are
// very easy to distinguish from the 2-digit hexadecimal values.
// This way, the used registers (permission != ACCESS_NONE) are easily spotted
// amongst unused (permission == ACCESS_NONE) registers.
#define ____ 0x00

// Helper define:
// Default reset values are not used if the corresponding register has a
// hardware preset. Since this is not directly visible in the default
// register reset values array, N_A is used as an indicator for a preset
// value, where any value will be ignored anyways (N_A: not available).
#define N_A 0

#endif /* TMC_HELPERS_REGISTERACCESS_H */
