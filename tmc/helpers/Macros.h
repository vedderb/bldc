/*
 * Macros.h
 *
 *  Created on: 04.01.2018
 *      Author: LH
 */

#ifndef TMC_MACROS_H_
#define TMC_MACROS_H_

/* Cast a n bit signed int to a 32 bit signed int
 * This is done by checking the MSB of the signed int (Bit n).
 * If it is 1, the value is negative and the Bits 32 to n+1 are set to 1
 * If it is 0, the value remains unchanged
 */
#define CAST_Sn_TO_S32(value, n) ((value) | (((value) & ((uint32_t)1<<((n)-1)))? ~(((uint32_t)1<<(n))-1) : 0 ))

// Min/Max macros
#ifndef MIN
	#define MIN(a,b) (((a)<(b)) ? (a) : (b))
#endif
#ifndef MAX
	#define MAX(a,b) (((a)>(b)) ? (a) : (b))
#endif

// Static Array length
#define ARRAY_SIZE(x) (sizeof(x)/sizeof(x[0]))

// Generic mask/shift macros
#define FIELD_GET(data, mask, shift) \
	(((data) & (mask)) >> (shift))
#define FIELD_SET(data, mask, shift, value) \
	(((data) & (~(mask))) | (((value) << (shift)) & (mask)))

// Register read/write/update macros using Mask/Shift:
#define FIELD_READ(read, motor, address, mask, shift) \
	FIELD_GET(read(motor, address), mask, shift)
#define FIELD_WRITE(write, motor, address, mask, shift, value) \
	(write(motor, address, ((value)<<(shift)) & (mask)))
#define FIELD_UPDATE(read, write, motor, address, mask, shift, value) \
	(write(motor, address, FIELD_SET(read(motor, address), mask, shift, value)))

// Macro to surpress unused parameter warnings
#ifndef UNUSED
	#define UNUSED(x) (void)(x)
#endif

// Memory access helpers
// Force the compiler to access a location exactly once
#define ACCESS_ONCE(x) *((volatile typeof(x) *) (&x))

// Macro to remove write bit for shadow register array access
#define TMC_ADDRESS(x) ((x) & (TMC_ADDRESS_MASK))

#endif /* TMC_MACROS_H_ */
