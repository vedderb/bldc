/*
 * blackbox.h
 *
 * RAM ring-buffer "black box" for capturing FOC state around faults
 * (primarily FAULT_CODE_ABS_OVER_CURRENT). Written from the FOC ADC ISR,
 * read from thread context (terminal / RTT export).
 *
 * Design constraints:
 * - Single writer (FOC ADC ISR), no locks, no malloc, no formatting in ISR.
 * - On fault the buffer keeps writing BB_POST_TRIGGER more records and then
 *   freezes, preserving data from before and after the trigger.
 */

#ifndef BLACKBOX_H_
#define BLACKBOX_H_

#include <stdint.h>
#include <stdbool.h>

// Number of records in the ring buffer. 256 x 56 B = 14 KB in main SRAM.
// Stock 75_300 has ~25 KB of ram0 left for .bss + heap, so this leaves
// roughly 10 KB of heap headroom. Verify with the terminal "mem" command.
#ifndef BB_BUF_LEN
#define BB_BUF_LEN			256
#endif

// Record every BB_DECIMATION:th FOC ADC ISR (1 = every ISR, ~12.5 kHz).
#ifndef BB_DECIMATION
#define BB_DECIMATION		1
#endif

// Records still written after a fault notification before freezing (25 %).
#ifndef BB_POST_TRIGGER
#define BB_POST_TRIGGER		(BB_BUF_LEN / 4)
#endif

// bb_record_t::flags bits
#define BB_FLAG_FAULT_ACTIVE	(1 << 0) // Fault was already notified when this record was written

typedef struct {
	uint32_t tick;			// FOC ADC ISR counter (counts every ISR, also skipped ones)
	float ia;				// Phase currents (A)
	float ib;
	float ic;
	float id;				// dq currents (A)
	float iq;
	float i_abs;			// sqrt(id^2 + iq^2), the ABS_OVER_CURRENT comparison variable
	float i_abs_filter;		// Filtered version (used when l_slow_abs_current is true)
	float duty_now;
	float v_bus;			// Input voltage (V)
	float phase;			// Electrical phase (rad)
	float speed_rad_s;		// PLL speed (electrical rad/s)
	uint8_t fault_code;		// mc_fault_code that triggered the freeze (0 = none)
	uint8_t state;			// mc_state
	uint8_t control_mode;	// mc_control_mode
	uint8_t flags;			// BB_FLAG_*
} bb_record_t;

// Thread context API
void blackbox_init(void);
void blackbox_clear(void);
// Ask the dump thread to stream the whole ring buffer over RTT as CSV.
// Also triggerable from the host by sending 'd' on RTT down channel 0.
void blackbox_request_dump(void);
// Enable/disable freezing the buffer after a fault (default: disabled).
// Also toggleable from the host with 'f' on the RTT down channel.
void blackbox_set_freeze_enabled(bool enabled);
bool blackbox_freeze_enabled(void);
bool blackbox_is_frozen(void);
bool blackbox_is_triggered(void);
uint8_t blackbox_fault_code(void);
uint32_t blackbox_sample_count(void);	// Total records written since last clear
uint32_t blackbox_isr_tick(void);
// Get record by age; age 0 = newest, age 1 = previous, ... Returns NULL if not available.
const volatile bb_record_t *blackbox_get_record(uint32_t age);

// ISR context API (FOC ADC ISR only)
// Returns the slot to fill for this ISR cycle, or NULL when skipped
// (decimation) or frozen. Must be paired with blackbox_commit_isr().
volatile bb_record_t *blackbox_next_record_isr(void);
void blackbox_commit_isr(void);

// Callable from ISR or thread context. Arms the post-trigger countdown,
// after which the buffer freezes. Only the first notification counts.
void blackbox_notify_fault(uint8_t fault_code);

#endif /* BLACKBOX_H_ */
