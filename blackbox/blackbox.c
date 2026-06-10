/*
 * blackbox.c
 *
 * See blackbox.h for the design notes.
 */

#include "blackbox.h"
#include "SEGGER_RTT.h"
#include "conf_general.h"

// Ring buffer in main SRAM (.bss). Intentionally NOT in CCM (.ram4): CCM is
// nearly full (ADC sample buffers + LispBM) and the J-Link RTT/memory tools
// work best on the 0x20000000 region.
static volatile bb_record_t m_buf[BB_BUF_LEN];

static volatile uint32_t m_head = 0;			// Next write index
static volatile uint32_t m_count = 0;			// Total committed records since clear
static volatile uint32_t m_isr_tick = 0;		// Counts every ISR call, incl. skipped
static volatile bool m_triggered = false;		// Fault notified, post-trigger running
static volatile bool m_frozen = false;			// Writing stopped
static volatile uint32_t m_post_remaining = 0;	// Records left to write after trigger
static volatile uint8_t m_fault_code = 0;

void blackbox_init(void) {
	SEGGER_RTT_Init();
	SEGGER_RTT_printf(0, "VESC FW %d.%02d blackbox up. Build " __DATE__ " " __TIME__ "\r\n",
			FW_VERSION_MAJOR, FW_VERSION_MINOR);
	SEGGER_RTT_printf(0, "BB buf: %d rec x %d B, decimation %d, post-trigger %d\r\n",
			BB_BUF_LEN, sizeof(bb_record_t), BB_DECIMATION, BB_POST_TRIGGER);
}

volatile bb_record_t *blackbox_next_record_isr(void) {
	uint32_t tick = m_isr_tick;
	m_isr_tick = tick + 1;

	if (m_frozen) {
		return 0;
	}

#if BB_DECIMATION > 1
	if ((tick % BB_DECIMATION) != 0) {
		return 0;
	}
#endif

	volatile bb_record_t *rec = &m_buf[m_head];
	rec->tick = tick;
	rec->fault_code = m_fault_code;
	rec->flags = m_triggered ? BB_FLAG_FAULT_ACTIVE : 0;
	return rec;
}

void blackbox_commit_isr(void) {
	uint32_t head = m_head + 1;
	if (head >= BB_BUF_LEN) {
		head = 0;
	}
	m_head = head;
	m_count++;

	if (m_triggered) {
		if (m_post_remaining > 0) {
			m_post_remaining--;
		}
		if (m_post_remaining == 0) {
			m_frozen = true;
		}
	}
}

void blackbox_notify_fault(uint8_t fault_code) {
	if (m_triggered || m_frozen) {
		return;
	}
	m_fault_code = fault_code;
	m_post_remaining = BB_POST_TRIGGER;
	m_triggered = true;
}

void blackbox_clear(void) {
	// Freeze first so the ISR writer stays out while we reset.
	m_frozen = true;
	m_triggered = false;
	m_post_remaining = 0;
	m_fault_code = 0;
	m_head = 0;
	m_count = 0;
	m_frozen = false;
}

bool blackbox_is_frozen(void) {
	return m_frozen;
}

bool blackbox_is_triggered(void) {
	return m_triggered;
}

uint8_t blackbox_fault_code(void) {
	return m_fault_code;
}

uint32_t blackbox_sample_count(void) {
	return m_count;
}

uint32_t blackbox_isr_tick(void) {
	return m_isr_tick;
}

const volatile bb_record_t *blackbox_get_record(uint32_t age) {
	uint32_t count = m_count;
	uint32_t head = m_head;

	if (count == 0 || age >= count || age >= BB_BUF_LEN) {
		return 0;
	}

	uint32_t idx = (head + BB_BUF_LEN - 1 - age) % BB_BUF_LEN;
	return &m_buf[idx];
}
