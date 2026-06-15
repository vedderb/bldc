/*
 * blackbox.c
 *
 * See blackbox.h for the design notes.
 */

#include "blackbox.h"
#include "SEGGER_RTT.h"
#include "conf_general.h"
#include "mcpwm_foc.h"
#include "mc_interface.h"
#include "ch.h"

#include <stdio.h>
#include <string.h>
#include <math.h>

#ifndef BB_STREAM_DECIMATION
#define BB_STREAM_DECIMATION	1
#endif

#ifndef BB_STREAM_BATCH
#define BB_STREAM_BATCH			16
#endif

#define BB_STREAM_MAGIC			"BBIN"
#define BB_STREAM_VERSION		7

// Compact binary stream fixed-point scales. Currents are int16 at
// 0.02 A/LSB (+-655 A range). The electrical angle is int16 over the
// firmware -pi..pi range. Keeps the record at 16 B to fit RTT bandwidth.
#define BB_STREAM_I_LSB_PER_A	50.0f
#define BB_STREAM_ANG_FULL		32768.0f
#define BB_STREAM_ANG_LSB		(BB_STREAM_ANG_FULL / (float)M_PI)
#define BB_STREAM_I16_MAX		32767.0f
#define BB_STREAM_I16_MIN		(-32768.0f)

typedef struct __attribute__((packed)) {
	uint32_t tick;
	int16_t ia;				// Currents: int16, BB_STREAM_I_LSB_PER_A LSB per amp
	int16_t ib;
	int16_t ic;
	int16_t id;
	int16_t iq;
	int16_t phase;			// Electrical angle: int16 over -pi..pi
} bb_stream_record_t;

static inline int16_t bb_stream_clamp_i16(float v) {
	if (v > BB_STREAM_I16_MAX) {
		v = BB_STREAM_I16_MAX;
	} else if (v < BB_STREAM_I16_MIN) {
		v = BB_STREAM_I16_MIN;
	}
	return (int16_t)v;
}

typedef struct __attribute__((packed)) {
	char magic[4];
	uint8_t version;
	uint8_t record_size;
	uint16_t count;
	uint32_t checksum;
} bb_stream_header_t;

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
static volatile bool m_dump_request = false;

// Freeze-on-fault is disabled by default for now (RTT bring-up phase);
// toggle with 'f' on the RTT down channel.
static volatile bool m_freeze_enabled = false;
// Periodic live telemetry over RTT, toggle with 'l'.
static volatile bool m_live_print = false;
// Binary live stream over RTT. The host parser ignores other text.
static volatile bool m_stream_enabled = false;
static volatile uint32_t m_stream_next_count = 0;
static uint8_t m_stream_packet[sizeof(bb_stream_header_t) + BB_STREAM_BATCH * sizeof(bb_stream_record_t)];

static THD_WORKING_AREA(dump_thread_wa, 2048);
static THD_FUNCTION(dump_thread, arg);

void blackbox_init(void) {
	SEGGER_RTT_Init();
	SEGGER_RTT_printf(0, "VESC FW %d.%02d blackbox up. Build " __DATE__ " " __TIME__ "\r\n",
			FW_VERSION_MAJOR, FW_VERSION_MINOR);
	SEGGER_RTT_printf(0, "BB buf: %d rec x %d B, decimation %d, post-trigger %d\r\n",
			BB_BUF_LEN, sizeof(bb_record_t), BB_DECIMATION, BB_POST_TRIGGER);

	chThdCreateStatic(dump_thread_wa, sizeof(dump_thread_wa), LOWPRIO, dump_thread, NULL);
}

void blackbox_request_dump(void) {
	m_dump_request = true;
}

// Wait (in thread context) until the RTT up buffer has room for len bytes,
// then write. Gives up after ~2 s so a missing host reader never wedges us.
static bool rtt_write_chunk(const char *buf, unsigned int len) {
	int timeout_ms = 2000;
	while (SEGGER_RTT_GetAvailWriteSpace(0) < len) {
		if (timeout_ms-- <= 0) {
			return false;
		}
		chThdSleepMilliseconds(1);
	}
	SEGGER_RTT_Write(0, buf, len);
	return true;
}

static void dump_csv(void) {
	char line[224];
	int len;

	// Snapshot the indices. For a torn-free dump trigger it on a frozen
	// buffer; a live dump may contain a few records overwritten mid-read.
	uint32_t count = m_count;
	uint32_t head = m_head;
	uint32_t n = (count < BB_BUF_LEN) ? count : BB_BUF_LEN;

	len = snprintf(line, sizeof(line),
			"#BB_DUMP_BEGIN,ver=1,f_isr_hz=%.1f,decimation=%d,buflen=%d,records=%lu,frozen=%d,fault=%d\r\n",
			(double)mcpwm_foc_get_sampling_frequency_now(), BB_DECIMATION, BB_BUF_LEN,
			(unsigned long)n, m_frozen ? 1 : 0, m_fault_code);
	if (!rtt_write_chunk(line, len)) {
		return;
	}

	len = snprintf(line, sizeof(line),
			"tick,ia,ib,ic,id,iq,i_abs,i_abs_filter,duty,v_bus,phase,speed_rad_s,fault,state,mode,flags\r\n");
	if (!rtt_write_chunk(line, len)) {
		return;
	}

	for (uint32_t i = 0; i < n; i++) {
		uint32_t idx = (head + BB_BUF_LEN - n + i) % BB_BUF_LEN;
		volatile bb_record_t *r = &m_buf[idx];

		len = snprintf(line, sizeof(line),
				"%lu,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.4f,%.2f,%.4f,%.2f,%u,%u,%u,%u\r\n",
				(unsigned long)r->tick,
				(double)r->ia, (double)r->ib, (double)r->ic,
				(double)r->id, (double)r->iq,
				(double)r->i_abs, (double)r->i_abs_filter,
				(double)r->duty_now, (double)r->v_bus,
				(double)r->phase, (double)r->speed_rad_s,
				(unsigned int)r->fault_code, (unsigned int)r->state,
				(unsigned int)r->control_mode, (unsigned int)r->flags);
		if (!rtt_write_chunk(line, len)) {
			return;
		}
	}

	len = snprintf(line, sizeof(line), "#BB_DUMP_END\r\n");
	rtt_write_chunk(line, len);
}

// Periodic live telemetry line. Written in skip mode: if the RTT buffer is
// full (no host attached) the line is simply dropped, nothing blocks.
static void live_print(void) {
	char line[224];

	int len = snprintf(line, sizeof(line),
			"BB t=%lu iq=%.2f id=%.2f iabs=%.2f duty=%.3f vbus=%.1f erpm=%.0f "
			"ang_used=%.1f ang_obs=%.1f ang_enc=%.1f ang_hall=%.1f fault=%d\r\n",
			(unsigned long)m_isr_tick,
			(double)mcpwm_foc_get_iq(), (double)mcpwm_foc_get_id(),
			(double)mcpwm_foc_get_abs_motor_current(),
			(double)mcpwm_foc_get_duty_cycle_now(),
			(double)mc_interface_get_input_voltage_filtered(),
			(double)mcpwm_foc_get_rpm(),
			(double)mcpwm_foc_get_phase(),			// angle actually used by FOC (deg)
			(double)mcpwm_foc_get_phase_observer(),	// observer angle (deg)
			(double)mcpwm_foc_get_phase_encoder(),	// encoder angle (deg)
			(double)mcpwm_foc_get_phase_hall(),		// hall angle (deg)
			(int)mc_interface_get_fault());

	if (len > 0) {
		SEGGER_RTT_Write(0, line, (unsigned int)len);
	}
}

static uint32_t stream_checksum(const uint8_t *data, uint32_t len) {
	uint32_t sum = 0x9E3779B9;
	for (uint32_t i = 0; i < len; i++) {
		sum = (sum << 5) | (sum >> 27);
		sum ^= data[i];
		sum += 0x7F4A7C15;
	}
	return sum;
}

static void stream_binary(void) {
	uint32_t available = m_count;

	if (m_stream_next_count == 0 || m_stream_next_count > (available + 1)) {
		m_stream_next_count = available + 1;
		return;
	}

	uint32_t min_count = 1;
	if (available > BB_BUF_LEN) {
		min_count = available - BB_BUF_LEN + 1;
	}

	if (m_stream_next_count < min_count) {
		m_stream_next_count = min_count;
	}

	if (m_stream_next_count > available) {
		return;
	}

	uint16_t rec_count = 0;
	uint32_t next = m_stream_next_count;

	uint32_t scan = next;
	while (scan <= available && rec_count < BB_STREAM_BATCH) {
		uint32_t idx = (scan - 1) % BB_BUF_LEN;
		volatile bb_record_t *r = &m_buf[idx];

		if ((r->tick % BB_STREAM_DECIMATION) == 0) {
			rec_count++;
		}

		scan++;
	}

	if (rec_count == 0) {
		m_stream_next_count = scan;
		return;
	}

	uint32_t payload_len = rec_count * sizeof(bb_stream_record_t);
	uint32_t packet_len = sizeof(bb_stream_header_t) + payload_len;
	if (SEGGER_RTT_GetAvailWriteSpace(0) < packet_len) {
		// Keep the control thread non-blocking. Drop stale data and resume live.
		m_stream_next_count = available + 1;
		return;
	}

	bb_stream_header_t *header = (bb_stream_header_t*)m_stream_packet;
	memcpy(header->magic, BB_STREAM_MAGIC, 4);
	header->version = BB_STREAM_VERSION;
	header->record_size = (uint8_t)sizeof(bb_stream_record_t);
	header->count = rec_count;

	uint8_t *payload = &m_stream_packet[sizeof(bb_stream_header_t)];
	uint32_t payload_ofs = 0;
	while (next < scan) {
		uint32_t idx = (next - 1) % BB_BUF_LEN;
		volatile bb_record_t *r = &m_buf[idx];

		if ((r->tick % BB_STREAM_DECIMATION) == 0) {
			bb_stream_record_t *out = (bb_stream_record_t*)&payload[payload_ofs];
			out->tick = r->tick;
			out->ia = bb_stream_clamp_i16(r->ia * BB_STREAM_I_LSB_PER_A);
			out->ib = bb_stream_clamp_i16(r->ib * BB_STREAM_I_LSB_PER_A);
			out->ic = bb_stream_clamp_i16(r->ic * BB_STREAM_I_LSB_PER_A);
			out->id = bb_stream_clamp_i16(r->id * BB_STREAM_I_LSB_PER_A);
			out->iq = bb_stream_clamp_i16(r->iq * BB_STREAM_I_LSB_PER_A);
			out->phase = bb_stream_clamp_i16(r->phase * BB_STREAM_ANG_LSB);
			payload_ofs += sizeof(bb_stream_record_t);
		}

		next++;
	}

	m_stream_next_count = next;
	header->checksum = stream_checksum(payload, payload_len);
	SEGGER_RTT_Write(0, m_stream_packet, packet_len);
}

static THD_FUNCTION(dump_thread, arg) {
	(void)arg;
	chRegSetThreadName("blackbox dump");

	int live_div = 0;

	for (;;) {
		// Host-side control over the RTT down channel:
		// 'd' = dump, 'c' = clear, 'l' = toggle live print, 's' = start binary stream,
		// 'x' = stop binary stream,
		// 'f' = toggle freeze-on-fault.
		char cmd;
		while (SEGGER_RTT_Read(0, &cmd, 1) > 0) {
			if (cmd == 'd' || cmd == 'D') {
				m_dump_request = true;
			} else if (cmd == 'c' || cmd == 'C') {
				blackbox_clear();
				SEGGER_RTT_WriteString(0, "#BB_CLEARED\r\n");
			} else if (cmd == 'l' || cmd == 'L') {
				m_live_print = !m_live_print;
				SEGGER_RTT_WriteString(0, m_live_print ? "#BB_LIVE_ON\r\n" : "#BB_LIVE_OFF\r\n");
			} else if (cmd == 's' || cmd == 'S') {
				m_stream_enabled = true;
				m_stream_next_count = m_count + 1;
				SEGGER_RTT_WriteString(0, "#BB_STREAM_ON\r\n");
			} else if (cmd == 'x' || cmd == 'X') {
				m_stream_enabled = false;
				SEGGER_RTT_WriteString(0, "#BB_STREAM_OFF\r\n");
			} else if (cmd == 'f' || cmd == 'F') {
				m_freeze_enabled = !m_freeze_enabled;
				SEGGER_RTT_WriteString(0, m_freeze_enabled ? "#BB_FREEZE_ON\r\n" : "#BB_FREEZE_OFF\r\n");
			}
		}

		if (m_dump_request) {
			m_dump_request = false;
			dump_csv();
		}

		if (m_stream_enabled) {
			stream_binary();
		}

		// 10 ms loop, live line every 20th iteration = 5 Hz.
		if (m_live_print && ++live_div >= 20) {
			live_div = 0;
			live_print();
		}

		chThdSleepMilliseconds(1);
	}
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

	if (m_freeze_enabled) {
		m_post_remaining = BB_POST_TRIGGER;
		m_triggered = true;
	}
}

void blackbox_set_freeze_enabled(bool enabled) {
	m_freeze_enabled = enabled;
}

bool blackbox_freeze_enabled(void) {
	return m_freeze_enabled;
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
