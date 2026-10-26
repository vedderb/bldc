/*
	Copyright 2024 - Custom app for VESC firmware

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

/*
 * Custom control: PPM throttle + ADC2 brake + ADC1 gain + reverse button.
 *
 * A current-control scheme modelled on the ADC "Current Reverse, ADC2 Brake,
 * Button" mode, but the throttle comes from the PPM/servo input instead of ADC1,
 * and ADC1 is repurposed as a 0..1 gain knob.
 *
 * Inputs
 * ------
 *   Throttle : PPM pulse on the ICU pin   (HW_ICU_GPIO / HW_ICU_PIN), 0..1
 *   Gain     : analog voltage on ADC1     (HW_ADC_EXT  / ADC_IND_EXT),  0..1
 *   Brake    : analog voltage on ADC2     (HW_ADC_EXT2 / ADC_IND_EXT2), 0..1
 *   Reverse  : digital button on UART RX  (HW_UART_RX_PORT / HW_UART_RX_PIN)
 *   Cruise   : digital button on UART TX  (HW_UART_TX_PORT / HW_UART_TX_PIN)  (optional)
 *
 * Configuration (all from the normal VESC Tool pages, no new fields)
 * ------------------------------------------------------------------
 *   Throttle deadband / curve / ramp : PPM page  (appconf.app_ppm_conf)
 *   Gain  voltage range / filter / inv: ADC page  voltage_start/end, use_filter,
 *                                        voltage_inverted
 *   Brake voltage range / filter / inv: ADC page  voltage2_start/end, use_filter,
 *                                        voltage2_inverted
 *   Brake ramp                        : ADC page  ramp_time_pos / ramp_time_neg
 *   Safe-start mode                   : PPM page  safe_start
 *
 * Signal pipeline (per loop, ~100 Hz)
 * -----------------------------------
 *   throttle = map(PPM)              -> 0..1
 *   throttle = deadband + throttle_curve(throttle)   (PPM page; pre-gain)
 *   throttle_stick = throttle                        (saved for safe-start idle)
 *   gain     = map(ADC1)             -> 0..1   (utils_map + truncate caps it)
 *   brake    = map(ADC2)             -> 0..1
 *   throttle *= gain ; brake *= gain           (ADC1 scales BOTH drive and brake)
 *
 *   Curve and safe-start idle are computed on the PRE-gain stick, so the gain
 *   knob changes power but never reshapes the curve nor fakes "throttle idle".
 *
 * Arbitration & output (single signed command, one ramp tracker)
 * --------------------------------------------------------------
 *   - BRAKE HAS PRIORITY: if brake > threshold, target = -brake and the PPM
 *     throttle is ignored entirely. Otherwise target = +throttle.
 *   - One ramp tracker on the overall signed output. The ramp time constant is
 *     taken from the *currently active* input (PPM page when driving, ADC page
 *     when braking); pos = |output| rising, neg = |output| falling. Ramp 0 =>
 *     instant. So full-throttle then stab-brake ramps +1 -> -1 on the brake
 *     ramp; releasing the brake ramps back toward throttle on the throttle ramp.
 *   - cmd < 0  => BRAKE path: mc_interface_set_brake_current_rel(|cmd|).
 *                 Respects the proportional brake and BYPASSES safe-start and the
 *                 PPM signal-loss timeout (braking is always allowed).
 *   - cmd >= 0 => DRIVE path: blocked by PPM signal-loss timeout and by
 *                 safe-start; reverse button negates; optional cruise control;
 *                 then mc_interface_set_current_rel(cmd).
 *
 * Safe-start (PPM throttle only)
 * ------------------------------
 *   ms_without_power counts UP only while the throttle is idle, and is reset to 0
 *   on boot and on fault (unless SAFE_START_NO_FAULT). Driving does NOT reset it.
 *   Driving is blocked until the throttle has been idle for MIN_MS_WITHOUT_POWER,
 *   i.e. you must release the throttle after boot/fault before it will drive.
 *   Braking is never gated by safe-start.
 */

#include "app.h"
#include "ch.h"
#include "hal.h"

#include "mc_interface.h"
#include "utils_math.h"
#include "utils_sys.h"
#include "servo_dec.h"
#include "comm_can.h"
#include "hw.h"
#include "commands.h"
#include "terminal.h"
#include "timeout.h"

#include <math.h>

// Reverse / cruise button pins (separate from the PPM/ICU pin)
#define REV_BUTTON_PORT			HW_UART_RX_PORT
#define REV_BUTTON_PIN			HW_UART_RX_PIN
#define CC_BUTTON_PORT			HW_UART_TX_PORT
#define CC_BUTTON_PIN			HW_UART_TX_PIN

// Buttons are active-low (pressed = pin pulled to GND). Set to 0 if active-high.
#define BUTTONS_ACTIVE_LOW		1

// Set to 1 to enable cruise-control (hold CC button with no throttle -> hold rpm)
#define ENABLE_CRUISE_CONTROL	1

// ADC1 gain knob. 1 = throttle/brake scaled by ADC1 (the intended behaviour).
// Set to 0 to bypass it (gain forced to 1.0) for bench-testing PPM-only when
// nothing is wired to ADC1 - otherwise a low/unwired ADC1 zeroes the throttle.
#ifndef USE_ADC1_GAIN
#define USE_ADC1_GAIN			1
#endif

// ADC2 brake. 1 = proportional brake from ADC2. Set to 0 when ADC2 is not wired
// so a FLOATING brake pin can't trigger spurious braking (brake has priority and
// would otherwise block the throttle and inject brake current at standstill).
#ifndef USE_ADC2_BRAKE
#define USE_ADC2_BRAKE			1
#endif

// The power-ratio (ADC1 gain) and the reverse direction are only re-sampled
// while the motor is at STANDSTILL and the throttle is released, then held for
// the whole ride so they can't change mid-drive. "Standstill" = |erpm| below the
// PPM page's "Max ERPM for direction" (max_erpm_for_dir), or this if that is 0.
#define LATCH_ERPM_FALLBACK		2000.0

#define MIN_MS_WITHOUT_POWER	500.0
#define FILTER_SAMPLES			5
#define RPM_FILTER_SAMPLES		8
#define MAX_CAN_AGE				0.1

// Threads
static THD_FUNCTION(ppm_adc_brake_thread, arg);
static THD_WORKING_AREA(ppm_adc_brake_thread_wa, 2048);

// Private variables
static volatile bool stop_now = true;
static volatile bool is_running = false;
static volatile app_configuration appconf;
static volatile float ms_without_power = 0.0;
static volatile float input_filtered_brake = 0.0;
static volatile float input_filtered_gain = 0.0;
// Control state that must survive between loop iterations but be reset on each
// app (re)start so a reconfigure never acts on a stale command.
static volatile float output_ramp = 0.0;        // ramped signed output (-brake..+drive)
static volatile systime_t last_ramp_time = 0;   // timestamp for the ramp dt
static volatile bool  cc_was_pid = false;        // cruise-control latch
static volatile float cc_pid_rpm = 0.0;          // cruise-control held rpm
static volatile bool  ppm_rx = false;            // set by servo_func on each PPM pulse
static volatile float latched_gain = 1.0;        // power ratio (ADC1) held during a ride
static volatile bool  latched_rev = false;       // reverse direction held during a ride

// Latest computed values, exposed via the "ppm_adc_dbg" terminal command.
static volatile float dbg_ppm = 0.0;       // raw PPM servo value (-1..1)
static volatile float dbg_gain = 0.0;      // ADC1 gain (0..1)
static volatile float dbg_throttle = 0.0;  // throttle after gain (0..1)
static volatile float dbg_brake = 0.0;     // brake after gain (0..1)
static volatile float dbg_cmd = 0.0;       // signed ramped output (-brake..+drive)
static volatile bool  dbg_armed = false;   // safe-start armed (drive allowed by safe-start)
static volatile bool  dbg_ppm_ok = false;  // valid PPM signal present this loop

// Experiment-plot streaming (toggled via the "ppm_adc_plot" terminal command).
static volatile bool  plot_enabled = false;
static volatile float plot_sample = 0.0;   // plot x-axis (seconds)

static void servo_func(void) {
	// Called by servodec on each decoded PPM pulse (interrupt context).
	ppm_rx = true;
}

// Terminal command: prints the LIVE values the control loop is actually using.
// VESC Tool's ADC RT tab reads the dormant ADC-app telemetry (0 when a custom
// app runs), so use this instead to see the real ADC pins. Type: ppm_adc_dbg
static void terminal_dbg(int argc, const char **argv) {
	(void)argc; (void)argv;
	commands_printf("--- app_ppm_adc_brake ---");
	commands_printf("PPM servo      : %.3f", (double)dbg_ppm);
	commands_printf("ADC1 gain pin  : %.3f V   (range %.2f..%.2f V)",
			(double)ADC_VOLTS(ADC_IND_EXT),
			(double)appconf.app_adc_conf.voltage_start,
			(double)appconf.app_adc_conf.voltage_end);
	commands_printf("ADC2 brake pin : %.3f V   (range %.2f..%.2f V)",
			(double)ADC_VOLTS(ADC_IND_EXT2),
			(double)appconf.app_adc_conf.voltage2_start,
			(double)appconf.app_adc_conf.voltage2_end);
	commands_printf("gain=%.3f  throttle=%.3f  brake=%.3f  cmd=%.3f",
			(double)dbg_gain, (double)dbg_throttle,
			(double)dbg_brake, (double)dbg_cmd);
	commands_printf("PPM signal=%s  safe-start armed=%s  ms_idle=%.0f/%.0f",
			dbg_ppm_ok ? "OK" : "LOST",
			dbg_armed ? "YES" : "NO (blocked)",
			(double)ms_without_power, (double)MIN_MS_WITHOUT_POWER);
	commands_printf("latched reverse=%d  latched gain=%.3f  (sampled only at standstill+idle)",
			latched_rev, (double)latched_gain);
	bool can_drive = dbg_ppm_ok && dbg_armed && !app_is_output_disabled();
	commands_printf("can drive now=%s  output_disabled=%d  fault=%d  [GAIN=%d BRAKE=%d]",
			can_drive ? "YES" : "NO",
			app_is_output_disabled(), mc_interface_get_fault(),
			USE_ADC1_GAIN, USE_ADC2_BRAKE);
}

// Terminal command: start/stop streaming live curves to VESC Tool's
// Realtime Data -> Experiment plot. "ppm_adc_plot 1" on, "ppm_adc_plot 0" off,
// no arg = toggle. Self-contained: no shared-code (app_adc/commands) changes.
static void terminal_plot(int argc, const char **argv) {
	bool en = !plot_enabled;            // toggle by default
	if (argc == 2) {
		en = (argv[1][0] != '0');       // "0" -> off, anything else -> on
	}

	if (en && !plot_enabled) {
		commands_init_plot("Time (s)", "Value");
		commands_plot_add_graph("ADC1 V");      // graph 0
		commands_plot_add_graph("ADC2 V");      // graph 1
		commands_plot_add_graph("throttle");    // graph 2
		commands_plot_add_graph("cmd");         // graph 3 (+drive / -brake)
		commands_plot_add_graph("armed");       // graph 4 (1=safe-start armed)
		plot_sample = 0.0;
	}

	plot_enabled = en;
	commands_printf(en ?
			"ppm_adc plot: ON  (open Realtime Data -> Experiment)" :
			"ppm_adc plot: OFF");
}

static inline bool read_button(ioportid_t port, int pin) {
	bool b = palReadPad(port, pin);
#if BUTTONS_ACTIVE_LOW
	b = !b;
#endif
	return b;
}

void app_custom_start(void) {
	// ADC1 (gain) and ADC2 (brake) pins -> analog input
	palSetPadMode(HW_ADC_EXT_GPIO,  HW_ADC_EXT_PIN,  PAL_MODE_INPUT_ANALOG);
	palSetPadMode(HW_ADC_EXT2_GPIO, HW_ADC_EXT2_PIN, PAL_MODE_INPUT_ANALOG);

	// Button pins -> digital input (pull-up because buttons are active-low)
	palSetPadMode(REV_BUTTON_PORT, REV_BUTTON_PIN, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(CC_BUTTON_PORT,  CC_BUTTON_PIN,  PAL_MODE_INPUT_PULLUP);

	// Start PPM/servo decoding on the ICU pin
	servodec_init(servo_func);

	terminal_register_command_callback(
			"ppm_adc_dbg",
			"Print live PPM/ADC1/ADC2/gain/brake/throttle for app_ppm_adc_brake",
			0,
			terminal_dbg);
	terminal_register_command_callback(
			"ppm_adc_plot",
			"Stream ADC1/ADC2/gain/throttle/brake to the Experiment plot (1=on 0=off, toggle if none)",
			"[0/1]",
			terminal_plot);

	stop_now = false;
	// Reset all persistent control state so a (re)start never acts on stale values.
	ms_without_power = 0.0;
	output_ramp = 0.0;
	last_ramp_time = chVTGetSystemTimeX();
	cc_was_pid = false;
	input_filtered_brake = 0.0;
	input_filtered_gain = 0.0;
	plot_enabled = false;
	ppm_rx = false;
	latched_gain = 1.0;
	latched_rev = false;
	// Set is_running here (not just in the thread) so a configure() call that
	// arrives right after start() applies the servo pulse options immediately.
	is_running = true;
	chThdCreateStatic(ppm_adc_brake_thread_wa, sizeof(ppm_adc_brake_thread_wa),
			NORMALPRIO, ppm_adc_brake_thread, NULL);
}

void app_custom_stop(void) {
	servodec_stop();
	terminal_unregister_callback(terminal_dbg);
	terminal_unregister_callback(terminal_plot);

	stop_now = true;
	while (is_running) {
		chThdSleepMilliseconds(1);
	}
}

void app_custom_configure(app_configuration *conf) {
	appconf = *conf;

	// Apply PPM pulse range to the servo decoder. Guard with is_running because
	// app.c calls app_custom_configure() on every config change regardless of
	// which app is active, and servodec must not be touched unless WE started it.
	if (is_running) {
		servodec_set_pulse_options(conf->app_ppm_conf.pulse_start,
				conf->app_ppm_conf.pulse_end,
				conf->app_ppm_conf.median_filter);
	}

	ms_without_power = 0.0;
}

static THD_FUNCTION(ppm_adc_brake_thread, arg) {
	(void)arg;

	chRegSetThreadName("App PPM+ADC Brake");
	is_running = true;

	const float sleep_time_s = 0.01; // 10 ms loop

	for (;;) {
		if (stop_now) {
			is_running = false;
			return;
		}

		chThdSleepMilliseconds(10);

		const volatile ppm_config *pconf = &appconf.app_ppm_conf;
		const volatile adc_config *aconf = &appconf.app_adc_conf;

		// Feed the global timeout on every received PPM pulse (like the stock PPM
		// app). Without this, if the global timeout ever latches during the boot/
		// arming window our drive gate bails to brake WITHOUT resetting it, so it
		// stays latched until an external timeout_reset() (VESC Tool COMM_ALIVE)
		// -- the "only works after connecting VESC Tool" bug.
		if (ppm_rx) {
			ppm_rx = false;
			timeout_reset();
		}

		// ---- 1) Throttle from PPM ----
		// servodec maps pulse_start..pulse_end -> -1..1. Use the FULL travel as
		// 0..1 throttle. (The old truncate(0,1) discarded the lower half, so the
		// throttle only responded past mid-stick - "press deep".)
		// With no valid PPM signal force 0 (servo_pos defaults to 0 -> would map
		// to 0.5); this also lets safe-start arm during the no-signal boot window.
		bool ppm_signal_ok = servodec_get_time_since_update() <= timeout_get_timeout_msec();
		float throttle = ppm_signal_ok ? (servodec_get_servo(0) + 1.0) * 0.5 : 0.0;
		utils_truncate_number(&throttle, 0.0, 1.0);

		// Deadband + throttle curve on the RAW stick (PPM-page settings). Done
		// before the ADC1 gain so the curve shape and the safe-start idle check
		// are independent of the gain knob (gain=0 must not look like "idle").
		utils_deadband(&throttle, pconf->hyst, 1.0);
		throttle = utils_throttle_curve(throttle, pconf->throttle_exp,
				pconf->throttle_exp_brake, pconf->throttle_exp_mode);
		float throttle_stick = throttle; // pre-gain, for safe-start idle detection

		// At rest = motor near standstill AND throttle released. The power ratio
		// (ADC1) and reverse are only re-sampled while at rest, then latched for
		// the whole ride so they can't change mid-drive.
		float latch_erpm = pconf->max_erpm_for_dir > 1.0 ?
				pconf->max_erpm_for_dir : LATCH_ERPM_FALLBACK;
		bool at_rest = fabsf(mc_interface_get_rpm()) < latch_erpm &&
				throttle_stick < 0.001;

		// ---- 1b) ADC1 = 0..1 gain knob (ADC-page voltage_start/end mapping) ----
		// utils_map() does NOT clamp on its own, so the truncate is the 0..1 cap.
		// With USE_ADC1_GAIN=0 the gain is forced to 1.0 (bench-test PPM-only).
		float gain = 1.0;
#if USE_ADC1_GAIN
		gain = ADC_VOLTS(ADC_IND_EXT);
		UTILS_LP_MOVING_AVG_APPROX(input_filtered_gain, gain, FILTER_SAMPLES);
		if (aconf->use_filter) {
			gain = input_filtered_gain;
		}
		gain = utils_map(gain, aconf->voltage_start, aconf->voltage_end, 0.0, 1.0);
		utils_truncate_number(&gain, 0.0, 1.0); // <-- the actual 0..1 cap
		if (aconf->voltage_inverted) {
			gain = 1.0 - gain;
		}
#endif
		// Only sample the power ratio at standstill+idle; otherwise hold it.
		if (at_rest) {
			latched_gain = gain;
		}
		gain = latched_gain;

		throttle *= gain; // master gain scales the (curved) throttle

		// ---- 2) Brake from ADC2 (ADC-page voltage2_start/end), also gain-scaled ----
		float brake = 0.0;
#if USE_ADC2_BRAKE
		brake = ADC_VOLTS(ADC_IND_EXT2);
		UTILS_LP_MOVING_AVG_APPROX(input_filtered_brake, brake, FILTER_SAMPLES);
		if (aconf->use_filter) {
			brake = input_filtered_brake;
		}
		brake = utils_map(brake, aconf->voltage2_start, aconf->voltage2_end, 0.0, 1.0);
		utils_truncate_number(&brake, 0.0, 1.0);
		if (aconf->voltage2_inverted) {
			brake = 1.0 - brake;
		}

		brake *= gain; // scale brake by the same ADC1 knob
#endif

		// ---- 3) Buttons ----
		// Reverse is only re-sampled at standstill+idle, then latched for the ride.
		if (at_rest) {
			latched_rev = read_button(REV_BUTTON_PORT, REV_BUTTON_PIN);
		}
		bool rev_button = latched_rev;
		bool cc_button  = read_button(CC_BUTTON_PORT,  CC_BUTTON_PIN);

		// Brake has priority: when the ADC2 brake is pressed we respect it and
		// ignore the PPM throttle entirely.
		const float brake_thres = 0.001;
		bool brake_active = brake > brake_thres;

		// ---- 4) Single ramp on the overall (signed) output ----
		// Target is negative for brake, positive for drive. The ramp time
		// constant comes from whichever input is currently active: the PPM page
		// for throttle, the ADC page for brake. pos = magnitude rising,
		// neg = magnitude falling (same convention as the stock apps). So if you
		// hold full throttle and stab the brake, the output ramps from +1 toward
		// -1 using the *brake* ramp; release the brake and it ramps back toward
		// the throttle using the *throttle* ramp. A ramp of 0 means instant.
		float target  = brake_active ? -brake : throttle;
		float ramp_pos = brake_active ? aconf->ramp_time_pos : pconf->ramp_time_pos;
		float ramp_neg = brake_active ? aconf->ramp_time_neg : pconf->ramp_time_neg;

		const float dt_ms = (float)ST2MS(chVTTimeElapsedSinceX(last_ramp_time));
		last_ramp_time = chVTGetSystemTimeX();

		float ramp_time = fabsf(target) > fabsf(output_ramp) ? ramp_pos : ramp_neg;
		if (ramp_time > 0.01) {
			float step = output_ramp;
			utils_step_towards(&step, target, dt_ms / (ramp_time * 1000.0));
			output_ramp = step;
		} else {
			output_ramp = target;
		}
		float cmd = output_ramp;

		// ---- Safe-start arming (PPM throttle only) ----
		// Counts up ONLY while the throttle is idle; reset to 0 on boot/fault.
		// Driving does NOT reset it (matches the stock apps), so once you've held
		// idle long enough you stay armed for the session until a fault occurs.
		if (mc_interface_get_fault() != FAULT_CODE_NONE &&
				pconf->safe_start != SAFE_START_NO_FAULT) {
			ms_without_power = 0.0;
		}
		// Idle = stick released (pre-gain), so the gain knob can't fake idle.
		if (throttle_stick < 0.001) {
			ms_without_power += 1000.0 * sleep_time_s;
		}
		dbg_ppm_ok = ppm_signal_ok;
		dbg_armed = !(ms_without_power < MIN_MS_WITHOUT_POWER && pconf->safe_start);

		// Expose live values for the ppm_adc_dbg terminal command
		dbg_ppm = servodec_get_servo(0);
		dbg_gain = gain;
		dbg_throttle = throttle;
		dbg_brake = brake;
		dbg_cmd = cmd;

		// Stream live curves to the VESC Tool Experiment plot when enabled.
		// Throttled to ~20 Hz (loop is 100 Hz) to keep the comm link light.
		// Graphs: ADC1 V, ADC2 V, throttle, cmd (+drive/-brake), armed (0/1).
		if (plot_enabled) {
			static int plot_div = 0;
			if (++plot_div >= 5) {
				plot_div = 0;
				commands_plot_set_graph(0);
				commands_send_plot_points(plot_sample, ADC_VOLTS(ADC_IND_EXT));
				commands_plot_set_graph(1);
				commands_send_plot_points(plot_sample, ADC_VOLTS(ADC_IND_EXT2));
				commands_plot_set_graph(2);
				commands_send_plot_points(plot_sample, throttle);
				commands_plot_set_graph(3);
				commands_send_plot_points(plot_sample, cmd);
				commands_plot_set_graph(4);
				commands_send_plot_points(plot_sample, dbg_armed ? 1.0 : 0.0);
				plot_sample += 0.05;
			}
		}

		// Output disabled by another part of the firmware (e.g. detection)
		if (app_is_output_disabled()) {
			continue;
		}

		// ---- 5) Brake path: respect the brake, bypass safe-start & PPM timeout ----
		if (cmd < 0.0) {
			float current_rel = fabsf(cmd);
			timeout_reset();
			mc_interface_set_brake_current_rel(current_rel);

			if (pconf->multi_esc) {
				for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
					can_status_msg *msg = comm_can_get_status_msg_index(i);
					if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < MAX_CAN_AGE) {
						comm_can_set_current_brake_rel(msg->id, current_rel);
					}
				}
			}
			continue;
		}

		// ---- 6) Drive path (PPM throttle) ----
		float current_rel = cmd;
		if (rev_button) {
			current_rel = -current_rel; // reverse: drive the other way
		}

		// PPM signal lost / timeout -> brake and bail (only blocks driving)
		if (timeout_has_timeout() ||
				servodec_get_time_since_update() > timeout_get_timeout_msec()) {
			mc_interface_set_brake_current(timeout_get_brake_current());
			continue;
		}

		// Safe-start: require the PPM throttle idle for a while after boot/fault
		// before driving. Does not apply to braking (handled above).
		if (ms_without_power < MIN_MS_WITHOUT_POWER && pconf->safe_start) {
			mc_interface_set_brake_current(timeout_get_brake_current());
			continue;
		}

		timeout_reset();

#if ENABLE_CRUISE_CONTROL
		// Cruise control: hold CC button with throttle released -> hold rpm
		static float rpm_filtered = 0.0;
		UTILS_LP_MOVING_AVG_APPROX(rpm_filtered, mc_interface_get_rpm(), RPM_FILTER_SAMPLES);

		if (cc_button && fabsf(cmd) < 0.001) {
			if (!cc_was_pid) {
				cc_was_pid = true;
				cc_pid_rpm = rpm_filtered;
			}
			mc_interface_set_pid_speed(cc_pid_rpm);
			continue;
		}
		cc_was_pid = false;
#else
		(void)cc_button;
#endif

		// ---- 7) Apply drive current ----
		mc_interface_set_current_rel(current_rel);

		if (pconf->multi_esc) {
			for (int i = 0; i < CAN_STATUS_MSGS_TO_STORE; i++) {
				can_status_msg *msg = comm_can_get_status_msg_index(i);
				if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < MAX_CAN_AGE) {
					comm_can_set_current_rel(msg->id, current_rel);
				}
			}
		}
	}
}
