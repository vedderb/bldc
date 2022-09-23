/*
	Copyright 2019 Kirill Kostiuchenko	kisel2626@gmail.com

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

#include "app.h"
#include "ch.h"
#include "hal.h"

// Some useful includes
#include "comm_can.h"
#include "commands.h"
#include "encoder/encoder.h"
#include "hw.h"
#include "mc_interface.h"
#include "terminal.h"
#include "timeout.h"
#include "utils_math.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Delays between pereodically prints of state, position and forces
static const int short_print_delay = 500; // Control loop counts, 1 millisecond about
static const int long_print_delay = 3000;

// Threads
static THD_FUNCTION(my_thread, arg);
static THD_WORKING_AREA(my_thread_wa, 2048);

// Private functions
static void terminal_move_tac(int argc, const char **argv);
static void terminal_show_skypuff_conf(int argc, const char **argv);

// Private variables
static volatile bool stop_now = true;
static volatile bool is_running = false;
static const volatile mc_configuration *mc_conf;
static int prev_abs_tac = 0;
static float prev_erpm = 0;
static int i;					 // use control loop counter in app functions
static int prev_print = 0;		 // loop counter value of the last state print
static int prev_printed_tac = 0; // do not print the same position

// Units converting calculations
#define METERS_PER_REV (mc_conf->si_wheel_diameter / mc_conf->si_gear_ratio * M_PI)
#define STEPS_PER_REV (mc_conf->si_motor_poles * 3)

// Control loop state machine
typedef enum
{
	UNINITIALIZED, // Unitialized state for application start
	BRAKING,	   // Braking zone near take off
	SLOWING,	   // Release the motor if ERPM is higher then config.slow_erpm
	SLOW,		   // PID controlled speed with slow_erpm
	UNWINDING,	 // Low force rope tension
	REWINDING,	 // High force fast rope winding to take off
} rewinder_state;

static rewinder_state state;

typedef struct
{
	float kg_to_amps;			  // winch force coefficient
	int braking_length;			  // tachometer range for braking zone
	int overwinding;			  // tachometer range to overwind braking zone
	int slowing_length;			  // range after braking zone to release and slow down
	float slow_erpm;			  // constant erpm for slow mode
	int rewinding_trigger_lenght; // fast rewinding after going back range
	int unwinding_trigger_length; // go unwinding if this range unwinded
	float brake_current;
	float unwinding_current;
	float rewinding_current;
	float slow_max_current; // On exceed max current go braking or unwinding
} rewinder_config;

static rewinder_config config;

static char *state_str(void)
{
	switch (state)
	{
	case UNINITIALIZED:
		return "unitialized";
	case BRAKING:
		return "braking";
	case UNWINDING:
		return "unwinding";
	case REWINDING:
		return "rewinding";
	case SLOWING:
		return "slowing";
	case SLOW:
		return "slow";
	default:
		return "unknown";
	}
}

int meters_to_tac_steps(float meters)
{
	float steps_per_rev = STEPS_PER_REV;
	float meters_per_rev = METERS_PER_REV;

	int steps = meters / meters_per_rev * steps_per_rev;

	// Low range warning
	if (steps == 0)
	{
		commands_printf("Skypuff: -- ZERO STEPS WARNING -- meters_to_tac_steps(%.5f) results in %d steps",
						(double)meters, steps);
	}
	return steps;
}

float tac_steps_to_meters(int steps)
{
	float steps_per_rev = STEPS_PER_REV;
	float meters_per_rev = METERS_PER_REV;

	return (float)steps / steps_per_rev * meters_per_rev;
}

// Convert speed in meters per second to erpm
float ms_to_erpm(float ms)
{
	float meters_per_rev = METERS_PER_REV;
	float rps = ms / meters_per_rev;
	float rpm = rps * 60;

	return rpm * (mc_conf->si_motor_poles / 2);
}

float erpm_to_ms(float erpm)
{
	float meters_per_rev = METERS_PER_REV;
	float erps = erpm / 60;
	float rps = erps / (mc_conf->si_motor_poles / 2);

	return rps * meters_per_rev;
}

inline static void brake(int cur_tac)
{
	if (state != BRAKING)
	{
		prev_print = i;
		commands_printf("Skypuff: state %s -> braking, position: %.2f meters (%d steps), breaking: %.1fKg (%.1fA)", state_str(),
						(double)tac_steps_to_meters(cur_tac), cur_tac,
						(double)(config.brake_current / config.kg_to_amps), (double)config.brake_current);
	}

	state = BRAKING;

	prev_abs_tac = abs(cur_tac);
	mc_interface_set_brake_current(config.brake_current);
	timeout_reset();
}

static void unwinding(int cur_tac)
{
	// Detect direction depending on tachometer value
	float current = cur_tac < 0 ? config.unwinding_current : -config.unwinding_current;

	prev_print = i;
	commands_printf("Skypuff: state %s -> unwinding, position: %.2fm (%d steps), pulling: %.1fKg (%.1fA)", state_str(),
					(double)tac_steps_to_meters(cur_tac), cur_tac,
					(double)(current / config.kg_to_amps), (double)current);

	state = UNWINDING;

	prev_abs_tac = abs(cur_tac);
	mc_interface_set_current(current);
	timeout_reset();
}

static void rewinding(int cur_tac)
{
	// Detect direction depending on tachometer value
	float current = cur_tac < 0 ? config.rewinding_current : -config.rewinding_current;

	prev_print = i;
	commands_printf("Skypuff: state %s -> rewinding, position: %.2fm (%d steps), pulling: %.1fKg (%.1fA)", state_str(),
					(double)tac_steps_to_meters(cur_tac), cur_tac,
					(double)(current / config.kg_to_amps), (double)current);

	state = REWINDING;

	prev_abs_tac = abs(cur_tac);
	mc_interface_set_current(current);
	timeout_reset();
}

static void slowing(int cur_tac, float erpm)
{
	prev_print = i;
	commands_printf("Skypuff: state %s -> slowing, position: %.2fm (%d steps), speed: %.1fms (%.0f ERPM)", state_str(),
					(double)tac_steps_to_meters(cur_tac), cur_tac,
					(double)erpm_to_ms(erpm), (double)erpm);

	state = SLOWING;

	mc_interface_release_motor();
}

static void slow(int cur_tac, float erpm)
{
	// Detect direction depending on tachometer value
	float constant_erpm = cur_tac < 0 ? config.slow_erpm : -config.slow_erpm;

	prev_print = i;
	commands_printf("Skypuff: state %s -> slow, position: %.2fm (%d steps), speed: %.1fms (%.0f ERPM), constant speed: %.1fms (%.0f ERPM)",
					state_str(), (double)tac_steps_to_meters(cur_tac), cur_tac,
					(double)erpm_to_ms(erpm), (double)erpm, (double)erpm_to_ms(constant_erpm), (double)constant_erpm);

	state = SLOW;

	prev_erpm = erpm;
	mc_interface_set_pid_speed(constant_erpm);
	timeout_reset();
}

static void set_example_config(void)
{
	// Some example ranges
	config.braking_length = meters_to_tac_steps(1);
	config.overwinding = meters_to_tac_steps(0.1);
	config.rewinding_trigger_lenght = meters_to_tac_steps(0.2);
	config.unwinding_trigger_length = meters_to_tac_steps(0.05);
	config.slowing_length = meters_to_tac_steps(3);
	config.slow_erpm = ms_to_erpm(1);

	// Forces
	config.kg_to_amps = 7;
	config.brake_current = 0.2 * config.kg_to_amps;
	config.unwinding_current = 0.2 * config.kg_to_amps;
	config.rewinding_current = 0.4 * config.kg_to_amps;
	config.slow_max_current = 0.3 * config.kg_to_amps;
}

// Called when the custom application is started. Start our
// threads here and set up callbacks.
void app_custom_start(void)
{
	mc_conf = mc_interface_get_configuration();

	set_example_config();

	state = UNINITIALIZED;

	stop_now = false;

	chThdCreateStatic(my_thread_wa, sizeof(my_thread_wa), NORMALPRIO, my_thread, NULL);

	// Terminal commands for the VESC Tool terminal can be registered.
	terminal_register_command_callback(
		"move_tac",
		"Move zero point on specified distance. Use negative value to move backward.",
		"[meters]", terminal_move_tac);
	terminal_register_command_callback(
		"skypuff",
		"Print SkyPUFF configuration here.",
		"", terminal_show_skypuff_conf);

	commands_printf("app_skypuff started");
}

// Called when the custom application is stopped. Stop our threads
// and release callbacks.
void app_custom_stop(void)
{
	terminal_unregister_callback(terminal_move_tac);
	terminal_unregister_callback(terminal_show_skypuff_conf);

	stop_now = true;
	while (is_running)
	{
		chThdSleepMilliseconds(1);
	}
	commands_printf("app_skypuff stopped");
}

void app_custom_configure(app_configuration *conf)
{
	(void)conf;
}

// The same code for unwinding and rewinding states
// Returns true if mode changed
static bool brake_or_slowing(int cur_tac, int abs_tac)
{
	// We are in the braking range with overwinding?
	if (abs_tac < config.braking_length - config.overwinding)
	{
		brake(cur_tac);
		return true;
	}

	// We are in the slowing range?
	if (abs_tac < config.braking_length + config.slowing_length)
	{
		float erpm = mc_interface_get_rpm();

		// Rewinding forward with more then slow speed?
		if (cur_tac < 0 && erpm > config.slow_erpm)
		{
			slowing(cur_tac, erpm);
			return true;
		}
		// Rewinding backward with more then slow speed?
		else if (cur_tac >= 0 && erpm < -config.slow_erpm)
		{
			slowing(cur_tac, erpm);
			return true;
		}
	}

	return false;
}

inline static void print_position_periodically(int cur_tac, int delay)
{
	// prolong delay if not moving
	if (cur_tac == prev_printed_tac)
	{
		prev_print = i;
		return;
	}

	if (i - prev_print > delay)
	{
		prev_print = i;
		prev_printed_tac = cur_tac;
		commands_printf("Skypuff: %s, position: %.2f meters (%d steps)",
						state_str(),
						(double)tac_steps_to_meters(cur_tac), cur_tac);
	}
}

static THD_FUNCTION(my_thread, arg)
{
	(void)arg;

	chRegSetThreadName("App SkyPUFF");

	is_running = true;

	int cur_tac, abs_tac;
	float cur_erpm, abs_erpm;
	float cur_current, abs_current;

	for (i = 0;; i++)
	{
		// Check if it is time to stop.
		if (stop_now)
		{
			is_running = false;
			return;
		}

		cur_tac = mc_interface_get_tachometer_value(false);
		abs_tac = abs(cur_tac);

		switch (state)
		{
		case UNINITIALIZED:
			if (abs_tac < config.braking_length)
				brake(cur_tac);
			else
				unwinding(cur_tac);
			break;
		case BRAKING:
			// We are in the breaking zone?
			if (abs_tac <= config.braking_length)
			{
				// Timeout thread will remove breaking every second by default
				// Apply break current again on position changed
				if (abs_tac != prev_abs_tac)
					brake(cur_tac);
			}
			else
				unwinding(cur_tac);

			print_position_periodically(cur_tac, long_print_delay);
			break;
		case UNWINDING:
			// No timeouts for unwinding state
			if (!(i % 500))
				timeout_reset();

			// Go breaking or slowing?
			if (brake_or_slowing(cur_tac, abs_tac))
				break;

			// Use prev_abs_tac as max tachometer
			if (abs_tac > prev_abs_tac)
			{
				// Print debug message if we are going out from slowing zone
				int eof_slowing = config.braking_length + config.slowing_length;
				if (prev_abs_tac < eof_slowing && abs_tac >= eof_slowing)
				{
					commands_printf("Skypuff: unwinded from slowing zone %.2f meters (%d steps)",
									(double)tac_steps_to_meters(cur_tac), cur_tac);
				}

				// Update maximum value of tachometer
				prev_abs_tac = abs_tac;
			}

			// Going back more then config.rewinding_trigger_length?
			if (abs_tac < prev_abs_tac - config.rewinding_trigger_lenght)
			{
				rewinding(cur_tac);
				break;
			}

			print_position_periodically(cur_tac, long_print_delay);
			break;
		case REWINDING:
			// No timeouts for rewinding state
			if (!(i % 500))
				timeout_reset();

			// Go breaking or slowing?
			if (brake_or_slowing(cur_tac, abs_tac))
				break;

			// Now use prev_abs_tac as min value
			if (abs_tac < prev_abs_tac)
				prev_abs_tac = abs_tac;

			// Unwinding again?
			if (abs_tac > prev_abs_tac + config.unwinding_trigger_length)
				unwinding(cur_tac);

			print_position_periodically(cur_tac, long_print_delay);
			break;
		case SLOWING:
			// We are in the braking range with overwinding?
			if (abs_tac < config.braking_length - config.overwinding)
			{
				brake(cur_tac);
				break;
			}

			cur_erpm = mc_interface_get_rpm();
			abs_erpm = abs(cur_erpm);

			// Slow enough for PID speed?
			if (abs_erpm < config.slow_erpm)
			{
				slow(cur_tac, cur_erpm);
				break;
			}

			// Print speed and tachometer to simpler tweaking slowing zone
			if (i - prev_print > short_print_delay)
			{
				int overwinding = config.braking_length - config.overwinding;
				int distance_left = abs(cur_tac) - overwinding;
				prev_print = i;
				commands_printf(
					"Skypuff: slowing, position %.2f meters (%d steps), speed: %.1fms (%.0f ERPM), waiting for: %.1fms, %.2f meters left until overwinding zone",
					(double)tac_steps_to_meters(cur_tac), cur_tac,
					(double)erpm_to_ms(cur_erpm), (double)cur_erpm, (double)erpm_to_ms(config.slow_erpm),
					(double)tac_steps_to_meters(distance_left));
			}
			break;
		case SLOW:
			cur_current = mc_interface_get_tot_current_directional_filtered();
			abs_current = abs(cur_current);
			cur_erpm = mc_interface_get_rpm();

			// No timeouts for slow state
			if (!(i % 500))
				timeout_reset();

			// Rotating direction changed or stopped?
			if ((prev_erpm < 0 && cur_erpm >= 0) || (prev_erpm > 0 && cur_erpm <= 0))
			{
				commands_printf("Skypuff: rotation direction changed");
				brake(cur_tac);
				break;
			}

			prev_erpm = cur_erpm;

			// If current above the limits - brake or unwinding
			if (abs_current > config.slow_max_current)
			{
				commands_printf(
					"Skypuff: slow pulling too high %.1fKg (%.1fA) is more %.1fKg (%.1fA)",
					(double)(abs_current / config.kg_to_amps), (double)cur_current,
					(double)(config.slow_max_current / config.kg_to_amps), (double)config.slow_max_current);

				if (abs_tac < config.braking_length - config.overwinding)
				{
					brake(cur_tac);
					break;
				}
				else
				{
					unwinding(cur_tac);
					break;
				}
			}

			// Slowly rewinded more then opposite side of (breaking - overwinding) zone?
			if ((cur_erpm > 0 && cur_tac > config.braking_length - config.overwinding) ||
				(cur_erpm < 0 && cur_tac < -config.braking_length + config.overwinding))
			{
				commands_printf("Skypuff: winded to opposite braking zone");
				brake(cur_tac);
				break;
			}

			if (i - prev_print > long_print_delay)
			{
				prev_print = i;
				commands_printf("Skypuff: slow, position %.2f meters (%d steps), speed: %.1fms (%.0f ERPM), pulling: %.1fKg (%.1fA)",
								(double)tac_steps_to_meters(cur_tac), cur_tac,
								(double)erpm_to_ms(cur_erpm), (double)cur_erpm,
								(double)(cur_current / config.kg_to_amps), (double)cur_current);
			}
			break;
		default:
			commands_printf("Skypuff: unknown control loop state, exiting!");
			stop_now = true;
		}

		chThdSleepMilliseconds(1);
	}
}

// Terminal command to change tachometer value
static void terminal_move_tac(int argc, const char **argv)
{
	if (argc == 2)
	{
		float d = 0;
		if (sscanf(argv[1], "%f", &d) == EOF)
		{
			commands_printf("move_tac: can't parse meters: '%s' value.", argv[1]);
			return;
		};

		int steps = meters_to_tac_steps(d);

		int cur_tac = mc_interface_get_tachometer_value(false);

		int new_tac = cur_tac + steps;

		commands_printf("move_tac: moving zero %.2fm (%d steps) %s, cur pos: %.2fm (%d steps), new pos: %.2fm (%d steps)",
						(double)d, steps, d < 0 ? "backward" : "forward",
						(double)tac_steps_to_meters(cur_tac), cur_tac,
						(double)tac_steps_to_meters(new_tac), new_tac);

		mc_interface_set_tachometer_value(new_tac);
	}
	else
	{
		commands_printf("This command requires one argument: 'move_tac -5.2' will move zero 5.2 meters backward");
	}
}

// Terminal command to show configuration
static void terminal_show_skypuff_conf(int argc, const char **argv)
{
	(void)argc;
	(void)argv;

	int cur_tac = mc_interface_get_tachometer_value(false);
	commands_printf("VESC additional configuration:");
	commands_printf("  wheel diameter: %.2fmm", (double)(mc_conf->si_wheel_diameter * 1000));
	commands_printf("  motor poles: %dp", mc_conf->si_motor_poles);
	commands_printf("  gear ratio: %.5f", (double)mc_conf->si_gear_ratio);
	commands_printf("SkyPUFF configuration:");
	commands_printf("  state %s, current position: %.2f meters (%d steps)", state_str(), (double)tac_steps_to_meters(cur_tac), cur_tac);
	commands_printf("  amperes per 1kg force: %.1fAKg", (double)config.kg_to_amps);
	commands_printf("  braking range: %.2f meters (%d steps)", (double)tac_steps_to_meters(config.braking_length), config.braking_length);
	commands_printf("  overwinding: %.2f meters (%d steps)", (double)tac_steps_to_meters(config.overwinding), config.overwinding);
	commands_printf("  slowing range: %.2f meters (%d steps)", (double)tac_steps_to_meters(config.slowing_length), config.slowing_length);
	commands_printf("  rewinding trigger range: %.2f meters (%d steps)",
					(double)tac_steps_to_meters(config.rewinding_trigger_lenght), config.rewinding_trigger_lenght);
	commands_printf("  unwinding trigger range: %.2f meters (%d steps)",
					(double)tac_steps_to_meters(config.unwinding_trigger_length), config.unwinding_trigger_length);
	commands_printf("  brake force: %.2fkg (%.1fA)", (double)(config.brake_current / config.kg_to_amps), (double)config.brake_current);
	commands_printf("  unwinding force: %.2fkg (%.1fA)", (double)(config.unwinding_current / config.kg_to_amps), (double)config.unwinding_current);
	commands_printf("  rewinding force: %.2fkg (%.1fA)", (double)(config.rewinding_current / config.kg_to_amps), (double)config.rewinding_current);
	commands_printf("  slow speed: %.1fms (%.0f ERPM)", (double)erpm_to_ms(config.slow_erpm), (double)config.slow_erpm);
	commands_printf("  maximum slow force: %.2fkg (%.1fA)", (double)(config.slow_max_current / config.kg_to_amps), (double)config.slow_max_current);
}
