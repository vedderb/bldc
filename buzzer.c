#include "buzzer.h"

#include "conf_general.h"
#include "mc_interface.h" // Motor control functions

// Default to using servo pin
#ifdef EXT_BUZZER_ON
#define CUSTOM_BUZZER
#else
#define EXT_BUZZER_ON()			palSetPad(HW_ICU_GPIO, HW_ICU_PIN)
#define EXT_BUZZER_OFF()		palClearPad(HW_ICU_GPIO, HW_ICU_PIN)
#endif

// TODO: Make this configurable from the app
#define ALERT_MIN_BEEP_MS 200

#define BEEP_SHORT 0
#define BEEP_LONG 1

static int alert_beep_num_left = 0;
static systime_t alert_beep_time;
static unsigned int alert_beep_duration = BEEP_SHORT;
static bool is_enabled = false;
static bool is_initialized = false;

static void buzzer_init(void) {
#ifndef CUSTOM_BUZZER
	// External Buzzer (using servo pin!)
	palSetPadMode(HW_ICU_GPIO, HW_ICU_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	EXT_BUZZER_OFF();
#endif
	is_initialized = true;
}

void buzzer_enable(bool enable) {
	is_enabled = enable;
	if (!is_initialized) {
		buzzer_init();
	}
	if (!enable) {
		EXT_BUZZER_OFF();
	}
}

bool is_buzzer_enabled() {
	return is_enabled;
}

// periodic update function, called from LED thread
void update_beep_alert(void)
{
	if (!is_enabled)
		return;
	if (alert_beep_num_left > 0) {
		if (chVTGetSystemTimeX() - alert_beep_time > alert_beep_duration) {
			alert_beep_time = chVTGetSystemTimeX();
			alert_beep_num_left--;

			if (alert_beep_num_left & 0x1)
				EXT_BUZZER_ON();
			else
				EXT_BUZZER_OFF();
		}
	}
}

void beep_alert(int num_beeps, bool longbeep)
{
	if (!is_enabled)
		return;
	if (alert_beep_num_left == 0) {
		alert_beep_num_left = num_beeps * 2;
		alert_beep_time = chVTGetSystemTimeX();
		alert_beep_duration = longbeep ? 4 * ALERT_MIN_BEEP_MS : ALERT_MIN_BEEP_MS;
		EXT_BUZZER_ON();
	}
}

void beep_off(bool force)
{
	// don't mess with the buzzer if we're in the process of doing a multi-beep
	if (force || (alert_beep_num_left == 0))
		EXT_BUZZER_OFF();
}

void beep_on(bool force)
{
	if (!is_enabled)
		return;
	// don't mess with the buzzer if we're in the process of doing a multi-beep
	if (force || (alert_beep_num_left == 0))
		EXT_BUZZER_ON();
}
