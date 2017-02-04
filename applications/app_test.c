#include "ch.h" // ChibiOS
#include "hal.h" // ChibiOS HAL
#include "mc_interface.h" // Motor control functions
#include "hw.h" // Pin mapping on this hardware
#include "timeout.h" // To reset the timeout
#include "terminal.h"
#include "commands.h"

// Example thread
static THD_FUNCTION(test_thread, arg);
static THD_WORKING_AREA(test_thread_wa, 2048); // 2kb stack for this thread

const char *testCommand[] = {
	"test",
	"this is a glorious test command for the terminal hook"
};
int testint = 1337;

void test_terminal_command(int argc, const char ** argv) {
	const char *argument = "<empty>";
	if (argc > 1)
		argument = argv[1];
	commands_printf("Hello from the test command! %s - %d", argument, testint);
}

void app_mag_init(void) {
	// Set the UART TX pin as an input with pulldown
	terminal_register_command_callback(testCommand[0], testCommand[1], test_terminal_command);
	chThdCreateStatic(test_thread_wa, sizeof(mag_thread_wa),
		NORMALPRIO, mag_thread, NULL);
}
 
static THD_FUNCTION(test_thread, arg) {
	(void)arg;
 
	chRegSetThreadName("APP_TEST");
 
	for(;;) {
		chThdSleepMilliseconds(2);
 
		timeout_reset();
	}
}
