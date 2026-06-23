/*
 * password.c
 *
 *  Created on: Nov 11, 2019
 *      Author: motorcontrol
 */

#include "password.h"
#include "commands.h"
#include "terminal.h"
#include "string.h"
#include "stdio.h"
#include "conf_general.h"

#define EEPROM_ADDR_USER_PASSWORD 	1
static volatile uint8_t user_password[40];
static volatile uint8_t user_password_read[10];

#define EEPROM_ADDR_PASSWORD_SYSTEM_ENABLE	(EEPROM_ADDR_USER_PASSWORD + 2)
static volatile uint8_t system_enable_argument[40];
static volatile uint8_t pass_system_enable[10];//0 is false, 1 is true

static volatile bool system_locked = true;//this will be true until the user password is entered
static volatile bool password_is_erased = false;//this will be false if there is a valid password saved, or true if a new firmware was loaded, that erase the password
static volatile bool system_connection_alive = false;
static volatile bool password_system_enable = false; //this flag enables / disables the password system. it is set by the "ul <password> <enable/disable> command.
static volatile uint32_t password_timeout_counter = 0;
static volatile uint32_t password_timeout_limit = 0;

static void terminal_cmd_enter_user_password(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	if(password_is_erased){
		//initialize password to Calibike
		password_is_erased = 0;
		strcpy(user_password,"Calibike");
		// Store data in eeprom
		conf_general_store_eeprom_var_hw((eeprom_var*)user_password, EEPROM_ADDR_USER_PASSWORD);
		conf_general_store_eeprom_var_hw((eeprom_var*)(user_password+4), EEPROM_ADDR_USER_PASSWORD+1);

		//initialize password system enable to false = 0
		uint8_t index;
		for( index = 0; index<4 ; index++)
		{
			pass_system_enable[index]  = 0;
		}
		conf_general_store_eeprom_var_hw((eeprom_var*)(pass_system_enable), EEPROM_ADDR_PASSWORD_SYSTEM_ENABLE);
	}

	if( argc == 3 ) {
		sscanf(argv[1], "%s", user_password);
		sscanf(argv[2], "%s", system_enable_argument);

		uint8_t pass_length = strlen((const char*)user_password);

		if( pass_length != 8){
			commands_printf("wrong password, it needs to be 8 characters long");
		}else{

			if( (strncmp(system_enable_argument, "enable",6) == 0) ||
				( strncmp(system_enable_argument, "disable",7) == 0)
			  )
			{
				// Read stored password in eeprom
				conf_general_read_eeprom_var_hw((eeprom_var*)user_password_read,EEPROM_ADDR_USER_PASSWORD);
				conf_general_read_eeprom_var_hw((eeprom_var*)(user_password_read+4),EEPROM_ADDR_USER_PASSWORD+1);

				if( strncmp(user_password, user_password_read,8) == 0){
					system_locked = false;
					commands_printf("good password --> system unlocked");
					password_timeout_configure(300000);//5 minutes in msec

					if( strncmp(system_enable_argument, "enable",6) == 0){
						password_system_enable = true;
						uint8_t index;
						for( index = 0; index<4 ; index++)
						{
							pass_system_enable[index]  = 1;
						}
						commands_printf("password system is enabled");
					}else{
						password_system_enable = false;
						uint8_t index;
						for( index = 0; index<4 ; index++)
						{
							pass_system_enable[index]  = 0;
						}
						commands_printf("password system is disabled");
					}

					conf_general_store_eeprom_var_hw((eeprom_var*)(pass_system_enable), EEPROM_ADDR_PASSWORD_SYSTEM_ENABLE);

				}else{
					commands_printf("wrong password, it does not match current password");
				}
			}
			else
			{
				commands_printf("wrong command! second argument should be \"enable\" or \"disable\" ");
			}

		}

	}
	else {

		if( argc == 2 ) {
			sscanf(argv[1], "%s", user_password);

			uint8_t pass_length = strlen((const char*)user_password);

			if( pass_length != 8){
				commands_printf("wrong password, it needs to be 8 characters long");
			}else{

				// Read stored password in eeprom
				conf_general_read_eeprom_var_hw((eeprom_var*)user_password_read,EEPROM_ADDR_USER_PASSWORD);
				conf_general_read_eeprom_var_hw((eeprom_var*)(user_password_read+4),EEPROM_ADDR_USER_PASSWORD+1);

				if( strncmp(user_password, user_password_read,8) == 0){
					system_locked = false;
					commands_printf("good password --> system unlocked");
					password_timeout_configure(300000);//5 minutes in msec

				}else{
					commands_printf("wrong password, it does not match current password");
				}
			}
		}
		else
		{
			commands_printf("wrong command, please use 1 argument like \"ul <password>\" or 2 arguments such as \"ul <password> <enable/disable>");
		}

	}

	commands_printf(" ");
	return;
}

static void terminal_cmd_new_user_password(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	if( password_system_enable )
	{
		if( system_locked ){
			commands_printf("system is locked, first unlock it, then you will be able to load a new password.");
		}else{
			if( argc == 2 ) {

				sscanf(argv[1], "%s", user_password);

				uint8_t pass_length = strlen((const char*)user_password);

				if( pass_length != 8){
					commands_printf("wrong password, it needs to be 8 characters long");
				}else{
					commands_printf("good password, User password will change to: %s", user_password);

					// Store data in eeprom
					conf_general_store_eeprom_var_hw((eeprom_var*)user_password, EEPROM_ADDR_USER_PASSWORD);
					conf_general_store_eeprom_var_hw((eeprom_var*)(user_password+4), EEPROM_ADDR_USER_PASSWORD+1);

					//read back written data
					conf_general_read_eeprom_var_hw((eeprom_var*)user_password_read,EEPROM_ADDR_USER_PASSWORD);
					conf_general_read_eeprom_var_hw((eeprom_var*)(user_password_read+4),EEPROM_ADDR_USER_PASSWORD+1);

					commands_printf("password saved:%s", user_password_read);
				}

			}
			else {
				commands_printf("1 argument required. For example: new_user_password Calibike");
				commands_printf(" ");
			}
		}
	}
	else
	{
		commands_printf("password system is disabled, you need first to enable it by the \"ul <password> enable\" command." );
	}

	commands_printf(" ");
	return;
}

static void terminal_cmd_lock_system(int argc, const char **argv)
{
	if( password_system_enable )
	{
		system_locked = true;
		commands_printf("system has been locked\r\n");
	}
	else
	{
		commands_printf("password system is disabled, you need first to enable it by the \"ul <password> enable\" command." );
	}
	return;
}

void password_init(void){
	// Register terminal callbacks

	terminal_register_command_callback(
			"ul",
			"Unlocks system, and enables or disables password settings , command: unlock <password> <enable/disable>",
			0,
			terminal_cmd_enter_user_password);

	terminal_register_command_callback(
				"Ul",
				"same as ul",
				0,
				terminal_cmd_enter_user_password);

	terminal_register_command_callback(
			"sp",
			"Sets a new user password for lock function, that must be 8 characters long, example: sp <password>",
			0,
			terminal_cmd_new_user_password);

	terminal_register_command_callback(
			"Sp",
			"same as sp",
			0,
			terminal_cmd_new_user_password);

	terminal_register_command_callback(
			"lk",
			"locks system with password set in memory",
			0,
			terminal_cmd_lock_system);

	terminal_register_command_callback(
			"Lk",
			"same as lk",
			0,
			terminal_cmd_lock_system);

	// check if flash was erased during a firmware upgrade, then initialize the password to Calibike
	conf_general_read_eeprom_var_hw((eeprom_var*)user_password_read,EEPROM_ADDR_USER_PASSWORD);
	conf_general_read_eeprom_var_hw((eeprom_var*)(user_password_read+4),EEPROM_ADDR_USER_PASSWORD+1);

	conf_general_read_eeprom_var_hw((eeprom_var*)pass_system_enable,EEPROM_ADDR_PASSWORD_SYSTEM_ENABLE);

	if( (user_password_read[0] == 0) &&
		(user_password_read[1] == 0) &&
		(user_password_read[2] == 0) &&
		(user_password_read[3] == 0) &&
		(user_password_read[4] == 0) &&
		(user_password_read[5] == 0) &&
		(user_password_read[6] == 0) &&
		(user_password_read[7] == 0) )
	{
		//this means flash memory was just programmed, and we need to set default settings, password erased true, and password enable false and system unlocked
		password_is_erased = true;
		password_system_enable = false;
		system_locked = false;
	}else
	{
		if(pass_system_enable[0] == 0)
		{
			password_system_enable = false;
			system_locked = false;
		}
		else
		{
			password_system_enable = true;
			system_locked = true;
		}
	}

}

bool password_get_system_locked_flag(void){
	return system_locked;
}

void password_set_system_locked_flag( bool value){
	system_locked = value;
}

bool password_get_system_enable_flag(void){
	return password_system_enable;
}

void password_set_system_enable_flag( bool value){
	password_system_enable = value;
}

bool password_get_system_connection_alive(void){
	return system_connection_alive;
}

void password_set_system_connection_alive(bool value){
	system_connection_alive = value;
}

void password_timeout_deinit(void){
	password_timeout_limit = 0;
	password_timeout_counter = 0;
}

void password_timeout_configure( uint32_t timeout_msec ){
	password_timeout_limit = timeout_msec;
	password_timeout_counter = 0;
}

void password_timeout_reset(void){
	password_timeout_counter = 0;
}

void password_timeout_increment(uint16_t delta_msec){
	if( password_timeout_limit > 0 ){
		password_timeout_counter += delta_msec;//this is called from main timer that has a 10 msec tick
		if( password_timeout_counter > password_timeout_limit ){
			system_locked = true;
		}
	}
}
