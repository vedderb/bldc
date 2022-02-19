/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se
	Copyright 2022 Jakub Tomczak

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

#include "encoder.h"
#include "encoder/encoder_datatype.h"

#include "utils.h"
#include "commands.h"
#include "mcpwm_foc.h"
#include "mc_interface.h"
#include "mempools.h"
#include "terminal.h"

#include <math.h>

#define ENCODER_CONFIG_UNUSED {0}
#define ENCODER_VAR_UNINITIALIZED 0
#define ENCODER_STRUCT_UNDEFINED {0}
#define ENCODER_ABI_COUNTER_DEFAULT_VALUE 10000ul

#define SPI_BaudRatePrescaler_2         ((uint16_t)0x0000) //  42 MHz      21 MHZ
#define SPI_BaudRatePrescaler_4         ((uint16_t)0x0008) //  21 MHz      10.5 MHz
#define SPI_BaudRatePrescaler_8         ((uint16_t)0x0010) //  10.5 MHz    5.25 MHz
#define SPI_BaudRatePrescaler_16        ((uint16_t)0x0018) //  5.25 MHz    2.626 MHz
#define SPI_BaudRatePrescaler_32        ((uint16_t)0x0020) //  2.626 MHz   1.3125 MHz
#define SPI_BaudRatePrescaler_64        ((uint16_t)0x0028) //  1.3125 MHz  656.25 KHz
#define SPI_BaudRatePrescaler_128       ((uint16_t)0x0030) //  656.25 KHz  328.125 KHz
#define SPI_BaudRatePrescaler_256       ((uint16_t)0x0038) //  328.125 KHz 164.06 KHz
#define SPI_DATASIZE_16BIT				SPI_CR1_DFF

#define CH_MUTEX_INIT_ZERO {{NULL, NULL}, NULL, NULL}

ENCSPI_config_t encoder_conf_ENCSPI =
{
		ENCODER_VAR_UNINITIALIZED,
		{/*BB_SPI*/
				/*NSS*/HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3,
				/*SCK*/HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1,
#ifdef HW_SPI_PORT_MOSI
				/*MOSI*/HW_SPI_PORT_MOSI, HW_SPI_PIN_MOSI,
#else
				/*MOSI*/0, 0,
#endif
				/*MISO*/HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2,
				ENCODER_VAR_UNINITIALIZED,
				ENCODER_VAR_UNINITIALIZED,
				CH_MUTEX_INIT_ZERO
		},
#ifdef HW_SPI_DEV
		{//HARDWARE SPI CONFIG
				NULL, HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, SPI_BaudRatePrescaler_4 | SPI_CR1_CPOL | SPI_CR1_CPHA
				| SPI_DATASIZE_16BIT
		}
#endif
};

ABI_config_t encoder_conf_ABI =
{
		ENCODER_VAR_UNINITIALIZED,
		ENCODER_ABI_COUNTER_DEFAULT_VALUE,
		/*INCREMENTAL PROTOCOL PINOUT*/
		/*A*/HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1 ,
		/*B*/HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2
};

ENCSINCOS_config_t encoder_conf_ENCSINCOS = ENCODER_STRUCT_UNDEFINED;

TS5700N8501_config_t encoder_conf_TS5700N8501 =
{
		ENCODER_VAR_UNINITIALIZED,
		/*UART PINOUT*/
		HW_UART_TX_PORT, HW_UART_TX_PIN,
		HW_UART_RX_PORT, HW_UART_RX_PIN,
#ifdef HW_ADC_EXT_GPIO
		HW_ADC_EXT_GPIO, HW_ADC_EXT_PIN
#else
		ENCODER_VAR_UNINITIALIZED, ENCODER_VAR_UNINITIALIZED
#endif
		,
		{/*UART CONFIG*/
				2500000,
				0,
				USART_CR2_LINEN,
				0
		}
};

static encoder_type_t encoder_type_now = ENCODER_TYPE_NONE;
static bool index_found = false;
static float timer_rate_now = 1.0;

// Private functions
static void terminal_encoder(int argc, const char **argv);
static void terminal_encoder_clear_errors(int argc, const char **argv);
static void terminal_encoder_clear_multiturn(int argc, const char **argv);

static bool is_uart_defined(void);
static bool is_spi_defined(void);
static bool is_incremental_defined(void);
static void timer_start(float rate);

void encoder_deinit(void) {
	nvicDisableVector(HW_ENC_EXTI_CH);
	nvicDisableVector(HW_ENC_TIM_ISR_CH);
	TIM_DeInit(HW_ENC_TIM);

	if (encoder_type_now == ENCODER_TYPE_AS504x) {
		enc_as504x_deinit();
	} else if (encoder_type_now == ENCODER_TYPE_MT6816) {
		enc_mt6816_deinit();
	} else if (encoder_type_now == ENCODER_TYPE_AD2S1205_SPI) {
		enc_ad2s1205_deinit();
	} else if (encoder_type_now == ENCODER_TYPE_ABI) {
		enc_abi_deinit();
	} else if (encoder_type_now == ENCODER_TYPE_SINCOS) {
		enc_sincos_deinit();
	} else if (encoder_type_now == ENCODER_TYPE_TS5700N8501) {
		enc_ts5700n8501_deinit();
	}

	encoder_type_now = ENCODER_TYPE_NONE;
}

encoder_ret_t encoder_init(volatile mc_configuration *conf) {
	encoder_ret_t res = ENCODER_ERROR;

	if (encoder_type_now != ENCODER_TYPE_NONE) {
		return res;
	}

	switch (conf->m_sensor_port_mode) {
	case SENSOR_PORT_MODE_ABI: {
		SENSOR_PORT_5V();

		encoder_conf_ABI.counts = conf->m_encoder_counts;
		encoder_ret_t encoder_ret;

		if (is_incremental_defined() == false) {
			return ENCODER_ERROR;
		}

		encoder_conf_ABI.is_init = 0;

		encoder_ret = enc_abi_init(&encoder_conf_ABI);

		if (ENCODER_OK != encoder_ret || !encoder_conf_ABI.is_init) {
			encoder_type_now = ENCODER_TYPE_NONE;
			index_found = false;
			return ENCODER_ERROR;
		}
		encoder_type_now = ENCODER_TYPE_ABI;
		index_found = true;
		res = ENCODER_OK;
	} break;

	case SENSOR_PORT_MODE_AS5047_SPI: {
		SENSOR_PORT_3V3();

		encoder_ret_t encoder_ret;

		if (is_spi_defined() == false) {
			return ENCODER_ERROR;
		}

		encoder_conf_ENCSPI.is_init = 0;

		encoder_ret = enc_as504x_init(&encoder_conf_ENCSPI);

		if (ENCODER_OK != encoder_ret || !encoder_conf_ENCSPI.is_init) {
			index_found = false;
			return ENCODER_ERROR;
		}
		encoder_type_now = ENCODER_TYPE_AS504x;
		index_found = true;

		timer_start(20000);

		res = ENCODER_OK;
	} break;

	case SENSOR_PORT_MODE_MT6816_SPI: {
		SENSOR_PORT_5V();
		encoder_ret_t encoder_ret;

		if (is_spi_defined() == false) {
			return ENCODER_ERROR;
		}

		encoder_conf_ENCSPI.is_init = 0;

		encoder_ret = enc_mt6816_init(&encoder_conf_ENCSPI);

		if (ENCODER_OK != encoder_ret || !encoder_conf_ENCSPI.is_init) {
			encoder_type_now = ENCODER_TYPE_NONE;
			index_found = false;
			return ENCODER_ERROR;
		}
		encoder_type_now = ENCODER_TYPE_MT6816;
		index_found = true;

		timer_start(20000);

		res = ENCODER_OK;
	} break;

	case SENSOR_PORT_MODE_AD2S1205: {
		SENSOR_PORT_5V();
		encoder_ret_t encoder_ret;

		if (is_spi_defined() == false) {
			return ENCODER_ERROR;
		}

		encoder_conf_ENCSPI.is_init = 0;

		encoder_ret = enc_ad2s1205_init(&encoder_conf_ENCSPI);

		if (ENCODER_OK != encoder_ret || !encoder_conf_ENCSPI.is_init) {
			encoder_type_now = ENCODER_TYPE_NONE;
			index_found = false;
			return ENCODER_ERROR;
		}
		encoder_type_now = ENCODER_TYPE_AD2S1205_SPI;
		index_found = true;

		timer_start(20000);

		res = ENCODER_OK;
	} break;

	case SENSOR_PORT_MODE_SINCOS: {
		SENSOR_PORT_5V();

		encoder_conf_ENCSINCOS.s_gain = conf->foc_encoder_sin_gain;
		encoder_conf_ENCSINCOS.s_offset = conf->foc_encoder_sin_offset;
		encoder_conf_ENCSINCOS.c_gain = conf->foc_encoder_cos_gain;
		encoder_conf_ENCSINCOS.c_offset =  conf->foc_encoder_cos_offset;
		encoder_conf_ENCSINCOS.filter_constant = conf->foc_encoder_sincos_filter_constant;

		encoder_ret_t encoder_ret;

		encoder_conf_ENCSINCOS.is_init = 0;

		encoder_ret = enc_sincos_init(&encoder_conf_ENCSINCOS);

		if (ENCODER_OK != encoder_ret || !encoder_conf_ENCSINCOS.is_init) {
			encoder_type_now = ENCODER_TYPE_NONE;
			index_found = false;
			return ENCODER_ERROR;
		}
		encoder_type_now = ENCODER_TYPE_SINCOS;
		index_found = true;
		res = ENCODER_OK;
	} break;

	case SENSOR_PORT_MODE_TS5700N8501:
	case SENSOR_PORT_MODE_TS5700N8501_MULTITURN: {
		SENSOR_PORT_5V();
		app_configuration *appconf = mempools_alloc_appconf();
		conf_general_read_app_configuration(appconf);
		if (appconf->app_to_use == APP_ADC ||
				appconf->app_to_use == APP_UART ||
				appconf->app_to_use == APP_PPM_UART ||
				appconf->app_to_use == APP_ADC_UART) {
			appconf->app_to_use = APP_NONE;
			conf_general_store_app_configuration(appconf);
		}
		mempools_free_appconf(appconf);

		encoder_ret_t encoder_ret;

		if (is_uart_defined() == false) {
			return ENCODER_ERROR;
		}

		encoder_conf_TS5700N8501.is_init = 0;

		encoder_ret = enc_ts5700n8501_init(&encoder_conf_TS5700N8501);

		if (ENCODER_OK != encoder_ret || !encoder_conf_TS5700N8501.is_init) {
			encoder_type_now = ENCODER_TYPE_NONE;
			index_found = false;
			return ENCODER_ERROR;
		}
		encoder_type_now = ENCODER_TYPE_TS5700N8501;
		index_found = true;
		res = ENCODER_OK;
	} break;

	default:
		SENSOR_PORT_5V();
		encoder_type_now = ENCODER_TYPE_NONE;
		break;
	}

	terminal_register_command_callback(
			"encoder",
			"Prints the status of the AS5047, AD2S1205, or TS5700N8501 encoder.",
			0,
			terminal_encoder);

	terminal_register_command_callback(
			"encoder_clear_errors",
			"Clear error of the TS5700N8501 encoder.",
			0,
			terminal_encoder_clear_errors);

	terminal_register_command_callback(
			"encoder_clear_multiturn",
			"Clear multiturn counter of the TS5700N8501 encoder.",
			0,
			terminal_encoder_clear_multiturn);

	return res;
}

float encoder_read_deg(void) {
	if (encoder_type_now == ENCODER_TYPE_AS504x) {
		return enc_as504x_read_deg();
	} else if (encoder_type_now == ENCODER_TYPE_MT6816) {
		return enc_mt6816_read_deg();
	} else if (encoder_type_now == ENCODER_TYPE_AD2S1205_SPI) {
		return enc_ad2s1205_read_deg();
	} else if (encoder_type_now == ENCODER_TYPE_ABI) {
		return enc_abi_read_deg();
	} else if (encoder_type_now == ENCODER_TYPE_SINCOS) {
		return enc_sincos_read_deg();
	} else if (encoder_type_now == ENCODER_TYPE_TS5700N8501) {
		return enc_ts5700n8501_read_deg();
	}
	return 0.0;
}

float encoder_read_deg_multiturn(void) {
	if (encoder_type_now == ENCODER_TYPE_TS5700N8501) {
		float ts_mt = (float)enc_ts5700n8501_get_abm();
		if (fabsf(ts_mt) > 5000.0) {
			ts_mt = 0;
			encoder_reset_multiturn();
		}

		ts_mt += 5000;

		return encoder_read_deg() / 10000.0 + (360 * ts_mt) / 10000.0;
	} else {
		return encoder_read_deg();
	}
}

encoder_type_t encoder_is_configured(void) {
	return encoder_type_now;
}

bool encoder_index_found(void) {
	return index_found;
}

void encoder_reset_multiturn(void) {
	if (encoder_type_now == ENCODER_TYPE_TS5700N8501) {
		return enc_ts5700n8501_reset_multiturn();
	}
}

void encoder_reset_errors(void) {
	if (encoder_type_now == ENCODER_TYPE_TS5700N8501) {
		enc_ts5700n8501_reset_errors();
	}
}

void encoder_check_faults(volatile mc_configuration *m_conf, bool is_second_motor) {
	// Trigger encoder error rate fault, using 5% errors as threshold.
	// Relevant only in FOC mode with encoder enabled
	if(m_conf->motor_type == MOTOR_TYPE_FOC &&
			m_conf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER &&
			m_conf->m_sensor_port_mode == SENSOR_PORT_MODE_AS5047_SPI &&
			mcpwm_foc_is_using_encoder() &&
			enc_as504x_spi_get_error_rate() > 0.05) {
		mc_interface_fault_stop(FAULT_CODE_ENCODER_SPI, is_second_motor, false);
	}

	if(m_conf->motor_type == MOTOR_TYPE_FOC &&
			m_conf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER &&
			m_conf->m_sensor_port_mode == SENSOR_PORT_MODE_MT6816_SPI &&
			mcpwm_foc_is_using_encoder() &&
			enc_mt6816_get_no_magnet_error_rate() > 0.05) {
		mc_interface_fault_stop(FAULT_CODE_ENCODER_NO_MAGNET, is_second_motor, false);
	}

	if(m_conf->motor_type == MOTOR_TYPE_FOC &&
			m_conf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER &&
			m_conf->m_sensor_port_mode == SENSOR_PORT_MODE_SINCOS) {

		if (enc_sincos_get_signal_below_min_error_rate() > 0.05)
			mc_interface_fault_stop(FAULT_CODE_ENCODER_SINCOS_BELOW_MIN_AMPLITUDE, is_second_motor, false);
		if (enc_sincos_get_signal_above_max_error_rate() > 0.05)
			mc_interface_fault_stop(FAULT_CODE_ENCODER_SINCOS_ABOVE_MAX_AMPLITUDE, is_second_motor, false);
	}

	if (m_conf->motor_type == MOTOR_TYPE_FOC &&
			m_conf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER &&
			m_conf->m_sensor_port_mode == SENSOR_PORT_MODE_AS5047_SPI) {
		AS504x_diag diag = enc_as504x_get_diag();

		if (!diag.is_connected) {
			mc_interface_fault_stop(FAULT_CODE_ENCODER_SPI, is_second_motor, false);
		}

		if (diag.is_Comp_high) {
			mc_interface_fault_stop(FAULT_CODE_ENCODER_NO_MAGNET, is_second_motor, false);
		} else if(diag.is_Comp_low) {
			mc_interface_fault_stop(FAULT_CODE_ENCODER_MAGNET_TOO_STRONG, is_second_motor, false);
		}
	}

	if(m_conf->motor_type == MOTOR_TYPE_FOC &&
			m_conf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER &&
			m_conf->m_sensor_port_mode == SENSOR_PORT_MODE_AD2S1205) {
		if (enc_ad2s1205_resolver_loss_of_tracking_error_rate() > 0.05)
			mc_interface_fault_stop(FAULT_CODE_RESOLVER_LOT, is_second_motor, false);
		if (enc_ad2s1205_resolver_degradation_of_signal_error_rate() > 0.05)
			mc_interface_fault_stop(FAULT_CODE_RESOLVER_DOS, is_second_motor, false);
		if (enc_ad2s1205_resolver_loss_of_signal_error_rate() > 0.04)
			mc_interface_fault_stop(FAULT_CODE_RESOLVER_LOS, is_second_motor, false);
	}
}

void encoder_pin_isr(void) {
	// Only reset if the pin is still high to avoid too short pulses, which
	// most likely are noise.
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	if (palReadPad(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3)) {
		const unsigned int cnt = HW_ENC_TIM->CNT;
		static int bad_pulses = 0;
		const unsigned int lim = encoder_conf_ABI.counts / 20;

		if (encoder_index_found()) {
			// Some plausibility filtering.
			if (cnt > (encoder_conf_ABI.counts - lim) || cnt < lim) {
				HW_ENC_TIM->CNT = 0;
				bad_pulses = 0;
			} else {
				bad_pulses++;

				if (bad_pulses > 5) {
					index_found = 0;
				}
			}
		} else {
			HW_ENC_TIM->CNT = 0;
			index_found = true;
			bad_pulses = 0;
		}
	}
}

void encoder_tim_isr(void) {
	if (encoder_type_now == ENCODER_TYPE_AS504x) {
		enc_as504x_routine(timer_rate_now);
	} else if (encoder_type_now == ENCODER_TYPE_MT6816) {
		enc_mt6816_routine(timer_rate_now);
	} else if (encoder_type_now == ENCODER_TYPE_AD2S1205_SPI) {
		enc_ad2s1205_routine(timer_rate_now);
	}
}

static void terminal_encoder(int argc, const char **argv) {
	(void)argc; (void)argv;

	const volatile mc_configuration *mcconf = mc_interface_get_configuration();

	if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_AS5047_SPI ||
			mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_MT6816_SPI ||
			mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_AD2S1205 ||
			mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_TS5700N8501 ||
			mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_TS5700N8501_MULTITURN) {

		unsigned int spi_val = 0;
		float error_rate = 0.0;
		unsigned int error_cnt = 0;
		AS504x_diag diag = { 0 };

		if (encoder_is_configured() == ENCODER_TYPE_AS504x) {
			spi_val = enc_as504x_spi_get_val();
			error_rate = enc_as504x_spi_get_error_rate();
			error_cnt = enc_as504x_spi_get_error_cnt();
			diag = enc_as504x_get_diag();
		} else if (encoder_is_configured() == ENCODER_TYPE_MT6816) {
			spi_val = enc_mt6816_spi_get_val();
			error_cnt = enc_mt6816_spi_get_error_cnt();
		}

		if (mcconf->m_sensor_port_mode != SENSOR_PORT_MODE_AS5047_SPI) {
			commands_printf("SPI encoder value: %d, errors: %d, error rate: %.3f %%",
					spi_val,
					error_cnt,
					(double)(error_rate * 100.0));
		} else {
			commands_printf("SPI encoder value: %d, errors: %d, error rate: %.3f %%, Connected: %u",
					spi_val,
					error_cnt,
					(double)(error_rate * 100.0),
					diag.is_connected);
		}

		if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_TS5700N8501 ||
				mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_TS5700N8501_MULTITURN) {
			char sf[9];
			char almc[9];
			utils_byte_to_binary(enc_ts5700n8501_get_raw_status()[0], sf);
			utils_byte_to_binary(enc_ts5700n8501_get_raw_status()[7], almc);
			commands_printf("TS5700N8501 ABM: %d, SF: %s, ALMC: %s\n", enc_ts5700n8501_get_abm(), sf, almc);
		}

		if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_MT6816_SPI) {
			commands_printf("Low flux error (no magnet): errors: %d, error rate: %.3f %%",
					enc_mt6816_get_no_magnet_error_cnt(),
					(double)(enc_mt6816_get_no_magnet_error_rate() * 100.0));
		}

#if AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS
		if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_AS5047_SPI) {
			commands_printf("\nAS5047 DIAGNOSTICS:\n"
					"AGC       : %u\n"
					"Magnitude : %u\n"
					"COF       : %u\n"
					"OCF       : %u\n"
					"COMP_low  : %u\n"
					"COMP_high : %u\n",
					diag.AGC_value, diag.magnitude,
					diag.is_COF, diag.is_OCF,
					diag.is_Comp_low,
					diag.is_Comp_high);
		}
#endif
	}

	if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_SINCOS) {
		commands_printf("Sin/Cos encoder signal below minimum amplitude: errors: %d, error rate: %.3f %%",
				enc_sincos_get_signal_below_min_error_cnt(),
				(double)(enc_sincos_get_signal_below_min_error_rate() * 100.0));

		commands_printf("Sin/Cos encoder signal above maximum amplitude: errors: %d, error rate: %.3f %%",
				enc_sincos_get_signal_above_max_error_cnt(),
				(double)(enc_sincos_get_signal_above_max_error_rate() * 100.0));
	}

	if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_AD2S1205) {
		commands_printf("Resolver Loss Of Tracking (>5%c error): errors: %d, error rate: %.3f %%", 0xB0,
				enc_ad2s1205_resolver_loss_of_tracking_error_cnt(),
				(double)(enc_ad2s1205_resolver_loss_of_tracking_error_rate() * 100.0));
		commands_printf("Resolver Degradation Of Signal (>33%c error): errors: %d, error rate: %.3f %%", 0xB0,
				enc_ad2s1205_resolver_degradation_of_signal_error_cnt(),
				(double)(enc_ad2s1205_resolver_degradation_of_signal_error_rate() * 100.0));
		commands_printf("Resolver Loss Of Signal (>57%c error): errors: %d, error rate: %.3f %%", 0xB0,
				enc_ad2s1205_resolver_loss_of_signal_error_cnt(),
				(double)(enc_ad2s1205_resolver_loss_of_signal_error_rate() * 100.0));
	}

	if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_ABI) {
		commands_printf("Index found: %d\n", encoder_index_found());
	}
}

static void terminal_encoder_clear_errors(int argc, const char **argv) {
	(void)argc; (void)argv;
	encoder_reset_errors();
	commands_printf("Done!\n");
}

static void terminal_encoder_clear_multiturn(int argc, const char **argv) {
	(void)argc; (void)argv;
	encoder_reset_multiturn();
	commands_printf("Done!\n");
}

static bool is_uart_defined(void) {
	if (!encoder_conf_TS5700N8501.RX_gpio
			|| !encoder_conf_TS5700N8501.TX_gpio) {
		return false;
	}
	return true;
}

static bool is_spi_defined(void) {
	if (!encoder_conf_ENCSPI.sw_spi.miso_gpio
			|| !encoder_conf_ENCSPI.sw_spi.nss_gpio
			|| !encoder_conf_ENCSPI.sw_spi.sck_gpio) {
		return false;
	}
	return true;
}

static bool is_incremental_defined(void) {
	if (!encoder_conf_ABI.A_gpio
			|| !encoder_conf_ABI.B_gpio) {
		return false;
	}
	return true;
}

static void timer_start(float rate) {
	timer_rate_now = rate;

	TIM_TimeBaseInitTypeDef TIM_TimeBaseStructure;

	// Enable timer clock
	HW_ENC_TIM_CLK_EN();

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = ((168000000 / 2 / rate) - 1);
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(HW_ENC_TIM, &TIM_TimeBaseStructure);

	// Enable overflow interrupt
	TIM_ITConfig(HW_ENC_TIM, TIM_IT_Update, ENABLE);

	// Enable timer
	TIM_Cmd(HW_ENC_TIM, ENABLE);
}
