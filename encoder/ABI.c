

#include "encoder/ABI.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>

static float last_enc_angle = 0.0;
static ABI_config_t abi_config_now = {0};
static uint32_t enc_counts = 10000;

void ABI_deinit(void){
	nvicDisableVector(HW_ENC_EXTI_CH);

	TIM_DeInit(HW_ENC_TIM);

	palSetPadMode(abi_config_now.incremental_config.gpio_A.port, abi_config_now.incremental_config.gpio_A.pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(abi_config_now.incremental_config.gpio_B.port, abi_config_now.incremental_config.gpio_B.pin, PAL_MODE_INPUT_PULLUP);

	last_enc_angle = 0.0;
}

encoders_ret_t ABI_init(ABI_config_t *abi_config){

	EXTI_InitTypeDef   EXTI_InitStructure;

	// Initialize variables
	enc_counts = abi_config->counts;

	palSetPadMode(abi_config->incremental_config.gpio_A.port, abi_config->incremental_config.gpio_A.pin, PAL_MODE_ALTERNATE(HW_ENC_TIM_AF));
	palSetPadMode(abi_config->incremental_config.gpio_B.port, abi_config->incremental_config.gpio_B.pin, PAL_MODE_ALTERNATE(HW_ENC_TIM_AF));
//	palSetPadMode(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, PAL_MODE_ALTERNATE(HW_ENC_TIM_AF));

	// Enable timer clock
	HW_ENC_TIM_CLK_EN();

	// Enable SYSCFG clock
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_SYSCFG, ENABLE);

	TIM_EncoderInterfaceConfig (HW_ENC_TIM, TIM_EncoderMode_TI12,
			TIM_ICPolarity_Rising,
			TIM_ICPolarity_Rising);
	TIM_SetAutoreload(HW_ENC_TIM, enc_counts - 1);

	// Filter
	HW_ENC_TIM->CCMR1 |= 6 << 12 | 6 << 4;
	HW_ENC_TIM->CCMR2 |= 6 << 4;

	TIM_Cmd(HW_ENC_TIM, ENABLE);

	// Interrupt on index pulse

	// Connect EXTI Line to pin
	SYSCFG_EXTILineConfig(HW_ENC_EXTI_PORTSRC, HW_ENC_EXTI_PINSRC);

	// Configure EXTI Line
	EXTI_InitStructure.EXTI_Line = HW_ENC_EXTI_LINE;
	EXTI_InitStructure.EXTI_Mode = EXTI_Mode_Interrupt;
	EXTI_InitStructure.EXTI_Trigger = EXTI_Trigger_Rising;
	EXTI_InitStructure.EXTI_LineCmd = ENABLE;
	EXTI_Init(&EXTI_InitStructure);

	// Enable and set EXTI Line Interrupt to the highest priority
	nvicEnableVector(HW_ENC_EXTI_CH, 0);
	abi_config->is_init = 1;

	abi_config_now = *abi_config;
	return ENCODERS_OK;
}

float ABI_read_deg(void){
	last_enc_angle = ((float)HW_ENC_TIM->CNT * 360.0) / (float)enc_counts;
	return last_enc_angle;
}

