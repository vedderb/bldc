/*
	Copyright 2019 Maximiliano Cordoba	mcordoba@powerdesigns.ca

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
#include "virtual_motor.h"
#include "terminal.h"
#include "mc_interface.h"
#include "mcpwm_foc.h"
#include "utils.h"
#include "math.h"
#include "stdio.h"
#include "commands.h"
#include "encoder.h"

typedef struct{
	//constant variables
	float Ts;					//Sample Time in s
	float J;					//Rotor/Load Inertia in Nm*s^2
	int v_max_adc;				//max voltage that ADC can measure
	int pole_pairs;				//number of pole pairs ( pole numbers / 2)
	float km;					//constant = 1.5 * pole pairs
	float ld;					//motor inductance in D axis in uHy
	float lq;					//motor inductance in Q axis in uHy

	//non constant variables
	float id;		            //Current in d-Direction in Amps
	float id_int;		        //Integral part of id in Amps
	float iq;		            //Current in q-Direction in A
	float me;		            //Electrical Torque in Nm
	float we;		            //Electrical Angular Velocity in rad/s
	float phi;		            //Electrical Rotor Angle in rad
	float sin_phi;
	float cos_phi;
	bool connected;				//true => connected; false => disconnected;
	float tsj;					// Ts / J;
	float ml;					//load torque
	float v_alpha;				//alpha axis voltage in Volts
	float v_beta; 				//beta axis voltage in Volts
	float va;					//phase a voltage in Volts
	float vb;					//phase b voltage in Volts
	float vc;					//phase c voltage in Volts
	float vd;					//d axis voltage in Volts
	float vq;					//q axis voltage in Volts
	float i_alpha;				//alpha axis current in Amps
	float i_beta;				//beta axis current in Amps
	float ia;					//phase a current in Amps
	float ib;					//phase b current in Amps
	float ic;					//phase c current in Amps
}virtual_motor_t;

static volatile virtual_motor_t virtual_motor;
static volatile int m_curr0_offset_backup;
static volatile int m_curr1_offset_backup;
static volatile int m_curr2_offset_backup;
static volatile mc_configuration *m_conf;

//private functions
static void connect_virtual_motor(float ml, float J, float Vbus);
static void disconnect_virtual_motor(void);
static inline void run_virtual_motor_electrical(float v_alpha, float v_beta);
static inline void run_virtual_motor_mechanics(float ml);
static inline void run_virtual_motor(float v_alpha, float v_beta, float ml);
static inline void run_virtual_motor_park_clark_inverse( void );
static void terminal_cmd_connect_virtual_motor(int argc, const char **argv);
static void terminal_cmd_disconnect_virtual_motor(int argc, const char **argv);

//Public Functions

/**
 * Virtual motor initialization
 */
void virtual_motor_init(volatile mc_configuration *conf){

	virtual_motor_set_configuration(conf);

	//virtual motor variables init
	virtual_motor.connected = false; //disconnected

	virtual_motor.me = 0.0;
	virtual_motor.va = 0.0;
	virtual_motor.vb = 0.0;
	virtual_motor.vc = 0.0;
	virtual_motor.ia = 0.0;
	virtual_motor.ib = 0.0;
	virtual_motor.ic = 0.0;
	virtual_motor.we = 0.0;
	virtual_motor.v_alpha = 0.0;
	virtual_motor.v_beta = 0.0;
	virtual_motor.i_alpha = 0.0;
	virtual_motor.i_beta = 0.0;
	virtual_motor.id_int = 0.0;
	virtual_motor.iq = 0.0;

	// Register terminal callbacks used for virtual motor setup
	terminal_register_command_callback(
				"connect_virtual_motor",
				"connects virtual motor",
				"[ml][J][Vbus]",
				terminal_cmd_connect_virtual_motor);

	terminal_register_command_callback(
				"disconnect_virtual_motor",
				"disconnect virtual motor",
				0,
				terminal_cmd_disconnect_virtual_motor);
}

void virtual_motor_set_configuration(volatile mc_configuration *conf){
	m_conf = conf;

	//recalculate constants that depend on m_conf
	virtual_motor.pole_pairs = m_conf->si_motor_poles / 2;
	virtual_motor.km = 1.5 * virtual_motor.pole_pairs;
#ifdef HW_HAS_PHASE_SHUNTS
	if (m_conf->foc_sample_v0_v7) {
		virtual_motor.Ts = (1.0 / m_conf->foc_f_sw) ;
	} else {
		virtual_motor.Ts = (1.0 / (m_conf->foc_f_sw / 2.0));
	}
#else
	virtual_motor.Ts = (1.0 / m_conf->foc_f_sw) ;
#endif

	if(m_conf->foc_motor_ld_lq_diff > 0.0){
		virtual_motor.lq = m_conf->foc_motor_l + m_conf->foc_motor_ld_lq_diff /2;
		virtual_motor.ld = m_conf->foc_motor_l - m_conf->foc_motor_ld_lq_diff /2;
	}else{
		virtual_motor.lq = m_conf->foc_motor_l ;
		virtual_motor.ld = m_conf->foc_motor_l ;
	}
}

/**
 * Virtual motor interrupt handler
 */
void virtual_motor_int_handler(float v_alpha, float v_beta){
	if(virtual_motor.connected){
		run_virtual_motor(v_alpha, v_beta, virtual_motor.ml );
		mcpwm_foc_adc_int_handler( NULL, 0);
	}
}

bool virtual_motor_is_connected(void){
	return virtual_motor.connected;
}

float virtual_motor_get_angle_deg(void){
	return (virtual_motor.phi * 180.0 / M_PI);
}

//Private Functions

/**
 * void connect_virtual_motor( )
 *
 * -disconnects TIM8 trigger to the ADC:
 * 		mcpwm_foc_adc_int_handler() will be called from TIM8 interrupt
 * 		while virtual motor is connected
 * -sets virtual motor parameters
 *
 * @param ml : torque present at motor axis in Nm
 * @param J: rotor inertia Nm*s^2
 * @param Vbus: Bus voltage in Volts
 */
static void connect_virtual_motor(float ml , float J, float Vbus){
	if(virtual_motor.connected == false){
		//first we send 0.0 current command to make system stop PWM outputs
		mcpwm_foc_set_current(0.0);
		//first we disconnect the ADC triggering from TIM8_CC1
		ADC_InitTypeDef ADC_InitStructure;

		ADC_InitStructure.ADC_Resolution = ADC_Resolution_12b;
		ADC_InitStructure.ADC_ScanConvMode = ENABLE;
		ADC_InitStructure.ADC_ContinuousConvMode = DISABLE;
		ADC_InitStructure.ADC_ExternalTrigConvEdge = ADC_ExternalTrigConvEdge_None;
		ADC_InitStructure.ADC_ExternalTrigConv = 0;
		ADC_InitStructure.ADC_DataAlign = ADC_DataAlign_Right;
		ADC_InitStructure.ADC_NbrOfConversion = HW_ADC_NBR_CONV;

		ADC_Init(ADC1, &ADC_InitStructure);

		//save current offsets
		mcpwm_foc_get_current_offsets(&m_curr0_offset_backup,
										&m_curr1_offset_backup,
										&m_curr2_offset_backup,
										false);
		//set current offsets to 2048
		mcpwm_foc_set_current_offsets(2048, 2048, 2048);

		//sets virtual motor variables
		ADC_Value[ ADC_IND_TEMP_MOS ] = 2048;
		ADC_Value[ ADC_IND_TEMP_MOTOR ] = 2048;
#ifdef 	HW_HAS_GATE_DRIVER_SUPPLY_MONITOR
		//we load 1 to get the transfer function indirectly
		ADC_Value[ ADC_IND_VOUT_GATE_DRV ] = 1;

		float tempVoltage = GET_GATE_DRIVER_SUPPLY_VOLTAGE();
		if(tempVoltage != 0.0){
			ADC_Value[ ADC_IND_VOUT_GATE_DRV ] = ( (HW_GATE_DRIVER_SUPPLY_MAX_VOLTAGE + HW_GATE_DRIVER_SUPPLY_MIN_VOLTAGE) / 2.0 ) /
																							GET_GATE_DRIVER_SUPPLY_VOLTAGE();
		}
#endif
		virtual_motor.phi = mcpwm_foc_get_phase() * M_PI / 180.0;
		utils_fast_sincos_better(virtual_motor.phi, (float*)&virtual_motor.sin_phi,
														(float*)&virtual_motor.cos_phi);

		if(m_conf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER){
			encoder_deinit();
		}
	}

	//we load 1 to get the transfer function indirectly
	ADC_Value[ ADC_IND_VIN_SENS ] = 1;

	float tempVoltage = GET_INPUT_VOLTAGE();
	if(tempVoltage != 0.0){
		ADC_Value[ ADC_IND_VIN_SENS ] = Vbus / GET_INPUT_VOLTAGE();
	}

	//initialize constants
	virtual_motor.v_max_adc = Vbus;
	virtual_motor.J = J;
	virtual_motor.tsj = virtual_motor.Ts / virtual_motor.J;
	virtual_motor.ml = ml;

	virtual_motor.connected = true;
}

/**
 *  void disconnect_virtual_motor( )
 *
 *	if motor is connected:
 *		-stop motor
 *		-disconnect virtual motor
 *		-connects TIM8 back to the trigger of the ADC peripheral
 */
static void disconnect_virtual_motor( void ){

	if(virtual_motor.connected){
		mcpwm_foc_set_current( 0.0 );

		//disconnect virtual motor
		virtual_motor.connected = false;

		//set current offsets back
		mcpwm_foc_set_current_offsets(m_curr0_offset_backup, m_curr1_offset_backup,
															m_curr2_offset_backup);

		//then we reconnect the ADC triggering to TIM8_CC1
		ADC_InitTypeDef ADC_InitStructure;

		ADC_InitStructure.ADC_Resolution = ADC_Resolution_12b;
		ADC_InitStructure.ADC_ScanConvMode = ENABLE;
		ADC_InitStructure.ADC_ContinuousConvMode = DISABLE;
		ADC_InitStructure.ADC_ExternalTrigConvEdge = ADC_ExternalTrigConvEdge_Falling;
		ADC_InitStructure.ADC_ExternalTrigConv = ADC_ExternalTrigConv_T8_CC1;
		ADC_InitStructure.ADC_DataAlign = ADC_DataAlign_Right;
		ADC_InitStructure.ADC_NbrOfConversion = HW_ADC_NBR_CONV;

		ADC_Init(ADC1, &ADC_InitStructure);

		if(m_conf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER){
			switch (m_conf->m_sensor_port_mode) {
			case SENSOR_PORT_MODE_ABI:
				encoder_init_abi(m_conf->m_encoder_counts);
				break;

			case SENSOR_PORT_MODE_AS5047_SPI:
				encoder_init_as5047p_spi();
				break;

			case SENSOR_PORT_MODE_AD2S1205:
				encoder_init_ad2s1205_spi();
				break;

			case SENSOR_PORT_MODE_SINCOS:
				encoder_init_sincos(m_conf->foc_encoder_sin_gain, m_conf->foc_encoder_sin_offset,
									m_conf->foc_encoder_cos_gain, m_conf->foc_encoder_cos_offset,
									m_conf->foc_encoder_sincos_filter_constant);
				break;

			default:
				break;
			}
		}
	}
}

/*
 * Run complete Motor Model
 * @param ml	externally applied load torque in Nm (adidionally to the Inertia)
 */
static inline void run_virtual_motor(float v_alpha, float v_beta, float ml){
	run_virtual_motor_electrical(v_alpha, v_beta);
	run_virtual_motor_mechanics(ml);
	run_virtual_motor_park_clark_inverse();
}

/**
 * Run electrical model of the machine
 *
 * Takes as parameters v_alpha and v_beta,
 * which are outputs from the mcpwm_foc system,
 * representing which voltages the controller tried to set at last step
 *
 * @param v_alpha	alpha axis Voltage in V
 * @param v_beta	beta axis Voltage in V
 */
static inline void run_virtual_motor_electrical(float v_alpha, float v_beta){

	virtual_motor.vd =  virtual_motor.cos_phi * v_alpha + virtual_motor.sin_phi * v_beta;
	virtual_motor.vq =  virtual_motor.cos_phi * v_beta - virtual_motor.sin_phi * v_alpha;

	// d axis current
	virtual_motor.id_int += ((virtual_motor.vd +
								virtual_motor.we *
								virtual_motor.pole_pairs *
								virtual_motor.lq * virtual_motor.iq -
								m_conf->foc_motor_r * virtual_motor.id )
								* virtual_motor.Ts ) / virtual_motor.ld;
	virtual_motor.id = virtual_motor.id_int - m_conf->foc_motor_flux_linkage / virtual_motor.ld;

	// q axis current
	virtual_motor.iq += (virtual_motor.vq -
						virtual_motor.we *
						virtual_motor.pole_pairs *
						(virtual_motor.ld * virtual_motor.id + m_conf->foc_motor_flux_linkage) -
						m_conf->foc_motor_r * virtual_motor.iq )
						* virtual_motor.Ts / virtual_motor.lq;

//	// limit current maximum values
	utils_truncate_number_abs((float *) &(virtual_motor.iq) , (2048 * FAC_CURRENT) );
	utils_truncate_number_abs((float *) &(virtual_motor.id) , (2048 * FAC_CURRENT) );
}

/**
 * Run mechanical side of the machine
 * @param ml	externally applied load torque in Nm
 */
static inline void run_virtual_motor_mechanics(float ml){
	virtual_motor.me =  virtual_motor.km * (m_conf->foc_motor_flux_linkage +
											(virtual_motor.ld - virtual_motor.lq) *
											virtual_motor.id ) * virtual_motor.iq;
	// omega
	virtual_motor.we += virtual_motor.tsj * (virtual_motor.me - ml);

	// phi
	virtual_motor.phi += virtual_motor.we * virtual_motor.Ts;

	// phi limits
	while( virtual_motor.phi > M_PI ){
		virtual_motor.phi -= ( 2 * M_PI);
	}

	while( virtual_motor.phi < -1.0 * M_PI ){
		virtual_motor.phi += ( 2 * M_PI);
	}
}

/**
 * Take the id and iq calculated values and translate them into ADC_Values
 */
static inline void run_virtual_motor_park_clark_inverse( void ){
	utils_fast_sincos_better( virtual_motor.phi , (float*)&virtual_motor.sin_phi,
													(float*)&virtual_motor.cos_phi );

	//	Park Inverse
	virtual_motor.i_alpha = virtual_motor.cos_phi * virtual_motor.id -
							virtual_motor.sin_phi * virtual_motor.iq;
	virtual_motor.i_beta  = virtual_motor.cos_phi * virtual_motor.iq +
							virtual_motor.sin_phi * virtual_motor.id;

	virtual_motor.v_alpha = virtual_motor.cos_phi * virtual_motor.vd -
							virtual_motor.sin_phi * virtual_motor.vq;
	virtual_motor.v_beta  = virtual_motor.cos_phi * virtual_motor.vq +
							virtual_motor.sin_phi * virtual_motor.vd;

	//	Clark Inverse
	virtual_motor.ia = virtual_motor.i_alpha;
	virtual_motor.ib = -0.5 * virtual_motor.i_alpha + SQRT3_BY_2 * virtual_motor.i_beta;
	virtual_motor.ic = -0.5 * virtual_motor.i_alpha - SQRT3_BY_2 * virtual_motor.i_beta;

	virtual_motor.va = virtual_motor.v_alpha;
	virtual_motor.vb = -0.5 * virtual_motor.v_alpha + SQRT3_BY_2 * virtual_motor.v_beta;
	virtual_motor.vc = -0.5 * virtual_motor.v_alpha - SQRT3_BY_2 * virtual_motor.v_beta;

	//	simulate current samples
	ADC_Value[ ADC_IND_CURR1 ] =  virtual_motor.ia / FAC_CURRENT + 2048;
	ADC_Value[ ADC_IND_CURR2 ] =  virtual_motor.ib / FAC_CURRENT + 2048;
#ifdef HW_HAS_3_SHUNTS
	ADC_Value[ ADC_IND_CURR3 ] =  virtual_motor.ic / FAC_CURRENT + 2048;
#endif
	//	simulate voltage samples
	ADC_Value[ ADC_IND_SENS1 ] = virtual_motor.va * VOLTAGE_TO_ADC_FACTOR + 2048;
	ADC_Value[ ADC_IND_SENS2 ] = virtual_motor.vb * VOLTAGE_TO_ADC_FACTOR + 2048;
	ADC_Value[ ADC_IND_SENS3 ] = virtual_motor.vc * VOLTAGE_TO_ADC_FACTOR + 2048;
}

/**
 * connect_virtual_motor command
 */
static void terminal_cmd_connect_virtual_motor(int argc, const char **argv) {
	if( argc == 4 ){
		float ml; //torque load in motor axis
		float J; //rotor inertia
		float Vbus;//Bus voltage

		sscanf(argv[1], "%f", &ml);
		sscanf(argv[2], "%f", &J);
		sscanf(argv[3], "%f", &Vbus);

		connect_virtual_motor( ml , J, Vbus);
		commands_printf("virtual motor connected");
	}
	else{
		commands_printf("arguments should be 3" );
	}
}

/**
 * disconnect_virtual_motor command
 */
static void terminal_cmd_disconnect_virtual_motor(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	disconnect_virtual_motor();
	commands_printf("virtual motor disconnected");
	commands_printf(" ");
}
