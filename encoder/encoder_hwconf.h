

#ifndef ENCODER_ENCODER_HWCONF_H_
#define ENCODER_ENCODER_HWCONF_H_

#include "encoder/encoder_datatype.h"

#define AS5047_SAMPLE_RATE_HZ       20000
#define AD2S1205_SAMPLE_RATE_HZ     20000       //25MHz max spi clk
#define MT6816_SAMPLE_RATE_HZ       20000
#define MT6816_NO_MAGNET_ERROR_MASK 0x0002
#define SINCOS_SAMPLE_RATE_HZ       20000
#define SINCOS_MIN_AMPLITUDE        1.0         // sqrt(sin^2 + cos^2) has to be larger than this
#define SINCOS_MAX_AMPLITUDE        1.65        // sqrt(sin^2 + cos^2) has to be smaller than this

extern encoders_config_t conf_AS5047;
extern encoders_config_t conf_MT6816;
extern encoders_config_t conf_AD2S1205;
extern encoders_config_t conf_ABI;

#endif /* ENCODER_ENCODER_HWCONF_H_ */
