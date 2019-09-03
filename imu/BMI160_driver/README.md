# BMI160 sensor API
## Introduction
This package contains the Bosch Sensortec's BMI160 sensor driver (sensor API)

The sensor driver package includes bmi160.h, bmi160.c and bmi160_defs.h files

## Version
File          | Version | Date
--------------|---------|---------------
bmi160.c      |   3.7.7 |   13 Mar 2019
bmi160.h      |   3.7.7 |   13 Mar 2019
bmi160_defs.h |   3.7.7 |   13 Mar 2019

## Integration details
* Integrate bmi160.h, bmi160_defs.h and bmi160.c file in to your project.
* Include the bmi160.h file in your code like below.
``` c
#include "bmi160.h"
```

## File information
* bmi160_defs.h : This header file has the constants, macros and datatype declarations.
* bmi160.h : This header file contains the declarations of the sensor driver APIs.
* bmi160.c : This source file contains the definitions of the sensor driver APIs.

## Supported sensor interface
* SPI 4-wire
* I2C

## Usage guide
### Initializing the sensor
To initialize the sensor, you will first need to create a device structure. You 
can do this by creating an instance of the structure bmi160_dev. Then go on to 
fill in the various parameters as shown below.

#### Example for SPI 4-Wire
``` c
struct bmi160_dev sensor;

/* You may assign a chip select identifier to be handled later */
sensor.id = 0;
sensor.interface = BMI160_SPI_INTF;
sensor.read = user_spi_read;
sensor.write = user_spi_write;
sensor.delay_ms = user_delay_ms;


int8_t rslt = BMI160_OK;
rslt = bmi160_init(&sensor);
/* After the above function call, accel_cfg and gyro_cfg parameters in the device 
structure are set with default values, found in the datasheet of the sensor */
```

#### Example for I2C
``` c
struct bmi160_dev sensor;

sensor.id = BMI160_I2C_ADDR;
sensor.interface = BMI160_I2C_INTF;
sensor.read = user_i2c_read;
sensor.write = user_i2c_write;
sensor.delay_ms = user_delay_ms;

int8_t rslt = BMI160_OK;
rslt = bmi160_init(&sensor);
/* After the above function call, accel and gyro parameters in the device structure 
are set with default values, found in the datasheet of the sensor */
```

### Configuring accel and gyro sensor
#### Example for configuring accel and gyro sensors in normal mode
``` c

int8_t rslt = BMI160_OK;

/* Select the Output data rate, range of accelerometer sensor */
sensor.accel_cfg.odr = BMI160_ACCEL_ODR_1600HZ;
sensor.accel_cfg.range = BMI160_ACCEL_RANGE_2G;
sensor.accel_cfg.bw = BMI160_ACCEL_BW_NORMAL_AVG4;

/* Select the power mode of accelerometer sensor */
sensor.accel_cfg.power = BMI160_ACCEL_NORMAL_MODE;

/* Select the Output data rate, range of Gyroscope sensor */
sensor.gyro_cfg.odr = BMI160_GYRO_ODR_3200HZ;
sensor.gyro_cfg.range = BMI160_GYRO_RANGE_2000_DPS;
sensor.gyro_cfg.bw = BMI160_GYRO_BW_NORMAL_MODE;

/* Select the power mode of Gyroscope sensor */
sensor.gyro_cfg.power = BMI160_GYRO_NORMAL_MODE; 

/* Set the sensor configuration */
rslt = bmi160_set_sens_conf(&sensor);
```

### Reading sensor data
#### Example for reading sensor data
``` c

int8_t rslt = BMI160_OK;
struct bmi160_sensor_data accel;
struct bmi160_sensor_data gyro;

/* To read only Accel data */
rslt = bmi160_get_sensor_data(BMI160_ACCEL_SEL, &accel, NULL, &sensor);

/* To read only Gyro data */
rslt = bmi160_get_sensor_data(BMI160_GYRO_SEL, NULL, &gyro, &sensor);

/* To read both Accel and Gyro data */
bmi160_get_sensor_data((BMI160_ACCEL_SEL | BMI160_GYRO_SEL), &accel, &gyro, &sensor);

/* To read Accel data along with time */
rslt = bmi160_get_sensor_data((BMI160_ACCEL_SEL | BMI160_TIME_SEL) , &accel, NULL, &sensor);

/* To read Gyro data along with time */
rslt = bmi160_get_sensor_data((BMI160_GYRO_SEL | BMI160_TIME_SEL), NULL, &gyro, &sensor);

/* To read both Accel and Gyro data along with time*/
bmi160_get_sensor_data((BMI160_ACCEL_SEL | BMI160_GYRO_SEL | BMI160_TIME_SEL), &accel, &gyro, &sensor);
```

### Setting the power mode of sensors
#### Example for setting power mode of accel and gyro
``` c

int8_t rslt = BMI160_OK;

/* Select the power mode */
sensor.accel_cfg.power = BMI160_ACCEL_SUSPEND_MODE; 
sensor.gyro_cfg.power = BMI160_GYRO_FASTSTARTUP_MODE; 

/*  Set the Power mode  */
rslt = bmi160_set_power_mode(&sensor);

/* Select the power mode */
sensor.accel_cfg.power = BMI160_ACCEL_NORMAL_MODE;
sensor.gyro_cfg.power = BMI160_GYRO_NORMAL_MODE; 

/*  Set the Power mode  */
rslt = bmi160_set_power_mode(&sensor);

```

### Reading sensor data register
#### Example for reading Chip Address
``` c

int8_t rslt = BMI160_OK;
uint8_t reg_addr = BMI160_CHIP_ID_ADDR;
uint8_t data;
uint16_t len = 1;
rslt = bmi160_get_regs(reg_addr, &data, len, &sensor);
```


### Writing to sensor data register
#### Example for writing data to any motion threshold register
``` c

int8_t rslt = BMI160_OK;
uint8_t reg_addr = BMI160_INT_MOTION_1_ADDR;
uint8_t data = 20;
uint16_t len = 1;
rslt = bmi160_set_regs(reg_addr, &data, len, &sensor);
```

### Resetting the device using soft-reset
#### Example for writing soft-reset command to command register
``` c

int8_t rslt = BMI160_OK;
rslt = bmi160_soft_reset(&sensor);
```


### Configuring interrupts for sensors
To configure the sensor interrupts, you will first need to create an interrupt 
structure. You can do this by creating an instance of the structure bmi160_int_settg.
Then go on to fill in the various parameters as shown below


### Configuring Any-motion Interrupt
#### Example for configuring Any-motion Interrupt
Note:- User can check the currently active interrupt(any-motion or sig-motion) by checking the **any_sig_sel** of bmi160_dev structure.
``` c

struct bmi160_int_settg int_config;

/* Select the Interrupt channel/pin */
int_config.int_channel = BMI160_INT_CHANNEL_1;// Interrupt channel/pin 1

/* Select the Interrupt type */
int_config.int_type = BMI160_ACC_ANY_MOTION_INT;// Choosing Any motion interrupt
/* Select the interrupt channel/pin settings */
int_config.int_pin_settg.output_en = BMI160_ENABLE;// Enabling interrupt pins to act as output pin
int_config.int_pin_settg.output_mode = BMI160_DISABLE;// Choosing push-pull mode for interrupt pin
int_config.int_pin_settg.output_type = BMI160_DISABLE;// Choosing active low output
int_config.int_pin_settg.edge_ctrl = BMI160_ENABLE;// Choosing edge triggered output
int_config.int_pin_settg.input_en = BMI160_DISABLE;// Disabling interrupt pin to act as input
int_config.int_pin_settg.latch_dur = BMI160_LATCH_DUR_NONE;// non-latched output

/* Select the Any-motion interrupt parameters */
int_config.int_type_cfg.acc_any_motion_int.anymotion_en = BMI160_ENABLE;// 1- Enable the any-motion, 0- disable any-motion 
int_config.int_type_cfg.acc_any_motion_int.anymotion_x = BMI160_ENABLE;// Enabling x-axis for any motion interrupt
int_config.int_type_cfg.acc_any_motion_int.anymotion_y = BMI160_ENABLE;// Enabling y-axis for any motion interrupt
int_config.int_type_cfg.acc_any_motion_int.anymotion_z = BMI160_ENABLE;// Enabling z-axis for any motion interrupt
int_config.int_type_cfg.acc_any_motion_int.anymotion_dur = 0;// any-motion duration
int_config.int_type_cfg.acc_any_motion_int.anymotion_thr = 20;// (2-g range) -> (slope_thr) * 3.91 mg, (4-g range) -> (slope_thr) * 7.81 mg, (8-g range) ->(slope_thr) * 15.63 mg, (16-g range) -> (slope_thr) * 31.25 mg 

/* Set the Any-motion interrupt */
bmi160_set_int_config(&int_config, &sensor); /* sensor is an instance of the structure bmi160_dev  */

```
### Configuring Flat Interrupt
#### Example for configuring Flat Interrupt
``` c

struct bmi160_int_settg int_config;

/* Select the Interrupt channel/pin */
int_config.int_channel = BMI160_INT_CHANNEL_1;// Interrupt channel/pin 1

/* Select the Interrupt type */
int_config.int_type = BMI160_ACC_FLAT_INT;// Choosing flat interrupt
/* Select the interrupt channel/pin settings */
int_config.int_pin_settg.output_en = BMI160_ENABLE;// Enabling interrupt pins to act as output pin
int_config.int_pin_settg.output_mode = BMI160_DISABLE;// Choosing push-pull mode for interrupt pin
int_config.int_pin_settg.output_type = BMI160_DISABLE;// Choosing active low output
int_config.int_pin_settg.edge_ctrl = BMI160_ENABLE;// Choosing edge triggered output
int_config.int_pin_settg.input_en = BMI160_DISABLE;// Disabling interrupt pin to act as input
int_config.int_pin_settg.latch_dur = BMI160_LATCH_DUR_NONE;// non-latched output

/* Select the Flat interrupt parameters */
int_config.int_type_cfg.acc_flat_int.flat_en = BMI160_ENABLE;// 1-enable, 0-disable the flat interrupt
int_config.int_type_cfg.acc_flat_int.flat_theta = 8;// threshold for detection of flat position in range from 0° to 44.8°.
int_config.int_type_cfg.acc_flat_int.flat_hy = 1;// Flat hysteresis
int_config.int_type_cfg.acc_flat_int.flat_hold_time = 1;// Flat hold time (0 -> 0 ms, 1 -> 640 ms, 2 -> 1280 ms, 3 -> 2560 ms)

/* Set the Flat interrupt */
bmi160_set_int_config(&int_config, &sensor); /* sensor is an instance of the structure bmi160_dev */

```


### Configuring Step Detector Interrupt
#### Example for configuring Step Detector Interrupt
``` c

struct bmi160_int_settg int_config;

/* Select the Interrupt channel/pin */
int_config.int_channel = BMI160_INT_CHANNEL_1;// Interrupt channel/pin 1

/* Select the Interrupt type */
int_config.int_type = BMI160_STEP_DETECT_INT;// Choosing Step Detector interrupt
/* Select the interrupt channel/pin settings */
int_config.int_pin_settg.output_en = BMI160_ENABLE;// Enabling interrupt pins to act as output pin
int_config.int_pin_settg.output_mode = BMI160_DISABLE;// Choosing push-pull mode for interrupt pin
int_config.int_pin_settg.output_type = BMI160_ENABLE;// Choosing active High output
int_config.int_pin_settg.edge_ctrl = BMI160_ENABLE;// Choosing edge triggered output
int_config.int_pin_settg.input_en = BMI160_DISABLE;// Disabling interrupt pin to act as input
int_config.int_pin_settg.latch_dur =BMI160_LATCH_DUR_NONE;// non-latched output

/* Select the Step Detector interrupt parameters, Kindly use the recommended settings for step detector */
int_config.int_type_cfg.acc_step_detect_int.step_detector_mode = BMI160_STEP_DETECT_NORMAL;
int_config.int_type_cfg.acc_step_detect_int.step_detector_en = BMI160_ENABLE;// 1-enable, 0-disable the step detector

/* Set the Step Detector interrupt */
bmi160_set_int_config(&int_config, &sensor); /* sensor is an instance of the structure bmi160_dev */

```

### Configuring Step counter
To configure the step counter, user need to configure the step detector interrupt as described in above section.
After configuring step detector, see the below code snippet for user space & ISR

### User space
``` c
int8_t rslt = BMI160_OK;
uint8_t step_enable = 1;//enable the step counter

rslt = bmi160_set_step_counter(step_enable,  &sensor);
```

### ISR
``` c
int8_t rslt = BMI160_OK;
uint16_t step_count = 0;//stores the step counter value

rslt = bmi160_read_step_counter(&step_count,  &sensor);
```

### Unmapping Interrupt
#### Example for unmapping Step Detector Interrupt
``` c
struct bmi160_int_settg int_config;

/* Deselect the Interrupt channel/pin */
int_config.int_channel = BMI160_INT_CHANNEL_NONE;
/* Select the Interrupt type */
int_config.int_type = BMI160_STEP_DETECT_INT;// Choosing Step Detector interrupt
/* Set the Step Detector interrupt */
bmi160_set_int_config(&int_config, &sensor); /* sensor is an instance of the structure bmi160_dev */
```

### Reading interrupt status
#### Example for reading interrupt status for step detector
``` c
union bmi160_int_status interrupt;
enum bmi160_int_status_sel int_status_sel;

/* Interrupt status selection to read all interrupts */
int_status_sel = BMI160_INT_STATUS_ALL;
rslt = bmi160_get_int_status(int_status_sel, &interrupt, &sensor);
if (interrupt.bit.step)
	printf("Step detector interrupt occured\n");
```

### Configuring the auxiliary sensor BMM150
It is assumed that secondary interface of bmi160 has external pull-up resistor in order to access the auxiliary sensor bmm150.

### Accessing auxiliary BMM150 with BMM150 APIs via BMI160 secondary interface.

## Integration details 
* Integrate the souce codes of BMM150 and BMI160 in project.
* Include the bmi160.h and bmm150.h file in your code like below.
* It is mandatory to initialize the bmi160 device structure for primary interface and auxiliary sensor settings.
* Create two wrapper functions , user_aux_read and user_aux_write in order to match the signature as mentioned below.
* Invoke the "bmi160_aux_init" API to initialise the secondary interface in BMI160.
* Invoke the "bmm150_init" API to initialise the BMM150 sensor.
* Now we can use the BMM150 sensor APIs to access the BMM150 via BMI160.

``` c
/* main.c file */
#include "bmi160.h"
#include "bmm150.h"
```
### Initialization of auxiliary sensor BMM150
```

/* main.c file */
struct bmm150_dev bmm150;

/* function declaration */
int8_t user_aux_read(uint8_t id, uint8_t reg_addr, uint8_t *aux_data, uint16_t len);
int8_t user_aux_write(uint8_t id, uint8_t reg_addr, uint8_t *aux_data, uint16_t len);

/* Configure device structure for auxiliary sensor parameter */
sensor.aux_cfg.aux_sensor_enable = 1; // auxiliary sensor enable
sensor.aux_cfg.aux_i2c_addr = BMI160_AUX_BMM150_I2C_ADDR; // auxiliary sensor address
sensor.aux_cfg.manual_enable = 1; // setup mode enable
sensor.aux_cfg.aux_rd_burst_len = 2;// burst read of 2 byte

/* Configure the BMM150 device structure by 
mapping user_aux_read and user_aux_write */
bmm150.read = user_aux_read;
bmm150.write = user_aux_write;
bmm150.id = BMM150_DEFAULT_I2C_ADDRESS; 
/* Ensure that sensor.aux_cfg.aux_i2c_addr = bmm150.id
   for proper sensor operation */
bmm150.delay_ms = delay_ms;
bmm150.interface = BMM150_I2C_INTF;

/* Initialize the auxiliary sensor interface */
rslt = bmi160_aux_init(&sensor);

/* Auxiliary sensor is enabled and can be accessed from this point */

/* Configure the desired settings in auxiliary BMM150 sensor 
 * using the bmm150 APIs */

/* Initialising the bmm150 sensor */
rslt = bmm150_init(&bmm150);

/* Set the power mode and preset mode to enable Mag data sampling */
bmm150.settings.pwr_mode = BMM150_NORMAL_MODE;
rslt = bmm150_set_op_mode(&bmm150);

bmm150.settings.preset_mode= BMM150_PRESETMODE_LOWPOWER;
rslt = bmm150_set_presetmode(&bmm150);

```
### Wrapper functions
```

/*wrapper function to match the signature of bmm150.read */
int8_t user_aux_read(uint8_t id, uint8_t reg_addr, uint8_t *aux_data, uint16_t len)
{
	int8_t rslt;
	
	/* Discarding the parameter id as it is redundant*/
        rslt = bmi160_aux_read(reg_addr, aux_data, len, &bmi160);

	return rslt;
}

/*wrapper function to match the signature of bmm150.write */
int8_t user_aux_write(uint8_t id, uint8_t reg_addr, uint8_t *aux_data, uint16_t len)
{
	int8_t rslt;
	
	/* Discarding the parameter id as it is redundant */
	rslt = bmi160_aux_write(reg_addr, aux_data, len, &bmi160);

	return rslt;
}

```

### Initialization of auxiliary BMM150 in auto mode
Any sensor whose data bytes are less than or equal to 8 bytes can be synchronized with the BMI160 
and read out of Accelerometer + Gyroscope + Auxiliary sensor data of that instance is possible
which helps in creating less latency fusion data

```
/* Initialize the Auxiliary BMM150 following the above code 
 * until setting the power mode (Set the power mode as forced mode)
 * and preset mode */

	/* In BMM150 Mag data starts from register address 0x42 */
	uint8_t aux_addr = 0x42;
	/* Buffer to store the Mag data from 0x42 to 0x48 */	
	uint8_t mag_data[8] = {0};
	
	uint8_t index;
		
	/* Configure the Auxiliary sensor either in auto/manual modes and set the 
	polling frequency for the Auxiliary interface */	
	sensor.aux_cfg.aux_odr = 8; /* Represents polling rate in 100 Hz*/
	rslt = bmi160_config_aux_mode(&sensor)
	
	/* Set the auxiliary sensor to auto mode */
	rslt = bmi160_set_aux_auto_mode(&aux_addr, &sensor);

	/* Reading data from BMI160 data registers */
	rslt = bmi160_read_aux_data_auto_mode(mag_data, &sensor);

	printf("\n RAW DATA ");
	for(index = 0 ; index < 8 ; index++)
	{
		printf("\n MAG DATA[%d] : %d ", index, mag_data[index]);
	}
	
	/* Compensating the raw mag data available from the BMM150 API */
	rslt = bmm150_aux_mag_data(mag_data, &bmm150);
	
	printf("\n COMPENSATED DATA ");
	printf("\n MAG DATA X : %d Y : %d Z : %d", bmm150.data.x, bmm150.data.y, bmm150.data.z);
	

```

### Auxiliary FIFO data parsing
The Auxiliary sensor data can be stored in FIFO , Here we demonstrate an example for 
using the Bosch Magnetometer sensor BMM150 and storing its data in FIFO

```
/* Initialize the Aux BMM150 following the above 
 * code and by creating the Wrapper functions */

	int8_t rslt = 0;
	uint8_t aux_instance = 0;
	uint16_t fifo_cnt = 0;
	uint8_t auto_mode_addr;
	uint8_t i;

	/* Setup and configure the FIFO buffer */
	/* Declare memory to store the raw FIFO buffer information */
	uint8_t fifo_buff[1000] = {0};

	/* Modify the FIFO buffer instance and link to the device instance */
	struct bmi160_fifo_frame fifo_frame;
	fifo_frame.data = fifo_buff;
	fifo_frame.length = 1000;
	dev->fifo = &fifo_frame;

	/* Declare instances of the sensor data structure to store the parsed FIFO data */
	struct bmi160_aux_data aux_data[112]; //1000 / 9 bytes per frame ~ 111 data frames

	rslt = bmi160_init(dev);
	printf("\n BMI160 chip ID is : %d ",dev->chip_id);

	rslt = bmi160_aux_init(dev);

	rslt = bmm150_init(&bmm150);
	printf("\n BMM150 CHIP ID : %d",bmm150.chip_id);

	bmm150.settings.preset_mode = BMM150_PRESETMODE_LOWPOWER;
	rslt = bmm150_set_presetmode(&bmm150);

	bmm150.settings.pwr_mode = BMM150_FORCED_MODE;
	rslt = bmm150_set_op_mode(&bmm150);

	/* Enter the data register of BMM150 to "auto_mode_addr" here it is 0x42 */
	auto_mode_addr = 0x42;
	printf("\n ENTERING AUX. AUTO MODE ");
	dev->aux_cfg.aux_odr = BMI160_AUX_ODR_25HZ;
	rslt = bmi160_set_aux_auto_mode(&auto_mode_addr, dev);


	/* Disable other FIFO settings */
	rslt = bmi160_set_fifo_config(BMI160_FIFO_CONFIG_1_MASK , BMI160_DISABLE, dev);

	/* Enable the required FIFO settings */
	rslt = bmi160_set_fifo_config(BMI160_FIFO_AUX | BMI160_FIFO_HEADER, BMI160_ENABLE, dev);

	/* Delay for the FIFO to get filled */
	dev->delay_ms(400);


	printf("\n FIFO DATA REQUESTED (in bytes): %d",dev->fifo->length);
	rslt = bmi160_get_fifo_data(dev);
	printf("\n FIFO DATA AVAILABLE (in bytes): %d",dev->fifo->length);

	/* Print the raw FIFO data obtained */
	for(fifo_cnt = 0; fifo_cnt < dev->fifo->length ; fifo_cnt++) {
		printf("\n FIFO DATA [%d] IS : %x  ",fifo_cnt ,dev->fifo->data[fifo_cnt]);
	}

	printf("\n\n----------------------------------------------------\n");

	/* Set the number of required sensor data instances */
	aux_instance = 150;

	/* Extract the aux data , 1frame = 8 data bytes */
	printf("\n AUX DATA REQUESTED TO BE EXTRACTED (in frames): %d",aux_instance);
	rslt = bmi160_extract_aux(aux_data, &aux_instance, dev);
	printf("\n AUX DATA ACTUALLY EXTRACTED (in frames): %d",aux_instance);

	/* Printing the raw aux data */
	for (i = 0; i < aux_instance; i++) {
		printf("\n Aux data[%d] : %x",i,aux_data[i].data[0]);
		printf("\n Aux data[%d] : %x",i,aux_data[i].data[1]);
		printf("\n Aux data[%d] : %x",i,aux_data[i].data[2]);
		printf("\n Aux data[%d] : %x",i,aux_data[i].data[3]);
		printf("\n Aux data[%d] : %x",i,aux_data[i].data[4]);
		printf("\n Aux data[%d] : %x",i,aux_data[i].data[5]);
		printf("\n Aux data[%d] : %x",i,aux_data[i].data[6]);
		printf("\n Aux data[%d] : %x",i,aux_data[i].data[7]);
	}

	printf("\n\n----------------------------------------------------\n");

	/* Compensate the raw mag data using BMM150 API */
	for (i = 0; i < aux_instance; i++) {
		printf("\n----------------------------------------------------");
		printf("\n Aux data[%d] : %x , %x , %x , %x , %x , %x , %x , %x",i
					,aux_data[i].data[0],aux_data[i].data[1]
					,aux_data[i].data[2],aux_data[i].data[3]
					,aux_data[i].data[4],aux_data[i].data[5]
					,aux_data[i].data[6],aux_data[i].data[7]);

		/* Compensated mag data using BMM150 API */
		rslt = bmm150_aux_mag_data(&aux_data[i].data[0], &bmm150);

		/* Printing the  Compensated mag data */
		if (rslt == BMM150_OK) {
			printf("\n MAG DATA COMPENSATION USING BMM150 APIs");
			printf("\n COMPENSATED DATA ");
			printf("\n MAG DATA X : %d	Y : %d      Z : %d"
				, bmm150.data.x, bmm150.data.y, bmm150.data.z);

		} else {
			printf("\n MAG DATA COMPENSATION IN BMM150 API is FAILED ");
		}
		printf("\n----------------------------------------------------\n");
	}

```

## Self-test  
#### Example for performing accel self test
```
/* Call the "bmi160_init" API as a prerequisite before performing self test
 * since invoking self-test will reset the sensor */

	rslt = bmi160_perform_self_test(BMI160_ACCEL_ONLY, sen);
	/* Utilize the enum BMI160_GYRO_ONLY instead of BMI160_ACCEL_ONLY
	   to perform self test for gyro */
	if (rslt == BMI160_OK) {
		printf("\n ACCEL SELF TEST RESULT SUCCESS);
	} else {
		printf("\n ACCEL SELF TEST RESULT FAIL);
	}
```


## FIFO 
#### Example for reading FIFO and extracting Gyro data in Header mode
```
/* An example to read the Gyro data in header mode along with sensor time (if available)
 * Configure the gyro sensor as prerequisite and follow the below example to read and
 * obtain the gyro data from FIFO */
int8_t fifo_gyro_header_time_data(struct bmi160_dev *dev)
{
	int8_t rslt = 0;

	/* Declare memory to store the raw FIFO buffer information */
	uint8_t fifo_buff[300];
	
	/* Modify the FIFO buffer instance and link to the device instance */
	struct bmi160_fifo_frame fifo_frame;
	fifo_frame.data = fifo_buff;
	fifo_frame.length = 300;
	dev->fifo = &fifo_frame;
	uint16_t index = 0;
	
	/* Declare instances of the sensor data structure to store the parsed FIFO data */
	struct bmi160_sensor_data gyro_data[42]; // 300 bytes / ~7bytes per frame ~ 42 data frames
	uint8_t gyro_frames_req = 42; 
	uint8_t gyro_index;
	
	/* Configure the sensor's FIFO settings */
	rslt = bmi160_set_fifo_config(BMI160_FIFO_GYRO | BMI160_FIFO_HEADER | BMI160_FIFO_TIME,
					BMI160_ENABLE, dev);
					
	if (rslt == BMI160_OK) {
		/* At ODR of 100 Hz ,1 frame gets updated in 1/100 = 0.01s
		i.e. for 42 frames we need 42 * 0.01 = 0.42s = 420ms delay */
		dev->delay_ms(420); 
	
		/* Read data from the sensor's FIFO and store it the FIFO buffer,"fifo_buff" */
		printf("\n USER REQUESTED FIFO LENGTH : %d\n",dev->fifo->length);
		rslt = bmi160_get_fifo_data(dev);

		if (rslt == BMI160_OK) {
			printf("\n AVAILABLE FIFO LENGTH : %d\n",dev->fifo->length);
			/* Print the raw FIFO data */
			for (index = 0; index < dev->fifo->length; index++) {
				printf("\n FIFO DATA INDEX[%d] = %d", index,
					dev->fifo->data[index]);
			}
			/* Parse the FIFO data to extract gyro data from the FIFO buffer */
			printf("\n REQUESTED GYRO DATA FRAMES : %d\n ",gyro_frames_req);
			rslt = bmi160_extract_gyro(gyro_data, &gyro_frames_req, dev);

			if (rslt == BMI160_OK) {
				printf("\n AVAILABLE GYRO DATA FRAMES : %d\n ",gyro_frames_req);
				
				/* Print the parsed gyro data from the FIFO buffer */
				for (gyro_index = 0; gyro_index < gyro_frames_req; gyro_index++) {
					printf("\nFIFO GYRO FRAME[%d]",gyro_index);
					printf("\nGYRO X-DATA : %d \t Y-DATA : %d \t Z-DATA : %d"
						,gyro_data[gyro_index].x ,gyro_data[gyro_index].y
						,gyro_data[gyro_index].z);
				}
				/* Print the special FIFO frame data like sensortime */
				printf("\n SENSOR TIME DATA : %d \n",dev->fifo->sensor_time);
				printf("SKIPPED FRAME COUNT : %d",dev->fifo->skipped_frame_count);
			} else {
				printf("\n Gyro data extraction failed");
			}
		} else {
			printf("\n Reading FIFO data failed");
		}
	} else {
		printf("\n Setting FIFO configuration failed");
	}

	return rslt;
}
```

## FOC and offset compensation
> FOC shouldnot be used in Low-power mode
#### Example for configuring FOC for accel and gyro
```
/* An example for configuring FOC for accel and gyro data */
int8_t start_foc(struct bmi160_dev *dev)
{
	int8_t rslt = 0;
	/* FOC configuration structure */
	struct bmi160_foc_conf foc_conf;
	/* Structure to store the offsets */
	struct bmi160_offsets offsets;
	
	/* Enable FOC for accel with target values of z = 1g ; x,y as 0g */
	foc_conf.acc_off_en = BMI160_ENABLE;
	foc_conf.foc_acc_x  = BMI160_FOC_ACCEL_0G;
	foc_conf.foc_acc_y  = BMI160_FOC_ACCEL_0G;
	foc_conf.foc_acc_z  = BMI160_FOC_ACCEL_POSITIVE_G;
	
	/* Enable FOC for gyro */
	foc_conf.foc_gyr_en = BMI160_ENABLE;
	foc_conf.gyro_off_en = BMI160_ENABLE;

	rslt = bmi160_start_foc(&foc_conf, &offsets, sen);
	
	if (rslt == BMI160_OK) {
		printf("\n FOC DONE SUCCESSFULLY ");
		printf("\n OFFSET VALUES AFTER FOC : ");
		printf("\n OFFSET VALUES ACCEL X : %d ",offsets.off_acc_x);
		printf("\n OFFSET VALUES ACCEL Y : %d ",offsets.off_acc_y);
		printf("\n OFFSET VALUES ACCEL Z : %d ",offsets.off_acc_z);
		printf("\n OFFSET VALUES GYRO  X : %d ",offsets.off_gyro_x);
		printf("\n OFFSET VALUES GYRO  Y : %d ",offsets.off_gyro_y);
		printf("\n OFFSET VALUES GYRO  Z : %d ",offsets.off_gyro_z);	
	}
	
	/* After start of FOC offsets will be updated automatically and 
	 * the data will be very much close to the target values of measurement */

	return rslt;
}
```

#### Example for updating the offsets manually
> The offsets set by this method will be reset on soft-reset/POR 
```
/* An example for updating manual offsets to sensor */
int8_t write_offsets(struct bmi160_dev *dev)
{
	int8_t rslt = 0;
	/* FOC configuration structure */
	struct bmi160_foc_conf foc_conf;
	/* Structure to store the offsets */
	struct bmi160_offsets offsets;
	
	/* Enable offset update for accel */
	foc_conf.acc_off_en = BMI160_ENABLE;

	/* Enable offset update for gyro */
	foc_conf.gyro_off_en = BMI160_ENABLE;
	
	/* offset values set by user */
	offsets.off_acc_x = 0x10;
	offsets.off_acc_y = 0x10;
	offsets.off_acc_z = 0x10;
	offsets.off_gyro_x = 0x10;
	offsets.off_gyro_y = 0x10;
	offsets.off_gyro_z = 0x10;

	rslt = bmi160_set_offsets(&foc_conf, &offsets, sen);
	
	/* After offset setting the data read from the 
	 * sensor will have the corresponding offset */
	
	return rslt;
}
```

#### Example for updating the offsets into NVM
> The offsets set by this method will be present in NVM and will be 
> restored on POR/soft-reset
```
/* An example for updating manual offsets to sensor */
int8_t write_offsets_nvm(struct bmi160_dev *dev)
{
	int8_t rslt = 0;
	/* FOC configuration structure */
	struct bmi160_foc_conf foc_conf;
	/* Structure to store the offsets */
	struct bmi160_offsets offsets;
	
	/* Enable offset update for accel */
	foc_conf.acc_off_en = BMI160_ENABLE;

	/* Enable offset update for gyro */
	foc_conf.gyro_off_en = BMI160_ENABLE;
	
	/* offset values set by user as per their reference 
	 * Resolution of accel = 3.9mg/LSB 
	 * Resolution of gyro  = (0.061degrees/second)/LSB */
	offsets.off_acc_x = 10;
	offsets.off_acc_y = -15;
	offsets.off_acc_z = 20;
	offsets.off_gyro_x = 30;
	offsets.off_gyro_y = -35;
	offsets.off_gyro_z = -40;

	rslt = bmi160_set_offsets(&foc_conf, &offsets, sen);
	 
	if (rslt == BMI160_OK) {
		/* Update the NVM */
		rslt = bmi160_update_nvm(dev);
	}
	
	/* After this procedure the offsets are written to 
	 * NVM and restored on POR/soft-reset 
	 * The set values can be removed to ideal case by 
	 * invoking the following APIs
	 *     - bmi160_start_foc()	 
	 *     - bmi160_update_nvm()
	 */

	return rslt;
}
```



## Copyright (C) 2019 Bosch Sensortec GmbH