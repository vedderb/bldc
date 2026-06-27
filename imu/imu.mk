IMUSRC = 	imu/mpu9150.c \
			imu/icm20948.c \
			imu/ahrs.c \
			imu/imu.c \
			imu/BMI160_driver/bmi160.c \
			imu/bmi160_wrapper.c \
			imu/lsm6ds3.c \
			imu/transport_i2c_bb.c \
			imu/transport_spi_bb.c \
			imu/transport_spi_hw.c \
			imu/imu_thread.c \
			imu/Fusion/FusionAhrs.c \
			imu/Fusion/FusionBias.c \
			imu/Fusion/FusionCompass.c

IMUINC = 	imu \
			imu/BMI160_driver \
			imu/Fusion
