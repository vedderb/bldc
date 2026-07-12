/*
	Copyright 2026 Lukas Hrazky

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

#include "transport_spi_hw.h"
#include <string.h>

// SPI read sets bit 7 of the register address, write clears it.
#define SPI_READ_BIT 0x80

#define SPI_DATASIZE_8BIT	0
#define SPI_MODE_0			0
#define SPI_MODE_1			SPI_CR1_CPHA
#define SPI_MODE_2			SPI_CR1_CPOL
#define SPI_MODE_3			(SPI_CR1_CPOL | SPI_CR1_CPHA)

// Default IMU SPI clock used when bus_hz is 0.
#define SPI_HW_DEFAULT_HZ	10500000

static SPIDriver *dev_of(transport_t *t) {
	return t->bus.spi_hw.spid;
}

static void spi_err_cb(SPIDriver *spip) {
	if (spip->app_arg) {
		*(volatile bool *)spip->app_arg = true;
	}
}

static bool read_reg(transport_t *t, uint8_t dev_addr, uint8_t reg, uint8_t *rx, size_t len) {
	(void)dev_addr;
	if (len > IMU_MAX_BURST) {
		return false;
	}
	SPIDriver *spid = dev_of(t);
	uint8_t *txd = t->bus.spi_hw.txd;
	uint8_t *rxd = t->bus.spi_hw.rxd;

	txd[0] = reg | SPI_READ_BIT;
	memset(txd + 1, 0, len);

	volatile bool err = false;
	spiAcquireBus(spid);
	spid->app_arg = (void *)&err;
	spiSelect(spid);
	// One full-duplex exchange (address byte + len bytes read back), not a separate
	// send-then-receive: a single bus acquisition and DMA setup per read instead of
	// two. The halved per-read overhead matters at the sampling burst rate.
	spiExchange(spid, 1 + len, txd, rxd);
	spiUnselect(spid);
	spid->app_arg = NULL;
	spiReleaseBus(spid);

	if (err) {
		return false;
	}
	memcpy(rx, rxd + 1, len);
	return true;
}

static bool write_reg(transport_t *t, uint8_t dev_addr, uint8_t reg, const uint8_t *tx, size_t len) {
	(void)dev_addr;
	if (len > IMU_MAX_BURST) {
		return false;
	}
	SPIDriver *spid = dev_of(t);
	uint8_t *txd = t->bus.spi_hw.txd;

	txd[0] = reg & ~SPI_READ_BIT;
	memcpy(txd + 1, tx, len);

	volatile bool err = false;
	spiAcquireBus(spid);
	spid->app_arg = (void *)&err;
	spiSelect(spid);
	spiSend(spid, 1 + len, txd);
	spiUnselect(spid);
	spid->app_arg = NULL;
	spiReleaseBus(spid);

	return !err;
}

static uint16_t max_sample_rate(transport_t *t) {
	(void)t;
	return 10000;
}

static const transport_interface_t spi_hw_interface = {
	.name = "spi-hw",
	.max_sample_rate = max_sample_rate,
	.read_reg = read_reg,
	.write_reg = write_reg,
	.recover = NULL,
	.deinit = NULL,
};

// CR1.BR field value (0-7) for the fastest prescaler whose SPI clock does not
// exceed bus_hz (0 = SPI_HW_DEFAULT_HZ). SPI clock = pclk / 2^(br + 1).
static uint8_t hz_to_cr1br(SPIDriver *spid, uint32_t bus_hz) {
	// SPI1 is clocked from PCLK2, every other SPI from PCLK1.
	uint32_t pclk = STM32_PCLK1;
#if STM32_SPI_USE_SPI1
	if (spid == &SPID1) {
		pclk = STM32_PCLK2;
	}
#else
	(void)spid;
#endif

	if (bus_hz == 0) {
		bus_hz = SPI_HW_DEFAULT_HZ;
	}

	for (uint8_t br = 0; br < 7; br++) {
		if ((pclk >> (br + 1)) <= bus_hz) {
			return br;
		}
	}

	return 7; // equals prescaler /256
}

void transport_spi_hw_init(transport_t *t, SPIDriver *spid, uint32_t af,
		stm32_gpio_t *nss_gpio, uint8_t nss_pin, stm32_gpio_t *sck_gpio, uint8_t sck_pin,
		stm32_gpio_t *mosi_gpio, uint8_t mosi_pin, stm32_gpio_t *miso_gpio, uint8_t miso_pin,
		uint32_t bus_hz) {
	t->interface = &spi_hw_interface;
	t->bus.spi_hw.spid = spid;

	palSetPadMode(nss_gpio, nss_pin,
			PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(sck_gpio, sck_pin,
			PAL_MODE_ALTERNATE(af) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(mosi_gpio, mosi_pin,
			PAL_MODE_ALTERNATE(af) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(miso_gpio, miso_pin,
			PAL_MODE_ALTERNATE(af) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);

	t->bus.spi_hw.cfg = (SPIConfig){
		NULL, nss_gpio, nss_pin,
		(uint16_t)((hz_to_cr1br(spid, bus_hz) << 3) | SPI_MODE_3 | SPI_DATASIZE_8BIT)
	};
	spid->err_cb = spi_err_cb;
	spiStart(spid, &t->bus.spi_hw.cfg);
}
