/*
	Copyright 2023 Benjamin Vedder	benjamin@vedder.se
	Copyright 2023 Joel Svensson     svenssonjoel@yahoo.se

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

#include "hwspi.h"
#include <string.h>

#include "soc/gpio_struct.h"
#include "driver/gpio.h"
#include "driver/spi_master.h"
#include "soc/gpio_reg.h"
#include "esp_heap_caps.h"

#if CONFIG_IDF_TARGET_ESP32S3
	#define SET_CS() 		(GPIO.out_w1ts = 1 << m_pin_cs)
	#define CLEAR_CS()		(GPIO.out_w1tc = 1 << m_pin_cs)
#elif CONFIG_IDF_TARGET_ESP32C3
	#define SET_CS() 		(GPIO.out_w1ts.val = 1 << m_pin_cs)
	#define CLEAR_CS()		(GPIO.out_w1tc.val = 1 << m_pin_cs)
#else
	#error "Unsupported target"
#endif

// Stream buffer for triple buffering
typedef struct data_stream_buffer_s {
	uint8_t *data;
	int pos;
	spi_transaction_t trans;
	struct data_stream_buffer_s *next;
} data_stream_buffer_t;

// Private variables
static spi_device_handle_t m_spi;
static int m_pin_cs = -1;
static data_stream_buffer_t m_data_buffers[HWSPI_BUFFERS];
static data_stream_buffer_t *m_active_buffer = 0;
static bool m_init_done = false;
static spi_bus_config_t m_buscfg = {0};
static spi_device_interface_config_t m_devcfg = {0};

// Global variables
uint8_t *hwspi_buffer_pointer = 0;
int *hwspi_buffer_pos = 0;

void hwspi_init(int clk_mhz, int mode,
		int pin_miso, int pin_mosi, int pin_clk, int pin_cs) {

	if (!m_init_done) {
		for (int i = 0;i < HWSPI_BUFFERS;i++) {
			m_data_buffers[i].data = heap_caps_malloc(HWSPI_DATA_BUFFER_SIZE, MALLOC_CAP_DMA);
		}
	}

	m_pin_cs = pin_cs;

	m_buscfg.miso_io_num = pin_miso;
	m_buscfg.mosi_io_num = pin_mosi;
	m_buscfg.sclk_io_num = pin_clk;
	m_buscfg.quadwp_io_num = -1;
	m_buscfg.quadhd_io_num = -1;
	m_buscfg.max_transfer_sz = 4092;

	m_devcfg.clock_speed_hz = clk_mhz * 1000 * 1000;
	m_devcfg.mode = mode;
	m_devcfg.spics_io_num = -1; // We handle CS manually
	m_devcfg.flags = 0;
	m_devcfg.queue_size = 1; // Must be 1, otherwise multiple buffers will be queued at the same time
	m_devcfg.pre_cb = NULL;

	gpio_config_t gpconf = {0};
	gpconf.pin_bit_mask = BIT(m_pin_cs);
	gpconf.mode = GPIO_MODE_OUTPUT;
	gpconf.pull_down_en = GPIO_PULLDOWN_DISABLE;
	gpconf.pull_up_en = GPIO_PULLUP_DISABLE;
	gpconf.intr_type =  GPIO_INTR_DISABLE;

	gpio_config(&gpconf);
	SET_CS();

	if (m_init_done) {
		static spi_transaction_t *tmp_ptr = 0;
		spi_device_get_trans_result(m_spi, &tmp_ptr, 0);
		spi_device_get_trans_result(m_spi, &tmp_ptr, 0);
		spi_bus_remove_device(m_spi);
		spi_bus_free(SPI2_HOST);
	}

	spi_bus_initialize(SPI2_HOST, &m_buscfg, SPI_DMA_CH_AUTO);
	spi_bus_add_device(SPI2_HOST, &m_devcfg, &m_spi);

	m_init_done = true;
}

void hwspi_begin(void) {
	spi_device_acquire_bus(m_spi, portMAX_DELAY);
	CLEAR_CS();
}

void hwspi_end(void) {
	SET_CS();
	spi_device_release_bus(m_spi);
}

void hwspi_swap_buffer(void) {
	m_active_buffer->trans.length = m_active_buffer->pos * 8;
	m_active_buffer->pos = 0;
	spi_device_queue_trans(m_spi, &m_active_buffer->trans, portMAX_DELAY);
	m_active_buffer = m_active_buffer->next;
	hwspi_buffer_pointer = m_active_buffer->data;
	hwspi_buffer_pos = &m_active_buffer->pos;
}

void hwspi_data_stream_start(void) {
	for (int i = 0;i < HWSPI_BUFFERS;i++) {
		memset(&m_data_buffers[i].trans, 0, sizeof(spi_transaction_t));
		m_data_buffers[i].pos = 0;
		m_data_buffers[i].trans.tx_buffer = m_data_buffers[i].data;
		m_data_buffers[i].trans.flags = 0;

		if (i == (HWSPI_BUFFERS - 1)) {
			m_data_buffers[i].next = &m_data_buffers[0];
		} else {
			m_data_buffers[i].next = &m_data_buffers[i + 1];
		}
	}

	m_active_buffer = &m_data_buffers[0];
	hwspi_buffer_pointer = m_active_buffer->data;
	hwspi_buffer_pos = &m_active_buffer->pos;
}

void hwspi_data_stream_finish(void) {
	if (m_active_buffer->pos > 0) {
		hwspi_send_data(m_active_buffer->data, m_active_buffer->pos);
	}
}

void hwspi_send_data(const uint8_t *data, int len) {
	if (len <= 0) return;
	spi_transaction_t t;
	memset(&t, 0, sizeof(t));
	t.length = len * 8;
	t.tx_buffer = data;
	t.flags = 0;
	spi_device_polling_transmit(m_spi, &t);
}
