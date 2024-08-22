/*
	Copyright 2020 Benjamin Vedder	benjamin@vedder.se

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

#include "mempools.h"
#include "packet.h"

// Private types
typedef struct {
	volatile bool is_taken;
	mc_configuration conf;
} mcconf_container_t;

typedef struct {
	volatile bool is_taken;
	app_configuration conf;
} appconf_container_t;

// Private variables
static mcconf_container_t m_mc_confs[MEMPOOLS_MCCONF_NUM] = {{0}};
static appconf_container_t m_app_confs[MEMPOOLS_APPCONF_NUM] = {{0}};
static int m_mcconf_highest = 0;
static int m_appconf_highest = 0;

static uint8_t packet_buffer[PACKET_MAX_PL_LEN];
static mutex_t packet_buffer_mutex;

void mempools_init(void) {
	chMtxObjectInit(&packet_buffer_mutex);
}

mc_configuration *mempools_alloc_mcconf(void) {
	for (int i = 0;i < MEMPOOLS_MCCONF_NUM;i++) {
		if (i > m_mcconf_highest) {
			m_mcconf_highest = i;
		}
		if (!m_mc_confs[i].is_taken) {
			m_mc_confs[i].is_taken = true;
			return &m_mc_confs[i].conf;
		}
	}

	m_mcconf_highest++;

	return 0;
}

void mempools_free_mcconf(mc_configuration *conf) {
	for (int i = 0;i < MEMPOOLS_MCCONF_NUM;i++) {
		if (&m_mc_confs[i].conf == conf) {
			m_mc_confs[i].is_taken = false;
			return;
		}
	}
}

app_configuration *mempools_alloc_appconf(void) {
	for (int i = 0;i < MEMPOOLS_APPCONF_NUM;i++) {
		if (i > m_appconf_highest) {
			m_appconf_highest = i;
		}
		if (!m_app_confs[i].is_taken) {
			m_app_confs[i].is_taken = true;
			return &m_app_confs[i].conf;
		}
	}

	m_appconf_highest++;

	return 0;
}

void mempools_free_appconf(app_configuration *conf) {
	for (int i = 0;i < MEMPOOLS_APPCONF_NUM;i++) {
		if (&m_app_confs[i].conf == conf) {
			m_app_confs[i].is_taken = false;
			return;
		}
	}
}

int mempools_mcconf_highest(void) {
	return m_mcconf_highest;
}

int mempools_appconf_highest(void) {
	return m_appconf_highest;
}

int mempools_mcconf_allocated_num(void) {
	int res = 0;
	for (int i = 0;i < MEMPOOLS_MCCONF_NUM;i++) {
		if (m_mc_confs[i].is_taken) {
			res++;
		}
	}
	return res;
}

int mempools_appconf_allocated_num(void) {
	int res = 0;
	for (int i = 0;i < MEMPOOLS_APPCONF_NUM;i++) {
		if (m_app_confs[i].is_taken) {
			res++;
		}
	}
	return res;
}

uint8_t *mempools_get_packet_buffer(void) {
	chMtxLock(&packet_buffer_mutex);
	return packet_buffer;
}

void mempools_free_packet_buffer(uint8_t *buffer) {
	if (buffer == packet_buffer) {
		chMtxUnlock(&packet_buffer_mutex);
	}
}
