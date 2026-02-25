/*
    Copyright 2025    Joel Svensson  svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef LBM_SOUND_H_
#define LBM_SOUND_H_

#define _POSIX_C_SOURCE 200809L
#include <alsa/asoundlib.h>
#include <stdbool.h>

// Initialize ALSA sound system and start sample generation thread
bool lbm_sound_init(void);

// Stop sample generation thread and cleanup ALSA sound system
void lbm_sound_cleanup(void);


// interface
snd_pcm_t *lbm_sound_pcm_handle(void);

#endif
