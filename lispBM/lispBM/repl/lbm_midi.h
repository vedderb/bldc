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

#ifndef LBM_MIDI_H_
#define LBM_MIDI_H_

#include <stdbool.h>

// Initialize ALSA sequencer and connect to Q49 keyboard
bool lbm_midi_init(void);

// Cleanup ALSA sequencer
void lbm_midi_cleanup(void);

#endif
