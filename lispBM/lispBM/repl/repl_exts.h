/*
    Copyright 2024 Joel Svensson  svenssonjoel@yahoo.se

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

#ifndef REPL_EXTS_H_
#define REPL_EXTS_H_

#include "lispbm.h"
#include "extensions/array_extensions.h"
#include "extensions/string_extensions.h"
#include "extensions/math_extensions.h"
#include "extensions/runtime_extensions.h"


int init_exts(void);

bool dynamic_loader(const char *str, const char **code);
void set_allow_print(bool);

// Writes image_storage (image + imports) to --image_file, if --image_file
// was given at startup - calling this is itself the explicit signal to
// persist, so there's no separate --persist_image gate here (that flag
// instead gates the automatic per-word write-through in image_write()).
// A no-op success (true) if --image_file wasn't given - image-save's
// original in-memory-only behavior still works unchanged without it.
// Only returns false if a write was actually attempted and failed.
bool image_save_to_disk(void);

// Looks up path in the imports region; on a miss, appends it (name\0,
// length, data) and dedups on path for future calls. Shares back a const
// array either way.
lbm_value import_area_add(const char *path, const uint8_t *data, size_t size);
#endif
 
