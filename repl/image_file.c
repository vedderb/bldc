/*
    Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

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

#include "image_file.h"
#include <string.h>
#include <stdbool.h>

int open_image(const char *filename, image_file_t *image_file) {
  memset(&image_file->header, 0, sizeof(image_file_header_t));
  image_file->image_fp = NULL;

  FILE *f = fopen(filename, "rb");
  if (!f) {
    return IMAGE_FILE_NOT_FOUND;
  }

  image_file_header_t header;
  if (fread(&header, sizeof(image_file_header_t), 1, f) != 1) {
    fclose(f);
    return IMAGE_FILE_INVALID;
  }

  if (header.magic != IMAGE_FILE_MAGIC || header.version != IMAGE_FILE_VERSION) {
    fclose(f);
    return IMAGE_FILE_INVALID;
  }

  image_file->header = header;
  image_file->image_fp = f;
  return IMAGE_FILE_OK;
}

int load_image(image_file_t *image_file, uint32_t *image_storage) {
  size_t total = (size_t)image_file->header.image_size +
                 (size_t)image_file->header.imports_high_water_mark;

  size_t n = fread((uint8_t*)image_storage, 1, total, image_file->image_fp);

  fclose(image_file->image_fp);
  image_file->image_fp = NULL;

  return (n == total) ? IMAGE_FILE_OK : IMAGE_FILE_INVALID;
}

int close_image(image_file_t *image_file) {
  if (image_file->image_fp) {
    fclose(image_file->image_fp);
    image_file->image_fp = NULL;
  }
  return IMAGE_FILE_OK;
}

int save_image(const char *filename,
               const uint32_t *image_storage,
               size_t image_size,
               size_t imports_high_water_mark) {
  if (image_size > UINT32_MAX || imports_high_water_mark > UINT32_MAX) {
    return IMAGE_FILE_INVALID;
  }

  image_file_header_t header;
  header.magic = IMAGE_FILE_MAGIC;
  header.version = IMAGE_FILE_VERSION;
  header.image_size = (uint32_t)image_size;
  header.imports_high_water_mark = (uint32_t)imports_high_water_mark;

  FILE *f = fopen(filename, "wb");
  if (!f) {
    return IMAGE_FILE_NOT_FOUND;
  }

  const uint8_t *data = (const uint8_t*)image_storage;

  bool ok = fwrite(&header, sizeof(header), 1, f) == 1;
  if (ok && image_size > 0) {
    ok = fwrite(data, 1, image_size, f) == image_size;
  }
  if (ok && imports_high_water_mark > 0) {
    ok = fwrite(data + image_size, 1, imports_high_water_mark, f) == imports_high_water_mark;
  }

  fclose(f);
  return ok ? IMAGE_FILE_OK : IMAGE_FILE_INVALID;
}
