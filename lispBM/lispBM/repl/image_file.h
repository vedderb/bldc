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

#ifndef _IMAGE_FILE_H_
#define _IMAGE_FILE_H_

#include <stdio.h>
#include <stddef.h>
#include <stdint.h>

#define IMAGE_FILE_MAGIC 0x4C424D49 // "LBMI"
#define IMAGE_FILE_VERSION 1

#define IMAGE_FILE_OK         0
#define IMAGE_FILE_NOT_FOUND  1
#define IMAGE_FILE_INVALID    2

typedef struct {
  uint32_t magic;
  uint32_t version;
  uint32_t image_size;
  uint32_t imports_high_water_mark;
} image_file_header_t;

typedef struct {
  image_file_header_t header;
  FILE *image_fp;
} image_file_t;

// After the header follows image_size bytes of image data
// to be copied to the image_storage as well as imports_high_water_mark
// bytes of imports data to be copied.
// A single read can be used to repopulate the image_storage.


// Open file and validate header.
int open_image(const char *filename, image_file_t *image_file);

// Load image file contants to address
int load_image(image_file_t *image_file, uint32_t *image_storage);

// Close image file.
int close_image(image_file_t *image_file);

// save image to file
int save_image(const char *filename,
               const uint32_t *image_storage,
               size_t image_size,
               size_t imports_high_water_mark);

#endif 
