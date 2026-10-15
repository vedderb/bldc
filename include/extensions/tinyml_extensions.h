/*
  Copyright 2026 Joel Svensson              svenssonjoel@yahoo.se

  This file is part of LispBM.

  LispBM is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LispBM is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef TINYML_EXTENSIONS_H_
#define TINYML_EXTENSIONS_H_

#include <stdint.h>
#include <stdbool.h>

#include "lispbm.h"

#ifdef __cplusplus
extern "C" {
#endif

#define TINYML_MAX_SHAPE_DIMS 4

// Tensor element type
typedef enum {
  TINYML_I8,
  TINYML_U8,
  TINYML_I16,
  TINYML_F32,
} tinyml_dtype_t;

// Tensor information provided per tensor or module designer
typedef struct {
  tinyml_dtype_t dtype;
  uint8_t        ndim;
  uint16_t       shape[TINYML_MAX_SHAPE_DIMS];
  float          scale;
  int32_t        zero_point;
} tinyml_tensor_info_t;

typedef struct tinyml_model {
  const char                  *name;
  uint8_t                      n_inputs;
  const tinyml_tensor_info_t  *inputs;
  uint8_t                      n_outputs;
  const tinyml_tensor_info_t  *outputs;
  uint32_t                     arena_bytes;

  int (*init)(const struct tinyml_model *model, void *arena);

  int (*run)(const struct tinyml_model *model,
             const void *const *in,
             void *const *out);

  void *ctx;
} tinyml_model_t;

bool lbm_tinyml_register(const tinyml_model_t *model);

void lbm_tinyml_extensions_init(void);

#ifdef __cplusplus
}
#endif
#endif
