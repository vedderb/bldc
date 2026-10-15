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

#include <extensions/tinyml_extensions.h>
#include <string.h>

#define TINYML_INSTANCE_MAGIC ((uint32_t)0x544D4C00) // "TML\0"
#define TINYML_MAX_TENSORS 8

typedef struct tinyml_model_entry {
  const tinyml_model_t      *model;
  struct tinyml_model_entry *next;
} tinyml_model_entry_t;

static tinyml_model_entry_t *models = NULL;

bool lbm_tinyml_register(const tinyml_model_t *model) {
  if (!model || !model->name) return false;
  tinyml_model_entry_t *e = lbm_malloc(sizeof(tinyml_model_entry_t));
  if (!e) return false;
  e->model = model;
  e->next = models;
  models = e;
  return true;
}

static const tinyml_model_t *find_model(const char *name) {
  for (tinyml_model_entry_t *e = models; e; e = e->next) {
    if (strcmp(e->model->name, name) == 0) return e->model;
  }
  return NULL;
}

static lbm_uint dtype_size(tinyml_dtype_t dt) {
  switch (dt) {
  case TINYML_I8:  return 1;
  case TINYML_U8:  return 1;
  case TINYML_I16: return 2;
  case TINYML_F32: return 4;
  }
  return 0;
}

static lbm_uint tensor_bytes(const tinyml_tensor_info_t *t) {
  lbm_uint n = 1;
  for (uint8_t i = 0; i < t->ndim; i++) n *= t->shape[i];
  return n * dtype_size(t->dtype);
}

// Symbols used by tinyml-info's alist output.
static lbm_uint symbol_f32 = 0;
static lbm_uint symbol_i8 = 0;
static lbm_uint symbol_u8 = 0;
static lbm_uint symbol_i16 = 0;
static lbm_uint symbol_name = 0;
static lbm_uint symbol_n_inputs = 0;
static lbm_uint symbol_n_outputs = 0;
static lbm_uint symbol_inputs = 0;
static lbm_uint symbol_outputs = 0;
static lbm_uint symbol_dtype = 0;
static lbm_uint symbol_shape = 0;
static lbm_uint symbol_scale = 0;
static lbm_uint symbol_zero_point = 0;

static lbm_value dtype_to_sym(tinyml_dtype_t dt) {
  switch (dt) {
  case TINYML_I8:  return lbm_enc_sym(symbol_i8);
  case TINYML_U8:  return lbm_enc_sym(symbol_u8);
  case TINYML_I16: return lbm_enc_sym(symbol_i16);
  case TINYML_F32: return lbm_enc_sym(symbol_f32);
  }
  return ENC_SYM_NIL;
}

static lbm_value encode_string(const char *s) {
  size_t len = strlen(s) + 1;
  char *buf = lbm_malloc(len);
  if (!buf) return ENC_SYM_MERROR;
  memcpy(buf, s, len);
  lbm_value res;
  if (!lbm_lift_array(&res, buf, len)) {
    lbm_free(buf);
    return ENC_SYM_MERROR;
  }
  return res;
}

typedef struct {
  uint32_t              magic;
  const tinyml_model_t *model;
  uint8_t                arena[];
} tinyml_instance_blob_t;

static bool tinyml_instance_destructor(lbm_uint value) {
  lbm_free((void*)value);
  return true;
}

static tinyml_instance_blob_t *get_instance_blob(lbm_value v) {
  if (!lbm_is_custom(v)) return NULL;
  lbm_uint value = lbm_get_custom_value(v);
  if (!value) return NULL;
  tinyml_instance_blob_t *blob = (tinyml_instance_blob_t*)value;
  if (blob->magic != TINYML_INSTANCE_MAGIC) return NULL;
  return blob;
}

// (tinyml-models) -> list of registered model name strings
static lbm_value ext_tinyml_models(lbm_value *args, lbm_uint argn) {
  (void)args;
  if (argn != 0) return ENC_SYM_TERROR;
  lbm_value res = ENC_SYM_NIL;
  for (tinyml_model_entry_t *e = models; e; e = e->next) {
    lbm_value s = encode_string(e->model->name);
    if (lbm_is_symbol(s)) return s;
    res = lbm_cons(s, res);
  }
  return res;
}

// (tinyml-open "name") -> instance handle
static lbm_value ext_tinyml_open(lbm_value *args, lbm_uint argn) {
  if (argn != 1 || !lbm_is_array_r(args[0])) return ENC_SYM_TERROR;
  char *name = lbm_dec_str(args[0]);
  if (!name) return ENC_SYM_TERROR;

  const tinyml_model_t *model = find_model(name);
  if (!model) return ENC_SYM_EERROR;

  lbm_uint size = sizeof(tinyml_instance_blob_t) + model->arena_bytes;
  uint8_t *buf = lbm_malloc(size);
  if (!buf) return ENC_SYM_MERROR;

  tinyml_instance_blob_t *blob = (tinyml_instance_blob_t*)buf;
  blob->magic = TINYML_INSTANCE_MAGIC;
  blob->model = model;

  if (model->init &&
      model->init(model, model->arena_bytes ? blob->arena : NULL) != 0) {
    lbm_free(buf);
    return ENC_SYM_EERROR;
  }

  lbm_value res;
  if (!lbm_custom_type_create((lbm_uint)buf, tinyml_instance_destructor,
                               "tinyml-instance", &res)) {
    lbm_free(buf);
    return ENC_SYM_MERROR;
  }
  return res;
}

static lbm_value ext_tinyml_is_instance(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  return get_instance_blob(args[0]) ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

// (tinyml-input-bytes h i) / (tinyml-output-bytes h i)
static lbm_value ext_tinyml_input_bytes(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  tinyml_instance_blob_t *blob = get_instance_blob(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  int32_t i = lbm_dec_as_i32(args[1]);
  if (i < 0 || i >= blob->model->n_inputs) return ENC_SYM_TERROR;
  return lbm_enc_i((lbm_int)tensor_bytes(&blob->model->inputs[i]));
}

static lbm_value ext_tinyml_output_bytes(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  tinyml_instance_blob_t *blob = get_instance_blob(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  int32_t i = lbm_dec_as_i32(args[1]);
  if (i < 0 || i >= blob->model->n_outputs) return ENC_SYM_TERROR;
  return lbm_enc_i((lbm_int)tensor_bytes(&blob->model->outputs[i]));
}

// //////////////////////////////////////////////////
// Info

static lbm_value encode_shape(const tinyml_tensor_info_t *t) {
  lbm_value res = ENC_SYM_NIL;
  for (uint8_t i = t->ndim; i > 0; i--) {
    res = lbm_cons(lbm_enc_i((lbm_int)t->shape[i - 1]), res);
  }
  return res;
}

static lbm_value encode_tensor_info(const tinyml_tensor_info_t *t) {
  lbm_value res = ENC_SYM_NIL;
  res = lbm_cons(lbm_cons(lbm_enc_sym(symbol_zero_point), lbm_enc_i(t->zero_point)), res);
  res = lbm_cons(lbm_cons(lbm_enc_sym(symbol_scale), lbm_enc_float(t->scale)), res);
  res = lbm_cons(lbm_cons(lbm_enc_sym(symbol_shape), encode_shape(t)), res);
  res = lbm_cons(lbm_cons(lbm_enc_sym(symbol_dtype), dtype_to_sym(t->dtype)), res);
  return res;
}

static lbm_value encode_tensor_info_list(const tinyml_tensor_info_t *arr, uint8_t n) {
  lbm_value res = ENC_SYM_NIL;
  for (uint8_t i = n; i > 0; i--) {
    res = lbm_cons(encode_tensor_info(&arr[i - 1]), res);
  }
  return res;
}

// (tinyml-info h) -> alist describing the model behind an open instance
static lbm_value ext_tinyml_info(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  tinyml_instance_blob_t *blob = get_instance_blob(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  const tinyml_model_t *model = blob->model;

  lbm_value res = ENC_SYM_NIL;
  res = lbm_cons(lbm_cons(lbm_enc_sym(symbol_outputs),
                          encode_tensor_info_list(model->outputs, model->n_outputs)), res);
  res = lbm_cons(lbm_cons(lbm_enc_sym(symbol_inputs),
                          encode_tensor_info_list(model->inputs, model->n_inputs)), res);
  res = lbm_cons(lbm_cons(lbm_enc_sym(symbol_n_outputs), lbm_enc_i(model->n_outputs)), res);
  res = lbm_cons(lbm_cons(lbm_enc_sym(symbol_n_inputs), lbm_enc_i(model->n_inputs)), res);
  res = lbm_cons(lbm_cons(lbm_enc_sym(symbol_name), encode_string(model->name)), res);
  return res;
}

// (tinyml-input-type h i) / (tinyml-output-type h i) -> one tensor's info alist
static lbm_value ext_tinyml_input_type(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  tinyml_instance_blob_t *blob = get_instance_blob(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  int32_t i = lbm_dec_as_i32(args[1]);
  if (i < 0 || i >= blob->model->n_inputs) return ENC_SYM_TERROR;
  return encode_tensor_info(&blob->model->inputs[i]);
}

static lbm_value ext_tinyml_output_type(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  tinyml_instance_blob_t *blob = get_instance_blob(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  int32_t i = lbm_dec_as_i32(args[1]);
  if (i < 0 || i >= blob->model->n_outputs) return ENC_SYM_TERROR;
  return encode_tensor_info(&blob->model->outputs[i]);
}

// (tinyml-n-inputs h) / (tinyml-n-outputs h) -> number of input/output tensors
static lbm_value ext_tinyml_n_inputs(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  tinyml_instance_blob_t *blob = get_instance_blob(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  return lbm_enc_i(blob->model->n_inputs);
}

static lbm_value ext_tinyml_n_outputs(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  tinyml_instance_blob_t *blob = get_instance_blob(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  return lbm_enc_i(blob->model->n_outputs);
}

static bool collect_buffers(lbm_value buffers, uint8_t n_expected,
                             const tinyml_tensor_info_t *infos,
                             void **out_ptrs) {
  if (n_expected == 0) return true;
  if (n_expected == 1 && lbm_is_array_r(buffers)) {
    lbm_array_header_t *arr = lbm_dec_array_r(buffers);
    if (!arr || arr->size != tensor_bytes(&infos[0])) return false;
    out_ptrs[0] = arr->data;
    return true;
  }
  if (!lbm_is_list(buffers) || lbm_list_length(buffers) != n_expected) return false;
  lbm_value curr = buffers;
  for (uint8_t i = 0; i < n_expected; i++) {
    lbm_value elt = lbm_car(curr);
    lbm_array_header_t *arr = lbm_dec_array_r(elt);
    if (!arr || arr->size != tensor_bytes(&infos[i])) return false;
    out_ptrs[i] = arr->data;
    curr = lbm_cdr(curr);
  }
  return true;
}

// (tinyml-run h in out) -> t or an error symbol
static lbm_value ext_tinyml_run(lbm_value *args, lbm_uint argn) {
  if (argn != 3) return ENC_SYM_TERROR;
  tinyml_instance_blob_t *blob = get_instance_blob(args[0]);
  if (!blob) return ENC_SYM_TERROR;

  const tinyml_model_t *model = blob->model;
  if (model->n_inputs > TINYML_MAX_TENSORS || model->n_outputs > TINYML_MAX_TENSORS) {
    return ENC_SYM_EERROR;
  }

  void *in_ptrs[TINYML_MAX_TENSORS];
  void *out_ptrs[TINYML_MAX_TENSORS];
  if (!collect_buffers(args[1], model->n_inputs, model->inputs, in_ptrs)) {
    return ENC_SYM_TERROR;
  }
  if (!collect_buffers(args[2], model->n_outputs, model->outputs, out_ptrs)) {
    return ENC_SYM_TERROR;
  }

  int rc = model->run(model, (const void *const *)in_ptrs, out_ptrs);
  return rc == 0 ? ENC_SYM_TRUE : ENC_SYM_EERROR;
}

void lbm_tinyml_extensions_init(void) {
  lbm_add_symbol_const("f32", &symbol_f32);
  lbm_add_symbol_const("i8", &symbol_i8);
  lbm_add_symbol_const("u8", &symbol_u8);
  lbm_add_symbol_const("i16", &symbol_i16);
  lbm_add_symbol_const("name", &symbol_name);
  lbm_add_symbol_const("n-inputs", &symbol_n_inputs);
  lbm_add_symbol_const("n-outputs", &symbol_n_outputs);
  lbm_add_symbol_const("inputs", &symbol_inputs);
  lbm_add_symbol_const("outputs", &symbol_outputs);
  lbm_add_symbol_const("dtype", &symbol_dtype);
  lbm_add_symbol_const("shape", &symbol_shape);
  lbm_add_symbol_const("scale", &symbol_scale);
  lbm_add_symbol_const("zero-point", &symbol_zero_point);

  lbm_add_extension("tinyml-models", ext_tinyml_models);
  lbm_add_extension("tinyml-open", ext_tinyml_open);
  lbm_add_extension("tinyml-instance?", ext_tinyml_is_instance);
  lbm_add_extension("tinyml-input-bytes", ext_tinyml_input_bytes);
  lbm_add_extension("tinyml-output-bytes", ext_tinyml_output_bytes);
  lbm_add_extension("tinyml-input-type", ext_tinyml_input_type);
  lbm_add_extension("tinyml-output-type", ext_tinyml_output_type);
  lbm_add_extension("tinyml-n-inputs", ext_tinyml_n_inputs);
  lbm_add_extension("tinyml-n-outputs", ext_tinyml_n_outputs);
  lbm_add_extension("tinyml-info", ext_tinyml_info);
  lbm_add_extension("tinyml-run", ext_tinyml_run);
}
