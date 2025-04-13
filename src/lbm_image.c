/*
    Copyright 2025 Joel Svensson  svenssonjoel@yahoo.se

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


#include <extensions.h>
#include <lbm_image.h>
#include <heap.h>
#include <env.h>
#include <lbm_flat_value.h>
#include <eval_cps.h>
#include <extensions.h>

// Assumptions about the image memory:
// * It is part of the address space.
// * Image is always available at the same address (across reboots)
// * It is a write-once memory.
// * Can be cleared in its entirety.
// * Can check on a byte-level is "is-writable" (has a known initial state when cleared)

// Details
// * @const_start @const_end is tricky.
// * Constant heap is needed because of small amount of RAM.
// * Arbitrary pointers will be tricky.

// Want to be able to build an image incrementally.
// Can @const_start @const_end be used in conjuction with a
// more exlicit image manipulation subsystem.

//TBD:
// * does an image contain running threads ? (I would prefer if no)
//   instead there is a startup-entry that represents some code that will
//   be executed after setting up an image.
//   This startup-entry can spawn threads and initialize resources.
// * Will we have a heap-image or will all bindings move into the const heap.

// FEB 26:
// -- There will be no "heap-image" or "memory-image"
//    Just flattened and stored bindings from the environment (which is as good but likely smaller).
// -- There will be an image of the const-heap. in fact the const heap lives in the image always.
//    A bit problematic with building on image incrementally as it is in flash and the contents cannot be changed.
//    Contents can be added though!  (keep track of const-heap write-ptr is an issue)
// -- Maybe we should implement image for a read-write memory and then make the flash situation a special case?
// -- Maybe size fields should always be in bytes.
// -- TODO: a flatten function that flattens a value directly to flash and also does not flatten things that are
//          already in flash, but rather then just refers to them.

// FEB 27:
// -- Symbol numbering will be an issue.
//    * Store the symboltable in the image and restore it on boot.
//    * Names already in flash can be refered to.
//    * Names in ram can be copied.
//    * Entire subtable may already be in flash - leave in place and refer to it.
// -- loading an image and then adding to it can be tricky to make possible.
//    * const-heap write pointer needs to be stored. (but if it is stored then it cannot be changed)
//      Could allow multiple "const-heap-write-pointer" fields in the image and use the one that appears last...
//    * Symboltable could be created incrementally in a similar way. Append later symbol_table data fields
//      to the previously loaded.

// FEB 28:
// -- Symbol numbering problem. The structure of the symboltable may
//    need to change. It is currently impossible to append a symbol list stored
//    in flash to the global symbol table. A richer structure is needed.
// -- The symbols in the SYMTAB can all be in flash (all in the image) and
//    all name strings can be written to flash as well.
//    * May be easiest if names go to the flash heap (as it is now)
//      and table entries are a tagged field in the image (1 extra byte per symbol...)
//    * Means the image must be initialized (to a degree) before symbols are created.

// MARCH 1:
// -- Symbols are added to and restored from the image.
// -- lbm_add_symbol_const, still creates a lbm_memory list structure.
//    Const symbols should also be stored into the image and add_symbol_const
//    should check and reuse stored symbol id.
//    Check order of initialization to see how easy this is to fix.

// Offline image tools
// - Image compaction: remove overwrite fields and compact the image.
// - Change of base address: relabel all memory accesses.
// - ...


// MARCH 5
// -- Can constants (anything on const heap) contain references into non-constant heap?
//    - I think not, but should verify this.
//    - eval_cps move_to_flash performs a deep copy into flash.

// Endianess woes...
// - little endian     least-significant byte at least address
// - big endian        most-significant byte at least address
// - all platforms we target currently are little-endian
//
//  0x11223344
//     | | | '--- [44] addr
//     | | '----- [33] addr + 1
//     | '------- [22] addr + 2
//     '--------- [11] addr + 3
//
// Images are going to be mainly little endian.  (what endianess does flatvalues use? I think BE)

// constant heap should be 4byte aligned so that there are 2 unused low end bits
// in all cell-pointers into constant heap.

// March 8
// -- flattening lead to duplication of shared nodes.
//    if a = '(1 2 3) and b = (cons 4 a) and c = (cons 5 a)
//    then the result of flattening a b c each contains a full copy of a.
// -- flattening a value that in turn points to a constant value, duplicates
//    the constant value.

//  TODO: Put more info into the IMAGE_INITIALIZED FIELD
//        - 32/64 bit  etc

#ifdef LBM64
#define IMAGE_INITIALIZED (uint32_t)0xBEEF4001    // [ 0xBEEF4001 ]
#else
#define IMAGE_INITIALIZED (uint32_t)0xBEEF2001    // [ 0xBEEF2001 ]
#endif
                                            // Address downwards ->
#define CONSTANT_HEAP_IX  (uint32_t)0x02    // [ 0x02 | uint32]
#define BINDING_CONST     (uint32_t)0x03    // [ 0x03 | key | lbm_uint ]
#define BINDING_FLAT      (uint32_t)0x04    // [ 0x04 | size | key | flatval ]
#define STARTUP_ENTRY     (uint32_t)0x05    // [ 0x05 | symbol ])
#define SYMBOL_ENTRY      (uint32_t)0x06    // [ 0x06 | NEXT_PTR |  ID | NAME PTR ] // symbol_entry with highest address is root.
#define SYMBOL_LINK_ENTRY (uint32_t)0x07    // [ 0x07 | C_LINK_PTR | NEXT_PTR | ID | NAME PTR ]
#define EXTENSION_TABLE   (uint32_t)0x08    // [ 0x08 | NUM | EXT ...]
#define VERSION_ENTRY     (uint32_t)0x09    // [ 0x09 | size | string ]
// Size is in number of 32bit words, even on 64 bit images.

// To be able to work on an image incrementally (even though it is not recommended)
// many fields are allowed to be duplicated and the later ones have priority
// over earlier ones.


#define DOWNWARDS true
#define UPWARDS   false

static lbm_image_write_fun image_write = NULL;

static uint32_t *image_address = NULL;
static int32_t write_index = 0;
static uint32_t image_size = 0;
static bool image_has_extensions = false;
static char* image_version = NULL;

uint32_t *lbm_image_get_image(void) {
  return image_address;
}

uint32_t lbm_image_get_size(void) {
  return image_size;
}

int32_t lbm_image_get_write_index(void) {
  return write_index;
}

bool lbm_image_has_extensions(void) {
  return image_has_extensions;
}

uint32_t read_u32(int32_t index) {
  return *((uint32_t*)(image_address + index));
}

uint64_t read_u64(int32_t index) {
  // image_addres is an u32 ptr. so addr + i is a step of i * 4 bytes
  return *((uint64_t*)(image_address + index));
}

bool write_u32(uint32_t w, int32_t *i, bool direction) {
  bool r = image_write(w, *i, false);
  (*i) += direction ? -1 : 1;
  return r;
}


bool write_u64(uint64_t dw, int32_t *i, bool direction) {
  uint32_t *words = (uint32_t*)&dw;

  // downwards   ... hw   lw
  //                 ix  ix-1
  // upwards     hw   lw ...
  //            ix+1  ix

  // true = downwards

  bool r = true;
  if (direction) {
    r = r && write_u32(words[1], i, direction);
    r = r && write_u32(words[0], i, direction);
  } else {
    r = r && write_u32(words[0], i, direction);
    r = r && write_u32(words[1], i, direction);
  }
  return r;
}

// fv_write function write values as big endian.

uint32_t fv_buf_ix = 0;
uint8_t  fv_buf[4] = {0};
bool fv_write_u8(uint8_t b) {
  bool r = true;
  if (fv_buf_ix >= 4) {
    r = write_u32(((uint32_t*)fv_buf)[0], &write_index, UPWARDS);
    memset(fv_buf,0,4);
    fv_buf_ix = 0;
  }
  fv_buf[fv_buf_ix] = b;
  fv_buf_ix++;
  return r;
}

bool fv_write_flush(void) {
  if (fv_buf_ix == 0) return true;
  else {
    bool r = write_u32(((uint32_t*)fv_buf)[0], &write_index, UPWARDS);;
    fv_buf_ix = 0;
    memset(fv_buf,0,4);
    return r;
  }
}

bool fv_write_u32(uint32_t w) {
  uint8_t * bytes = (uint8_t*)&w;
  return
    fv_write_u8(bytes[3]) &&
    fv_write_u8(bytes[2]) &&
    fv_write_u8(bytes[1]) &&
    fv_write_u8(bytes[0]);
}

bool fv_write_u64(uint64_t dw) {
  uint8_t * bytes = (uint8_t*)&dw;
   return
     fv_write_u8(bytes[7]) &&
     fv_write_u8(bytes[6]) &&
     fv_write_u8(bytes[5]) &&
     fv_write_u8(bytes[4]) &&
     fv_write_u8(bytes[3]) &&
     fv_write_u8(bytes[2]) &&
     fv_write_u8(bytes[1]) &&
     fv_write_u8(bytes[0]);
}


bool write_lbm_uint(lbm_uint ptr_val, int32_t *i, bool direction) {
#ifdef LBM64
  return write_u64(ptr_val, i, direction);
#else
  return write_u32(ptr_val, i, direction);
#endif
}

bool write_lbm_value(lbm_value v, int32_t *i, bool direction) {
#ifdef LBM64
  return write_u64(v, i, direction);
#else
  return write_u32(v, i, direction);
#endif
}

// ////////////////////////////////////////////////////////////
// Flatten a value into image

// TODO: Consants things that are stored in the image
//       does not need to be flattened. Could refer to these by
//       reference. Some new kinds of flat values needs to be added
//       for this referencing to work.

// TODO: Symbols in a flat_value in an image can be stored as
//       its numerical representation rather than its string rep.

static bool i_f_cons(void ) {
  return fv_write_u8(S_CONS);
}

static bool i_f_lisp_array(uint32_t size) {
  // arrays are smaller than 2^32 elements long
  bool r = fv_write_u8(S_LBM_LISP_ARRAY);
  r = r && fv_write_u32(size);
  return r;
}

static bool i_f_sym(lbm_value sym) {
  lbm_uint sym_id = lbm_dec_sym(sym);
  bool r = fv_write_u8(S_SYM_VALUE);
  #ifndef LBM64
  r = r && fv_write_u32(sym_id);
  #else
  r = r && fv_write_u64(sym_id);
  #endif
  return r;
}

static bool i_f_i(lbm_int i) {
  bool res = true;
#ifndef LBM64
  res = res && fv_write_u8(S_I28_VALUE);
  res = res && fv_write_u32((uint32_t)i);
#else
  res = res && fv_write_u8(S_I56_VALUE);
  res = res && fv_write_u64((uint64_t)i);
#endif
  return res;
}

static bool i_f_u(lbm_uint u) {
  bool res = true;
#ifndef LBM64
  res = res && fv_write_u8(S_U28_VALUE);
  res = res && fv_write_u32((uint32_t)u);
#else
  res = res && fv_write_u8(S_U56_VALUE);
  res = res && fv_write_u64((uint64_t)u);
#endif
  return res;
}

static bool i_f_b(uint8_t b) {
  bool res = true;
  res = res && fv_write_u8(S_BYTE_VALUE);
  res = res && fv_write_u8(b);
  return res;
}

static bool i_f_i32(int32_t w) {
  bool res = true;
  res = res && fv_write_u8(S_I32_VALUE);
  res = res && fv_write_u32((uint32_t)w);
  return res;
}

static bool i_f_u32(uint32_t w) {
  bool res = true;
  res = res && fv_write_u8(S_U32_VALUE);
  res = res && fv_write_u32(w);
  return res;
}

static bool i_f_float(float f) {
  bool res = true;
  res = res && fv_write_u8(S_FLOAT_VALUE);
  uint32_t u;
  memcpy(&u, &f, sizeof(uint32_t));
  res = res && fv_write_u32((uint32_t)u);
  return res;
}

static bool i_f_double(double d) {
  bool res = true;
  res = res && fv_write_u8(S_DOUBLE_VALUE);
  uint64_t u;
  memcpy(&u, &d, sizeof(uint64_t));
  res = res && fv_write_u64(u);
  return res;
}

static bool i_f_i64(int64_t w) {
  bool res = true;
  res = res && fv_write_u8(S_I64_VALUE);
  res = res && fv_write_u64((uint64_t)w);
  return res;
}

static bool i_f_u64(uint64_t w) {
  bool res = true;
  res = res && fv_write_u8(S_U64_VALUE);
  res = res && fv_write_u64(w);
  return res;
}

// num_bytes is specifically an uint32_t
static bool i_f_lbm_array(uint32_t num_bytes, uint8_t *data) {
  bool res = fv_write_u8(S_LBM_ARRAY);
  res = res && fv_write_u32(num_bytes);
  if (res) {
    for (uint32_t i = 0; i < num_bytes; i ++ ) {
      if (!fv_write_u8(data[i])) return false;
    }
  }
  return res;
}


static void size_acc(lbm_value v, void *acc) {
 int32_t *s = (int32_t*)acc;

  lbm_uint t = lbm_type_of(v);

  if (t >= LBM_POINTER_TYPE_FIRST && t < LBM_POINTER_TYPE_LAST) {
    t = t & ~(LBM_PTR_TO_CONSTANT_BIT);
  }

  if (lbm_is_ptr(v) && (v & LBM_PTR_TO_CONSTANT_BIT)) {
    *s += (int32_t)sizeof(lbm_uint) + 1;
    return;
  }

  switch (t) {
  case LBM_TYPE_CONS:
    *s += 1;
    break;
  case LBM_TYPE_LISPARRAY:
    *s += 4 + 1;
    break;
  case LBM_TYPE_BYTE:
    *s += 2;
    break;
  case LBM_TYPE_U:
    *s += (int32_t)sizeof(lbm_uint) + 1;
    break;
  case LBM_TYPE_I:
    *s += (int32_t)sizeof(lbm_uint) + 1;
    break;
  case LBM_TYPE_U32:
    *s += 4 + 1;
    break;
  case LBM_TYPE_I32:
    *s += 4 + 1;
    break;
  case LBM_TYPE_U64:
    *s += 8 + 1;
    break;
  case LBM_TYPE_I64:
    *s += 8 + 1;
    break;
  case LBM_TYPE_FLOAT:
    *s += 4 + 1;
    break;
  case LBM_TYPE_DOUBLE:
    *s += 8 + 1;
    break;
  case LBM_TYPE_SYMBOL:
    *s += (int32_t)sizeof(lbm_uint) + 1;
    break;
  case LBM_TYPE_ARRAY: {
    lbm_int arr_size = lbm_heap_array_get_size(v);
    const uint8_t *d = lbm_heap_array_get_data_ro(v);
    if (arr_size > 0 && d != NULL) {
      *s += 1 + 4 + arr_size;
    }
  }break;
  }
}

static void flatten_node(lbm_value v, void *res) {
  bool *acc = (bool*)res;
  lbm_uint t = lbm_type_of(v);

  if (t >= LBM_POINTER_TYPE_FIRST && t < LBM_POINTER_TYPE_LAST) {
    t = t & ~(LBM_PTR_TO_CONSTANT_BIT);
  }

  if (lbm_is_ptr(v) && (v & LBM_PTR_TO_CONSTANT_BIT)) {
    *acc = *acc && fv_write_u8(S_CONSTANT_REF);
#ifdef LBM64
    *acc = *acc && fv_write_u64((lbm_uint)v);
#else
    *acc = *acc && fv_write_u32((lbm_uint)v);
#endif
    return;
  }

  switch (t) {
  case LBM_TYPE_CONS:
    *acc = *acc && i_f_cons();
    break;
  case LBM_TYPE_LISPARRAY: {
    lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(v);
    if (header) {
      uint32_t size = (uint32_t)(header->size / sizeof(lbm_value));
      *acc = *acc && i_f_lisp_array(size);
    } else {
      // hmm
    }
  } break;
  case LBM_TYPE_BYTE:
    *acc = *acc && i_f_b((uint8_t)lbm_dec_as_char(v));
    break;
  case LBM_TYPE_U:
    *acc = *acc && i_f_u(lbm_dec_u(v));
    break;
  case LBM_TYPE_I:
    *acc = *acc && i_f_i(lbm_dec_i(v));
    break;
  case LBM_TYPE_U32:
    *acc = *acc && i_f_u32(lbm_dec_as_u32(v));
    break;
  case LBM_TYPE_I32:
    *acc = *acc && i_f_i32(lbm_dec_as_i32(v));
    break;
  case LBM_TYPE_U64:
    *acc = *acc && i_f_u64(lbm_dec_as_u64(v));
    break;
  case LBM_TYPE_I64:
    *acc = *acc && i_f_i64(lbm_dec_as_i64(v));
    break;
  case LBM_TYPE_FLOAT:
    *acc = *acc && i_f_float(lbm_dec_as_float(v));
    break;
  case LBM_TYPE_DOUBLE:
    *acc = *acc && i_f_double(lbm_dec_as_double(v));
    break;
  case LBM_TYPE_SYMBOL:
    *acc = *acc && i_f_sym(v);
    break;
  case LBM_TYPE_ARRAY: {
    lbm_int s = lbm_heap_array_get_size(v);
    const uint8_t *d = lbm_heap_array_get_data_ro(v);
    if (s > 0 && d != NULL) {
      *acc = *acc && i_f_lbm_array((uint32_t)s, (uint8_t*)d);
    }
  }break;
  default:
    break;
  }
}

static int32_t image_flatten_size(lbm_value v) {
  int32_t s = 0;
  if (lbm_ptr_rev_trav(size_acc, v, &s))
    return s;
  return -1;
}

static bool image_flatten_value(lbm_value v) {
  bool ok = true;
  bool trav_ok = lbm_ptr_rev_trav(flatten_node, v, &ok);
  return trav_ok && ok; // ok = enough space in image for flat val.
                        // trav_ok = no cycles in input value.
}

// ////////////////////////////////////////////////////////////
//

char *lbm_image_get_version(void) {
  if (image_version) {
    return image_version;
  } else {
    int32_t pos = (int32_t)image_size-2; // fixed position version string.
    uint32_t val = read_u32(pos); pos --;
    if (val == VERSION_ENTRY) {
      int32_t size = (int32_t)read_u32(pos);
      image_version = (char*)(image_address + (pos - size));
      return image_version;
    }
  }
  return NULL;
}

// ////////////////////////////////////////////////////////////
// Constant heaps as part of an image.

lbm_const_heap_t image_const_heap;
lbm_uint image_const_heap_start_ix = 0;

bool image_const_heap_write(lbm_uint w, lbm_uint ix) {
#ifdef LBM64
  int32_t i = (int32_t)(image_const_heap_start_ix + (ix * 2));
  uint32_t *words = (uint32_t*)&w;
  bool r = image_write(words[0], i, false);
  r = r && image_write(words[1], i + 1, false);
  return r;
#else
  int32_t i = (int32_t)(image_const_heap_start_ix + ix);
  return write_u32(w, &i, false);
#endif
}

// ////////////////////////////////////////////////////////////
// Image manipulation

lbm_uint *lbm_image_add_symbol(char *name, lbm_uint id, lbm_uint symlist) {
  // 64 bit                             | 32 bit
  // image[i] = SYMBOL_ENTRY            | image[i] = SYMBOL_ENTRY
  // image[i-1] = symlist_ptr_high_word | image[i-1] = symlist_ptr
  // image[i-2] = symlist_ptr_low_word  | image[i-2] = id
  // image[i-3] = id_high_word          | image[i-3] = name_ptr
  // image[i-4] = id_low_word
  // image[i-5] = name_ptr_high_word
  // image[i-6] = name_ptr_low_word
  bool r = write_u32(SYMBOL_ENTRY, &write_index,DOWNWARDS);
  r = r && write_lbm_uint(symlist, &write_index, DOWNWARDS);
  r = r && write_lbm_uint(id, &write_index, DOWNWARDS);
  r = r && write_lbm_uint((lbm_uint)name, &write_index, DOWNWARDS);
  lbm_uint entry_ptr = (lbm_uint)(image_address + write_index + 1);
  if (r)
    return (lbm_uint*)entry_ptr;
  return NULL;
}

// The symbol id is written to the link address upon image-boot
lbm_uint *lbm_image_add_and_link_symbol(char *name, lbm_uint id, lbm_uint symlist, lbm_uint *link) {
  // 64 bit                             | 32 bit
  // image[i] = SYMBOL_ENTRY            | image[i] = SYMBOL_ENTRY
  // image[i-1] = link_ptr_high         | image[i-1] link_ptr
  // image[i-2] = link_ptr_low          | image[i-2] = symlist_ptr
  // image[i-3] = symlist_ptr_high_word | image[i-3] = id
  // image[i-4] = symlist_ptr_low_word  | image[i-4] = name_ptr
  // image[i-5] = id_high_word
  // image[i-6] = id_low_word
  // image[i-7] = name_ptr_high_word
  // image[i-8] = name_ptr_low_word
  bool r = write_u32(SYMBOL_LINK_ENTRY, &write_index,DOWNWARDS);
  r = r && write_lbm_uint((lbm_uint)link, &write_index, DOWNWARDS);
  r = r && write_lbm_uint(symlist, &write_index, DOWNWARDS);
  r = r && write_lbm_uint(id, &write_index, DOWNWARDS);
  r = r && write_lbm_uint((lbm_uint)name, &write_index, DOWNWARDS);
    lbm_uint entry_ptr = (lbm_uint)(image_address + write_index + 1);
  if (r)
    return (lbm_uint*)entry_ptr;
  return NULL;
}

bool lbm_image_save_global_env(void) {
  lbm_value *env = lbm_get_global_env();
  if (env) {
    for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
      lbm_value curr = env[i];
      while(lbm_is_cons(curr)) {
        lbm_value name_field = lbm_caar(curr);
        lbm_value val_field  = lbm_cdr(lbm_car(curr));

        if (lbm_is_constant(val_field)) {
          write_u32(BINDING_CONST, &write_index, DOWNWARDS);
          write_lbm_value(name_field, &write_index, DOWNWARDS);
          write_lbm_value(val_field, &write_index, DOWNWARDS);
        } else {
          int fv_size = image_flatten_size(val_field);
          if (fv_size > 0) {
            fv_size = (fv_size % 4 == 0) ? (fv_size / 4) : (fv_size / 4) + 1; // num 32bit words
            if ((write_index - fv_size) <= (int32_t)image_const_heap.next) {
              return false;
            }
            write_u32(BINDING_FLAT, &write_index, DOWNWARDS);
            write_u32((uint32_t)fv_size , &write_index, DOWNWARDS);
            write_lbm_value(name_field, &write_index, DOWNWARDS);
            write_index = write_index - fv_size;  // subtract fv_size
            if (image_flatten_value(val_field)) { // adds fv_size backq
              // TODO: What error handling makes sense?
              fv_write_flush();
            }
            write_index = write_index - fv_size - 1; // subtract fv_size
          } else {
            return false;
          }
        }
        curr = lbm_cdr(curr);
      }
    }
    return true;
  }
  return false;
}

// The extension table is created at system startup.
// Extensions can also be added dynamically.
// Dynamically added extensions have names starting with "ext-"
// and their names are placed in RAM by the reader.
//
// Symbol_id -> index in extension table mapping
// is created as extensions are added.
// dynamic extensions are added after "built-in" extensions
// and have higher indices.

bool lbm_image_save_extensions(void) {
  bool r = true;
  lbm_uint num = lbm_get_num_extensions();
  if (num > 0) {
    r = r && write_u32(EXTENSION_TABLE, &write_index, DOWNWARDS);
    r = r && write_u32((uint32_t)num , &write_index, DOWNWARDS);
    for (lbm_uint i = 0; i < num; i ++) {
      if (!r) return r;

      char *name_ptr = extension_table[i].name;
      lbm_uint addr;
      // when PIC, name pointers may move around
      // between restarts. It is also the case that
      // the FPTRs will move around as well.
      // This makes dynamic extensions useless on Linux.
      // Static extensions are fine as they will be re-added after image-boot
      // and faulty FPTRs will be replaced.
      //#ifdef __PIC__
      //r = store_symbol_name_flash(name_ptr, &addr);
      //if (!r) return r;
      //name_ptr = (char *)addr;
      //#else
      if (lbm_memory_ptr_inside((lbm_uint *)name_ptr)) {
        r = store_symbol_name_flash(name_ptr, &addr);
        if (!r) return r;
        name_ptr = (char *)addr;
      }
      //#endif
#ifdef LBM64
      r = r && write_u64((uint64_t)name_ptr, &write_index, DOWNWARDS);
      r = r && write_u64((uint64_t)extension_table[i].fptr, &write_index, DOWNWARDS);
#else
      r = r && write_u32((uint32_t)name_ptr, &write_index, DOWNWARDS);
      r = r && write_u32((uint32_t)extension_table[i].fptr, &write_index, DOWNWARDS);
#endif
    }  }
  return true;
}

static uint32_t last_const_heap_ix = 0;

bool lbm_image_save_constant_heap_ix(void) {
  bool r = true; // saved or no need to save it.
  if (image_const_heap.next != last_const_heap_ix) {
    last_const_heap_ix = image_const_heap.next;
    r = write_u32(CONSTANT_HEAP_IX, &write_index, DOWNWARDS);
    r = r && write_u32((uint32_t)image_const_heap.next, &write_index, DOWNWARDS);
  }
  return r;
}

bool lbm_image_exists(void) {
  uint32_t val = read_u32((int32_t)image_size - 1);
  return val == IMAGE_INITIALIZED;
}

void lbm_image_init(uint32_t* image_mem_address,
                    uint32_t image_size_words,
                    lbm_image_write_fun image_write_fun) {
  image_write = image_write_fun;
  image_address = image_mem_address;
  image_size = image_size_words;
  write_index = (int32_t)image_size_words -1;
  image_has_extensions = false;
  image_version = NULL;
  last_const_heap_ix = 0;
}

void lbm_image_create(char *version_str) {
  write_u32(IMAGE_INITIALIZED, &write_index, DOWNWARDS);
  if (version_str) {
    uint32_t bytes = strlen(version_str) + 1;
    uint32_t words = (bytes % 4 == 0) ? bytes / 4 : (bytes / 4) + 1;
    write_u32(VERSION_ENTRY, &write_index, DOWNWARDS);
    write_u32(words, &write_index, DOWNWARDS);
    uint32_t w = 0;
    char *buf = (char*)&w;
    uint32_t i = 0;
    int32_t ix = write_index - (int32_t)(words -1);
    int wi = 0;
    while (i < bytes) {
      if (wi == 0 ) {
        w = 0;
      }
      if (wi == 4) wi = 0;
      buf[wi] = version_str[i];
      if (wi == 3) {
        write_u32(w, &ix, UPWARDS);
      }
      i ++;
      wi ++;
    }
    if (wi != 0) {
      write_u32(w, &ix, UPWARDS);
    }
    write_index -= (int32_t)words;
  }
}


bool lbm_image_boot(void) {
  //process image
  int32_t pos = (int32_t)image_size-1;
  last_const_heap_ix = 0;

  while (pos >= 0 && pos > (int32_t)last_const_heap_ix) {
    uint32_t val = read_u32(pos);
    pos --;
    switch(val) {
    case IMAGE_INITIALIZED: {
      image_const_heap_start_ix = 0; // const heap starts at 0
      lbm_const_heap_init(image_const_heap_write,
                          &image_const_heap,
                          (lbm_uint*)(image_address));
      // initialized is a one word field
    } break;
    case VERSION_ENTRY: {
      uint32_t size = read_u32(pos); pos --;
      image_version = (char*)(image_address + (pos - (int32_t)size + 1));
      pos -= (int32_t)size;
    } break;
    case CONSTANT_HEAP_IX: {
      uint32_t next = read_u32(pos);
      pos --;
      last_const_heap_ix = next;
      image_const_heap.next = next;
    } break;
    case BINDING_CONST: {
      // on 64 bit           | on 32 bit
      // pos     -> key_high | pos     -> key
      // pos - 1 -> key_low  | pos - 1 -> val
      // pos - 2 -> val_high
      // pos - 3 -> val_low
#ifdef LBM64
      lbm_uint bind_key = read_u64(pos-1);
      lbm_uint bind_val = read_u64(pos-3);
      pos -= 4;
#else
      lbm_uint bind_key = read_u32(pos);
      lbm_uint bind_val = read_u32(pos-1);
      pos -= 2;
#endif
      lbm_uint ix_key  = lbm_dec_sym(bind_key) & GLOBAL_ENV_MASK;
      lbm_value *global_env = lbm_get_global_env();
      lbm_uint orig_env = global_env[ix_key];
      lbm_value new_env = lbm_env_set(orig_env,bind_key,bind_val);

      if (lbm_is_symbol(new_env)) {
        return false;
      }
      global_env[ix_key] = new_env;
    } break;
    case BINDING_FLAT: {
      // on 64 bit           | on 32 bit
      // pos     -> size     | pos     -> size
      // pos - 1 -> key_high | pos - 1 -> key
      // pos - 2 -> key_low
      //
      int32_t s = (int32_t)read_u32(pos);
      // size in 32 or 64 bit words.
#ifdef LBM64
      lbm_uint bind_key = read_u64(pos-2);
      pos -= 3;
#else
      lbm_uint bind_key = read_u32(pos-1);
      pos -= 2;
#endif

      pos -= s;
      lbm_flat_value_t fv;
      fv.buf = (uint8_t*)(image_address + pos);
      fv.buf_size = (uint32_t)s * sizeof(lbm_uint); // GEQ to actual buf
      fv.buf_pos = 0;
      lbm_value unflattened;
      lbm_unflatten_value(&fv, &unflattened);
      if (lbm_is_symbol_merror(unflattened)) {
        lbm_perform_gc();
        lbm_unflatten_value(&fv, &unflattened);
      }

      lbm_uint ix_key  = lbm_dec_sym(bind_key) & GLOBAL_ENV_MASK;
      lbm_value *global_env = lbm_get_global_env();
      lbm_uint orig_env = global_env[ix_key];
      lbm_value new_env = lbm_env_set(orig_env,bind_key,unflattened);

      if (lbm_is_symbol(new_env)) {
        return false;
      }
      global_env[ix_key] = new_env;
      pos --;
    } break;
    case SYMBOL_ENTRY: {
      // on 64 bit                         | on 32 bit
      // pos     -> symlist_addr_high_word | pos     -> symlist_ptr
      // pos - 1 -> symlist_addr_low_word  | pos - 1 -> id
      // pos - 2 -> id_high_word           | pos - 2 -> name_ptr
      // pos - 3 -> id_low_word            |
      // pos - 4 -> name_ptr_high_word     |
      // pos - 5 -> name_ptr_low_word      |
#ifdef LBM64
      int32_t entry_pos = pos - 5;
      lbm_uint *p = (lbm_uint*)(image_address + entry_pos);
      uint32_t sym_id = (uint32_t)(p[1]);
      lbm_uint next_id = lbm_symrepr_get_next_id();
      if (sym_id >= RUNTIME_SYMBOLS_START && sym_id >= next_id ) {
        lbm_symrepr_set_next_id(next_id + 1);
      }
      lbm_symrepr_set_symlist((lbm_uint*)(image_address + entry_pos));
      pos -= 6;
#else
      int32_t entry_pos = pos - 2;
      lbm_uint *p = (lbm_uint*)(image_address + entry_pos);
      uint32_t sym_id = (uint32_t)(p[1]);
      lbm_uint next_id = lbm_symrepr_get_next_id();
      if (sym_id >= RUNTIME_SYMBOLS_START && sym_id >= next_id ) {
        lbm_symrepr_set_next_id(next_id + 1);
      }
      lbm_symrepr_set_symlist((lbm_uint*)(image_address + entry_pos));
      pos -= 3;
#endif
    } break;
    case SYMBOL_LINK_ENTRY: {
      // on 64 bits                        | on 32 bit
      // pos     -> link_ptr_high          | pos     -> link_ptr
      // pos - 1 -> link_ptr_low           | pos - 1 -> symlist_ptr
      // pos - 2 -> symlist_addr_high_word | pos - 2 -> id
      // pos - 3 -> symlist_addr_low_word  | pos - 3 -> name_ptr;
      // pos - 4 -> id_high_word
      // pos - 5 -> id_low_word
      // pos - 6 -> name_ptr_high_word
      // pos - 7 -> name_ptr_low_word
      //int32_t entry_pos = pos - (int32_t)(3 * (sizeof(lbm_uint) / 4));
      lbm_uint link_ptr;
      lbm_uint sym_id;
#ifdef LBM64
      link_ptr = read_u64(pos-1);
      sym_id   = read_u64(pos-5);
      *((lbm_uint*)link_ptr) = sym_id;
      lbm_uint next_id = lbm_symrepr_get_next_id();
      if (sym_id >= RUNTIME_SYMBOLS_START && sym_id >= next_id ) {
        lbm_symrepr_set_next_id(next_id + 1);
      }
      lbm_symrepr_set_symlist((lbm_uint*)(image_address + (pos - 7)));
      pos -= 8;
#else
      link_ptr = read_u32(pos);
      sym_id   = read_u32(pos-2);
      *((lbm_uint*)link_ptr) = sym_id;
      lbm_uint next_id = lbm_symrepr_get_next_id();
      if (sym_id >= RUNTIME_SYMBOLS_START && sym_id >= next_id ) {
        lbm_symrepr_set_next_id(next_id + 1);
      }
      lbm_symrepr_set_symlist((lbm_uint*)(image_address + (pos - 3)));
      pos -= 4;
#endif
    } break;
    case EXTENSION_TABLE: {
      // on 64 bit                | on 32 bit
      // pos     -> name_ptr_high | pos     -> name_ptr
      // pos - 1 -> name_ptr_low  | pos - 1 -> fptr
      // pos - 2 -> fptr_high
      // pos - 3 -> fptr_low
      int32_t num = (int32_t)read_u32(pos); pos --;

      int32_t i = 0;
      for (i = 0; i < num; i ++) {
        lbm_uint name;
        lbm_uint fptr;
#ifdef LBM64
        name = read_u64(pos-1);
        fptr = read_u64(pos-3);
        pos -= 4;
#else
        name = read_u32(pos);
        fptr = read_u32(pos-1);
        pos -= 2;
#endif
        extension_table[i].name = (char*)name;
        extension_table[i].fptr = (extension_fptr)fptr;
      }
      lbm_extensions_set_next((lbm_uint)i);
      image_has_extensions = true;
    } break;
    default:
      write_index = pos+1;
      goto done_loading_image;
      break;
    }
  }
 done_loading_image:
  return true;
}
