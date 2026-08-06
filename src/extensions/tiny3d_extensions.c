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

#include <extensions/tiny3d_extensions.h>
#include <extensions/display_extensions.h>
#include <lbm_utils.h>
#include <math.h>

#define TINY3D_MESH_MAGIC ((uint32_t)0x33444D00) // "3DM\0"
#define TINY3D_INSTANCE_MAGIC ((uint32_t)0x33444900) // "3DI\0"
#define TINY3D_STATE_MAGIC ((uint32_t)0x33445300) // "3DS\0"

// Symbols
static lbm_uint symbol_filled = 0;
static lbm_uint symbol_no_backface_cull = 0;

typedef struct {
  uint32_t magic;
  uint16_t vertex_count;
  uint16_t triangle_count;
  int32_t  bounding_radius;
} tiny3d_mesh_header_t;

static bool is_mesh(const uint8_t *data, lbm_uint size) {
  if (size < sizeof(tiny3d_mesh_header_t)) return false;
  const tiny3d_mesh_header_t *hdr = (const tiny3d_mesh_header_t*)data;
  if (hdr->magic != TINY3D_MESH_MAGIC) return false;
  lbm_uint expected = sizeof(tiny3d_mesh_header_t)
    + (lbm_uint)hdr->vertex_count * sizeof(tiny3d_vec_t)
    + (lbm_uint)hdr->triangle_count * sizeof(tiny3d_triangle_t);
  return size == expected;
}

static tiny3d_mesh_header_t *get_mesh_header(lbm_value v) {
  lbm_array_header_t *arr = lbm_dec_array_r(v);
  if (arr && is_mesh((const uint8_t*)arr->data, arr->size)) {
    return (tiny3d_mesh_header_t*)arr->data;
  }
  return NULL;
}

static tiny3d_mesh_t to_mesh(tiny3d_mesh_header_t *hdr) {
  uint8_t *base = (uint8_t*)hdr;
  tiny3d_mesh_t m;
  m.vertices = (const tiny3d_vec_t*)(base + sizeof(tiny3d_mesh_header_t));
  m.vertex_count = hdr->vertex_count;
  m.triangles = (const tiny3d_triangle_t*)
    (base + sizeof(tiny3d_mesh_header_t) + (lbm_uint)hdr->vertex_count * sizeof(tiny3d_vec_t));
  m.triangle_count = hdr->triangle_count;
  m.bounding_radius = hdr->bounding_radius;
  return m;
}

static bool validate_vertex_list(lbm_value verts_list, lbm_uint *out_count) {
  lbm_uint count = 0;
  lbm_value curr = verts_list;
  while (lbm_is_cons(curr)) {
    lbm_value v = lbm_car(curr);
    if (lbm_list_length(v) != 3) return false;
    if (!lbm_is_number(lbm_car(v)) || !lbm_is_number(lbm_cadr(v)) ||
        !lbm_is_number(lbm_car(lbm_cddr(v)))) {
      return false;
    }
    count++;
    curr = lbm_cdr(curr);
  }
  if (curr != ENC_SYM_NIL) return false; // improper list
  *out_count = count;
  return true;
}

static bool validate_index_triangle_list(lbm_value tris_list, lbm_uint vertex_count, lbm_uint *out_count) {
  lbm_uint count = 0;
  lbm_value curr = tris_list;
  while (lbm_is_cons(curr)) {
    lbm_value tri = lbm_car(curr);
    if (lbm_list_length(tri) != 4) return false;
    lbm_value e = tri;
    for (int i = 0; i < 4; i++) {
      if (!lbm_is_number(lbm_car(e))) return false;
      e = lbm_cdr(e);
    }
    uint32_t i0 = lbm_dec_as_u32(lbm_car(tri));
    uint32_t i1 = lbm_dec_as_u32(lbm_cadr(tri));
    uint32_t i2 = lbm_dec_as_u32(lbm_car(lbm_cddr(tri)));
    if (i0 >= vertex_count || i1 >= vertex_count || i2 >= vertex_count) return false;
    count++;
    curr = lbm_cdr(curr);
  }
  if (curr != ENC_SYM_NIL) return false; // improper list
  *out_count = count;
  return true;
}

// (tiny3d-mesh vertices triangles)
// vertices: list of (x y z), one entry per unique vertex.
// triangles: list of (i0 i1 i2 color) - indices into vertices, so
// vertices shared between triangles (e.g. a cube's 8 corners across its
// 12 triangles) are stored once rather than duplicated per triangle.
static lbm_value ext_tiny3d_mesh(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_list(args[0]) || !lbm_is_list(args[1])) return ENC_SYM_TERROR;

  lbm_uint vert_count;
  if (!validate_vertex_list(args[0], &vert_count) || vert_count == 0 || vert_count > 0xFFFF) {
    return ENC_SYM_TERROR;
  }
  lbm_uint tri_count;
  if (!validate_index_triangle_list(args[1], vert_count, &tri_count) || tri_count == 0 || tri_count > 0xFFFF) {
    return ENC_SYM_TERROR;
  }

  lbm_uint size = sizeof(tiny3d_mesh_header_t)
    + vert_count * sizeof(tiny3d_vec_t)
    + tri_count  * sizeof(tiny3d_triangle_t);

  uint8_t *buf = lbm_malloc(size);
  if (!buf) return ENC_SYM_MERROR;

  tiny3d_vec_t *verts = (tiny3d_vec_t*)(buf + sizeof(tiny3d_mesh_header_t));
  tiny3d_triangle_t *tris = (tiny3d_triangle_t*)
    (buf + sizeof(tiny3d_mesh_header_t) + vert_count * sizeof(tiny3d_vec_t));

  // One-time construction, not a hot-path function - libm (sqrtf, for the
  // bounding radius) is fine here, same policy as tiny3d_init.
  float max_r = 0.0f;
  lbm_uint vi = 0;
  lbm_value curr = args[0];
  while (lbm_is_cons(curr)) {
    lbm_value v = lbm_car(curr);
    float x = lbm_dec_as_float(lbm_car(v));
    float y = lbm_dec_as_float(lbm_cadr(v));
    float z = lbm_dec_as_float(lbm_car(lbm_cddr(v)));
    verts[vi].x = (int32_t)llroundf(x * 65536.0f);
    verts[vi].y = (int32_t)llroundf(y * 65536.0f);
    verts[vi].z = (int32_t)llroundf(z * 65536.0f);
    float r = sqrtf(x * x + y * y + z * z);
    if (r > max_r) max_r = r;
    vi++;
    curr = lbm_cdr(curr);
  }

  lbm_uint ti = 0;
  curr = args[1];
  while (lbm_is_cons(curr)) {
    lbm_value tri = lbm_car(curr);
    uint16_t i0 = (uint16_t)lbm_dec_as_u32(lbm_car(tri));
    uint16_t i1 = (uint16_t)lbm_dec_as_u32(lbm_cadr(tri));
    uint16_t i2 = (uint16_t)lbm_dec_as_u32(lbm_car(lbm_cddr(tri)));
    uint32_t color = lbm_dec_as_u32(lbm_car(lbm_cdr(lbm_cddr(tri))));
    tris[ti] = (tiny3d_triangle_t){ .i0 = i0, .i1 = i1, .i2 = i2, ._pad = 0, .color = color };
    ti++;
    curr = lbm_cdr(curr);
  }

  tiny3d_mesh_header_t *hdr = (tiny3d_mesh_header_t*)buf;
  hdr->magic = TINY3D_MESH_MAGIC;
  hdr->vertex_count = (uint16_t)vert_count;
  hdr->triangle_count = (uint16_t)tri_count;
  hdr->bounding_radius = (int32_t)llroundf(max_r * 65536.0f);

  lbm_value res;
  if (!lbm_lift_array(&res, (char*)buf, size)) {
    lbm_free(buf);
    return ENC_SYM_MERROR;
  }
  return res;
}

static lbm_value ext_tiny3d_is_mesh(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  return get_mesh_header(args[0]) ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

// //////////////////////////////////////////////////
// Instance: [magic][pos][orient][scale]
//
// An instance of a mesh, this is so that several
// "things" in the world can reuse one set of vertices and triangles.
//
// The instance_blob can be updated destructively.
// On the lisp side a full represntation of an instance is a (instance-blob . mesh) pair

typedef struct {
  uint32_t magic;
  tiny3d_pos_t pos;
  tiny3d_orient_t orient;
  int32_t scale;
} tiny3d_instance_blob_t;

static tiny3d_instance_blob_t *get_instance_blob(lbm_value v) {
  lbm_array_header_t *arr = lbm_dec_array_r(v);
  if (!arr || arr->size != sizeof(tiny3d_instance_blob_t)) return NULL;
  tiny3d_instance_blob_t *blob = (tiny3d_instance_blob_t*)arr->data;
  if (blob->magic != TINY3D_INSTANCE_MAGIC) return NULL;
  return blob;
}

// Setters accept either the bare instance value or an (instance-blob . mesh)
// pair
static tiny3d_instance_blob_t *resolve_instance(lbm_value v) {
  if (lbm_is_cons(v)) v = lbm_car(v);
  return get_instance_blob(v);
}

static bool decode_vec3(lbm_value v, tiny3d_vec_t *out);
static bool decode_orient(lbm_value v, tiny3d_orient_t *out);
static lbm_value encode_vec3(tiny3d_vec_t v);
static lbm_value encode_orient(tiny3d_orient_t o);

// (tiny3d-instance mesh pos orient opt-scale)
static lbm_value ext_tiny3d_instance(lbm_value *args, lbm_uint argn) {
  if ((argn != 3 && argn != 4) || !get_mesh_header(args[0])) return ENC_SYM_TERROR;
  if (argn == 4 && !lbm_is_number(args[3])) return ENC_SYM_TERROR;

  tiny3d_instance_blob_t *blob = lbm_malloc(sizeof(tiny3d_instance_blob_t));
  if (!blob) return ENC_SYM_MERROR;

  blob->magic = TINY3D_INSTANCE_MAGIC;
  blob->scale = (argn == 4)
    ? (int32_t)llround((double)lbm_dec_as_float(args[3]) * 65536.0)
    : TINY3D_SCALE_ONE;
  if (!decode_vec3(args[1], &blob->pos) || !decode_orient(args[2], &blob->orient)) {
    lbm_free(blob);
    return ENC_SYM_TERROR;
  }

  lbm_value inst_val;
  if (!lbm_lift_array(&inst_val, (char*)blob, sizeof(tiny3d_instance_blob_t))) {
    lbm_free(blob);
    return ENC_SYM_MERROR;
  }
  return lbm_cons(inst_val, args[0]);
}

static lbm_value ext_tiny3d_is_instance(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  return resolve_instance(args[0]) ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_tiny3d_instance_set_pos(lbm_value *args, lbm_uint argn) {
  if (argn != 2) return ENC_SYM_TERROR;
  tiny3d_instance_blob_t *blob = resolve_instance(args[0]);
  if (!blob || !decode_vec3(args[1], &blob->pos)) return ENC_SYM_TERROR;
  return ENC_SYM_TRUE;
}

static lbm_value ext_tiny3d_instance_set_orient(lbm_value *args, lbm_uint argn) {
  if (argn != 2) return ENC_SYM_TERROR;
  tiny3d_instance_blob_t *blob = resolve_instance(args[0]);
  if (!blob || !decode_orient(args[1], &blob->orient)) return ENC_SYM_TERROR;
  return ENC_SYM_TRUE;
}

static lbm_value ext_tiny3d_instance_set_transform(lbm_value *args, lbm_uint argn) {
  if (argn != 3) return ENC_SYM_TERROR;
  tiny3d_instance_blob_t *blob = resolve_instance(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  if (!decode_vec3(args[1], &blob->pos)) return ENC_SYM_TERROR;
  if (!decode_orient(args[2], &blob->orient)) return ENC_SYM_TERROR;
  return ENC_SYM_TRUE;
}

static lbm_value ext_tiny3d_instance_pos(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  tiny3d_instance_blob_t *blob = resolve_instance(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  return encode_vec3(blob->pos);
}

static lbm_value ext_tiny3d_instance_orient(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  tiny3d_instance_blob_t *blob = resolve_instance(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  return encode_orient(blob->orient);
}

// (tiny3d-instance-set-scale obj scale)
static lbm_value ext_tiny3d_instance_set_scale(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  tiny3d_instance_blob_t *blob = resolve_instance(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  blob->scale = (int32_t)llround((double)lbm_dec_as_float(args[1]) * 65536.0);
  return ENC_SYM_TRUE;
}

static lbm_value ext_tiny3d_instance_scale(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  tiny3d_instance_blob_t *blob = resolve_instance(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  return lbm_enc_float((float)((double)blob->scale / 65536.0));
}

static lbm_value ext_tiny3d_mesh_vertex_count(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  tiny3d_mesh_header_t *mh = get_mesh_header(args[0]);
  if (!mh) return ENC_SYM_TERROR;
  return lbm_enc_i(mh->vertex_count);
}

static lbm_value ext_tiny3d_mesh_triangle_count(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  tiny3d_mesh_header_t *mh = get_mesh_header(args[0]);
  if (!mh) return ENC_SYM_TERROR;
  return lbm_enc_i(mh->triangle_count);
}

static lbm_value ext_tiny3d_mesh_bounding_radius(lbm_value *args, lbm_uint argn) {
  if (argn != 1) return ENC_SYM_TERROR;
  tiny3d_mesh_header_t *mh = get_mesh_header(args[0]);
  if (!mh) return ENC_SYM_TERROR;
  return lbm_enc_float((float)mh->bounding_radius / 65536.0f);
}


static bool decode_image_buffer(lbm_value v, image_buffer_t *out) {
  lbm_array_header_t *arr = get_image_buffer(v);
  if (!arr) return false;
  uint8_t *data = (uint8_t*)arr->data;
  out->width    = image_buffer_width(data);
  out->height   = image_buffer_height(data);
  out->fmt      = (color_format_t)image_buffer_format(data);
  out->data     = image_buffer_data(data);
  out->mem_base = data;
  return true;
}

// //////////////////////////////////////////////////
// State: [magic][image_buffer_t][tiny3d_state_t][tiny3d_camera_tri_t x cap]

typedef struct {
  uint32_t magic;
  image_buffer_t img;
  tiny3d_state_t state;
  tiny3d_camera_tri_t tri_buffer_data[];
} tiny3d_state_blob_t;

static tiny3d_state_blob_t *get_state_blob(lbm_value v) {
  lbm_array_header_t *arr = lbm_dec_array_r(v);
  if (!arr || arr->size < sizeof(tiny3d_state_blob_t)) return NULL;
  tiny3d_state_blob_t *blob = (tiny3d_state_blob_t*)arr->data;
  if (blob->magic != TINY3D_STATE_MAGIC) return NULL;
  return blob;
}

// On the lispbm side, the "tiny3d state" is a (state-blob . image-buffer)
// pair. This is to ensure that the lifetime of the image buffer
// is the same as the lifetime of the state-blob.
// The state-blob contains raw pointers into the image buffer that
// are used by the Tiny3d C code without it knowing anything about
// lisp and its GC.
static tiny3d_state_blob_t *resolve_state(lbm_value v) {
  if (!lbm_is_cons(v)) return NULL;
  return get_state_blob(lbm_car(v));
}

// (tiny3d-state-create img max-tris-per-object near far fov-degrees cull-margin opt-attrs...)
// opt-attrs:
//   '(filled)            - solid triangles instead of the default wireframe
//   '(no-backface-cull)  - keep back-facing triangles (culled by default)
// Returns (state . img)
static lbm_value ext_tiny3d_state_create(lbm_value *args, lbm_uint argn) {
  if (argn < 6) return ENC_SYM_TERROR;
  if (!lbm_is_number(args[1]) || !lbm_is_number(args[2]) || !lbm_is_number(args[3]) ||
      !lbm_is_number(args[4]) || !lbm_is_number(args[5])) {
    return ENC_SYM_TERROR;
  }
  bool filled = false;
  bool cull_backfaces = true;
  for (lbm_uint i = 6; i < argn; i++) {
    if (!lbm_is_cons(args[i]) || lbm_list_length(args[i]) != 1 ||
        !lbm_is_symbol(lbm_car(args[i]))) {
      return ENC_SYM_TERROR;
    }
    lbm_uint s = lbm_dec_sym(lbm_car(args[i]));
    if (s == symbol_filled) filled = true;
    else if (s == symbol_no_backface_cull) cull_backfaces = false;
    else return ENC_SYM_TERROR;
  }

  image_buffer_t img;
  if (!decode_image_buffer(args[0], &img)) return ENC_SYM_TERROR;

  uint32_t max_tris = lbm_dec_as_u32(args[1]);
  if (max_tris == 0 || max_tris > 0xFFFF) return ENC_SYM_TERROR;

  int32_t near        = (int32_t)llround((double)lbm_dec_as_float(args[2]) * 65536.0);
  int32_t far          = (int32_t)llround((double)lbm_dec_as_float(args[3]) * 65536.0);
  float   fov_degrees  = lbm_dec_as_float(args[4]);
  int32_t cull_margin  = (int32_t)llround((double)lbm_dec_as_float(args[5]) * 65536.0);
  bool    wireframe    = !filled;

  lbm_uint size = sizeof(tiny3d_state_blob_t) + (lbm_uint)max_tris * sizeof(tiny3d_camera_tri_t);
  uint8_t *buf = lbm_malloc(size);
  if (!buf) return ENC_SYM_MERROR;

  tiny3d_state_blob_t *blob = (tiny3d_state_blob_t*)buf;
  blob->magic = TINY3D_STATE_MAGIC;
  blob->img = img;

  bool ok = tiny3d_init(&blob->state, &blob->img,
                         blob->tri_buffer_data, max_tris * (uint32_t)sizeof(tiny3d_camera_tri_t),
                         near, far, fov_degrees, cull_margin, wireframe, cull_backfaces);
  if (!ok) {
    lbm_free(buf);
    return ENC_SYM_TERROR;
  }

  lbm_value state_val;
  if (!lbm_lift_array(&state_val, (char*)buf, size)) {
    lbm_free(buf);
    return ENC_SYM_MERROR;
  }
  return lbm_cons(state_val, args[0]);
}

// //////////////////////////////////////////////////
// Render

static uint16_t degrees_to_phase(double deg) {
  int64_t raw = llround(deg / 360.0 * 65536.0);
  int64_t wrapped = raw % 65536;
  if (wrapped < 0) wrapped += 65536;
  return (uint16_t)wrapped;
}

static bool decode_vec3(lbm_value v, tiny3d_vec_t *out) {
  if (lbm_list_length(v) != 3) return false;
  out->x = (int32_t)llround((double)lbm_dec_as_float(lbm_car(v)) * 65536.0);
  out->y = (int32_t)llround((double)lbm_dec_as_float(lbm_cadr(v)) * 65536.0);
  out->z = (int32_t)llround((double)lbm_dec_as_float(lbm_car(lbm_cddr(v))) * 65536.0);
  return true;
}

static bool decode_orient(lbm_value v, tiny3d_orient_t *out) {
  if (lbm_list_length(v) != 3) return false;
  out->ang_x = degrees_to_phase((double)lbm_dec_as_float(lbm_car(v)));
  out->ang_y = degrees_to_phase((double)lbm_dec_as_float(lbm_cadr(v)));
  out->ang_z = degrees_to_phase((double)lbm_dec_as_float(lbm_car(lbm_cddr(v))));
  return true;
}

static lbm_value encode_vec3(tiny3d_vec_t v) {
  lbm_value x = lbm_enc_float((float)((double)v.x / 65536.0));
  lbm_value y = lbm_enc_float((float)((double)v.y / 65536.0));
  lbm_value z = lbm_enc_float((float)((double)v.z / 65536.0));
  return lbm_cons(x, lbm_cons(y, lbm_cons(z, ENC_SYM_NIL)));
}

static lbm_value encode_orient(tiny3d_orient_t o) {
  lbm_value x = lbm_enc_float((float)((double)o.ang_x / 65536.0 * 360.0));
  lbm_value y = lbm_enc_float((float)((double)o.ang_y / 65536.0 * 360.0));
  lbm_value z = lbm_enc_float((float)((double)o.ang_z / 65536.0 * 360.0));
  return lbm_cons(x, lbm_cons(y, lbm_cons(z, ENC_SYM_NIL)));
}

typedef struct {
  lbm_value cursor;
  tiny3d_mesh_t current_mesh;
} render_ctx_t;

static tiny3d_mesh_t render_ctx_get_mesh(uint16_t index, void *ctx_v) {
  (void)index;
  return ((render_ctx_t*)ctx_v)->current_mesh;
}

// Objects list elements are (instance-blob . mesh) pairs
static bool render_ctx_next_instance(void *ctx_v, tiny3d_instance_t *out) {
  render_ctx_t *ctx = (render_ctx_t*)ctx_v;
  if (!lbm_is_cons(ctx->cursor)) return false;

  lbm_value obj = lbm_car(ctx->cursor);
  if (!lbm_is_cons(obj)) return false;

  tiny3d_instance_blob_t *inst_blob = get_instance_blob(lbm_car(obj));
  tiny3d_mesh_header_t *mh = get_mesh_header(lbm_cdr(obj));
  if (!inst_blob || !mh) return false;

  tiny3d_instance_t inst = {0};
  inst.pos = inst_blob->pos;
  inst.orient = inst_blob->orient;
  inst.scale = inst_blob->scale;

  ctx->current_mesh = to_mesh(mh);
  *out = inst;
  ctx->cursor = lbm_cdr(ctx->cursor);
  return true;
}

// (tiny3d-render state objects cam-pos cam-orient)
// state: the (state . img) pair returned by tiny3d-state-create.
// objects: list of (instance-blob . mesh) pairs, as returned by tiny3d-instance.
// cam-pos/cam-orient: (list x y z) / (list ax ay az) in world units / degrees.
static lbm_value ext_tiny3d_render(lbm_value *args, lbm_uint argn) {
  if (argn != 4) return ENC_SYM_TERROR;

  tiny3d_state_blob_t *blob = resolve_state(args[0]);
  if (!blob) return ENC_SYM_TERROR;
  if (!lbm_is_list(args[1])) return ENC_SYM_TERROR;

  tiny3d_pos_t cam_pos;
  tiny3d_orient_t cam_orient;
  if (!decode_vec3(args[2], &cam_pos)) return ENC_SYM_TERROR;
  if (!decode_orient(args[3], &cam_orient)) return ENC_SYM_TERROR;

  render_ctx_t ctx = { .cursor = args[1] };
  tiny3d_render(&blob->state,
                render_ctx_get_mesh, &ctx,
                render_ctx_next_instance, &ctx,
                cam_pos, cam_orient);
  return ENC_SYM_TRUE;
}

// Culls a single object.
// TODO: Maybe an extension that loops through an entire list of objects
// and returns a list of those that survive the culling.
static lbm_value ext_tiny3d_cull(lbm_value *args, lbm_uint argn) {
  if (argn != 4) return ENC_SYM_TERROR;

  tiny3d_state_blob_t *blob = resolve_state(args[0]);
  if (!blob) return ENC_SYM_TERROR;

  if (!lbm_is_cons(args[1])) return ENC_SYM_TERROR;
  tiny3d_instance_blob_t *inst_blob = get_instance_blob(lbm_car(args[1]));
  tiny3d_mesh_header_t *mh = get_mesh_header(lbm_cdr(args[1]));
  if (!inst_blob || !mh) return ENC_SYM_TERROR;

  tiny3d_instance_t inst = {0};
  inst.pos = inst_blob->pos;
  inst.orient = inst_blob->orient;
  inst.scale = inst_blob->scale;

  tiny3d_pos_t cam_pos;
  tiny3d_orient_t cam_orient;
  if (!decode_vec3(args[2], &cam_pos)) return ENC_SYM_TERROR;
  if (!decode_orient(args[3], &cam_orient)) return ENC_SYM_TERROR;

  int32_t depth;
  bool visible = tiny3d_transform_cull(&blob->state, inst, mh->bounding_radius,
                                        cam_pos, cam_orient, &depth);
  if (!visible) return ENC_SYM_NIL;
  return lbm_enc_float((float)depth / 65536.0f);
}

void lbm_tiny3d_extensions_init(void) {
  lbm_add_symbol_const("filled", &symbol_filled);
  lbm_add_symbol_const("no-backface-cull", &symbol_no_backface_cull);

  lbm_add_extension("tiny3d-mesh", ext_tiny3d_mesh);
  lbm_add_extension("tiny3d-mesh?", ext_tiny3d_is_mesh);
  lbm_add_extension("tiny3d-instance", ext_tiny3d_instance);
  lbm_add_extension("tiny3d-instance?", ext_tiny3d_is_instance);
  lbm_add_extension("tiny3d-instance-set-pos", ext_tiny3d_instance_set_pos);
  lbm_add_extension("tiny3d-instance-set-orient", ext_tiny3d_instance_set_orient);
  lbm_add_extension("tiny3d-instance-set-transform", ext_tiny3d_instance_set_transform);
  lbm_add_extension("tiny3d-instance-set-scale", ext_tiny3d_instance_set_scale);
  lbm_add_extension("tiny3d-instance-pos", ext_tiny3d_instance_pos);
  lbm_add_extension("tiny3d-instance-orient", ext_tiny3d_instance_orient);
  lbm_add_extension("tiny3d-instance-scale", ext_tiny3d_instance_scale);
  lbm_add_extension("tiny3d-mesh-vertex-count", ext_tiny3d_mesh_vertex_count);
  lbm_add_extension("tiny3d-mesh-triangle-count", ext_tiny3d_mesh_triangle_count);
  lbm_add_extension("tiny3d-mesh-bounding-radius", ext_tiny3d_mesh_bounding_radius);
  lbm_add_extension("tiny3d-state-create", ext_tiny3d_state_create);
  lbm_add_extension("tiny3d-render", ext_tiny3d_render);
  lbm_add_extension("tiny3d-cull", ext_tiny3d_cull);
}
