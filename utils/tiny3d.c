/*
  Copyright 2026 Joel Svensson              svenssonjoel@yahoo.se

  Tiny3D is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Tiny3D is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
  Tiny3D is a small 3d graphics library for embedded platforms
  based loosely on "Black art of 3D Game Programming"-era 3d graphics techiques.

  Target platforms are small resource constrained embedded system
  that may not even have an FPU. So the situation may not be
  entirely different from the time when BAO3GP was written ;)
  Float and libm will be avoided in the "hot" parts but fine in
  called-once functions.

  Tiny3D is designed specifically to be easily integrated as
  extensions to LispBM but also keeping a somewhat useable C api.
  Whereever Tiny3D is integrated, that integration is responisble
  for all object lifetime and memory management.

  SCOPE:
   Aim at rendering of simple 3D objects, not complex 3d environments.
   The 3D objects exist in a 3d space:
     - This file should supply view frustrum culling of objects.
       - Objects are culled against the view frustrum
       - when decomposed into camera-coordinate polygons, ony clip against near-plane.
     - local -> world -> camera coordinate transformations.
       - Stream. The final perspective projection and render using fixed size buffers. 
     - larger environmental features will be prerendered images or solid fills.

  Pipeline plan:
    1: - Datastructure of object is passed in (accessible via an indexing function).
       - list of instances passed in (accessible via an iterator)
       - Camera orientation and position is passed in.
    2: transformation, culling and clipping pipeline:
       - instance is translated to world coordinates and to camera corrdinates.
       - instance is clipped against view frustrum. view frustrum rejects entire objects.
       - if object is unclipped:
           - cull backfaces
           - add remaining triangles to the "triangles_to_render" datastructure.
             (fixed small buffer with just enough room for the triangles of an object)
         else process next instance.
       - near plane clip per triangle.
       - screen project remaining triangles.
       - render the triangles using TinyGFX.
       - process the next instance (until done)

  Additional stuff and parameters
    - An intialization function that takes an image to draw onto (pixels w,h,colordepth)
    - An array to use as the triangles_to_render datastructure (pointer + size bytes). 
    - Aspect ratio is calulated from w,h.
    - desired near, far clipping planes.
    - Focal-length passed in as a field of view in degrees.
    - ..

*/

#include "tiny3d.h"
#include "tinyutils.h"
#include "cos_table.h"
#include <math.h>
#include <stdlib.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// //////////////////////////////////////////////////
// Matrices and their operations

typedef struct {
  int32_t  m[12];
} matrix3x4_t;

// matrix3x4_t is row-major.
//
//   | m0  m1  m2  | m3  |
//   | m4  m5  m6  | m7  |
//   | m8  m9  m10 | m11 |
//

// The 3x3 submatrix represents rotation/scaling and the
// column-vector at the right hand side represends a translation.
// All entries are Q16.16.

static inline int32_t q16_16_mul(int32_t a, int32_t b) {
  return (int32_t)(((int64_t)a * (int64_t)b) >> 16);
}

//convert Q1.15 to Q16.16
static inline int32_t q1_15_to_q16_16(int16_t q1_15) {
  return ((int32_t)q1_15) << 1;
}

static matrix3x4_t identity3x4(void) {
  matrix3x4_t r = {0};
  r.m[0]  = 1 << 16;
  r.m[5]  = 1 << 16;
  r.m[10] = 1 << 16;
  return r;
}

static matrix3x4_t rotation_x3x4(uint16_t ang_q9_7) {
  int32_t c = q1_15_to_q16_16(cos_lerp_q1_15(ang_q9_7));
  int32_t s = q1_15_to_q16_16(sin_lerp_q1_15(ang_q9_7));
  matrix3x4_t r = identity3x4();
  r.m[5] =  c; r.m[6]  = -s;
  r.m[9] =  s; r.m[10] =  c;
  return r;
}

static matrix3x4_t rotation_y3x4(uint16_t ang_q9_7) {
  int32_t c = q1_15_to_q16_16(cos_lerp_q1_15(ang_q9_7));
  int32_t s = q1_15_to_q16_16(sin_lerp_q1_15(ang_q9_7));
  matrix3x4_t r = identity3x4();
  r.m[0] =  c; r.m[2]  = s;
  r.m[8] = -s; r.m[10] = c;
  return r;
}

static matrix3x4_t rotation_z3x4(uint16_t ang_q9_7) {
  int32_t c = q1_15_to_q16_16(cos_lerp_q1_15(ang_q9_7));
  int32_t s = q1_15_to_q16_16(sin_lerp_q1_15(ang_q9_7));
  matrix3x4_t r = identity3x4();
  r.m[0] = c; r.m[1] = -s;
  r.m[4] = s; r.m[5] =  c;
  return r;
}

// Composition of 3x4 matrices is not quite matrix mult.
// It is a matrix3x3 multiply with some fixing up for the
// missing fourth row that is always 0 0 0 1.
static matrix3x4_t compose3x4(matrix3x4_t a, matrix3x4_t b) {
  matrix3x4_t r;
  for (int row = 0; row < 3; row ++) {
    for (int col = 0; col < 3; col ++) {
      int32_t sum = 0;
      for (int k = 0; k < 3; k ++) {
        sum += q16_16_mul(a.m[row * 4 + k], b.m[k * 4 + col]);
      }
      r.m[row * 4 + col] = sum;
    }
    int32_t t = a.m[row * 4 + 3];
    for (int k = 0; k < 3; k ++) {
      t += q16_16_mul(a.m[row * 4 + k], b.m[k * 4 + 3]);
    }
    r.m[row * 4 + 3] = t;
  }
  return r;
}

// //////////////////////////////////////////////////
// Local to world
//
// Sets up a local coordinate to world coordinate transformation matrix that performs
// rotation around x, y then z.
// scaling (as each instance has a unique scaling).
// translation.
static matrix3x4_t local_to_world3x4(tiny3d_pos_t pos, tiny3d_orient_t orient, int32_t scale) {
  // rotations
  matrix3x4_t rx = rotation_x3x4(orient.ang_x);
  matrix3x4_t ry = rotation_y3x4(orient.ang_y);
  matrix3x4_t rz = rotation_z3x4(orient.ang_z);
  matrix3x4_t r = compose3x4(rz, compose3x4(ry, rx));
  //scaling
  for (int row = 0; row < 3; row++) {
    for (int col = 0; col < 3; col++) {
      r.m[row * 4 + col] = q16_16_mul(r.m[row * 4 + col], scale);
    }
  }
  //translation
  r.m[3]  = pos.x;
  r.m[7]  = pos.y;
  r.m[11] = pos.z;
  return r;
}


// //////////////////////////////////////////////////
// World to camera
//
// Sets up a world coordinate to camera-space coordinate transformation.
//
//  |           /                  |
//  | .   .    /            \      |.     /
//  |          C   .     =>   \    |    /
//  |   .       \               \  |  /
//  |         .  \                \|/
//  |__________________            C___________________
//
//  Conceptually the camera is somewhere in space looking in some direction
//  and "sees" some objects. This transformation is used to compute each objects
//  coordinate in relation to the camera position, or in some sense into a coordinate
//  system with the camera at origin looking in direction positive Z?
//
//  Having the objects in this coordinate system is a prerequisite to performing
//  view frustum culling.

static matrix3x4_t world_to_camera3x4(tiny3d_pos_t cam_pos, tiny3d_orient_t cam_orient) {
  matrix3x4_t cam_to_world = local_to_world3x4(cam_pos, cam_orient, TINY3D_SCALE_ONE);
  matrix3x4_t r;
  // Inverse equals transpose for orthonormal matrices.
  r.m[0] = cam_to_world.m[0]; r.m[1] = cam_to_world.m[4]; r.m[2]  = cam_to_world.m[8];
  r.m[4] = cam_to_world.m[1]; r.m[5] = cam_to_world.m[5]; r.m[6]  = cam_to_world.m[9];
  r.m[8] = cam_to_world.m[2]; r.m[9] = cam_to_world.m[6]; r.m[10] = cam_to_world.m[10];

  int32_t tx = cam_to_world.m[3], ty = cam_to_world.m[7], tz = cam_to_world.m[11];
  r.m[3]  = -(q16_16_mul(r.m[0], tx) + q16_16_mul(r.m[1], ty) + q16_16_mul(r.m[2],  tz));
  r.m[7]  = -(q16_16_mul(r.m[4], tx) + q16_16_mul(r.m[5], ty) + q16_16_mul(r.m[6],  tz));
  r.m[11] = -(q16_16_mul(r.m[8], tx) + q16_16_mul(r.m[9], ty) + q16_16_mul(r.m[10], tz));
  return r;
}

// //////////////////////////////////////////////////
// Local to camera
//
// The composition of local to world and world to camera is
// the local to camera transformation.

static matrix3x4_t local_to_camera3x4(tiny3d_pos_t local_pos,
                                      tiny3d_orient_t local_orient,
                                      int32_t scale,
                                      matrix3x4_t world_to_camera) {
  matrix3x4_t l2w = local_to_world3x4(local_pos, local_orient, scale);
  return compose3x4(world_to_camera, l2w);
}

static tiny3d_vec_t mat_rotate3x4(matrix3x4_t m, tiny3d_vec_t v) {
  return (tiny3d_vec_t){
    q16_16_mul(m.m[0], v.x) + q16_16_mul(m.m[1], v.y) + q16_16_mul(m.m[2],  v.z),
    q16_16_mul(m.m[4], v.x) + q16_16_mul(m.m[5], v.y) + q16_16_mul(m.m[6],  v.z),
    q16_16_mul(m.m[8], v.x) + q16_16_mul(m.m[9], v.y) + q16_16_mul(m.m[10], v.z)
  };
}

static tiny3d_vec_t mat_apply3x4(matrix3x4_t m, tiny3d_vec_t v) {
  tiny3d_vec_t r = mat_rotate3x4(m, v);
  return (tiny3d_vec_t){ r.x + m.m[3], r.y + m.m[7], r.z + m.m[11] };
}

// //////////////////////////////////////////////////
// State / init

// Construct a plane  (used in view frustum culling).
// As the frustum is set up once we use float operations
// before converting to Q16.16
static tiny3d_plane_t side_plane_q16_16(float nx, float ny, float nz) {
  double len = sqrtf(nx * nx + ny * ny + nz * nz);
  tiny3d_plane_t p;
  p.normal.x = (int32_t)lround((nx / len) * 65536.0f);
  p.normal.y = (int32_t)lround((ny / len) * 65536.0f);
  p.normal.z = (int32_t)lround((nz / len) * 65536.0f);
  p.d = 0;
  return p;
}

bool tiny3d_init(tiny3d_state_t *state,
                 image_buffer_t *img,
                 tiny3d_camera_tri_t *tri_buffer, uint32_t tri_buffer_size_bytes,
                 int32_t near, int32_t far,
                 float fov_degrees,
                 int32_t cull_margin,
                 bool wireframe,
                 bool cull_backfaces,
                 const tiny3d_vec_t *light_source,
                 int32_t ambient) {
  if (!state || !img || !tri_buffer) return false;
  if (tri_buffer_size_bytes < sizeof(tiny3d_camera_tri_t)) return false;
  if (near <= 0 || far <= near) return false;
  if (fov_degrees <= 0.0f || fov_degrees >= 180.0f) return false;
  if (img->width == 0 || img->height == 0) return false;

  state->img             = img;
  state->tri_buffer      = tri_buffer;
  state->tri_buffer_cap  = (uint16_t)(tri_buffer_size_bytes / sizeof(tiny3d_camera_tri_t));
  state->near            = near;
  state->far             = far;
  state->cull_margin     = cull_margin;
  state->wireframe       = wireframe;
  state->cull_backfaces  = cull_backfaces;

  float half_fov_rad = fov_degrees * ((float)M_PI / 180.0f) * 0.5f;
  float focal_y = 1.0f / tanf(half_fov_rad);
  float focal_x = focal_y * (float)img->height / (float)img->width;
  state->focal_length_y = (int32_t)lround(focal_y * 65536.0);
  state->focal_length_x = (int32_t)lround(focal_x * 65536.0);

  // light_source
  state->light_source = light_source; // Either null or a light_source

  if (ambient < 0) ambient = 0;
  if (ambient > TINY3D_SCALE_ONE) ambient = TINY3D_SCALE_ONE;
  state->ambient = ambient;

  switch (img->fmt) {
  case indexed2:  state->shade_mode = TINY3D_SHADE_INDEX; state->index_max = 1;  break;
  case indexed4:  state->shade_mode = TINY3D_SHADE_INDEX; state->index_max = 3;  break;
  case indexed16: state->shade_mode = TINY3D_SHADE_INDEX; state->index_max = 15; break;
  case rgb332:
  case rgb565:
  case rgb888:    state->shade_mode = TINY3D_SHADE_RGB;   state->index_max = 0;  break;
  default:        state->shade_mode = TINY3D_SHADE_NONE;  state->index_max = 0;  break;
  }

  state->dither = TINY3D_DITHER_NONE; // enabled later via a setter, if at all

  // Camera-space view frustum
  state->planes[0] = (tiny3d_plane_t){ .normal = {0, 0,  (1 << 16)}, .d = near };
  state->planes[1] = (tiny3d_plane_t){ .normal = {0, 0, -(1 << 16)}, .d = -far };
  state->planes[2] = side_plane_q16_16( focal_x, 0.0, 1.0); // left
  state->planes[3] = side_plane_q16_16(-focal_x, 0.0, 1.0); // right
  state->planes[4] = side_plane_q16_16(0.0, -focal_y, 1.0); // top
  state->planes[5] = side_plane_q16_16(0.0,  focal_y, 1.0); // bottom
  return true;
}

static bool sphere_outside_frustum(const tiny3d_plane_t planes[6], tiny3d_vec_t center, int32_t radius) {
  for (int i = 0; i < 6; i++) {
    int32_t dist = q16_16_mul(center.x, planes[i].normal.x)
                 + q16_16_mul(center.y, planes[i].normal.y)
                 + q16_16_mul(center.z, planes[i].normal.z)
                 - planes[i].d;
    if (dist < -radius) return true;
  }
  return false;
}


static bool cull_instance(const tiny3d_state_t *state,
                          matrix3x4_t local_to_camera,
                          int32_t bounding_radius) {
  tiny3d_vec_t center = { local_to_camera.m[3], local_to_camera.m[7], local_to_camera.m[11] };
  return sphere_outside_frustum(state->planes, center, bounding_radius + state->cull_margin);
}

bool tiny3d_transform_cull(const tiny3d_state_t *state,
                            tiny3d_instance_t instance,
                            int32_t bounding_radius,
                            tiny3d_pos_t cam_pos, tiny3d_orient_t cam_orient,
                            int32_t *out_depth) {
  matrix3x4_t world_to_camera = world_to_camera3x4(cam_pos, cam_orient);
  matrix3x4_t l2c = local_to_camera3x4(instance.pos, instance.orient, instance.scale, world_to_camera);

  // apply the instance-specific scaling to the radius!
  int32_t effective_radius = q16_16_mul(bounding_radius, instance.scale);
  if (cull_instance(state, l2c, effective_radius)) return false;

  if (out_depth) *out_depth = l2c.m[11];
  return true;
}

// //////////////////////////////////////////////////
// Per-triangle pipeline: backface culling and projection

static inline int32_t q16_16_div(int32_t a, int32_t b) {
  return (int32_t)(((int64_t)a << 16) / b);
}

static tiny3d_vec_t vec_sub(tiny3d_vec_t a, tiny3d_vec_t b) {
  return (tiny3d_vec_t){ a.x - b.x, a.y - b.y, a.z - b.z };
}

static tiny3d_vec_t vec_cross(tiny3d_vec_t a, tiny3d_vec_t b) {
  return (tiny3d_vec_t){
    q16_16_mul(a.y, b.z) - q16_16_mul(a.z, b.y),
    q16_16_mul(a.z, b.x) - q16_16_mul(a.x, b.z),
    q16_16_mul(a.x, b.y) - q16_16_mul(a.y, b.x)
  };
}

static int32_t vec_dot(tiny3d_vec_t a, tiny3d_vec_t b) {
  return q16_16_mul(a.x, b.x) + q16_16_mul(a.y, b.y) + q16_16_mul(a.z, b.z);
}

static int32_t inv_scale_squared(int32_t scale) {
  int32_t scale_sq = q16_16_mul(scale, scale);
  if (scale_sq == 0) return 0;
  return q16_16_div(TINY3D_SCALE_ONE, scale_sq);
}

// A face normal is a cross product of two already-scaled (post
// mat_apply3x4) edge vectors, so it carries scale^2 - inv_scale_squared
// above cancels that. A vertex normal is instead a single direction
// rotated by mat_rotate3x4, which (local_to_world3x4 folds scale into
// the rotation sub-matrix) carries exactly one factor of scale, so it
// needs this plain reciprocal instead.
static int32_t inv_scale_linear(int32_t scale) {
  if (scale == 0) return 0;
  return q16_16_div(TINY3D_SCALE_ONE, scale);
}

// Shared by the flat (one normal/triangle) and Gouraud (one normal/vertex)
// paths - n and inv_scale_factor differ (face cross-product + inv_scale_squared
// vs a single rotated vertex normal + inv_scale_linear, see above), the
// diffuse+ambient blend itself is identical either way.
static int32_t lit_intensity(tiny3d_vec_t n, tiny3d_vec_t light_cam,
                              int32_t one_over_abs_n, int32_t inv_scale_factor,
                              int32_t ambient) {
  int32_t diffuse = q16_16_mul(q16_16_mul(vec_dot(n, light_cam), one_over_abs_n), inv_scale_factor);
  if (diffuse < 0) diffuse = 0;
  if (diffuse > TINY3D_SCALE_ONE) diffuse = TINY3D_SCALE_ONE;
  int32_t intensity = ambient + q16_16_mul(TINY3D_SCALE_ONE - ambient, diffuse);
  if (intensity > TINY3D_SCALE_ONE) intensity = TINY3D_SCALE_ONE;
  return intensity;
}

static uint32_t shade_rgb888(uint32_t color, int32_t intensity) {
  int32_t r = (int32_t)((color >> 16) & 0xFF);
  int32_t g = (int32_t)((color >> 8)  & 0xFF);
  int32_t b = (int32_t)(color         & 0xFF);
  r = (int32_t)(((int64_t)r * intensity) >> 16);
  g = (int32_t)(((int64_t)g * intensity) >> 16);
  b = (int32_t)(((int64_t)b * intensity) >> 16);
  return ((uint32_t)r << 16) | ((uint32_t)g << 8) | (uint32_t)b;
}

static uint32_t shade_index(int32_t intensity, int32_t index_max) {
  int32_t idx = (int32_t)((((int64_t)intensity * index_max) + (1 << 15)) >> 16);
  if (idx < 0) idx = 0;
  if (idx > index_max) idx = index_max;
  return (uint32_t)idx;
}

// Same mapping as shade_index, but instead of rounding to the nearest
// index it splits intensity into the two bracketing indices plus the
// Q16.16 fractional remainder between them, for ordered dithering.
static void shade_index_dither(int32_t intensity, int32_t index_max,
                                uint32_t *lo, uint32_t *hi, int32_t *ratio_q16) {
  int32_t scaled = (int32_t)(((int64_t)intensity * index_max)); // Q16.16, range [0, index_max<<16]
  if (scaled < 0) scaled = 0;
  int32_t max_q16 = index_max << 16;
  if (scaled > max_q16) scaled = max_q16;
  int32_t idx_lo = scaled >> 16;
  int32_t idx_hi = idx_lo < index_max ? idx_lo + 1 : idx_lo;
  *lo = (uint32_t)idx_lo;
  *hi = (uint32_t)idx_hi;
  *ratio_q16 = scaled & 0xFFFF; // fractional part between idx_lo and idx_hi
}

// The bayer matrices contains "intensity" levels.
// when a dithered pixel is drawn at position x,y with intensity, i,
// then the i is compared to the value in the tiled bayer matrix
// at position x,y and we pic to use hi-color (bright) or lo-color (dark)
// depending on i being larger than the bayer value or not.

// Bayer matrix is an even spread of intensities over 2d area (an image)
static const uint8_t bayer_2x2[2][2] = {
  {0, 2},
  {3, 1},
};

static const uint8_t bayer_4x4[4][4] = {
  { 0,  8,  2, 10},
  {12,  4, 14,  6},
  { 3, 11,  1,  9},
  {15,  7, 13,  5},
};

static const uint8_t bayer_8x8[8][8] = {
  { 0, 32,  8, 40,  2, 34, 10, 42},
  {48, 16, 56, 24, 50, 18, 58, 26},
  {12, 44,  4, 36, 14, 46,  6, 38},
  {60, 28, 52, 20, 62, 30, 54, 22},
  { 3, 35, 11, 43,  1, 33,  9, 41},
  {51, 19, 59, 27, 49, 17, 57, 25},
  {15, 47,  7, 39, 13, 45,  5, 37},
  {63, 31, 55, 23, 61, 29, 53, 21},
};

static inline bool dither_pick(int x, int y, int32_t ratio_q16, tiny3d_dither_t size) {
  int32_t n, v;
  switch (size) {
  case TINY3D_DITHER_2: n = 2; v = bayer_2x2[y & 1][x & 1]; break;
  case TINY3D_DITHER_4: n = 4; v = bayer_4x4[y & 3][x & 3]; break;
  case TINY3D_DITHER_8: n = 8; v = bayer_8x8[y & 7][x & 7]; break;
  default: return false; // TINY3D_DITHER_NONE
  }
  int32_t nn = n * n; // number of intensities in the bayer matrix.
  int32_t intensity = ratio_q16 * nn;
  int32_t bayer = (v << 16) + 32768; // Compare in q16.16 format.
  return intensity > bayer;
}

// Same scanline structure as tinygfx_fill_triangle (utils/tinygfx.c),
// but the color can differ at every pixel, so it cannot use h_line's
// flat-run write and instead calls putpixel directly per pixel.
static void fill_triangle_dither(image_buffer_t *img, int x0, int y0,
                                  int x1, int y1, int x2, int y2,
                                  uint32_t color_lo, uint32_t color_hi,
                                  int32_t ratio_q16, tiny3d_dither_t size) {
  if (y0 > y1) swap_points(&x0, &y0, &x1, &y1);
  if (y1 > y2) swap_points(&x1, &y1, &x2, &y2);
  if (y0 > y1) swap_points(&x0, &y0, &x1, &y1);

  if (y0 == y2) return;

  int32_t dx_long = tri_slope_fp(x0, x2, y0, y2);
  int32_t x_long = x0 * 256;

  if (y1 > y0) {
    int32_t dx_short = tri_slope_fp(x0, x1, y0, y1);
    int32_t x_short = x0 * 256;
    for (int y = y0; y < y1; y++) {
      int xa = (int)(x_long >> 8), xb = (int)(x_short >> 8);
      int lo = MIN(xa, xb), hi = MAX(xa, xb);
      for (int x = lo; x <= hi; x++) {
        putpixel(img, x, y, dither_pick(x, y, ratio_q16, size) ? color_hi : color_lo);
      }
      x_long += dx_long;
      x_short += dx_short;
    }
  }

  if (y2 > y1) {
    int32_t dx_short = tri_slope_fp(x1, x2, y1, y2);
    int32_t x_short = x1 * 256;
    for (int y = y1; y <= y2; y++) {
      int xa = (int)(x_long >> 8), xb = (int)(x_short >> 8);
      int lo = MIN(xa, xb), hi = MAX(xa, xb);
      for (int x = lo; x <= hi; x++) {
        putpixel(img, x, y, dither_pick(x, y, ratio_q16, size) ? color_hi : color_lo);
      }
      x_long += dx_long;
      x_short += dx_short;
    }
  } else {
    int xa = (int)(x_long >> 8), xb = x1;
    int lo = MIN(xa, xb), hi = MAX(xa, xb);
    for (int x = lo; x <= hi; x++) {
      putpixel(img, x, y1, dither_pick(x, y1, ratio_q16, size) ? color_hi : color_lo);
    }
  }
}


static inline void swap_val3(int32_t a[3], int32_t b[3]) {
  for (int k = 0; k < 3; k++) { int32_t t = a[k]; a[k] = b[k]; b[k] = t; }
}


// Essentially h_line for Gouraud
static void gouraud_row(image_buffer_t *img, int y, int xa, int xb,
                         const int32_t va[3], const int32_t vb[3], int n,
                         tiny3d_dither_t dither, int32_t index_max) {
  int lo = MIN(xa, xb), hi = MAX(xa, xb);
  const int32_t *v_lo = (xa <= xb) ? va : vb;
  const int32_t *v_hi = (xa <= xb) ? vb : va;
  int32_t dv[3] = {0, 0, 0};
  int32_t v[3] = {0, 0, 0};
  for (int k = 0; k < n; k++) {
    v[k] = v_lo[k] * 256;
    dv[k] = (hi > lo) ? tri_slope_fp(v_lo[k], v_hi[k], 0, hi - lo) : 0;
  }
  for (int x = lo; x <= hi; x++) {
    if (n == 3) {
      // RGB does not support dithering
      uint32_t r = (uint32_t)((v[0] + 128) >> 8);
      uint32_t g = (uint32_t)((v[1] + 128) >> 8);
      uint32_t b = (uint32_t)((v[2] + 128) >> 8);
      putpixel(img, x, y, (r << 16) | (g << 8) | b);
    } else if (dither == TINY3D_DITHER_NONE) {
      // Dithering is off
      int32_t resolved = (v[0] + 128) >> 8;      // back to Q16.16 index-space
      int32_t idx = (resolved + (1 << 15)) >> 16; // round to nearest index
      putpixel(img, x, y, (uint32_t)idx);
    } else {
      // Indexed format and dithering is on
      int32_t resolved = (v[0] + 128) >> 8;
      int32_t idx_lo = resolved >> 16;
      int32_t ratio_q16 = resolved & 0xFFFF;
      bool pick_hi = idx_lo < index_max && dither_pick(x, y, ratio_q16, dither);
      uint32_t idx = pick_hi ? (uint32_t)(idx_lo + 1) : (uint32_t)idx_lo;
      putpixel(img, x, y, idx);
    }
    for (int k = 0; k < n; k++) {
      v[k] += dv[k];
    }
  }
}

// Same edge-walk as in the fill_triangle function
static void fill_triangle_gouraud(image_buffer_t *img,
                                   int x0, int y0, int32_t val0[3],
                                   int x1, int y1, int32_t val1[3],
                                   int x2, int y2, int32_t val2[3],
                                   int n, tiny3d_dither_t dither, int32_t index_max) {
  if (y0 > y1) {
    swap_points(&x0, &y0, &x1, &y1);
    swap_val3(val0, val1);
  }
  if (y1 > y2) {
    swap_points(&x1, &y1, &x2, &y2);
    swap_val3(val1, val2);
  }
  if (y0 > y1) {
    swap_points(&x0, &y0, &x1, &y1);
    swap_val3(val0, val1);
  }

  if (y0 == y2) return;

  int32_t dx_long = tri_slope_fp(x0, x2, y0, y2);
  int32_t x_long = x0 * 256;
  int32_t dv_long[3] = {0, 0, 0};
  int32_t v_long[3] = {0, 0, 0};
  for (int k = 0; k < n; k++) {
    dv_long[k] = tri_slope_fp(val0[k], val2[k], y0, y2);
    v_long[k] = val0[k] * 256;
  }

  if (y1 > y0) {
    int32_t dx_short = tri_slope_fp(x0, x1, y0, y1);
    int32_t x_short = x0 * 256;
    int32_t dv_short[3] = {0, 0, 0};
    int32_t v_short[3] = {0, 0, 0};
    for (int k = 0; k < n; k++) {
      dv_short[k] = tri_slope_fp(val0[k], val1[k], y0, y1);
      v_short[k] = val0[k] * 256;
    }
    for (int y = y0; y < y1; y++) {
      int32_t rv_long[3];
      int32_t rv_short[3];
      for (int k = 0; k < n; k++) {
        rv_long[k] = v_long[k] >> 8;
        rv_short[k] = v_short[k] >> 8;
      }
      gouraud_row(img, y, (int)(x_long >> 8), (int)(x_short >> 8), rv_long, rv_short, n, dither, index_max);
      x_long += dx_long;
      x_short += dx_short;
      for (int k = 0; k < n; k++) {
        v_long[k] += dv_long[k];
        v_short[k] += dv_short[k];
      }
    }
  }

  if (y2 > y1) {
    int32_t dx_short = tri_slope_fp(x1, x2, y1, y2);
    int32_t x_short = x1 * 256;
    int32_t dv_short[3] = {0, 0, 0};
    int32_t v_short[3] = {0, 0, 0};
    for (int k = 0; k < n; k++) {
      dv_short[k] = tri_slope_fp(val1[k], val2[k], y1, y2);
      v_short[k] = val1[k] * 256;
    }
    for (int y = y1; y <= y2; y++) {
      int32_t rv_long[3];
      int32_t rv_short[3];
      for (int k = 0; k < n; k++) {
        rv_long[k] = v_long[k] >> 8;
        rv_short[k] = v_short[k] >> 8;
      }
      gouraud_row(img, y, (int)(x_long >> 8), (int)(x_short >> 8), rv_long, rv_short, n, dither, index_max);
      x_long += dx_long;
      x_short += dx_short;
      for (int k = 0; k < n; k++) {
        v_long[k] += dv_long[k];
        v_short[k] += dv_short[k];
      }
    }
  } else {
    int32_t rv_long[3];
    int32_t rv_end[3];
    for (int k = 0; k < n; k++) {
      rv_long[k] = v_long[k] >> 8;
      rv_end[k] = val1[k];
    }
    gouraud_row(img, y1, (int)(x_long >> 8), x1, rv_long, rv_end, n, dither, index_max);
  }
}

static void fill_triangle_gouraud_rgb(image_buffer_t *img,
                                       int x0, int y0, uint32_t color0,
                                       int x1, int y1, uint32_t color1,
                                       int x2, int y2, uint32_t color2) {
  int32_t val0[3] = { (int32_t)((color0 >> 16) & 0xFF), (int32_t)((color0 >> 8) & 0xFF), (int32_t)(color0 & 0xFF) };
  int32_t val1[3] = { (int32_t)((color1 >> 16) & 0xFF), (int32_t)((color1 >> 8) & 0xFF), (int32_t)(color1 & 0xFF) };
  int32_t val2[3] = { (int32_t)((color2 >> 16) & 0xFF), (int32_t)((color2 >> 8) & 0xFF), (int32_t)(color2 & 0xFF) };
  fill_triangle_gouraud(img, x0, y0, val0, x1, y1, val1, x2, y2, val2, 3, TINY3D_DITHER_NONE, 0);
}

static void fill_triangle_gouraud_index(image_buffer_t *img,
                                         int x0, int y0, int32_t value0,
                                         int x1, int y1, int32_t value1,
                                         int x2, int y2, int32_t value2) {
  int32_t val0[3] = { value0, 0, 0 };
  int32_t val1[3] = { value1, 0, 0 };
  int32_t val2[3] = { value2, 0, 0 };
  fill_triangle_gouraud(img, x0, y0, val0, x1, y1, val1, x2, y2, val2, 1, TINY3D_DITHER_NONE, 0);
}

static void fill_triangle_gouraud_index_dither(image_buffer_t *img,
                                                int x0, int y0, int32_t value0,
                                                int x1, int y1, int32_t value1,
                                                int x2, int y2, int32_t value2,
                                                tiny3d_dither_t size, int32_t index_max) {
  int32_t val0[3] = { value0, 0, 0 };
  int32_t val1[3] = { value1, 0, 0 };
  int32_t val2[3] = { value2, 0, 0 };
  fill_triangle_gouraud(img, x0, y0, val0, x1, y1, val1, x2, y2, val2, 1, size, index_max);
}

// Back faces are culled by a triangle winding order convention.
// The winding order comes into the computation of the triangle's
// normal. If this computed normal is pointing away from the
// camera the triangle should be culled.
static bool is_backface(tiny3d_vec_t v0, tiny3d_vec_t v1, tiny3d_vec_t v2) {
  tiny3d_vec_t normal = vec_cross(vec_sub(v1, v0), vec_sub(v2, v0));
  return vec_dot(normal, v0) > 0;
}

typedef struct { int32_t x, y; } screen_point_t;

// Perspective projection and transforming x = {-1, 1}, y = {1, -1}
// to x = {0, w}, y = {0, h}.
static screen_point_t project_to_screen(const tiny3d_state_t *state, tiny3d_vec_t v) {
  int32_t proj_x = q16_16_div(q16_16_mul(v.x, state->focal_length_x), v.z);
  int32_t proj_y = q16_16_div(q16_16_mul(v.y, state->focal_length_y), v.z);

  screen_point_t p;
  p.x = (int32_t)(((int64_t)(proj_x + (1 << 16)) * state->img->width)  >> 17);
  p.y = (int32_t)(((int64_t)((1 << 16) - proj_y) * state->img->height) >> 17);
  return p;
}

static tiny3d_vec_t clip_edge(tiny3d_vec_t inside, tiny3d_vec_t outside, int32_t near, int32_t *out_t) {
  int32_t t = q16_16_div(near - inside.z, outside.z - inside.z);
  *out_t = t;
  return (tiny3d_vec_t){
    inside.x + q16_16_mul(t, outside.x - inside.x),
    inside.y + q16_16_mul(t, outside.y - inside.y),
    inside.z + q16_16_mul(t, outside.z - inside.z)
  };
}

static int32_t lerp_q16(int32_t a, int32_t b, int32_t t) {
  return a + q16_16_mul(t, b - a);
}

// Per-vertex intensities interpolate the same way position does: when
// the triangle is not Gouraud-shaded, render_instance sets vi0/vi1/vi2
// all to -1 together (not just vi0), so lerp_q16(-1, -1, t) == -1 for
// any t and the sentinel survives clipping with no special-casing here.
static int clip_near(tiny3d_camera_tri_t tri, int32_t near, tiny3d_camera_tri_t out[2]) {
  tiny3d_vec_t v[3] = { tri.v0, tri.v1, tri.v2 };
  int32_t vi[3] = { tri.vi0, tri.vi1, tri.vi2 };
  bool inside[3] = { v[0].z >= near, v[1].z >= near, v[2].z >= near };
  int in_count = (inside[0] ? 1 : 0) + (inside[1] ? 1 : 0) + (inside[2] ? 1 : 0);

  if (in_count == 3) {
    out[0] = tri;
    return 1;
  }
  if (in_count == 0) {
    return 0;
  }
  if (in_count == 1) {
    int i_in = inside[0] ? 0 : (inside[1] ? 1 : 2);
    int i_a  = (i_in + 1) % 3;
    int i_b  = (i_in + 2) % 3;
    int32_t t_a, t_b;
    tiny3d_vec_t pa = clip_edge(v[i_in], v[i_a], near, &t_a);
    tiny3d_vec_t pb = clip_edge(v[i_in], v[i_b], near, &t_b);
    out[0] = (tiny3d_camera_tri_t){
      .v0 = v[i_in], .v1 = pa, .v2 = pb,
      .color = tri.color, .dither_ratio_q16 = tri.dither_ratio_q16,
      .vi0 = vi[i_in],
      .vi1 = lerp_q16(vi[i_in], vi[i_a], t_a),
      .vi2 = lerp_q16(vi[i_in], vi[i_b], t_b),
    };
    return 1;
  }
  int i_out = !inside[0] ? 0 : (!inside[1] ? 1 : 2);
  int i_a   = (i_out + 1) % 3;
  int i_b   = (i_out + 2) % 3;
  int32_t t_a, t_b;
  tiny3d_vec_t ia = clip_edge(v[i_a], v[i_out], near, &t_a);
  tiny3d_vec_t ib = clip_edge(v[i_b], v[i_out], near, &t_b);
  int32_t vi_a = lerp_q16(vi[i_a], vi[i_out], t_a);
  int32_t vi_b = lerp_q16(vi[i_b], vi[i_out], t_b);
  out[0] = (tiny3d_camera_tri_t){
    .v0 = ia, .v1 = v[i_a], .v2 = v[i_b],
    .color = tri.color, .dither_ratio_q16 = tri.dither_ratio_q16,
    .vi0 = vi_a, .vi1 = vi[i_a], .vi2 = vi[i_b],
  };
  out[1] = (tiny3d_camera_tri_t){
    .v0 = ia, .v1 = v[i_b], .v2 = ib,
    .color = tri.color, .dither_ratio_q16 = tri.dither_ratio_q16,
    .vi0 = vi_a, .vi1 = vi[i_b], .vi2 = vi_b,
  };
  return 2;
}

// //////////////////////////////////////////////////
// Pipeline

static int cmp_tri_depth_desc(const void *a, const void *b) {
  const tiny3d_camera_tri_t *ta = (const tiny3d_camera_tri_t*)a;
  const tiny3d_camera_tri_t *tb = (const tiny3d_camera_tri_t*)b;
  int32_t za = ta->v0.z + ta->v1.z + ta->v2.z;
  int32_t zb = tb->v0.z + tb->v1.z + tb->v2.z;
  if (za < zb) return 1;
  if (za > zb) return -1;
  return 0;
}

static void render_instance(tiny3d_state_t *state, const tiny3d_instance_t *inst,
                             matrix3x4_t world_to_camera, tiny3d_vec_t light_cam,
                             tiny3d_get_mesh_fn get_mesh, void *mesh_ctx) {
  tiny3d_mesh_t mesh = get_mesh(inst->mesh_index, mesh_ctx);
  if (mesh.triangle_count == 0) return;

  matrix3x4_t l2c = local_to_camera3x4(inst->pos, inst->orient, inst->scale, world_to_camera);
  int32_t inv_scale_sq = inv_scale_squared(inst->scale);
  int32_t inv_scale_lin = inv_scale_linear(inst->scale);
  bool lit = state->light_source && state->shade_mode != TINY3D_SHADE_NONE;
  bool gouraud = lit && mesh.normals != NULL;

  int32_t effective_radius = q16_16_mul(mesh.bounding_radius, inst->scale);
  if (cull_instance(state, l2c, effective_radius)) return;

  uint16_t out_count = 0;
  for (uint16_t i = 0; i < mesh.triangle_count && out_count < state->tri_buffer_cap; i++) {
    const tiny3d_triangle_t *t = &mesh.triangles[i];
    tiny3d_vec_t v0 = mat_apply3x4(l2c, mesh.vertices[t->i0]);
    tiny3d_vec_t v1 = mat_apply3x4(l2c, mesh.vertices[t->i1]);
    tiny3d_vec_t v2 = mat_apply3x4(l2c, mesh.vertices[t->i2]);

    if (state->cull_backfaces && is_backface(v0, v1, v2)) continue;

    uint32_t color = t->color;
    int32_t dither_ratio_q16 = -1;
    int32_t vi0 = -1, vi1 = -1, vi2 = -1;
    if (gouraud) {
      // Per-vertex normal, rotated (not translated) into camera space -
      // same reasoning as light_cam itself below. Base color stays
      // t->color (unshaded): the per-vertex intensities are carried
      // through clipping and only turned into a final color/index at
      // the fill call site, same as the flat dithered path does with
      // dither_ratio_q16.
      const tiny3d_normal_t *n0 = &mesh.normals[t->i0];
      const tiny3d_normal_t *n1 = &mesh.normals[t->i1];
      const tiny3d_normal_t *n2 = &mesh.normals[t->i2];
      vi0 = lit_intensity(mat_rotate3x4(l2c, n0->n), light_cam, n0->one_over_abs_n, inv_scale_lin, state->ambient);
      vi1 = lit_intensity(mat_rotate3x4(l2c, n1->n), light_cam, n1->one_over_abs_n, inv_scale_lin, state->ambient);
      vi2 = lit_intensity(mat_rotate3x4(l2c, n2->n), light_cam, n2->one_over_abs_n, inv_scale_lin, state->ambient);
    } else if (lit) {
      tiny3d_vec_t n = vec_cross(vec_sub(v1, v0), vec_sub(v2, v0));
      int32_t intensity = lit_intensity(n, light_cam, t->one_over_abs_n, inv_scale_sq, state->ambient);
      if (state->shade_mode == TINY3D_SHADE_RGB) {
        color = shade_rgb888(t->color, intensity);
      } else if (state->dither != TINY3D_DITHER_NONE) {
        uint32_t lo, hi;
        shade_index_dither(intensity, state->index_max, &lo, &hi, &dither_ratio_q16);
        color = lo;
      } else {
        color = shade_index(intensity, state->index_max);
      }
    }

    state->tri_buffer[out_count] = (tiny3d_camera_tri_t){ v0, v1, v2, color, dither_ratio_q16, vi0, vi1, vi2 };
    out_count++;
  }

  // The idea is to sort objects/meshes "roughly" after doing an local -> camera space
  // transformation. When a mesh is rendered, its triangles are culled and the remaining
  // ones are in a temporary buffer, that is sorted by distance here.
  qsort(state->tri_buffer, out_count, sizeof(tiny3d_camera_tri_t), cmp_tri_depth_desc);

  for (uint16_t i = 0; i < out_count; i++) {
    tiny3d_camera_tri_t clipped[2];
    int n = clip_near(state->tri_buffer[i], state->near, clipped);
    for (int c = 0; c < n; c++) {
      screen_point_t p0 = project_to_screen(state, clipped[c].v0);
      screen_point_t p1 = project_to_screen(state, clipped[c].v1);
      screen_point_t p2 = project_to_screen(state, clipped[c].v2);
      if (state->wireframe) {
        tinygfx_line(state->img, p0.x, p0.y, p1.x, p1.y, 1, 0, 0, clipped[c].color);
        tinygfx_line(state->img, p1.x, p1.y, p2.x, p2.y, 1, 0, 0, clipped[c].color);
        tinygfx_line(state->img, p2.x, p2.y, p0.x, p0.y, 1, 0, 0, clipped[c].color);
      } else if (clipped[c].vi0 >= 0) {
        if (state->shade_mode == TINY3D_SHADE_RGB) {
          uint32_t c0 = shade_rgb888(clipped[c].color, clipped[c].vi0);
          uint32_t c1 = shade_rgb888(clipped[c].color, clipped[c].vi1);
          uint32_t c2 = shade_rgb888(clipped[c].color, clipped[c].vi2);
          fill_triangle_gouraud_rgb(state->img, p0.x, p0.y, c0, p1.x, p1.y, c1, p2.x, p2.y, c2);
        } else {
          // Same Q16.16-index-space product shade_index itself rounds
          // (intensity * index_max), just left for the rasterizer to
          // interpolate and round (or dither) per pixel instead of once
          // per triangle.
          int32_t idx0 = clipped[c].vi0 * state->index_max;
          int32_t idx1 = clipped[c].vi1 * state->index_max;
          int32_t idx2 = clipped[c].vi2 * state->index_max;
          if (state->dither != TINY3D_DITHER_NONE) {
            fill_triangle_gouraud_index_dither(state->img, p0.x, p0.y, idx0, p1.x, p1.y, idx1,
                                                        p2.x, p2.y, idx2, state->dither, state->index_max);
          } else {
            fill_triangle_gouraud_index(state->img, p0.x, p0.y, idx0, p1.x, p1.y, idx1, p2.x, p2.y, idx2);
          }
        }
      } else if (clipped[c].dither_ratio_q16 >= 0) {
        uint32_t hi = clipped[c].color + 1;
        if (hi > (uint32_t)state->index_max) hi = (uint32_t)state->index_max;
        fill_triangle_dither(state->img, p0.x, p0.y, p1.x, p1.y, p2.x, p2.y,
                                      clipped[c].color, hi, clipped[c].dither_ratio_q16, state->dither);
      } else {
        tinygfx_fill_triangle(state->img, p0.x, p0.y, p1.x, p1.y, p2.x, p2.y, clipped[c].color);
      }
    }
  }
}

void tiny3d_render(tiny3d_state_t *state,
                   tiny3d_get_mesh_fn get_mesh, void *mesh_ctx,
                   tiny3d_next_instance_fn next_instance, void *instance_ctx,
                   tiny3d_pos_t cam_pos, tiny3d_orient_t cam_orient) {
  matrix3x4_t world_to_camera = world_to_camera3x4(cam_pos, cam_orient);
  tiny3d_vec_t light_cam = {0};
  if (state->light_source) {
    light_cam = mat_rotate3x4(world_to_camera, *state->light_source);
  }

  tiny3d_instance_t inst;
  while (next_instance(instance_ctx, &inst)) {
    render_instance(state, &inst, world_to_camera, light_cam, get_mesh, mesh_ctx);
  }
}

