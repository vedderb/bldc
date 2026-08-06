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


#ifndef TINY3D_H_
#define TINY3D_H_

#include <stdint.h>
#include <stdbool.h>
#include "tinygfx.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct  {
  int32_t x;
  int32_t y;
  int32_t z;
} tiny3d_vec_t;

typedef tiny3d_vec_t tiny3d_pos_t;

// Orientation are q9_7 fixed point.
// If another format of orientation is required, the cos_table.h code
// needs to be updated.
typedef struct  {
  uint16_t ang_x;
  uint16_t ang_y;
  uint16_t ang_z;
} tiny3d_orient_t;

// color is a raw TinyGFX color value either an index or
// the correct RGB888
typedef struct {
  uint16_t i0, i1, i2;
  uint16_t _pad;
  uint32_t color;
} tiny3d_triangle_t;

typedef struct {
  const tiny3d_vec_t *vertices;
  uint16_t vertex_count;
  const tiny3d_triangle_t *triangles;
  uint16_t triangle_count;
  int32_t bounding_radius;
} tiny3d_mesh_t;

// The user of Tiny3D provides a get_mesh function. This is
// so that Tiny3D does not need to care at all about how or where
// meshes are stored.
typedef tiny3d_mesh_t (*tiny3d_get_mesh_fn)(uint16_t index, void *ctx);

// Q16.16 value representing a scale factor of 1.0 
#define TINY3D_SCALE_ONE (1 << 16)

typedef struct {
  uint16_t mesh_index;
  tiny3d_pos_t pos;
  tiny3d_orient_t orient;
  int32_t scale;
} tiny3d_instance_t;

// The user of Tiny3D provides a next_instance function.
// Tiny3D does not need to know how or where your instance
// data is stored.
typedef bool (*tiny3d_next_instance_fn)(void *ctx, tiny3d_instance_t *out);

// triangles_to_render entry: camera-space, post local->camera, pre-clip.
typedef struct {
  tiny3d_vec_t v0, v1, v2;
  uint32_t color;
} tiny3d_camera_tri_t;

// Camera-space frustum plane.
// Unit normal + distance.
typedef struct {
  tiny3d_vec_t normal;
  int32_t d;
} tiny3d_plane_t;

typedef struct {
  image_buffer_t       *img;
  tiny3d_camera_tri_t  *tri_buffer;
  uint16_t              tri_buffer_cap;
  int32_t               near;
  int32_t               far;
  int32_t               focal_length_x;
  int32_t               focal_length_y;
  tiny3d_plane_t        planes[6];
  int32_t               cull_margin;
  bool                  wireframe;
  bool                  cull_backfaces;
} tiny3d_state_t;

bool tiny3d_init(tiny3d_state_t *state,
                  image_buffer_t *img,
                  tiny3d_camera_tri_t *tri_buffer, uint32_t tri_buffer_size_bytes,
                  int32_t near, int32_t far,
                  float fov_degrees,
                  int32_t cull_margin,
                  bool wireframe,
                  bool cull_backfaces);

bool tiny3d_transform_cull(const tiny3d_state_t *state,
                            tiny3d_instance_t instance,
                            int32_t bounding_radius,
                            tiny3d_pos_t cam_pos, tiny3d_orient_t cam_orient,
                            int32_t *out_depth);

void tiny3d_render(tiny3d_state_t *state,
                    tiny3d_get_mesh_fn get_mesh, void *mesh_ctx,
                    tiny3d_next_instance_fn next_instance, void *instance_ctx,
                    tiny3d_pos_t cam_pos, tiny3d_orient_t cam_orient);

#ifdef __cplusplus
}
#endif

#endif
