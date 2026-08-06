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
  based very loosely on "Black art of 3D Game Programming" by André Lamothe.

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
#include "cos_table.h"
#include <math.h>

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
  int32_t c = q1_15_to_q16_16(cos_q1_15(ang_q9_7));
  int32_t s = q1_15_to_q16_16(sin_q1_15(ang_q9_7));
  matrix3x4_t r = identity3x4();
  r.m[5] =  c; r.m[6]  = -s;
  r.m[9] =  s; r.m[10] =  c;
  return r;
}

static matrix3x4_t rotation_y3x4(uint16_t ang_q9_7) {
  int32_t c = q1_15_to_q16_16(cos_q1_15(ang_q9_7));
  int32_t s = q1_15_to_q16_16(sin_q1_15(ang_q9_7));
  matrix3x4_t r = identity3x4();
  r.m[0] =  c; r.m[2]  = s;
  r.m[8] = -s; r.m[10] = c;
  return r;
}

static matrix3x4_t rotation_z3x4(uint16_t ang_q9_7) {
  int32_t c = q1_15_to_q16_16(cos_q1_15(ang_q9_7));
  int32_t s = q1_15_to_q16_16(sin_q1_15(ang_q9_7));
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

static tiny3d_vec_t mat_apply3x4(matrix3x4_t m, tiny3d_vec_t v) {
  return (tiny3d_vec_t){
    q16_16_mul(m.m[0], v.x) + q16_16_mul(m.m[1], v.y) + q16_16_mul(m.m[2],  v.z) + m.m[3],
    q16_16_mul(m.m[4], v.x) + q16_16_mul(m.m[5], v.y) + q16_16_mul(m.m[6],  v.z) + m.m[7],
    q16_16_mul(m.m[8], v.x) + q16_16_mul(m.m[9], v.y) + q16_16_mul(m.m[10], v.z) + m.m[11]
  };
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
                 bool cull_backfaces) {
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

static tiny3d_vec_t clip_edge(tiny3d_vec_t inside, tiny3d_vec_t outside, int32_t near) {
  int32_t t = q16_16_div(near - inside.z, outside.z - inside.z);
  return (tiny3d_vec_t){
    inside.x + q16_16_mul(t, outside.x - inside.x),
    inside.y + q16_16_mul(t, outside.y - inside.y),
    inside.z + q16_16_mul(t, outside.z - inside.z)
  };
}

static int clip_near(tiny3d_camera_tri_t tri, int32_t near, tiny3d_camera_tri_t out[2]) {
  tiny3d_vec_t v[3] = { tri.v0, tri.v1, tri.v2 };
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
    tiny3d_vec_t pa = clip_edge(v[i_in], v[i_a], near);
    tiny3d_vec_t pb = clip_edge(v[i_in], v[i_b], near);
    out[0] = (tiny3d_camera_tri_t){ .v0 = v[i_in], .v1 = pa, .v2 = pb, .color = tri.color };
    return 1;
  }
  int i_out = !inside[0] ? 0 : (!inside[1] ? 1 : 2);
  int i_a   = (i_out + 1) % 3;
  int i_b   = (i_out + 2) % 3;
  tiny3d_vec_t ia = clip_edge(v[i_a], v[i_out], near);
  tiny3d_vec_t ib = clip_edge(v[i_b], v[i_out], near);
  out[0] = (tiny3d_camera_tri_t){ .v0 = ia, .v1 = v[i_a], .v2 = v[i_b], .color = tri.color };
  out[1] = (tiny3d_camera_tri_t){ .v0 = ia, .v1 = v[i_b], .v2 = ib,     .color = tri.color };
  return 2;
}

// //////////////////////////////////////////////////
// Pipeline

static void render_instance(tiny3d_state_t *state, const tiny3d_instance_t *inst,
                             matrix3x4_t world_to_camera,
                             tiny3d_get_mesh_fn get_mesh, void *mesh_ctx) {
  tiny3d_mesh_t mesh = get_mesh(inst->mesh_index, mesh_ctx);
  if (mesh.triangle_count == 0) return;

  matrix3x4_t l2c = local_to_camera3x4(inst->pos, inst->orient, inst->scale, world_to_camera);

  int32_t effective_radius = q16_16_mul(mesh.bounding_radius, inst->scale);
  if (cull_instance(state, l2c, effective_radius)) return;

  uint16_t out_count = 0;
  for (uint16_t i = 0; i < mesh.triangle_count && out_count < state->tri_buffer_cap; i++) {
    const tiny3d_triangle_t *t = &mesh.triangles[i];
    tiny3d_vec_t v0 = mat_apply3x4(l2c, mesh.vertices[t->i0]);
    tiny3d_vec_t v1 = mat_apply3x4(l2c, mesh.vertices[t->i1]);
    tiny3d_vec_t v2 = mat_apply3x4(l2c, mesh.vertices[t->i2]);

    if (state->cull_backfaces && is_backface(v0, v1, v2)) continue;

    state->tri_buffer[out_count] = (tiny3d_camera_tri_t){ v0, v1, v2, t->color };
    out_count++;
  }

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

  tiny3d_instance_t inst;
  while (next_instance(instance_ctx, &inst)) {
    render_instance(state, &inst, world_to_camera, get_mesh, mesh_ctx);
  }
}

