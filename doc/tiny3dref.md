# LispBM Tiny3D Library

This document describes how to use the tiny3d_extensions to lispbm. Tiny3D is a single c/h file pair implementing a simple 3d pipeline that view-transforms, culls and renders instanced **objects** (meshes) Tiny3D itself does not allocate any memory and is not at all aware of the lifetimes of objects and image buffers. Managing image buffers and object lifetimes is left to the integrator, in this case LispBM. 

The renderer works with the same image buffers as the display library (see the Display library reference) - a scene is rendered into an `img-buffer`, which can then be shown with `disp-render` like any other image. Meshes work unmodified with indexed image buffers - a triangle's `color` is just an opaque value interpreted by the image's own format, index or 0xRRGGBB. 

The building blocks are: 

   - **mesh** - shared vertex/triangle geometry, created once with `tiny3d-mesh`.
   - **instance** - a positioned, oriented, optionally scaled placement of a mesh in the world; many instances can share one mesh.
   - **state** - a `tiny3d-state-create`d rendering context bound to a destination image buffer, near/far/fov and render options.

A minimal scene looks like: 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define cube-verts (list
  (list -1.0 -1.0 -1.0)   ; 0
  (list  1.0 -1.0 -1.0)   ; 1
  (list  1.0  1.0 -1.0)   ; 2
  (list -1.0  1.0 -1.0)   ; 3
  (list -1.0 -1.0  1.0)   ; 4
  (list  1.0 -1.0  1.0)   ; 5
  (list  1.0  1.0  1.0)   ; 6
  (list -1.0  1.0  1.0))) ; 7
```


</td>
<td>

```clj
((-1.000000f32 -1.000000f32 -1.000000f32) (1.000000f32 -1.000000f32 -1.000000f32) (1.000000f32 1.000000f32 -1.000000f32) (-1.000000f32 1.000000f32 -1.000000f32) (-1.000000f32 -1.000000f32 1.000000f32) (1.000000f32 -1.000000f32 1.000000f32) (1.000000f32 1.000000f32 1.000000f32) (-1.000000f32 1.000000f32 1.000000f32))
```


</td>
</tr>
</table>

The examples uses a function to create colored triangles: 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(defun cube-tris (color)
 (list
  (list 4 5 6 color) (list 4 6 7 color)
  (list 0 2 1 color) (list 0 3 2 color)
  (list 1 2 6 color) (list 1 6 5 color)
  (list 0 7 3 color) (list 0 4 7 color)
  (list 3 6 2 color) (list 3 7 6 color)
  (list 0 1 5 color) (list 0 5 4 color)))

```


</td>
<td>

```clj
(closure (color) 
  (list (list 4 5 6 color) (list 4 6 7 color) (list 0 2 1 color) (list 0 3 2 color) (list 1 2 6 color) (list 1 6 5 color) (list 0 7 3 color) (list 0 4 7 color) (list 3 6 2 color) (list 3 7 6 color) (list 0 1 5 color) (list 0 5 4 color))
  nil)
```


</td>
</tr>
</table>

A standalone object or **mesh** is then created from these vertices and triangles using `tiny3d-mesh`: 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(defun cube-mesh (color) (tiny3d-mesh cube-verts (cube-tris color)))
```


</td>
<td>

```clj
(closure (color) 
  (tiny3d-mesh cube-verts (cube-tris color))
  nil)
```


</td>
</tr>
</table>


# Reference


### tiny3d-mesh

Creates a mesh from a list of vertices and a list of index triangles. Vertices shared between triangles (for example a cube's 8 corners across its 12 triangles) are stored once, not duplicated per triangle. The form of a `tiny3d-mesh` expression is `(tiny3d-mesh vertices triangles)`. 

|Arg || 
 |----|----|
 `vertices`  | List of `(x y z)`, one entry per unique vertex.
 `triangles` | List of `(i0 i1 i2 color)` - `i0`/`i1`/`i2` index into `vertices`, `color` is a raw TinyGFX color (index or 0xRRGGBB).
 

A mesh's bounding radius (used for frustum culling) is computed automatically from the vertex furthest from the local origin. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-mesh cube-verts (cube-tris 0xE04030))
```


</td>
<td>

```clj
[0 77 68 51 8 0 12 0 104 187 1 0 0 0 255 255 0 0 255 255 0 0 255 255 0 0 1 0 0 0 255 255 0 0 255 255 0 0 1 0 0 0 1 0 0 0 255 255 0 0 255 255 0 0 1 0 0 0 255 255 0 0 255 255 0 0 255 255 0 0 1 0 0 0 1 0 0 0 255 255 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 255 255
```


</td>
</tr>
</table>




---


### tiny3d-mesh?

Checks if the argument is a tiny3d mesh. The form of a `tiny3d-mesh?` expression is `(tiny3d-mesh? v)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-mesh? my-mesh)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(tiny3d-mesh? 'apa)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>




---


### tiny3d-mesh-vertex-count

Returns the number of vertices in a mesh. The form of a `tiny3d-mesh-vertex-count` expression is `(tiny3d-mesh-vertex-count mesh)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-mesh-vertex-count my-mesh)
```


</td>
<td>

```clj
8
```


</td>
</tr>
</table>




---


### tiny3d-mesh-triangle-count

Returns the number of triangles in a mesh. The form of a `tiny3d-mesh-triangle-count` expression is `(tiny3d-mesh-triangle-count mesh)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-mesh-triangle-count my-mesh)
```


</td>
<td>

```clj
12
```


</td>
</tr>
</table>




---


### tiny3d-mesh-bounding-radius

Returns a mesh's bounding sphere radius (local, unscaled space), as used internally for frustum culling. The form of a `tiny3d-mesh-bounding-radius` expression is `(tiny3d-mesh-bounding-radius mesh)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-mesh-bounding-radius my-mesh)
```


</td>
<td>

```clj
1.732056f32
```


</td>
</tr>
</table>




---


### tiny3d-instance

Creates an instance of a mesh: a positioned, oriented (and optionally scaled) placement of the mesh's geometry in the world. Several instances can share one mesh, so the mesh's vertices/triangles only need to exist once no matter how many copies of it appear in the scene. The form of a `tiny3d-instance` expression is `(tiny3d-instance mesh pos orient opt-scale)`. 

|Arg || 
 |----|----|
 `mesh`      | A mesh created with `tiny3d-mesh`.
 `pos`       | `(x y z)`, world units.
 `orient`    | `(ax ay az)`, degrees, rotation order X then Y then Z.
 `opt-scale` | Optional uniform scale factor, defaults to `1.0`.
 

Returns an `(instance . mesh)` pair - this is the value passed in the `objects` list to `tiny3d-render`/`tiny3d-cull`, and the value the `tiny3d-instance-*` accessors/setters below operate on (they also accept the bare instance). 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance my-mesh (list 0.0 0.0 6.0) (list 20.0 35.0 0.0))
```


</td>
<td>

```clj
([0 73 68 51 0 0 0 0 0 0 0 0 0 0 6 0 57 14 228 24 0 0 0 0 0 0 1 0] . [0 77 68 51 8 0 12 0 104 187 1 0 0 0 255 255 0 0 255 255 0 0 255 255 0 0 1 0 0 0 255 255 0 0 255 255 0 0 1 0 0 0 1 0 0 0 255 255 0 0 255 255 0 0 1 0 0 0 255 255 0 0 255 255 0 0 255 255 0 0 1 0 0 0 1 0 0 0 255 255 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 255 255)
```


</td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance my-mesh (list 0.0 0.0 6.0) (list 0.0 0.0 0.0) 2.0)
```


</td>
<td>

```clj
([0 73 68 51 0 0 0 0 0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 0 0 2 0] . [0 77 68 51 8 0 12 0 104 187 1 0 0 0 255 255 0 0 255 255 0 0 255 255 0 0 1 0 0 0 255 255 0 0 255 255 0 0 1 0 0 0 1 0 0 0 255 255 0 0 255 255 0 0 1 0 0 0 255 255 0 0 255 255 0 0 255 255 0 0 1 0 0 0 1 0 0 0 255 255 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 255 255)
```


</td>
</tr>
</table>




---


### tiny3d-instance?

Checks if the argument is a tiny3d instance (or an `(instance . mesh)` pair). The form of a `tiny3d-instance?` expression is `(tiny3d-instance? v)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance? my-instance)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance? 'apa)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>




---


### tiny3d-instance-set-pos

Updates an instance's position in place, no reallocation. The form of a `tiny3d-instance-set-pos` expression is `(tiny3d-instance-set-pos obj pos)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance-set-pos my-instance (list 1.0 0.0 6.0))
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance-pos my-instance)
```


</td>
<td>

```clj
(1.000000f32 0.000000f32 6.000000f32)
```


</td>
</tr>
</table>




---


### tiny3d-instance-set-orient

Updates an instance's orientation in place, no reallocation. The form of a `tiny3d-instance-set-orient` expression is `(tiny3d-instance-set-orient obj orient)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance-set-orient my-instance (list 0.0 90.0 0.0))
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance-orient my-instance)
```


</td>
<td>

```clj
(0.000000f32 90.000000f32 0.000000f32)
```


</td>
</tr>
</table>




---


### tiny3d-instance-set-transform

Updates an instance's position and orientation together, in one call. The form of a `tiny3d-instance-set-transform` expression is `(tiny3d-instance-set-transform obj pos orient)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance-set-transform my-instance (list 0.0 0.0 6.0) (list 20.0 35.0 0.0))
```


</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### tiny3d-instance-set-scale

Updates an instance's uniform scale factor in place. The form of a `tiny3d-instance-set-scale` expression is `(tiny3d-instance-set-scale obj scale)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance-set-scale my-instance 1.5)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance-scale my-instance)
```


</td>
<td>

```clj
1.500000f32
```


</td>
</tr>
</table>




---


### tiny3d-instance-pos

Returns an instance's position as `(x y z)`. The form of a `tiny3d-instance-pos` expression is `(tiny3d-instance-pos obj)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance-pos my-instance)
```


</td>
<td>

```clj
(0.000000f32 0.000000f32 6.000000f32)
```


</td>
</tr>
</table>




---


### tiny3d-instance-orient

Returns an instance's orientation as `(ax ay az)` degrees. The form of a `tiny3d-instance-orient` expression is `(tiny3d-instance-orient obj)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance-orient my-instance)
```


</td>
<td>

```clj
(20.000610f32 35.002441f32 0.000000f32)
```


</td>
</tr>
</table>




---


### tiny3d-instance-scale

Returns an instance's uniform scale factor. The form of a `tiny3d-instance-scale` expression is `(tiny3d-instance-scale obj)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-instance-scale my-instance)
```


</td>
<td>

```clj
1.500000f32
```


</td>
</tr>
</table>




---


### tiny3d-state-create

Creates a tiny3d rendering state bound to a destination image buffer. Returns a `(state . img)` pair - a "tiny3d state" is this whole pair, not just its `car`. Pass it as-is to `tiny3d-render`/`tiny3d-cull`: internally `state` holds raw pointers into `img`'s bytes, so keeping the pair together is what keeps `img` reachable for as long as `state` is actually used. The form of a `tiny3d-state-create` expression is `(tiny3d-state-create img max-tris-per-object near far fov-degrees cull-margin ..option)`. 

|Arg || 
 |----|----|
 `img`                 | Destination image buffer, created with `img-buffer`.
 `max-tris-per-object` | Size of the internal triangle scratch buffer, in triangles - must be at least as large as the most complex single object's triangle count.
 `near far`            | Near/far clipping plane distances, world units.
 `fov-degrees`         | Vertical field of view, degrees.
 `cull-margin`         | Extra frustum-cull margin, world units.
 

<br> 

|Option                || 
 |----|----|
 `'(filled)`           | Solid triangles instead of the default wireframe outlines.
 `'(no-backface-cull)` | Keep back-facing triangles, which are culled by default.
 

Each option is its own separate `'(name)` argument, the same convention used by the `img-*` drawing functions - for example `'(filled) '(no-backface-cull)`, not `'(filled no-backface-cull)`. 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(progn
  (img-clear my-img 0x101018)
  (tiny3d-render (tiny3d-state-create my-img 36 0.5 100.0 60.0 0.5)
                 (list my-instance) cam-pos cam-orient))
```


</td>
<td>

<img src=./images/tiny3d-img1.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(progn
  (img-clear my-img 0x101018)
  (tiny3d-render (tiny3d-state-create my-img 36 0.5 100.0 60.0 0.5 '(filled))
                 (list my-instance) cam-pos cam-orient))
```


</td>
<td>

<img src=./images/tiny3d-img2.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(progn
  (img-clear my-img 0x101018)
  (tiny3d-render (tiny3d-state-create my-img 36 0.5 100.0 60.0 0.5 '(filled) '(no-backface-cull))
                 (list my-instance) cam-pos cam-orient))
```


</td>
<td>

<img src=./images/tiny3d-img3.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>

Top to bottom: default (wireframe, backface-culled), `'(filled)`, `'(filled) '(no-backface-cull)`. 




---


### tiny3d-render

Renders a list of instances into `state`'s destination image. Each instance is transformed, frustum- and backface-culled, near-plane clipped and rasterized - objects entirely outside the view are skipped cheaply without touching their vertices. The form of a `tiny3d-render` expression is `(tiny3d-render state objects cam-pos cam-orient)`. 

|Arg || 
 |----|----|
 `state`      | The `(state . img)` pair from `tiny3d-state-create`.
 `objects`    | List of `(instance . mesh)` pairs, as returned by `tiny3d-instance`.
 `cam-pos`    | `(x y z)`, world units.
 `cam-orient` | `(ax ay az)`, degrees.
 

`tiny3d-render` does not sort objects by depth itself - for scenes where overlapping objects need correct back-to-front ordering, cull with `tiny3d-cull` first, sort the survivors by the returned depth, then render the sorted list (see `repl/examples/tiny3d_orbit.lisp` for the full pattern). 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(progn
  (img-clear my-img 0x101018)
  (tiny3d-render my-state (list my-instance) cam-pos cam-orient))
```


</td>
<td>

<img src=./images/tiny3d-img4.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(progn
  (img-clear my-img 0x101018)
  (tiny3d-render my-state
    (list my-instance
          (tiny3d-instance (cube-mesh 0x30C060) (list -1.6 0.0 7.0) (list 0.0 10.0 0.0))
          (tiny3d-instance (cube-mesh 0x3080E0) (list 1.6 0.3 5.0) (list 10.0 60.0 0.0)))
    cam-pos cam-orient))
```


</td>
<td>

<img src=./images/tiny3d-img5.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### tiny3d-cull

Transforms and culls a single object without rendering it - the stage-1 half of `tiny3d-render`'s pipeline, exposed so objects can be sorted by depth before the actual rendering pass (`tiny3d-render` re-culls internally too, so this is safe to run ahead of time on the same objects). The form of a `tiny3d-cull` expression is `(tiny3d-cull state obj cam-pos cam-orient)`. 

|Arg || 
 |----|----|
 `state`      | The `(state . img)` pair from `tiny3d-state-create`.
 `obj`        | A single `(instance . mesh)` pair.
 `cam-pos`    | `(x y z)`, world units.
 `cam-orient` | `(ax ay az)`, degrees.
 

Returns the object's camera-space depth (a float, useful as a sort key - larger means farther away) if any part of it survives culling, or `nil` if the whole object is definitely outside the view frustum. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tiny3d-cull my-state my-instance cam-pos cam-orient)
```


</td>
<td>

```clj
5.999634f32
```


</td>
</tr>
<tr>
<td>

```clj
(tiny3d-cull my-state my-instance (list 0.0 0.0 -50.0) cam-orient)
```


</td>
<td>

```clj
55.996582f32
```


</td>
</tr>
</table>




---

# Examples


### Example: cull, sort, render

This example shows how to use the culling and rendering pipelines together. First instances are transformed into camera space and culled using the view frustum.  Next the remaining instances are sorted in distance to camera and finally rendered. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define objs (list
  (tiny3d-instance (cube-mesh 0xE04030) (list 0.0 0.0 9.0) (list 20.0 35.0 0.0))
  (tiny3d-instance (cube-mesh 0x30C060) (list -1.6 0.0 6.0) (list 0.0 10.0 0.0))
  (tiny3d-instance (cube-mesh 0x3080E0) (list 1.6 0.3 7.5) (list 10.0 60.0 0.0))))
```


</td>
<td>

```clj
(([0 73 68 51 0 0 0 0 0 0 0 0 0 0 9 0 57 14 228 24 0 0 0 0 0 0 1 0] . [0 77 68 51 8 0 12 0 104 187 1 0 0 0 255 255 0 0 255 255 0 0 255 255 0 0 1 0 0 0 255 255 0 0 255 255 0 0 1 0 0 0 1 0 0 0 255 255 0 0 255 255 0 0 1 0 0 0 255 255 0 0 255 255 0 0 255 255 0 0 1 0 0 0 1 0 0 0 255 255 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 255 255) ([0 73 68 51 102 102 254 255 0 0 0 0 0 0 6 0 0 0 28 7 0 0 0 0 0 0 1 0] . [0 77 68 51 8 0 12 0 104 187 1 0 0 0 255 255 0 0 255 255 0 0 255 255 0 0 1 0 0 0 255 255 0 0 255 255 0 0 1 0 0 0 1 0 0 0 255 255 0 0 255 255 0 0 1 0 0 0 255 255 0 0 255 255 0 0 255 255 0 0 1 0 0 0 1 0 0 0 255 255 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 255 255) ([0 73 68 51 154 153 1 0 205 76 0 0 0 128 7 0 28 7 171 42 0 0 0 0 0 0 1 0] . [0 77 68 51 8 0 12 0 104 187 1 0 0 0 255 255 0 0 255 255 0 0 255 255 0 0 1 0 0 0 255 255 0 0 255 255 0 0 1 0 0 0 1 0 0 0 255 255 0 0 255 255 0 0 1 0 0 0 255 255 0 0 255 255 0 0 255 255 0 0 1 0 0 0 1 0 0 0 255 255 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 255 255))
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define depths (map (lambda (o)
                      (cons o (tiny3d-cull my-state o cam-pos cam-orient))) objs))
(define visible (filter (lambda (p)
                          (cdr p)) depths))
(define sorted (sort (lambda (a b)
                       (> (cdr a) (cdr b))) visible))
(img-clear my-img 1052696)
(tiny3d-render my-state (map car sorted) cam-pos cam-orient)
(disp-render my-img 0 0 (list 0 16777215))
```


</td>
<td>

<img src=./images/tiny3d-img6.png >

</td>
<td>


```clj
t
```


</td>
</tr>
</table>




---

This document was generated by LispBM version 0.38.0 

