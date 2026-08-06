
(set-pic-prefix "tiny3d")

(display-to-img)
(define render-target (img-buffer 'rgb888 320 200))
(set-active-img render-target)
(disp-clear)

(define my-img (img-buffer 'rgb888 320 200))

;; A cube mesh shared by the examples below. Side 2, centered at the
;; local origin: 8 shared vertices, 12 triangles referencing them by
;; index (rather than each triangle carrying its own duplicated 3
;; corners). Windings verified against tiny3d.c's front-facing
;; convention (see repl/examples/tiny3d_orbit.lisp).
(define cube-verts (list
  (list -1.0 -1.0 -1.0)   ; 0
  (list  1.0 -1.0 -1.0)   ; 1
  (list  1.0  1.0 -1.0)   ; 2
  (list -1.0  1.0 -1.0)   ; 3
  (list -1.0 -1.0  1.0)   ; 4
  (list  1.0 -1.0  1.0)   ; 5
  (list  1.0  1.0  1.0)   ; 6
  (list -1.0  1.0  1.0))) ; 7

(defun cube-tris (color) (list
  (list 4 5 6 color) (list 4 6 7 color)   ; front  z=+1
  (list 0 2 1 color) (list 0 3 2 color)   ; back   z=-1
  (list 1 2 6 color) (list 1 6 5 color)   ; right  x=+1
  (list 0 7 3 color) (list 0 4 7 color)   ; left   x=-1
  (list 3 6 2 color) (list 3 7 6 color)   ; top    y=+1
  (list 0 1 5 color) (list 0 5 4 color))) ; bottom y=-1

(defun cube-mesh (color) (tiny3d-mesh cube-verts (cube-tris color)))

;; A single reusable instance and a fixed camera looking down +z at
;; the origin, used across the examples that just need "some mesh on
;; screen" rather than demonstrating positioning itself.
(define my-mesh (cube-mesh 0xE04030))
(define my-instance (tiny3d-instance my-mesh (list 0.0 0.0 6.0) (list 20.0 35.0 0.0)))
(define cam-pos (list 0.0 0.0 0.0))
(define cam-orient (list 0.0 0.0 0.0))
(define my-state (tiny3d-state-create my-img 36 0.5 100.0 60.0 0.5 '(filled)))

(defun code-disp-str (xs) (code-disp (map (lambda (x) (list 'read-eval x)) xs)))
(defun code-str (xs) (code (map (lambda (x) (list 'read-eval x)) xs)))
(defun code-png-str (img c xs) (code-png img c (map (lambda (x) (list 'read-eval x)) xs)))

(define entry-tiny3d-mesh
  (ref-entry "tiny3d-mesh"
             (list
              (para (list "Creates a mesh from a list of vertices and a list of index"
                          "triangles. Vertices shared between triangles (for example a"
                          "cube's 8 corners across its 12 triangles) are stored once, not"
                          "duplicated per triangle."
                          "The form of a `tiny3d-mesh` expression is `(tiny3d-mesh vertices triangles)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`vertices`  | List of `(x y z)`, one entry per unique vertex.\n"
                          "`triangles` | List of `(i0 i1 i2 color)` - `i0`/`i1`/`i2` index into `vertices`, `color` is a raw TinyGFX color (index or 0xRRGGBB).\n"
                          ))
              (para (list "A mesh's bounding radius (used for frustum culling) is computed"
                          "automatically from the vertex furthest from the local origin."
                          ))
              (code-str (list "(tiny3d-mesh cube-verts (cube-tris 0xE04030))"))
              end)))

(define entry-tiny3d-mesh-p
  (ref-entry "tiny3d-mesh?"
             (list
              (para (list "Checks if the argument is a tiny3d mesh."
                          "The form of a `tiny3d-mesh?` expression is `(tiny3d-mesh? v)`."
                          ))
              (code-str (list "(tiny3d-mesh? my-mesh)"
                              "(tiny3d-mesh? 'apa)"))
              end)))

(define entry-tiny3d-mesh-vertex-count
  (ref-entry "tiny3d-mesh-vertex-count"
             (list
              (para (list "Returns the number of vertices in a mesh."
                          "The form of a `tiny3d-mesh-vertex-count` expression is `(tiny3d-mesh-vertex-count mesh)`."
                          ))
              (code-str (list "(tiny3d-mesh-vertex-count my-mesh)"))
              end)))

(define entry-tiny3d-mesh-triangle-count
  (ref-entry "tiny3d-mesh-triangle-count"
             (list
              (para (list "Returns the number of triangles in a mesh."
                          "The form of a `tiny3d-mesh-triangle-count` expression is `(tiny3d-mesh-triangle-count mesh)`."
                          ))
              (code-str (list "(tiny3d-mesh-triangle-count my-mesh)"))
              end)))

(define entry-tiny3d-mesh-bounding-radius
  (ref-entry "tiny3d-mesh-bounding-radius"
             (list
              (para (list "Returns a mesh's bounding sphere radius (local, unscaled space),"
                          "as used internally for frustum culling."
                          "The form of a `tiny3d-mesh-bounding-radius` expression is `(tiny3d-mesh-bounding-radius mesh)`."
                          ))
              (code-str (list "(tiny3d-mesh-bounding-radius my-mesh)"))
              end)))

(define entry-tiny3d-instance
  (ref-entry "tiny3d-instance"
             (list
              (para (list "Creates an instance of a mesh: a positioned, oriented (and"
                          "optionally scaled) placement of the mesh's geometry in the"
                          "world. Several instances can share one mesh, so the mesh's"
                          "vertices/triangles only need to exist once no matter how many"
                          "copies of it appear in the scene."
                          "The form of a `tiny3d-instance` expression is `(tiny3d-instance mesh pos orient opt-scale)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`mesh`      | A mesh created with `tiny3d-mesh`.\n"
                          "`pos`       | `(x y z)`, world units.\n"
                          "`orient`    | `(ax ay az)`, degrees, rotation order X then Y then Z.\n"
                          "`opt-scale` | Optional uniform scale factor, defaults to `1.0`.\n"
                          ))
              (para (list "Returns an `(instance . mesh)` pair - this is the value passed"
                          "in the `objects` list to `tiny3d-render`/`tiny3d-cull`, and the"
                          "value the `tiny3d-instance-*` accessors/setters below operate on"
                          "(they also accept the bare instance)."
                          ))
              (code-str (list "(tiny3d-instance my-mesh (list 0.0 0.0 6.0) (list 20.0 35.0 0.0))"
                              "(tiny3d-instance my-mesh (list 0.0 0.0 6.0) (list 0.0 0.0 0.0) 2.0)"))
              end)))

(define entry-tiny3d-instance-p
  (ref-entry "tiny3d-instance?"
             (list
              (para (list "Checks if the argument is a tiny3d instance (or an"
                          "`(instance . mesh)` pair)."
                          "The form of a `tiny3d-instance?` expression is `(tiny3d-instance? v)`."
                          ))
              (code-str (list "(tiny3d-instance? my-instance)"
                              "(tiny3d-instance? 'apa)"))
              end)))

(define entry-tiny3d-instance-set-pos
  (ref-entry "tiny3d-instance-set-pos"
             (list
              (para (list "Updates an instance's position in place, no reallocation."
                          "The form of a `tiny3d-instance-set-pos` expression is `(tiny3d-instance-set-pos obj pos)`."
                          ))
              (code-str (list "(tiny3d-instance-set-pos my-instance (list 1.0 0.0 6.0))"
                              "(tiny3d-instance-pos my-instance)"))
              end)))

(define entry-tiny3d-instance-set-orient
  (ref-entry "tiny3d-instance-set-orient"
             (list
              (para (list "Updates an instance's orientation in place, no reallocation."
                          "The form of a `tiny3d-instance-set-orient` expression is `(tiny3d-instance-set-orient obj orient)`."
                          ))
              (code-str (list "(tiny3d-instance-set-orient my-instance (list 0.0 90.0 0.0))"
                              "(tiny3d-instance-orient my-instance)"))
              end)))

(define entry-tiny3d-instance-set-transform
  (ref-entry "tiny3d-instance-set-transform"
             (list
              (para (list "Updates an instance's position and orientation together, in"
                          "one call."
                          "The form of a `tiny3d-instance-set-transform` expression is `(tiny3d-instance-set-transform obj pos orient)`."
                          ))
              (code-str (list "(tiny3d-instance-set-transform my-instance (list 0.0 0.0 6.0) (list 20.0 35.0 0.0))"))
              end)))

(define entry-tiny3d-instance-set-scale
  (ref-entry "tiny3d-instance-set-scale"
             (list
              (para (list "Updates an instance's uniform scale factor in place."
                          "The form of a `tiny3d-instance-set-scale` expression is `(tiny3d-instance-set-scale obj scale)`."
                          ))
              (code-str (list "(tiny3d-instance-set-scale my-instance 1.5)"
                              "(tiny3d-instance-scale my-instance)"))
              end)))

(define entry-tiny3d-instance-pos
  (ref-entry "tiny3d-instance-pos"
             (list
              (para (list "Returns an instance's position as `(x y z)`."
                          "The form of a `tiny3d-instance-pos` expression is `(tiny3d-instance-pos obj)`."
                          ))
              (code-str (list "(tiny3d-instance-pos my-instance)"))
              end)))

(define entry-tiny3d-instance-orient
  (ref-entry "tiny3d-instance-orient"
             (list
              (para (list "Returns an instance's orientation as `(ax ay az)` degrees."
                          "The form of a `tiny3d-instance-orient` expression is `(tiny3d-instance-orient obj)`."
                          ))
              (code-str (list "(tiny3d-instance-orient my-instance)"))
              end)))

(define entry-tiny3d-instance-scale
  (ref-entry "tiny3d-instance-scale"
             (list
              (para (list "Returns an instance's uniform scale factor."
                          "The form of a `tiny3d-instance-scale` expression is `(tiny3d-instance-scale obj)`."
                          ))
              (code-str (list "(tiny3d-instance-scale my-instance)"))
              end)))

(define entry-tiny3d-state-create
  (ref-entry "tiny3d-state-create"
             (list
              (para (list "Creates a tiny3d rendering state bound to a destination image"
                          "buffer. Returns a `(state . img)` pair - a \"tiny3d state\" is"
                          "this whole pair, not just its `car`. Pass it as-is to"
                          "`tiny3d-render`/`tiny3d-cull`: internally `state` holds raw"
                          "pointers into `img`'s bytes, so keeping the pair together is"
                          "what keeps `img` reachable for as long as `state` is actually used."
                          "The form of a `tiny3d-state-create` expression is"
                          "`(tiny3d-state-create img max-tris-per-object near far fov-degrees cull-margin ..option)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`img`                 | Destination image buffer, created with `img-buffer`.\n"
                          "`max-tris-per-object` | Size of the internal triangle scratch buffer, in triangles - must be at least as large as the most complex single object's triangle count.\n"
                          "`near far`            | Near/far clipping plane distances, world units.\n"
                          "`fov-degrees`         | Vertical field of view, degrees.\n"
                          "`cull-margin`         | Extra frustum-cull margin, world units.\n"
                          ))
              (para (list "<br>"))
              (para (list "|Option                || \n"
                          "|----|----|\n"
                          "`'(filled)`           | Solid triangles instead of the default wireframe outlines.\n"
                          "`'(no-backface-cull)` | Keep back-facing triangles, which are culled by default.\n"
                          ))
              (para (list "Each option is its own separate `'(name)` argument, the same"
                          "convention used by the `img-*` drawing functions - for example"
                          "`'(filled) '(no-backface-cull)`, not `'(filled no-backface-cull)`."
                          ))
              (code-png-str 'my-img '(0x000000 0xffffff)
                        (list
                         "(progn\n  (img-clear my-img 0x101018)\n  (tiny3d-render (tiny3d-state-create my-img 36 0.5 100.0 60.0 0.5)\n                 (list my-instance) cam-pos cam-orient))"
                         "(progn\n  (img-clear my-img 0x101018)\n  (tiny3d-render (tiny3d-state-create my-img 36 0.5 100.0 60.0 0.5 '(filled))\n                 (list my-instance) cam-pos cam-orient))"
                         "(progn\n  (img-clear my-img 0x101018)\n  (tiny3d-render (tiny3d-state-create my-img 36 0.5 100.0 60.0 0.5 '(filled) '(no-backface-cull))\n                 (list my-instance) cam-pos cam-orient))"
                         ))
              (para (list "Top to bottom: default (wireframe, backface-culled), `'(filled)`,"
                          "`'(filled) '(no-backface-cull)`."
                          ))
              end)))

(define entry-tiny3d-render
  (ref-entry "tiny3d-render"
             (list
              (para (list "Renders a list of instances into `state`'s destination image."
                          "Each instance is transformed, frustum- and backface-culled,"
                          "near-plane clipped and rasterized - objects entirely outside the"
                          "view are skipped cheaply without touching their vertices."
                          "The form of a `tiny3d-render` expression is `(tiny3d-render state objects cam-pos cam-orient)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`state`      | The `(state . img)` pair from `tiny3d-state-create`.\n"
                          "`objects`    | List of `(instance . mesh)` pairs, as returned by `tiny3d-instance`.\n"
                          "`cam-pos`    | `(x y z)`, world units.\n"
                          "`cam-orient` | `(ax ay az)`, degrees.\n"
                          ))
              (para (list "`tiny3d-render` does not sort objects by depth itself - for"
                          "scenes where overlapping objects need correct back-to-front"
                          "ordering, cull with `tiny3d-cull` first, sort the survivors by"
                          "the returned depth, then render the sorted list (see"
                          "`repl/examples/tiny3d_orbit.lisp` for the full pattern)."
                          ))
              (code-png-str 'my-img '(0x000000 0xffffff)
                        (list
                         "(progn\n  (img-clear my-img 0x101018)\n  (tiny3d-render my-state (list my-instance) cam-pos cam-orient))"
                         (str-merge
                          "(progn\n  (img-clear my-img 0x101018)\n  (tiny3d-render my-state\n    (list my-instance\n"
                          "          (tiny3d-instance (cube-mesh 0x30C060) (list -1.6 0.0 7.0) (list 0.0 10.0 0.0))\n"
                          "          (tiny3d-instance (cube-mesh 0x3080E0) (list 1.6 0.3 5.0) (list 10.0 60.0 0.0)))\n"
                          "    cam-pos cam-orient))")
                         ))
              end)))

(define entry-tiny3d-cull
  (ref-entry "tiny3d-cull"
             (list
              (para (list "Transforms and culls a single object without rendering it -"
                          "the stage-1 half of `tiny3d-render`'s pipeline, exposed so"
                          "objects can be sorted by depth before the actual rendering"
                          "pass (`tiny3d-render` re-culls internally too, so this is safe"
                          "to run ahead of time on the same objects)."
                          "The form of a `tiny3d-cull` expression is `(tiny3d-cull state obj cam-pos cam-orient)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`state`      | The `(state . img)` pair from `tiny3d-state-create`.\n"
                          "`obj`        | A single `(instance . mesh)` pair.\n"
                          "`cam-pos`    | `(x y z)`, world units.\n"
                          "`cam-orient` | `(ax ay az)`, degrees.\n"
                          ))
              (para (list "Returns the object's camera-space depth (a float, useful as a"
                          "sort key - larger means farther away) if any part of it survives"
                          "culling, or `nil` if the whole object is definitely outside the"
                          "view frustum."
                          ))
              (code-str (list "(tiny3d-cull my-state my-instance cam-pos cam-orient)"
                              "(tiny3d-cull my-state my-instance (list 0.0 0.0 -50.0) cam-orient)"))
              end)))

(define pipeline-example
  (ref-entry "Example: cull, sort, render"
             (list
              (para (list "This example shows how to use the culling and rendering pipelines"
                          "together. First instances are transformed into camera space and culled using the view frustum. "
                          "Next the remaining instances are sorted in distance to camera and finally rendered."
                          ))
              (code-str (list (str-merge
                               "(define objs (list\n"  
                               "  (tiny3d-instance (cube-mesh 0xE04030) (list 0.0 0.0 9.0) (list 20.0 35.0 0.0))\n"
                               "  (tiny3d-instance (cube-mesh 0x30C060) (list -1.6 0.0 6.0) (list 0.0 10.0 0.0))\n"
                               "  (tiny3d-instance (cube-mesh 0x3080E0) (list 1.6 0.3 7.5) (list 10.0 60.0 0.0))))"
                               )))
              (program-disp '(((define depths (map (lambda (o) (cons o (tiny3d-cull my-state o cam-pos cam-orient))) objs))
                               (define visible (filter (lambda (p) (cdr p)) depths))
                               (define sorted (sort (lambda (a b) (> (cdr a) (cdr b))) visible))
                               (img-clear my-img 0x101018)
                               (tiny3d-render my-state (map car sorted) cam-pos cam-orient)
                               (disp-render my-img 0 0 (list 0x000000 0xffffff))
                               )))
              end)))


(define manual
  (list
   (section 1 "LispBM Tiny3D Library"
            (list
             (para (list "This document describes how to use the tiny3d_extensions to lispbm."
                         "Tiny3D is a single c/h file pair implementing a simple 3d pipeline"
                         "that view-transforms, culls and renders instanced **objects** (meshes)"
                         "Tiny3D itself does not allocate any memory and is not at all aware"
                         "of the lifetimes of objects and image buffers. Managing image buffers"
                         "and object lifetimes is left to the integrator, in this case LispBM."
                         ))
             (para (list "The renderer works with the same image buffers as the display"
                         "library (see the Display library reference) - a scene is"
                         "rendered into an `img-buffer`, which can then be shown with"
                         "`disp-render` like any other image. Meshes work unmodified with"
                         "indexed image buffers - a triangle's `color` is just an opaque"
                         "value interpreted by the image's own format, index or 0xRRGGBB."
                         ))
             (para (list "The building blocks are:"
                         ))
             (bullet '("**mesh** - shared vertex/triangle geometry, created once with `tiny3d-mesh`."
                       "**instance** - a positioned, oriented, optionally scaled placement of a mesh in the world; many instances can share one mesh."
                       "**state** - a `tiny3d-state-create`d rendering context bound to a destination image buffer, near/far/fov and render options."
                       ))
             (para (list "A minimal scene looks like:"
                         ))
             (code-str (list (str-merge
                              "(define cube-verts (list\n  (list -1.0 -1.0 -1.0)   ; 0\n  (list  1.0 -1.0 -1.0)   ; 1\n  (list  1.0  1.0 -1.0)   ; 2\n  (list -1.0  1.0 -1.0)   ; 3\n"
                              "  (list -1.0 -1.0  1.0)   ; 4\n  (list  1.0 -1.0  1.0)   ; 5\n  (list  1.0  1.0  1.0)   ; 6\n  (list -1.0  1.0  1.0))) ; 7")))
             (para (list "The examples uses a function to create colored triangles:"
                         ))
             (code-str (list (str-merge
                              "(defun cube-tris (color)\n"
                              " (list\n"
                              "  (list 4 5 6 color) (list 4 6 7 color)\n"
                              "  (list 0 2 1 color) (list 0 3 2 color)\n"
                              "  (list 1 2 6 color) (list 1 6 5 color)\n"
                              "  (list 0 7 3 color) (list 0 4 7 color)\n"
                              "  (list 3 6 2 color) (list 3 7 6 color)\n" 
                              "  (list 0 1 5 color) (list 0 5 4 color)))\n"
                              )))
             (para (list "A standalone object or **mesh** is then created from these"
                         "vertices and triangles using `tiny3d-mesh`:"
                         ))
             (code-str (list "(defun cube-mesh (color) (tiny3d-mesh cube-verts (cube-tris color)))"
                             ))
             
             end))
   (section 1 "Reference"
            (list entry-tiny3d-mesh
                  entry-tiny3d-mesh-p
                  entry-tiny3d-mesh-vertex-count
                  entry-tiny3d-mesh-triangle-count
                  entry-tiny3d-mesh-bounding-radius
                  entry-tiny3d-instance
                  entry-tiny3d-instance-p
                  entry-tiny3d-instance-set-pos
                  entry-tiny3d-instance-set-orient
                  entry-tiny3d-instance-set-transform
                  entry-tiny3d-instance-set-scale
                  entry-tiny3d-instance-pos
                  entry-tiny3d-instance-orient
                  entry-tiny3d-instance-scale
                  entry-tiny3d-state-create
                  entry-tiny3d-render
                  entry-tiny3d-cull
                  ))
   (section 1 "Examples"
            (list pipeline-example))
   info
   )
  )

(defun render-manual ()
  (let ((h (f-open "tiny3dref.md" "w"))
        (r (lambda (s) (f-write-str h s))))
    {
    (var t0 (systime))
    (render r manual)
    (print "Tiny3D reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
