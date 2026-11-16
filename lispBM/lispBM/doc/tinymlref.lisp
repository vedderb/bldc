(define h (tinyml-open "moons"))

(define entry-tinyml-models
  (ref-entry "tinyml-models"
             (list
              (para (list "Returns the names of all models registered (by C-side adapter"
                          "code, at startup) with the tinyml extensions."
                          "The form of a `tinyml-models` expression is `(tinyml-models)`."
                          ))
              (code '((tinyml-models)
                      ))
              end)))

(define entry-tinyml-open
  (ref-entry "tinyml-open"
             (list
              (para (list "Opens an instance of a registered model by name, allocating"
                          "any scratch memory (`arena`) the model's adapter declared it"
                          "needs and running the adapter's one-time `init`, if it has one."
                          "The form of a `tinyml-open` expression is `(tinyml-open name)`."
                          ))
              (para (list "Returns an instance handle - an opaque custom-type value, not"
                          "a plain array, specifically so that it cannot be accidentally"
                          "captured into a saved LispBM image (custom types are not"
                          "flattenable, so attempting to persist one fails loudly instead"
                          "of silently saving a pointer that would be invalid on the next"
                          "boot). The handle's underlying memory is freed automatically by"
                          "the garbage collector once it becomes unreachable."
                          ))
              (para (list "Raises `eval_error` if no model with that name is registered."
                          ))
              (code '((define h (tinyml-open "moons"))
                      (tinyml-instance? h)
                      ))
              end)))

(define entry-tinyml-instance-p
  (ref-entry "tinyml-instance?"
             (list
              (para (list "Checks if the argument is a tinyml instance handle, as"
                          "returned by `tinyml-open`."
                          "The form of a `tinyml-instance?` expression is `(tinyml-instance? v)`."
                          ))
              (code '((tinyml-instance? h)
                      (tinyml-instance? 'apa)
                      ))
              end)))

(define entry-tinyml-n-inputs
  (ref-entry "tinyml-n-inputs"
             (list
              (para (list "Returns the number of input tensors a model takes."
                          "The form of a `tinyml-n-inputs` expression is `(tinyml-n-inputs h)`."
                          ))
              (code '((tinyml-n-inputs h)
                      ))
              end)))

(define entry-tinyml-n-outputs
  (ref-entry "tinyml-n-outputs"
             (list
              (para (list "Returns the number of output tensors a model produces."
                          "The form of a `tinyml-n-outputs` expression is `(tinyml-n-outputs h)`."
                          ))
              (code '((tinyml-n-outputs h)
                      ))
              end)))

(define entry-tinyml-input-bytes
  (ref-entry "tinyml-input-bytes"
             (list
              (para (list "Returns the exact byte size input tensor `i` expects - the"
                          "size to pass to `bufcreate` when allocating a buffer for"
                          "`tinyml-run`."
                          "The form of a `tinyml-input-bytes` expression is `(tinyml-input-bytes h i)`."
                          ))
              (code '((tinyml-input-bytes h 0)
                      ))
              end)))

(define entry-tinyml-output-bytes
  (ref-entry "tinyml-output-bytes"
             (list
              (para (list "Returns the exact byte size output tensor `i` produces."
                          "The form of a `tinyml-output-bytes` expression is `(tinyml-output-bytes h i)`."
                          ))
              (code '((tinyml-output-bytes h 0)
                      ))
              end)))

(define entry-tinyml-input-type
  (ref-entry "tinyml-input-type"
             (list
              (para (list "Returns input tensor `i`'s type as an association list:"
                          "`dtype` (one of the symbols `f32`, `i8`, `u8`, `i16`), `shape`"
                          "(a list of dimensions, no batch dimension), `scale` and"
                          "`zero-point` (quantization parameters, `1.0`/`0` for a plain"
                          "float tensor)."
                          "The form of a `tinyml-input-type` expression is `(tinyml-input-type h i)`."
                          ))
              (para (list "The returned alist is exactly the shape `tensorlib.lisp`'s"
                          "`tensor-set`/`tensor-to-list` expect as their `type` argument"
                          "(see the Example section below) - it is plain value data, safe"
                          "to store, pass around or build by hand."
                          ))
              (code '((tinyml-input-type h 0)
                      ))
              end)))

(define entry-tinyml-output-type
  (ref-entry "tinyml-output-type"
             (list
              (para (list "Returns output tensor `i`'s type, in the same alist shape as"
                          "`tinyml-input-type`."
                          "The form of a `tinyml-output-type` expression is `(tinyml-output-type h i)`."
                          ))
              (code '((tinyml-output-type h 0)
                      ))
              end)))

(define entry-tinyml-info
  (ref-entry "tinyml-info"
             (list
              (para (list "Returns everything about a model in one call: its `name`,"
                          "`n-inputs`, `n-outputs`, and the full `inputs`/`outputs` lists"
                          "of per-tensor type alists (the same shape `tinyml-input-type`/"
                          "`tinyml-output-type` return one of)."
                          "The form of a `tinyml-info` expression is `(tinyml-info h)`."
                          ))
              (code '((tinyml-info h)
                      ))
              end)))

(define entry-tinyml-run
  (ref-entry "tinyml-run"
             (list
              (para (list "Runs one inference. `in`/`out` are byte arrays (from"
                          "`bufcreate`), sized exactly as `tinyml-input-bytes`/"
                          "`tinyml-output-bytes` report - `tinyml-run` validates this and"
                          "raises `type_error` on any mismatch. With more than one input"
                          "or output tensor, pass a list of buffers instead of a single one."
                          "The form of a `tinyml-run` expression is `(tinyml-run h in out)`."
                          ))
              (para (list "Neither `in` nor `out` is allocated by `tinyml-run` - the"
                          "caller allocates both, and `tinyml-run` writes results"
                          "directly into `out`'s memory with no copying anywhere in the"
                          "call chain down to the underlying model code. Returns `t` on"
                          "success, or raises `eval_error` if the underlying model"
                          "reports failure."
                          ))
              (code '((define in (bufcreate (tinyml-input-bytes h 0)))
                      (define out (bufcreate (tinyml-output-bytes h 0)))
                      (bufset-f32 in 0 1.0 'little-endian)
                      (bufset-f32 in 4 -0.4 'little-endian)
                      (tinyml-run h in out)
                      (bufget-f32 out 0 'little-endian)
                      (bufget-f32 out 4 'little-endian)
                      ))
              end)))

(define example-endianness
  (ref-entry "A note on byte order"
             (list
              (para (list "`bufset-*`/`bufget-*` default to **big-endian** unless"
                          "`'little-endian` is passed explicitly, but `tinyml-run`'s"
                          "underlying model code always reads and writes tensor bytes in"
                          "the target's native order - little-endian on essentially any"
                          "real target (ARM Cortex-M, RISC-V, x86). Forgetting"
                          "`'little-endian` on a `bufset-*`/`bufget-*` call touching a"
                          "tinyml buffer does not raise an error - it silently produces"
                          "byte-swapped garbage."
                          ))
              end)))

(define example-tensorlib
  (ref-entry "Example: tensorlib.lisp"
             (list
              (para (list "Writing `bufset-f32`/`bufget-f32` calls by hand, with the"
                          "right byte offsets and `'little-endian` on every call, gets"
                          "tedious and error-prone once a model has more than a couple of"
                          "elements per tensor. `repl/examples/tinyml/tensorlib.lisp` is a"
                          "small pure-Lisp library - built entirely on the generic"
                          "`bufset-*`/`bufget-*` extensions plus a type alist from"
                          "`tinyml-input-type`/`tinyml-output-type` - that removes both"
                          "problems: `tensor-set`/`tensor-to-list` compute the right byte"
                          "offset from a shape and a multi-dimensional index, dispatch to"
                          "the right typed `bufset-*`/`bufget-*` automatically, and always"
                          "pass `'little-endian` internally."
                          ))
              (para (list "|Function || \n"
                          "|----|----|\n"
                          "`(tensor-set type indices tensor value)`   | Writes one element at the position `indices` fully specifies.\n"
                          "`(tensor-to-list type indices buf)`        | Reads a slice back as a list - `indices` fixes a prefix of leading dimensions (row-major); `'()` flattens the whole tensor.\n"
                          ))
              (code '((import "../repl/examples/tinyml/tensorlib.lisp" 'tensorlib-src)
                      (read-eval-program tensorlib-src)
                      (define in-type (tinyml-input-type h 0))
                      (define out-type (tinyml-output-type h 0))
                      (define in (bufcreate (tinyml-input-bytes h 0)))
                      (define out (bufcreate (tinyml-output-bytes h 0)))
                      (tensor-set in-type '(0) in 1.0)
                      (tensor-set in-type '(1) in -0.4)
                      (tinyml-run h in out)
                      (tensor-to-list out-type '() out)
                      ))
              (para (list "See `repl/examples/tinyml/moons.lisp` for the full worked"
                          "example this reference's model comes from: a tiny MLP trained"
                          "on `sklearn.datasets.make_moons`, exported to ONNX, translated"
                          "to C with `onnx2c`, and wired in as the `\"moons\"` model."
                          ))
              end)))

(define example-program
  (ref-entry "Example: a small classification program"
             (list
              (para (list "Putting the pieces together: a self-contained `classify`"
                          "function built on `tensorlib.lisp` (from the previous"
                          "example, so `h`/`in-type`/`out-type` are already in scope),"
                          "run over a few points. The whole program below is evaluated"
                          "as one unit and its final result shown, not one row per form -"
                          "this is the same shape of code as"
                          "`repl/examples/tinyml/moons.lisp`, just condensed."
                          ))
              (program '(((defun classify (x y)
                            (let ((in (bufcreate (tinyml-input-bytes h 0)))
                                  (out (bufcreate (tinyml-output-bytes h 0))))
                              (progn
                                (tensor-set in-type '(0) in x)
                                (tensor-set in-type '(1) in y)
                                (tinyml-run h in out)
                                (let ((scores (tensor-to-list out-type '() out)))
                                  (if (> (car (cdr scores)) (car scores)) 1 0)))))
                          (list (classify -1.0 0.4)
                                (classify 1.0 -0.4)
                                (classify 0.5 -0.3))
                          )))
              end)))

(define manual
  (list
   (section 1 "LispBM TinyML Library"
            (list
             (para (list "This document describes how to use the tinyml_extensions to"
                         "LispBM. tinyml_extensions provides a single, backend-agnostic"
                         "interface for running inference on a compiled-in neural"
                         "network - the model's own code may come from `onnx2c`, a"
                         "hand-written engine, or any other backend implementing the"
                         "same small C interface (`tinyml_model_t`). tinyml_extensions"
                         "itself never allocates a model's weights or scratch memory, and"
                         "never allocates a tensor's input/output buffers - only the"
                         "per-instance handle itself. Buffers are always caller-allocated"
                         "plain LispBM byte arrays (`bufcreate`), and `tinyml-run` copies"
                         "nothing: it writes results directly into memory the caller"
                         "already owns."
                         ))
             (para (list "The building blocks are:"
                         ))
             (bullet '("**model** - a backend implementation of one neural network, registered by name at startup by C adapter code; not something a LispBM script creates."
                       "**instance** - a `tinyml-open`d handle to one open model, holding any scratch memory (`arena`) the model needs."
                       "**tensor type** - the `dtype`/`shape`/`scale`/`zero-point` alist `tinyml-input-type`/`tinyml-output-type`/`tinyml-info` return, describing one tensor without reference to any buffer."
                       ))
             (para (list "A minimal round trip looks like:"
                         ))
             (code '((define h (tinyml-open "moons"))
                     ))
             end))
   (section 1 "Reference"
            (list entry-tinyml-models
                  entry-tinyml-open
                  entry-tinyml-instance-p
                  entry-tinyml-n-inputs
                  entry-tinyml-n-outputs
                  entry-tinyml-input-bytes
                  entry-tinyml-output-bytes
                  entry-tinyml-input-type
                  entry-tinyml-output-type
                  entry-tinyml-info
                  entry-tinyml-run
                  ))
   (section 1 "Examples"
            (list example-endianness
                  example-tensorlib
                  example-program))
   info
   )
  )

(defun render-manual ()
  (let ((h (f-open "tinymlref.md" "w"))
        (r (lambda (s) (f-write-str h s))))
    {
    (gc)
    (var t0 (systime))
    (render r manual)
    (print "TinyML reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
