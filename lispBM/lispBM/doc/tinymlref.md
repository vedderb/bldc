# LispBM TinyML Library

This document describes how to use the tinyml_extensions to LispBM. tinyml_extensions provides a single, backend-agnostic interface for running inference on a compiled-in neural network - the model's own code may come from `onnx2c`, a hand-written engine, or any other backend implementing the same small C interface (`tinyml_model_t`). tinyml_extensions itself never allocates a model's weights or scratch memory, and never allocates a tensor's input/output buffers - only the per-instance handle itself. Buffers are always caller-allocated plain LispBM byte arrays (`bufcreate`), and `tinyml-run` copies nothing: it writes results directly into memory the caller already owns. 

The building blocks are: 

   - **model** - a backend implementation of one neural network, registered by name at startup by C adapter code; not something a LispBM script creates.
   - **instance** - a `tinyml-open`d handle to one open model, holding any scratch memory (`arena`) the model needs.
   - **tensor type** - the `dtype`/`shape`/`scale`/`zero-point` alist `tinyml-input-type`/`tinyml-output-type`/`tinyml-info` return, describing one tensor without reference to any buffer.

A minimal round trip looks like: 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define h (tinyml-open "moons"))
```


</td>
<td>

```clj
tinyml-instance
```


</td>
</tr>
</table>


# Reference


### tinyml-models

Returns the names of all models registered (by C-side adapter code, at startup) with the tinyml extensions. The form of a `tinyml-models` expression is `(tinyml-models)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tinyml-models)
```


</td>
<td>

```clj
("moons")
```


</td>
</tr>
</table>




---


### tinyml-open

Opens an instance of a registered model by name, allocating any scratch memory (`arena`) the model's adapter declared it needs and running the adapter's one-time `init`, if it has one. The form of a `tinyml-open` expression is `(tinyml-open name)`. 

Returns an instance handle - an opaque custom-type value, not a plain array, specifically so that it cannot be accidentally captured into a saved LispBM image (custom types are not flattenable, so attempting to persist one fails loudly instead of silently saving a pointer that would be invalid on the next boot). The handle's underlying memory is freed automatically by the garbage collector once it becomes unreachable. 

Raises `eval_error` if no model with that name is registered. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define h (tinyml-open "moons"))
```


</td>
<td>

```clj
tinyml-instance
```


</td>
</tr>
<tr>
<td>

```clj
(tinyml-instance? h)
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


### tinyml-instance?

Checks if the argument is a tinyml instance handle, as returned by `tinyml-open`. The form of a `tinyml-instance?` expression is `(tinyml-instance? v)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tinyml-instance? h)
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
(tinyml-instance? 'apa)
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


### tinyml-n-inputs

Returns the number of input tensors a model takes. The form of a `tinyml-n-inputs` expression is `(tinyml-n-inputs h)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tinyml-n-inputs h)
```


</td>
<td>

```clj
1
```


</td>
</tr>
</table>




---


### tinyml-n-outputs

Returns the number of output tensors a model produces. The form of a `tinyml-n-outputs` expression is `(tinyml-n-outputs h)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tinyml-n-outputs h)
```


</td>
<td>

```clj
1
```


</td>
</tr>
</table>




---


### tinyml-input-bytes

Returns the exact byte size input tensor `i` expects - the size to pass to `bufcreate` when allocating a buffer for `tinyml-run`. The form of a `tinyml-input-bytes` expression is `(tinyml-input-bytes h i)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tinyml-input-bytes h 0)
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


### tinyml-output-bytes

Returns the exact byte size output tensor `i` produces. The form of a `tinyml-output-bytes` expression is `(tinyml-output-bytes h i)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tinyml-output-bytes h 0)
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


### tinyml-input-type

Returns input tensor `i`'s type as an association list: `dtype` (one of the symbols `f32`, `i8`, `u8`, `i16`), `shape` (a list of dimensions, no batch dimension), `scale` and `zero-point` (quantization parameters, `1.0`/`0` for a plain float tensor). The form of a `tinyml-input-type` expression is `(tinyml-input-type h i)`. 

The returned alist is exactly the shape `tensorlib.lisp`'s `tensor-set`/`tensor-to-list` expect as their `type` argument (see the Example section below) - it is plain value data, safe to store, pass around or build by hand. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tinyml-input-type h 0)
```


</td>
<td>

```clj
((dtype . f32) (shape 2) (scale . 1.000000f32) (zero-point . 0))
```


</td>
</tr>
</table>




---


### tinyml-output-type

Returns output tensor `i`'s type, in the same alist shape as `tinyml-input-type`. The form of a `tinyml-output-type` expression is `(tinyml-output-type h i)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tinyml-output-type h 0)
```


</td>
<td>

```clj
((dtype . f32) (shape 2) (scale . 1.000000f32) (zero-point . 0))
```


</td>
</tr>
</table>




---


### tinyml-info

Returns everything about a model in one call: its `name`, `n-inputs`, `n-outputs`, and the full `inputs`/`outputs` lists of per-tensor type alists (the same shape `tinyml-input-type`/ `tinyml-output-type` return one of). The form of a `tinyml-info` expression is `(tinyml-info h)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tinyml-info h)
```


</td>
<td>

```clj
((name . "moons") (n-inputs . 1) (n-outputs . 1) (inputs ((dtype . f32) (shape 2) (scale . 1.000000f32) (zero-point . 0))) (outputs ((dtype . f32) (shape 2) (scale . 1.000000f32) (zero-point . 0))))
```


</td>
</tr>
</table>




---


### tinyml-run

Runs one inference. `in`/`out` are byte arrays (from `bufcreate`), sized exactly as `tinyml-input-bytes`/ `tinyml-output-bytes` report - `tinyml-run` validates this and raises `type_error` on any mismatch. With more than one input or output tensor, pass a list of buffers instead of a single one. The form of a `tinyml-run` expression is `(tinyml-run h in out)`. 

Neither `in` nor `out` is allocated by `tinyml-run` - the caller allocates both, and `tinyml-run` writes results directly into `out`'s memory with no copying anywhere in the call chain down to the underlying model code. Returns `t` on success, or raises `eval_error` if the underlying model reports failure. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define in (bufcreate (tinyml-input-bytes h 0)))
```


</td>
<td>

```clj
[0 0 0 0 0 0 0 0]
```


</td>
</tr>
<tr>
<td>

```clj
(define out (bufcreate (tinyml-output-bytes h 0)))
```


</td>
<td>

```clj
[0 0 0 0 0 0 0 0]
```


</td>
</tr>
<tr>
<td>

```clj
(bufset-f32 in 0 1.000000f32 'little-endian)
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
(bufset-f32 in 4 -0.400000f32 'little-endian)
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
(tinyml-run h in out)
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
(bufget-f32 out 0 'little-endian)
```


</td>
<td>

```clj
-2.506411f32
```


</td>
</tr>
<tr>
<td>

```clj
(bufget-f32 out 4 'little-endian)
```


</td>
<td>

```clj
2.226587f32
```


</td>
</tr>
</table>




---

# Examples


### A note on byte order

`bufset-*`/`bufget-*` default to **big-endian** unless `'little-endian` is passed explicitly, but `tinyml-run`'s underlying model code always reads and writes tensor bytes in the target's native order - little-endian on essentially any real target (ARM Cortex-M, RISC-V, x86). Forgetting `'little-endian` on a `bufset-*`/`bufget-*` call touching a tinyml buffer does not raise an error - it silently produces byte-swapped garbage. 




---


### Example: tensorlib.lisp

Writing `bufset-f32`/`bufget-f32` calls by hand, with the right byte offsets and `'little-endian` on every call, gets tedious and error-prone once a model has more than a couple of elements per tensor. `repl/examples/tinyml/tensorlib.lisp` is a small pure-Lisp library - built entirely on the generic `bufset-*`/`bufget-*` extensions plus a type alist from `tinyml-input-type`/`tinyml-output-type` - that removes both problems: `tensor-set`/`tensor-to-list` compute the right byte offset from a shape and a multi-dimensional index, dispatch to the right typed `bufset-*`/`bufget-*` automatically, and always pass `'little-endian` internally. 

|Function || 
 |----|----|
 `(tensor-set type indices tensor value)`   | Writes one element at the position `indices` fully specifies.
 `(tensor-to-list type indices buf)`        | Reads a slice back as a list - `indices` fixes a prefix of leading dimensions (row-major); `'()` flattens the whole tensor.
 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(import "../repl/examples/tinyml/tensorlib.lisp" 'tensorlib-src)
```


</td>
<td>

```clj
"
(defun shape-product (s)
  (if (eq s nil) 1 (* (car s) (shape-product (cdr s)))))

(defun tensor-offset (shape indices)
  (if (eq indices nil)
      0
      (+ (* (car indices) (shape-product (cdr shape)))
         (tensor-offset (cdr shape) (cdr indices)))))

(defun dtype-bytes (dtype)
  (cond
    ((eq dtype 'f32) 4)
    ((eq dtype 'i16) 2)
    (t 1)))

(defun list-drop (lst k)
  (if (= k 0) lst (list-drop (cdr lst) (- k 1))))


(defun bufset-typed (dtype buf offset value)
  (cond
    ((eq dtype 'f32) (bufset-f32 buf offset value 'little-endian))
    ((eq dtype 'i16) (bufset-i16 buf offset value 'little-endian))
    ((eq dtype 'i8)  (bufset-i8  buf offset value))
    ((eq dtype 'u8)  (bufset-u8  buf offset value))
    (t 'type_error)))

(defun tensor-set (type indices tensor value)
  (let ((dtype (assoc type 'dtype))
        (shape (assoc type 'shape)))
    (let ((offset (* (tensor-offset shape indices) (dtype-bytes dtype))))
      (bufset-typed dtype tensor offset value))))

(defun bufget-typed (dtype buf offset)
  (cond
    ((eq dtype 'f32) (bufget-f32 buf offset 'little-endian))
    ((eq dtype 'i16) (bufget-i16 buf offset 'little-endian))
    ((eq dtype 'i8)  (bufget-i8  buf offset))
    ((eq dtype 'u8)  (bufget-u8  buf offset))
    (t 'type_error)))

(defun tensor-read-n (dtype elem-bytes buf offset n)
  (if (= n 0)
      nil
      (cons (bufget-typed dtype buf offset)
            (tensor-read-n dtype elem-bytes buf (+ offset elem-bytes) (- n 1)))))

(defun tensor-to-list (type indices buf)
  (let ((dtype (assoc type 'dtype))
        (shape (assoc type 'shape)))
    (let ((eb (dtype-bytes dtype))
          (base-offset (* (tensor-offset shape indices) (dtype-bytes dtype)))
          (remaining-shape (list-drop shape (length indices))))
      (tensor-read-n dtype eb buf base-offset (shape-product remaining-shape)))))
"
```


</td>
</tr>
<tr>
<td>

```clj
(read-eval-program tensorlib-src)
```


</td>
<td>

```clj
(closure (type indices buf) 
  (let ((dtype (assoc type 'dtype))
        (shape (assoc type 'shape)))
       (let ((eb (dtype-bytes dtype))
             (base-offset (* (tensor-offset shape indices) (dtype-bytes dtype)))
             (remaining-shape (list-drop shape (length indices))))
            (tensor-read-n dtype eb buf base-offset (shape-product remaining-shape))))
  nil)
```


</td>
</tr>
<tr>
<td>

```clj
(define in-type (tinyml-input-type h 0))
```


</td>
<td>

```clj
((dtype . f32) (shape 2) (scale . 1.000000f32) (zero-point . 0))
```


</td>
</tr>
<tr>
<td>

```clj
(define out-type (tinyml-output-type h 0))
```


</td>
<td>

```clj
((dtype . f32) (shape 2) (scale . 1.000000f32) (zero-point . 0))
```


</td>
</tr>
<tr>
<td>

```clj
(define in (bufcreate (tinyml-input-bytes h 0)))
```


</td>
<td>

```clj
[0 0 0 0 0 0 0 0]
```


</td>
</tr>
<tr>
<td>

```clj
(define out (bufcreate (tinyml-output-bytes h 0)))
```


</td>
<td>

```clj
[0 0 0 0 0 0 0 0]
```


</td>
</tr>
<tr>
<td>

```clj
(tensor-set in-type '(0) in 1.000000f32)
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
(tensor-set in-type '(1) in -0.400000f32)
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
(tinyml-run h in out)
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
(tensor-to-list out-type 'nil out)
```


</td>
<td>

```clj
(-2.506411f32 2.226587f32)
```


</td>
</tr>
</table>

See `repl/examples/tinyml/moons.lisp` for the full worked example this reference's model comes from: a tiny MLP trained on `sklearn.datasets.make_moons`, exported to ONNX, translated to C with `onnx2c`, and wired in as the `"moons"` model. 




---


### Example: a small classification program

Putting the pieces together: a self-contained `classify` function built on `tensorlib.lisp` (from the previous example, so `h`/`in-type`/`out-type` are already in scope), run over a few points. The whole program below is evaluated as one unit and its final result shown, not one row per form - this is the same shape of code as `repl/examples/tinyml/moons.lisp`, just condensed. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(defun classify (x y)
  (let ((in (bufcreate (tinyml-input-bytes h 0)))
        (out (bufcreate (tinyml-output-bytes h 0))))
       (progn 
           (tensor-set in-type '(0) in x)
           (tensor-set in-type '(1) in y)
           (tinyml-run h in out)
           (let ((scores (tensor-to-list out-type 'nil out)))
                (if (> (car (cdr scores)) (car scores)) 1 0)))))
(list (classify -1.000000f32 0.400000f32) (classify 1.000000f32 -0.400000f32) (classify 0.500000f32 -0.300000f32))
```


</td>
<td>


```clj
(0 1 1)
```


</td>
</tr>
</table>




---

This document was generated by LispBM version 0.40.0 

