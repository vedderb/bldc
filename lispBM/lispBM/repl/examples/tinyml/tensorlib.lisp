
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
