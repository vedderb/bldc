

(define signal-re (bufcreate (* 4 5)))
(define signal-im (bufcreate (* 4 5)))

(bufset-f32 signal-re 0 1.0 'little-endian)


(define r0 (and (= (bufget-f32 signal-re 0 'little-endian) 1.0)
                (= (bufget-f32 signal-re 4 'little-endian) 0)))


(define filter-re (bufcreate (* 4 3)))
(define filter-im (bufcreate (* 4 3)))
(bufset-f32 filter-re 0 1.0 'little-endian)
(bufset-f32 filter-re 4 2.0 'little-endian)
(bufset-f32 filter-re 8 3.0 'little-endian)

(define res-cmplx (complex-convolve signal-re signal-im filter-re filter-im 'little-endian))

(define res (car res-cmplx))

(define v0 (bufget-f32 res 0 'little-endian))
(define v1 (bufget-f32 res 4 'little-endian))
(define v2 (bufget-f32 res 8 'little-endian))
(define v3 (bufget-f32 res 12 'little-endian))
(define v4 (bufget-f32 res 16 'little-endian))

(print v0 " " v1 " " v2 " " v3 " " v4)
(print res)

(define r1 (and (= v0 1.0)
                (= v1 2.0)
                (= v2 3.0)
                (= v3 0.0)
                (= v4 0.0)))

(if (and r0 r1)
    (print "SUCCESS")
  (print "FAILURE"))
