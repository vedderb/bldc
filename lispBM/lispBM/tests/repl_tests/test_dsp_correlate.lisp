(define signal (bufcreate (* 4 5)))
(bufset-f32 signal 8 1.0 'little-endian)

(define r0 (and (= (bufget-f32 signal 8 'little-endian) 1.0)
                (= (bufget-f32 signal 0 'little-endian) 0)))

(define filter (bufcreate (* 4 3)))
(bufset-f32 filter 0 1.0 'little-endian)
(bufset-f32 filter 4 2.0 'little-endian)
(bufset-f32 filter 8 3.0 'little-endian)

(define res (correlate signal filter 'little-endian))

(define v0 (bufget-f32 res 0 'little-endian))
(define v1 (bufget-f32 res 4 'little-endian))
(define v2 (bufget-f32 res 8 'little-endian))
(define v3 (bufget-f32 res 12 'little-endian))
(define v4 (bufget-f32 res 16 'little-endian))

(define r1 (and (= v0 3.0)
                (= v1 2.0)
                (= v2 1.0)
                (= v3 0.0)
                (= v4 0.0)))

(if (and r0 r1)
    (print "SUCCESS")
  (print "FAILED"))
