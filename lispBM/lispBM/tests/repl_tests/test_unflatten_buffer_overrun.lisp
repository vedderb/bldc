; Test unflatten buffer overrun vulnerability
; This test demonstrates that unflatten does not properly validate
; that claimed array sizes match the available buffer data.

; Create a malicious byte array that claims to contain a large flattened array
; but actually contains much less data than claimed
(define malicious-data [
  0x0D          ; S_LBM_ARRAY type tag (from lbm_flat_value.h)
  0x00 0x00 0x03 0xE8  ; Size field: claims 1000 bytes (big-endian)
  0x01 0x02 0x03 0x04  ; Only 4 bytes of actual data
])

; Try to unflatten the malicious data
; This should fail due to buffer overrun, but currently succeeds
; due to lack of proper bounds checking
(define result (trap (unflatten malicious-data)))

; Check the result - after the fix, unflatten should return nil for oversized claims
(if (and (eq (car result) 'exit-ok)
         (eq (car (cdr result)) nil))
    (print "SUCCESS - unflatten correctly rejected oversized array claim")
    (if (eq (car result) 'exit-error)
        (print "SUCCESS - unflatten correctly rejected oversized array claim with error")
        {
            (print "FAILURE - unflatten accepted oversized array claim")
            (print "This demonstrates a buffer bounds checking vulnerability!")
            (print (list "Result:" result))
        }))