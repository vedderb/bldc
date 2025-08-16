; Test to verify the array buffer overrun vulnerability has been fixed

(print "Verifying array vulnerability fix...")

; Create malicious byte array claiming to be a 100-byte array
; but only containing 4 bytes of actual data
(define exploit-data [
  0x0D          ; S_LBM_ARRAY type tag
  0x00 0x00 0x00 0x64  ; Size: claims 100 bytes (0x64 = 100)
  0xAA 0xBB 0xCC 0xDD  ; Only 4 bytes of actual data
])

; Attempt to unflatten - this should now return nil due to bounds checking
(define result (trap (unflatten exploit-data)))

; Check what we got
(print "Unflatten result type:" (car result))
(define value (car (cdr result)))
(print "Unflatten value:" value)

(if (and (eq (car result) 'exit-ok)
         (eq value nil))
    (print "SUCCESS: Array vulnerability has been fixed! Returns nil for oversized claims")
    (if (eq (car result) 'exit-error)
        (print "SUCCESS: Array vulnerability has been fixed! Returns error for oversized claims") 
        {
            (print "FAILURE: Array vulnerability still exists")
            (print "Got unexpected result")
        }
    ))