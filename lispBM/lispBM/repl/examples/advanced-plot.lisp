
(define gp (gnuplot-open))

(define data (bufcreate (* 1000 4)))

(loopfor i 0 (< i 1000) (+ i 1) {
      (bufset-f32 data (* i 4) (sin (* i 0.01)) 'little-endian )
      })

(define f (f-open "wave.bin" "wb"))
(f-write f data)
(f-close f)

(gnuplot-cmd gp "set title 'Waveform'")
(gnuplot-cmd gp "plot 'wave.bin' binary array=1000 format='%float' with lines title 'Sine Wave'")
