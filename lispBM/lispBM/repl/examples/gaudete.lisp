
;; Lisp interpretation of Gaudette

(defun male-voice (patch-num)
  {
  (patch-clear patch-num)
  ;; Slow attack for breath, longer decay, lower sustain for rhythmic emphasis, gentle release
  (patch-adsr-set patch-num 0.05 0.35 0.3 0.2)
  ;; Mix of saw (fundamental) and pulse (adds throaty quality)
  (patch-osc-tvp-set patch-num 0 'osc-saw 0.6 0.0)   ;0.7
  (patch-osc-tvp-set patch-num 1 'osc-square 0.2 0.0) ;0.3
  ;; Subtle vibrato (5-6 Hz is natural vocal vibrato)
  (patch-lfo-set patch-num 0 'osc-sine 5.5)
  (patch-mod-set patch-num 0 1 'mod-lfo1 2.5)   ; gentle pitch vibrato
  (patch-mod-set patch-num 1 1 'mod-lfo1 2.0)
  ;; Low-pass filter to cut harsh highs, simulate vocal formants
  (patch-filter-set patch-num 'simple-lpf 1800.0)
  })

;; Pure, clear female voice patch (soprano/alto) - Powerful version
;; Characteristics: stronger projection, more presence, richer harmonics
(defun female-voice (patch-num)
  {
  (patch-clear patch-num)
  ;; Quick attack for clarity, longer decay, lower sustain for rhythmic emphasis
  (patch-adsr-set patch-num 0.02 0.32 0.3 0.15)
  ;; Mix of saw and triangle for more power and harmonics
  (patch-osc-tvp-set patch-num 0 'osc-saw 0.5 0.0) ;0.6    ; adds richness
  (patch-osc-tvp-set patch-num 1 'osc-tri 0.3 0.0) ;0.4   ; keeps clarity
  ;; Stronger vibrato for dramatic effect
  (patch-lfo-set patch-num 0 'osc-sine 6.2)
  (patch-mod-set patch-num 0 1 'mod-lfo1 3.5)   ; stronger pitch vibrato
  (patch-mod-set patch-num 1 1 'mod-lfo1 3.0)
  ;; Even higher cutoff for brilliant, cutting sound
  (patch-filter-set patch-num 'simple-lpf 4500.0)
  })

;; Alternative: Dark male voice - more distinct from female
(defun male-voice-dark (patch-num)
  {
  (patch-clear patch-num)
  ;; Slower, weightier attack
  (patch-adsr-set patch-num 0.08 0.35 0.3 0.25)
  ;; More square wave for darker, hollow chest voice quality
  (patch-osc-tvp-set patch-num 0 'osc-saw 0.4 0.0)
  (patch-osc-tvp-set patch-num 1 'osc-square 0.5 0.0)  ; Dominant square = darker
  ;; Slower, deeper vibrato
  (patch-lfo-set patch-num 0 'osc-sine 4.5)
  (patch-mod-set patch-num 0 1 'mod-lfo1 2.0)
  (patch-mod-set patch-num 1 1 'mod-lfo1 1.5)
  ;; Much lower filter for warm, dark sound
  (patch-filter-set patch-num 'simple-lpf 1200.0)
  })

;; Alternative: Bright female voice - more distinct from male
(defun female-voice-bright (patch-num)
  {
  (patch-clear patch-num)
  ;; Very quick, crisp attack
  (patch-adsr-set patch-num 0.01 0.32 0.3 0.15)
  ;; More triangle for purer, brighter tone
  (patch-osc-tvp-set patch-num 0 'osc-tri 0.6 0.0)  ; Dominant triangle = brighter
  (patch-osc-tvp-set patch-num 1 'osc-saw 0.3 0.0)
  ;; Faster, lighter vibrato
  (patch-lfo-set patch-num 0 'osc-sine 6.8)
  (patch-mod-set patch-num 0 1 'mod-lfo1 4.0)
  (patch-mod-set patch-num 1 1 'mod-lfo1 3.5)
  ;; Very high filter for bright, cutting sound
  (patch-filter-set patch-num 'simple-lpf 5500.0)
  })

;; Frame drum / Bodhran patch
;; Characteristics: short, punchy attack with quick decay
(defun drum-patch (patch-num)
  {
  (patch-clear patch-num)
  ;; Very short attack, longer decay for more presence
  (patch-adsr-set patch-num 0.001 0.3 0.0 0.1)
  ;; More noise for attack punch, strong sine for body
  (patch-osc-tvp-set patch-num 0 'osc-noise 0.8 0.0)   ; Attack noise (louder)
  (patch-osc-tvp-set patch-num 1 'osc-sine 0.1 0.0)    ; Drum body (louder)
  ;; No LFO needed for drum
  ;; Higher cutoff for more presence
  (patch-filter-set patch-num 'simple-lpf 1500.0)
  })

;; Tom-tom drum patch
;; Characteristics: lower pitch, resonant, pitch decay
(defun tom-patch (patch-num)
  {
  (patch-clear patch-num)
  ;; Short attack, medium decay for resonance
  (patch-adsr-set patch-num 0.001 0.4 0.0 0.2)
  ;; Less noise, more tone for tom sound
  (patch-osc-tvp-set patch-num 0 'osc-sine 1.5 0.5)   ; Attack click
  (patch-osc-tvp-set patch-num 1 'osc-sine 1.5 0.0)    ; Tom body (dominant)
  ;; Pitch envelope - pitch drops as note decays (tom characteristic)
  (patch-mod-set patch-num 1 0 'mod-env 12.0)          ; Pitch drops an octave
  ;; Low-pass filter for warm tom sound
  (patch-filter-set patch-num 'simple-lpf 4000.0)
  })

;; Initialize patches
;;(male-voice 0)
;;(female-voice 1)
(male-voice-dark 0)
(female-voice-bright 1)
(tom-patch 2)
(drum-patch 3)


(patch-osc-pan-set 0 0 -0.7)  ; Oscillator 0 slightly left
(patch-osc-pan-set 0 1 -0.7)  ; Oscillator 1 slightly left

(patch-osc-pan-set 1 0 0.7)   ; Oscillator 0 slightly right
(patch-osc-pan-set 1 1 0.7)   ; Oscillator 1 slightly right



(define A2 45)
;; Note definitions (MIDI note numbers)
;; Octave 3
(define C3 48)
(define D3 50)
(define E3 52)
(define F3 53)
(define G3 55)
(define A3 57)
(define B3 59)

;; Octave 4 (Middle octave)
(define C4 60)
(define D4 62)
(define E4 64)
(define F4 65)
(define G4 67)
(define A4 69)
(define B4 71)

;; Octave 5
(define C5 72)
(define D5 74)
(define E5 76)
(define F5 77)
(define G5 79)
(define A5 81)
(define B5 83)

(define sharp (lambda (x) (+ x 1)))
(define flat  (lambda (x) (- x 1)))

;; Lets define a note as the pair '(note-type, note-val)
;; so for example a quarter-note middle C sharp is `(quarter . ,(sharp C4))
;; a rest is represented as '(quarter . rest)

;; a bar is a list of notes with a time-signature. '(t-sig (n1 n2 n3 n4))


(define treble-1 (list '(4 . 4)
                 (list `(quarter . ,E4) `(quarter . ,A4))
                 (list `(quarter . ,E4) `(quarter . ,A4))
                 (list `(quarter . ,E4) `(quarter . ,G4))
                 (list `(eighth  . ,E4) `(eighth . ,A4))
                 (list `(eighth  . ,G4) `(eighth . ,B4))))
(define treble-2 (list '(6 . 8)
                        (list `(quarter . ,G4) `(quarter . ,C5))
                        (list `(eighth . ,G4) `(eighth . ,C5))
                        (list `(quarter . ,G4) `(quarter . ,B4))
                        (list `(eighth . ,E4) `(eighth . ,A4))))
(define treble-3 (list '(2 . 4)
                        (list `(quarter . ,E4) `(quarter . ,G4))
                        (list `(quarter . ,E4) `(quarter . ,G4))))
(define treble-4 (list '(4 . 4)
                        (list `(quarter . ,E4) `(quarter . ,G4))
                        (list `(quarter . ,E4) `(quarter . ,A4))
                        (list `(quarter-dotted . ,G4) `(quarter-dotted . ,B4))
                        (list `(eighth . ,E4)  `(eighth . ,A4))))
(define treble-5 (list '(6 . 8)
                       (list `(quarter . ,E4) `(quarter . ,G4))
                       (list `(eighth . ,E4) `(eighth . ,A4))
                       (list `(quarter . ,G4) `(quarter . ,B4))
                       (list `(eighth . ,E4)  `(eighth . ,A4)))) 
(define treble-6 treble-3)


(define bass-1 (list '(4 . 4)
                     (list `(quarter . ,A3))
                     (list `(quarter . ,A3) `(quarter . ,C4))
                     (list `(quarter . ,E3) `(quarter . ,B3))
                     (list `(eighth  . ,A3) `(eighth . ,C4))
                     (list `(eighth  . ,G3) `(eighth . ,D4))))
(define bass-2 (list '(6 . 8)
                     (list `(quarter . ,C3) `(quarter . ,E4))
                     (list `(eighth . ,C3) `(eighth . ,E4))
                     (list `(quarter . ,G3) `(quarter . ,D4))
                     (list `(eighth . ,A3) `(eighth . ,C4))))
(define bass-3 (list '(2 . 4)
                     (list `(quarter . ,E3) `(quarter . ,B3))
                     (list `(quarter . ,E3) `(quarter . ,B3))))
(define bass-4 (list '(4 . 4)
                     (list `(quarter . ,E3) `(quarter . ,B3))
                     (list `(quarter . ,A3) `(quarter . ,C4))
                     (list `(quarter-dotted . ,G3) `(quarter-dotted . ,B3))
                     (list `(eighth . ,A3) `(eighth . ,C4))))
(define bass-5 (list '(6 . 8)
                     (list `(quarter . ,E3) `(quarter . ,B3))
                     (list `(eighth  . ,A3) `(eighth  . ,C4))
                     (list `(quarter . ,G3) `(quarter . ,D4))
                     (list `(eighth  . ,A3) `(eighth . ,C4))))
(define bass-6 (list '(2 . 4)
                     (list `(quarter . ,E3) `(quarter . ,B3))
                     (list `(quarter . ,A2) `(quarter . ,A3))))


(define solo-treb-1 (list '(4 . 4)
                          (list `(eighth . ,A4))
                          (list `(eighth . ,A4))
                          (list `(eighth . ,G4))
                          (list `(eighth . ,A4))
                          (list `(eighth . ,C5))
                          (list `(eighth . ,B4))
                          (list `(quarter . ,A4))))
(define solo-treb-2 (list '(4 . 4)
                          (list `(eighth . ,A4))
                          (list `(eighth . ,F4))
                          (list `(eighth . ,E4))
                          (list `(eighth . ,F4))
                          (list `(quarter . ,D4))
                          (list `(quarter . ,D4))))
(define solo-treb-3 (list '(4 . 4)
                          (list `(eighth . ,D4))
                          (list `(eighth . ,D4))
                          (list `(eighth . ,F4))
                          (list `(eighth . ,D4))
                          (list `(eighth . ,F5))
                          (list `(eighth . ,G4))
                          (list `(quarter . ,A4))))
(define solo-treb-4 (list '(4 . 4)
                          (list `(eighth . ,C5))
                          (list `(eighth . ,A4))
                          (list `(eighth . ,B4))
                          (list `(eighth . ,C5))
                          (list `(quarter . ,A4))
                          (list `(quarter . ,A4))))

(define solo-bass-1 (list '(4 . 4)
                          (list `(eighth . ,A3))
                          (list `(eighth . ,A3))
                          (list `(eighth . ,G3))
                          (list `(eighth . ,A3))
                          (list `(eighth . ,C4))
                          (list `(eighth . ,B4))
                          (list `(quarter . ,A3))))
(define solo-bass-2 (list '(4 . 4)
                          (list `(eighth . ,A3))
                          (list `(eighth . ,F3))
                          (list `(eighth . ,E3))
                          (list `(eighth . ,F3))
                          (list `(quarter . ,D3))
                          (list `(quarter . ,D3))))
(define solo-bass-3 (list '(4 . 4)
                          (list `(eighth . ,D3))
                          (list `(eighth . ,D3))
                          (list `(eighth . ,F3))
                          (list `(eighth . ,D3))
                          (list `(eighth . ,F4))
                          (list `(eighth . ,G3))
                          (list `(quarter . ,A3))))
(define solo-bass-4 (list '(4 . 4)
                          (list `(eighth . ,C4))
                          (list `(eighth . ,A3))
                          (list `(eighth . ,B3))
                          (list `(eighth . ,C4))
                          (list `(quarter . ,A3))
                          (list `(quarter . ,A3))))
(define DRUM 60)

(define perc-4/4 (list '(4 . 4)
                       (list `(eighth . ,DRUM))
                       (list `(eighth . rest))
                       (list `(quarter . rest))
                       (list `(eighth . ,DRUM))
                       (list `(eighth . rest))
                       (list `(quarter . rest))))

(define perc-2/4 (list '(2 . 4)
                       (list `(eighth . ,DRUM))
                       (list `(eighth . rest))
                       (list `(quarter . rest))))

(define perc-6/8 (list '(6 . 8)
                       (list `(eighth . ,DRUM))
                       (list `(eighth . rest))
                       (list `(eighth . rest))
                       (list `(eighth . ,DRUM))
                       (list `(eighth . rest))
                       (list `(eighth . rest))))


(defun bar-time-sig (bar) (car bar))
(defun bar-notes (bar) (cdr bar))

(defun quarter-duration (bpm)
  (/ 60000000 bpm))

(define note-duration
    (lambda (x bpm)
      (let ((q-dur (quarter-duration bpm)))
        (cond ((eq x 'whole) (* 4.0 q-dur))
              ((eq x 'half)  (* 2.0 q-dur))
              ((eq x 'quarter) q-dur)
              ((eq x 'eighth) (* 0.5 q-dur))
              ((eq x 'sixteenth) (* 0.25 q-dur))
              ((eq x 'quarter-dotted) (* 1.5 q-dur))
              ((eq x 'eighth-dotted) (* 0.75 q-dur))
              (t q-dur)))))



(defun beat-strengths (time-sig)
  (let ((beats (car time-sig))
        (note-value (cdr time-sig)))
    (cond ((eq beats 4) '(strong weak medium weak))            ; 4/x
          ((eq beats 3) '(strong weak weak))                   ; 3/x
          ((eq beats 2) '(strong weak))                        ; 2/x
          ((eq beats 6) '(strong weak weak medium weak weak))  ; 6/x
          (t (cons 'strong (map (lambda (x) 'weak) (range 1 beats)))))))

(defun strength-to-velocity (strength)
  (cond ((eq strength 'strong) 110)
        ((eq strength 'medium) 95)
        ((eq strength 'weak) 80)
        (t 90)))

(defun cmp-events (a b)
  (< (car a) (car b)))

;; Convert bar to events, returns (t-end . event-list)
(defun bar-to-events (bar bpm patch-no t-zero) {
       (var s-events nil)
       (var e-events nil)
       (var t-now t-zero)
       (var time-sig (bar-time-sig bar))
       (var strengths (beat-strengths time-sig))
       (var beat-idx 0)

       (loopforeach b (bar-notes bar) {
             (var max-dur 0)

             (var velocity (strength-to-velocity (ix strengths beat-idx)))

             (loopforeach k b { ;; For each note in chord
                   (var dur (note-duration (car k) bpm))
                   (if (> dur max-dur) (setq max-dur dur))

                   (if (not (eq (cdr k) 'rest))
                       {
                       (setq s-events (cons `(,t-now (note-on ,patch-no ,(cdr k) ,velocity)) s-events))
                       (setq e-events (cons `(,(+ t-now dur) (note-off ,patch-no ,(cdr k))) e-events))
                       }
                       nil)
                   })
             (setq t-now (+ t-now max-dur))
             (setq beat-idx (+ beat-idx 1))
             })

       (cons t-now (append s-events e-events))
       })

;; Convert a list of bars to events
;; Returns (t-end . event-list)
(defun bars-to-events (bars bpm patch-no t-zero) {
       (var t-now t-zero)
       (var all-events nil)

       (loopforeach bar bars {
             (var result (bar-to-events bar bpm patch-no t-now))
             (setq t-now (car result))
             (setq all-events (append all-events (cdr result)))
             })

       (cons t-now all-events)
       })

(defun tracks-to-events (tracks) {
       (var all-events nil)

       (loopforeach track tracks {
             (var bars (car track))
             (var bpm (car (cdr track)))
             (var patch (car (cdr (cdr track))))
             (var result (bars-to-events bars bpm patch 0))
             (setq all-events (append all-events (cdr result)))
             })

       (sort cmp-events all-events)
       })

(defun play-events (xs)
  (let ((t0 (systime)))
    (loopforeach x xs {
          (loopwhile (< (- (systime) t0) (car x)) nil )
          (eval (car (cdr x)))
          })))

(define t1 (list treble-1 treble-2 treble-3 treble-4 treble-5 treble-6))
(define t2 (list bass-1   bass-2   bass-3   bass-4   bass-5   bass-6))
(define t3 (list perc-4/4  perc-6/8  perc-2/4  perc-4/4  perc-6/8  perc-2/4))

(define r1 (append t1 t1))
(define r2 (append t2 t2))
(define r3 (append t3 t3))

(define s1 (list solo-treb-1 solo-treb-2 solo-treb-3 solo-treb-4))
(define s2 (list solo-bass-1 solo-bass-2 solo-bass-3 solo-bass-4))
(define s3 (list perc-4/4 perc-4/4 perc-4/4 perc-4/4))

(define play-drums
    (lambda ()
      (play-events (tracks-to-events (list (list r3 120 2))))))  ; percussion only

(define play-Chorus
    (lambda ()
      (play-events (tracks-to-events (list (list r1 110 1)
                                            (list r2 110 0)
                                            (list r3 110 2)))))) 
(define play-solo
    (lambda ()
      (play-events (tracks-to-events (list (list s1 110 1)
                                           (list s2 110 0)
                                           (list s3 110 2))))))
                                     
                                       


(define play-it
    (lambda () {
      (play-chorus)
      (play-solo)
      (play-chorus)
      (play-solo)
      (play-chorus)
      }))
    

