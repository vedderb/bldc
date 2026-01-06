#!/usr/bin/env -S shlbm -M 1048576 --

;; connect the midi-player to the LBM synth.
;; aplaymidi -p 128:0 level1.mid
;;
;; check with 'aconnect -l' for seeing which port LBM is located at
;; you will see something as
;;   Client N: 'LispBM MIDI'
;;        P 'LispBM MIDI In'
;; use N  and P in the aplaymidi command 'aplaymidi -p N:P midi-file.mid'

;; Doom E1M1 Patches

;; Distortion Guitar - Main riff
(defun distortion-guitar (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 0.005 0.1 0.6 0.15)
  (patch-osc-tvp-set patch-num 0 'osc-square 0.7 0.0)
  (patch-osc-tvp-set patch-num 1 'osc-saw 0.6 0.02)
  (patch-filter-set patch-num 'simple-lpf 2500.0)
  })

;; Electric Bass
(defun electric-bass (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 0.003 0.08 0.7 0.1)
  (patch-osc-tvp-set patch-num 0 'osc-sine 0.8 0.0)
  (patch-osc-tvp-set patch-num 1 'osc-tri 0.3 0.0)
  (patch-filter-set patch-num 'simple-lpf 800.0)
  })

;; Bass Drum
(defun bass-drum (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 0.001 0.15 0.0 0.05)
  (patch-osc-tvp-set patch-num 0 'osc-sine 2.0 0.0)
  (patch-osc-tvp-set patch-num 1 'osc-noise 0.3 0.0)
  (patch-mod-set patch-num 0 0 'mod-env 24.0)
  (patch-filter-set patch-num 'simple-lpf 200.0)
  })

;; Snare Drum
(defun snare-drum (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 0.001 0.12 0.0 0.08)
  (patch-osc-tvp-set patch-num 0 'osc-noise 1.2 0.0)
  (patch-osc-tvp-set patch-num 1 'osc-sine 0.4 0.0)
  (patch-filter-set patch-num 'simple-lpf 3000.0)
  })

;; Hi-hat Closed
(defun hihat-closed (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 0.001 0.05 0.0 0.03)
  (patch-osc-tvp-set patch-num 0 'osc-noise 0.8 0.0)
  (patch-filter-set patch-num 'simple-lpf 8000.0)
  })

;; Hi-hat Open
(defun hihat-open (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 0.001 0.25 0.2 0.15)
  (patch-osc-tvp-set patch-num 0 'osc-noise 0.7 0.0)
  (patch-filter-set patch-num 'simple-lpf 9000.0)
  })

;; Crash Cymbal
(defun crash-cymbal (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 0.001 0.8 0.1 0.6)
  (patch-osc-tvp-set patch-num 0 'osc-noise 1.0 0.0)
  (patch-filter-set patch-num 'simple-lpf 10000.0)
  })

;; Tom Drum
(defun tom-drum (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 0.001 0.3 0.0 0.15)
  (patch-osc-tvp-set patch-num 0 'osc-sine 1.5 0.0)
  (patch-osc-tvp-set patch-num 1 'osc-noise 0.3 0.0)
  (patch-mod-set patch-num 0 0 'mod-env 8.0)
  (patch-filter-set patch-num 'simple-lpf 2000.0)
  })

;; String Ensemble
(defun string-ensemble (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 0.08 0.3 0.7 0.4)
  (patch-osc-tvp-set patch-num 0 'osc-saw 0.5 0.0)
  (patch-osc-tvp-set patch-num 1 'osc-saw 0.5 0.015)
  (patch-lfo-set patch-num 0 'osc-sine 5.0)
  (patch-mod-set patch-num 0 1 'mod-lfo1 3.0)
  (patch-mod-set patch-num 1 1 'mod-lfo1 3.0)
  (patch-filter-set patch-num 'simple-lpf 3500.0)
  })

;; Brass Section
(defun brass-section (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 0.02 0.15 0.6 0.2)
  (patch-osc-tvp-set patch-num 0 'osc-square 0.6 0.0)
  (patch-osc-tvp-set patch-num 1 'osc-saw 0.5 0.0)
  (patch-lfo-set patch-num 0 'osc-sine 5.5)
  (patch-mod-set patch-num 0 1 'mod-lfo1 2.0)
  (patch-mod-set patch-num 1 1 'mod-lfo1 2.0)
  (patch-filter-set patch-num 'simple-lpf 4000.0)
  })

;; Setup patches for each MIDI channel
;; Channels 0-8, 10-15 for melodic instruments
(distortion-guitar 0)
(distortion-guitar 1)
(electric-bass 2)
(string-ensemble 3)
(brass-section 4)
(distortion-guitar 5)
(electric-bass 6)
(string-ensemble 7)
(brass-section 8)

;; Channel 9 - Drum kit (map by note number, not channel)
(bass-drum 10)
(snare-drum 11)
(hihat-closed 12)
(hihat-open 13)
(crash-cymbal 14)
(tom-drum 15)



;; Map MIDI percussion notes to patches
(defun drum-note-to-patch (note)
  (cond ((or (= note 35) (= note 36)) 10)  ; Bass drum
        ((or (= note 38) (= note 40)) 11)  ; Snare
        ((or (= note 42) (= note 44)) 12)  ; Hi-hat closed
        ((or (= note 46) (= note 26)) 13)  ; Hi-hat open
        ((or (= note 49) (= note 57)) 14)  ; Crash cymbal
        ((or (= note 41) (= note 43) (= note 45) (= note 47) (= note 48) (= note 50)) 15)  ; Toms
        (t 11)))  ; Default to snare for unknown drums

(defun midi-synth-loop ()
  (loopwhile t {
        (var event (midi-read))
        (match event
               ((note-on (? ch) (? n) (? v)) {
                 (var patch (if (= ch 9)
                                (drum-note-to-patch n)  ; Channel 9 uses drum mapping
                                ch))                     ; Other channels use patch = channel
                 (print "channel " ch " : " (note-on patch n v))
               })
               ((note-off (? ch) (? n)) {  ; Add _ to match the trailing nil
                 (var patch (if (= ch 9)
                                (drum-note-to-patch n)
                                ch))
                 (note-off patch n)
                })
               ((pitch-bend (? ch) (? bv)) (ch-pitch-bend bv ch))
               ;; all midi events are lists, this one with just one elt.
               ((port-unsubscribed) (break))
               (_ (print (list "unmatched" event))))  ; Debug unmatched events
        }))

(define client-port (str-join
                     (list
                      (to-str (midi-client)) ":"
                      (to-str (midi-in-port)))))

(proc-spawn-detached "aplaymidi" "-p" client-port (car args))

(midi-synth-loop)
