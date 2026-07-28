(import "/libs/dsp_lang.lisp" 'dsp)
(read-eval-program dsp)

(plot-signal (signal-sin 440.0) 44100.0 1.0 "440 Hz Sine")
