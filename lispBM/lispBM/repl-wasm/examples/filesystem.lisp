
;; This example uses file operations implemented
;; on top of the MEMFS provided by emscripten.

(def f (f-open "data.bin" "w"))
(def buf (bufcreate 8))
(bufset-f32 buf 0 3.14)
(bufset-u32 buf 4 42u32)
(f-write f buf)
(f-close f)

(def txt (f-open "notes.txt" "w"))
(f-write-str txt "hello from lispbm\n")
(f-write-str txt "second line\n")
(f-close txt)

(def r (f-open "notes.txt" "r"))
(def contents (load-file r))
(f-close r)
(print contents)

(print (f-list))

(wasm-save-file "data.bin")
(wasm-save-file "notes.txt")
