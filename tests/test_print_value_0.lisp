

(check (and
        (eq (to-str 1) "1")
        (eq (to-str 1u) "1u")
        (eq (to-str 1i32) "1i32")
        (eq (to-str 1u32) "1u32")
        (eq (to-str 3.0f32) "3.000000f32")
        (eq (to-str 3.0f64) "3.000000f64")
        (eq (to-str 1i64) "1i64")
        (eq (to-str 1u64) "1u64")
        (eq (to-str 1b) "1b")
        (eq (to-str "hello") "hello")
        (eq (to-str "hello\n") "hello\n")
        (eq (to-str "hello \"hello\"") "hello \"hello\"")))

