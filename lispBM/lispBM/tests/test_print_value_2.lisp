

(check (and
        (eq (read (to-str 1)) 1)
        (eq (read (to-str 1u)) 1u)
        (eq (read (to-str 1i32)) 1i32)
        (eq (read (to-str 1u32)) 1u32)
        (eq (read (to-str 3.0f32)) 3.0f32)
        (eq (read (to-str 3.0f64)) 3.0f64)
        (eq (read (to-str 1i64)) 1i64)
        (eq (read (to-str 1u64)) 1u64)
        (eq (read (to-str 1b)) 1b)
        (eq (read (to-str "hello")) 'hello)))

