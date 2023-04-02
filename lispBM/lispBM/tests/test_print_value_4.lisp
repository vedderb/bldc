
(and
 (eq (read (to-str [1])) [1])
 (eq (read (to-str [1 2])) [1 2])
 (eq (read (to-str [1 2 3])) [1 2 3])
 (eq (read (to-str [1 2 3 4])) [1 2 3 4])
 (eq (read (to-str [1 2 3 4 5])) [1 2 3 4 5])
 (eq (read (to-str [type-i32 1 2 3 4 5])) [type-i32 1 2 3 4 5])
 (eq (read (to-str [type-u32 1 2 3 4 5])) [type-u32 1 2 3 4 5])
 (eq (read (to-str [type-f32 1 2 3 4 5])) [type-f32 1 2 3 4 5]))
