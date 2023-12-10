
(define ls (list 1u32 2u32 3u32 4u32 5u32 6u32))

(gc)

(check (eq ls (list 1u32 2u32 3u32 4u32 5u32 6u32)))
