
# KNOWN BUGS

## flatten

   `flatten` as used in `(flatten a)` from lisp does not detect
   sharing and instead duplicates shared nodes in the flattened representation
   of the value.

   `flatten` is implemented as recursion on the C side and protects
   against stack exhaustion via a recusion counter.

   Note that the flattening that happens when Lisp Images are created
   does not have this shortcoming. The image-flattener uses pointer-reversal
   and thus only O(1) extra memory.

   Work around: Just be aware that `flatten` looses sharing structure.
