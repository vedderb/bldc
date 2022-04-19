
(defun elem (ls e)
  (if (eq ls 'nil)
      'nil
      (if (eq (first ls) e)
          e
          (elem (rest ls) e))))

(defun elem-pm (ls e)
  (match ls
         ( nil nil )
         ( ((? x) . (? xs))
           (if (eq x e) e (elem-pm xs e)) )))

(defun elem-pm2 (ls e)
  (match ls
         ( nil nil )
         ( (e . _) e )
         ( (_ . (? xs)) (elem-pm xs e) )))
