(in-package :maxima)


#|
The function trigrat attempts to simplify a rational expression in trig functions. Basically it

(a) converts trigonometric functions to exponential form (exponentialize)
(b) rationally simplifies (ratsimp)
(c) expands
(d) convert from exponential form to trigonometric form (demoivre)
(e) rationally simplifies (ratsimp)

|#

 ;; D. Lazard wrote the initial version of `trigrat` in August 1988. Since then, the code has been modified and
;; rewritten by many contributors. For the historical record since about 2000, consult the Git history.

(defun gather-exp-args (e)
 "Return a Common Lisp list (not a Maxima list) of all exponents X such that
   the expression E contains a subexpression of the form ((mexpt) %e X).  When 
   X isn't a sum, return (list X), and when X is a sum return return (cdr X).
   Since exp(a+b) = exp(a) exp(b), the returned list is at least partially semantic.
   The resulting list is unsorted and may contain duplicates."
  (cond
    (($mapatom e) nil)
    ((and (mexptp e) (eq (second e) '$%e)) ; term has the form ((mexpt) %e X), return (list X)
     (if (mplusp (third e))
        (third e)
        (list (third e)))) 
    (t (mapcan #'gather-exp-args (cdr e))))) ;map gather-exp-args over args and collect 

(defmfun $trigrat (e &optional (canonical t))
  (flet ((aratsimp (x) (let (($algebraic t)) (simplify ($ratsimp x '$%i)))))
  (cond (($mapatom e) e)
        ((or (mbagp e) ($setp e) (mrelationp e)) ; map trigrat over mbags, sets, and inequations.
         (fapply (caar e) (mapcar #'(lambda (q) ($trigrat q canonical)) (cdr e))))
        (t
         (let* (($radsubstflag t)
                (e ($exponentialize e))
                ($%e_to_numlog t)
                (subs nil)
                (lll (fapply '$set (gather-exp-args e)))
                (ec ($equiv_classes lll #'(lambda (a b) ($ratnump (div a b)))))) ; the members of lll are not zero.
           (setq ec (mapcar #'(lambda (s) ($apply '$ezgcd ($listify s))) (cdr ec)))
           (dolist (ecx ec)
             (let ((g (gensym))
                   (ker (ftake 'mexpt '$%e (second ecx))))
               (push (ftake 'mequal g ker) subs)
               (setq e ($ratsubst g ker e))))
           (setq e (aratsimp e))
           ;(mtell "e = ~M ~%" e)
           (let ((p ($num e)) (q ($denom e)) (pdeg 0) (qdeg 0) (n) (z))
             ;(mtell "subs = ~M" (fapply 'mlist subs))
             (dolist (subx subs)
               (setq z (second subx))
               (setq pdeg ($hipow p z))
               (setq qdeg ($hipow q z)))
             (setq n (ftake '$floor (div (max pdeg qdeg) 2)))
             ;(mtell "p = ~M ; q = ~M ~%" p q)
             (setq p ($expand (div p (ftake 'mexpt z n))))
             (setq q ($expand (div q (ftake 'mexpt z n))))
             ;(mtell "p = ~M ; q = ~M ~%" p q)
             (dolist (subx subs)
               ;(mtell "subx = ~M ~%" subx)
               (setq p (maxima-substitute (third subx) (second subx) p))
               (setq q (maxima-substitute (third subx) (second subx) q)))
             (let ((ans (aratsimp ($ratsimp (div ($demoivre p) ($demoivre q)) '$%i))))
               ;; conditionally return either e or ans
               (if (or canonical (< (trig-count ans) (trig-count e)))
                   ans
                   e))))))))
   
(defun trig-count (e)
 "Return the number of trigonometric operators, including the hyperbolic operators, in the Maxima expression `e`."
   (cond ((atom e) 0)
         (t
           (reduce #'+ (mapcar #'trig-count (cdr e)) 
              :initial-value (if (and (consp e) (consp (car e)) (trigp (caar e))) 1 0)))))

(defmfun $xtrigrat (e &optional (canonical t))
"Similar to `trigrat`, but attempt to change the structure of the input as little as possible. "
   (cond ((or ($mapatom e) (eql 0 (trig-count e))) e)
         ;; for either a sum or a product, apply `trigrat` only to the terms that involve a
         ;; trig function. This avoids unnecessary expansion: try trigrat((1+x)^2 + sin(x)^2 + cos(x)^2)
         ((mplusp e)
           (multiple-value-bind (p q) (partition-by #'(lambda (q) (eql 0 (trig-count q))) (cdr e))
             (add (fapply 'mplus p) ($trigrat (fapply 'mplus q) canonical))))
          ((mtimesp e)
           (multiple-value-bind (p q) (partition-by  #'(lambda (q) (eql 0 (trig-count q))) (cdr e))
             (add (fapply 'mtimes p) ($trigrat (fapply 'mtimes q) canonical))))
          ;; Does this need to check for subscripted functions? Yes it does!
          (($subvarp (mop e)) ;subscripted function
		      (subfunmake 
		        (subfunname e) 
			     (mapcar #'$xtrigrat (subfunsubs e)) 
			     (mapcar #'$xtrigrat (subfunargs e))))
          ;; map xtrigrat over the arguments
          (t (ftake (caar e) (mapcar #'(lambda (q) ($xtrigrat q canonical)) (cdr e))))))
            

