(in-package :maxima)

#|
The function trigrat attempts to simplify a rational expression in trig functions. Basically it

(a) converts trigonometric functions to exponential form (exponentialize)
(b) rationally simplifies (ratsimp)
(c) expands
(d) convert from exponential form to trigonometric form (demoivre)
(e) rationally simplifies (ratsimp)

A step-by-step example:

(%i2) algebraic : true$

(%i3) xxx : sin(3*a)/sin(a+%pi/3)$

STEP 'a'

(%i4) xxx : exponentialize(xxx);

(%o4) (%e^(3*%i*a)-%e^-(3*%i*a))/(%e^(%i*a)*((sqrt(3)*%i)/2+1/2)
                                 -%e^-(%i*a)*(1/2-(sqrt(3)*%i)/2))

STEP 'b'

(%i5) xxx : ratsimp(xxx);

(%o5) -((%e^-(2*%i*a)*((sqrt(3)*%e^(4*%i*a)-sqrt(3))*%i
                      -%e^(4*%i*a)+2*%e^(2*%i*a)-1))/2)

STEP 'c'

(%i6) xxx : expand(xxx);

(%o6) -((sqrt(3)*%e^(2*%i*a)*%i)/2)+(sqrt(3)*%e^-(2*%i*a)*%i)/2+%e^(2*%i*a)/2
                                   +%e^-(2*%i*a)/2-1

STEP 'd'

(%i7) xxx : demoivre(xxx);

(%o7) -((sqrt(3)*%i*(%i*sin(2*a)+cos(2*a)))/2)
 +(%i*sin(2*a)+cos(2*a))/2+(sqrt(3)*%i*(cos(2*a)-%i*sin(2*a)))/2
 +(cos(2*a)-%i*sin(2*a))/2-1

 STEP 'e'

(%i8) xxx : ratsimp(xxx);

(%o8) sqrt(3)*sin(2*a)+cos(2*a)-1


 |#

(defun trig-count (e)
 "Return the number of trigonometric operators, including the hyperbolic operators, in the Maxima expression `e`."
   (cond ((atom e) 0)
         (t
           (reduce #'+ (mapcar #'trig-count (cdr e)) 
              :initial-value (if (and (consp e) (consp (car e)) (trigp (caar e))) 1 0)))))

;; D. Lazard wrote the initial version of `trigrat` in August 1988. Since then, the code has been modified and
;; rewritten by many contributors. For the historical record since about 2000, consult the Git history.
(defmfun $trigrat (e &optional (canonical t))
 "Simplify a trigonometric expression `e` by exponential substitution, expansion, and 
  rational simplification. This function does *not* return canonical representation--it 
  is possible that trigrat will simplify equivalent expressions to syntactically distinct 
  expressions. This code uses a metric based on number of trig operators to optionally return
  the expression unchanged. "
  (cond (($mapatom e) e)

        ((or (mbagp e) ($setp e) (mrelationp e)) ;map trigrat over mbags, sets, and inequations.
           (fapply (caar e) (mapcar #'(lambda (q) ($trigrat q canonical)) (cdr e))))

        (t
          (let* ((n (trig-count e))
                (ans
                  (sratsimp
                     ($demoivre 
                        ($expand 
                           (let (($algebraic t)) (sratsimp ($exponentialize e))))))))
             ;; conditionally return either e or ans
             (if (or canonical (< (trig-count ans) n))
               ans
               e
               )))))

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
            

              



   
