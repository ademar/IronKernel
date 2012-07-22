
(define display 
	(lambda (x) 
	(. System.Console WriteLine x)))

(defn (f return) (return 2) 3)
 
(display (f (lambda (x) x))) ; displays 3
 
(display (call/cc f))
