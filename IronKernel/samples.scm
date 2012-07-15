
; IronKernel samples

; .NET interop
(define write 
	(lambda (x) 
	(. System.Console Write x)))


; recursion
(defn (fact n)
	(write n)
		(write "\n")
		(if (<= n 1) 
			1 
			(* n (fact (- n 1)))))

(defn (fib n)
    (write n)
    (write "\n")
    (if (<= n 1)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))
; letrec
(letrec ((loop (lambda (i)      ; define a recursive
                  (write i)   ; procedure whose body
				  (write "\n")
                  (if (<= i 10)  ; is the loop body
                      (loop (+ i 1)) #inert))))
   (loop 0)) ;; start the recursion with 0 as arg i

   
(letrec ((foo (lambda (x) (bar x))) 
           (bar (lambda (x) (+ 1 x)))) 
    (foo 5))
	
(letrec ((sum (lambda (x)
             (if (zero? x)
                 0
                 (+ x (sum (- x 1)))))))
   (sum 5))	


; letrec*
(letrec* ((sum (lambda (x)
            (if (zero? x)
                    0
                (+ x (sum (- x 1))))))
         ((f (lambda () (cons n n-sum)))
         (n 15)
         (n-sum (sum n)))
  (f)))
   
; Continuations

(define frozen #f)

(+ 2 (call/cc (lambda (k) (set! frozen k) 3)))

;