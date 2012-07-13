
; IronKernel bootstrapping library
;

(define quote (vau (x) _ x))

(define list 
	(vau xs env 
		(if (null? xs) 
		'() 
		(cons 
			(eval env (car xs)) 
			(eval env (cons list (cdr xs)))))))

; lambda is built in terms of vau
(define lambda 
	(vau (params body) static-env 
		; inlined comment
		(wrap (eval static-env (list vau params '_ body)))))

(define last (lambda (xs)
    (if (null? (cdr xs))
        (car xs)
        (last (cdr xs)))))

(define begin (lambda xs (last xs)))

; lets re-write lambda to take a body with multiple elements

(set! lambda
    ((lambda (base-lambda)
        (vau (param & body) env
            (eval env (list base-lambda param (cons begin body)))))
    lambda))

; redefining 'define' here to implement scheme define's syntax sugar does not work with our aproach

;(set! define
;    ((lambda (base-define)
;        (vau (param & body) env
;            (if (pair? param)
;                (eval env
;                    (list base-define (car param)
;                        (cons lambda (cons (cdr param) body))))
;                (eval env (cons base-define (cons param body))))))          
;    define))

;(define (compose f g)
;    (lambda (x) (f (g x))))

(define map
  (lambda (f xs)
    (if (null? xs)
      ()
      (cons (f (car xs)) (map f (cdr xs))))))

(define let (vau (binds & body) env
    (eval env
        (cons
            (cons lambda (cons (map car binds) body))
            (map cadr binds)))))

(define zip
  (lambda (f & xss)
    (let ((rest (map cdr xss)))
      (cons (apply f (map car xss))
            (if (any? null? rest)
              ()
              (apply zip (cons f rest)))))))

(define let*
  (vau (bindings & body) env
    (eval env (if (null? bindings)
            (list let bindings body)
            (list let (list (car bindings))
                  (list let* (cdr bindings) body))))))

(define letrec
  (vau (bindings & body) env
    (eval env (list let ()
                 (list define
                   (map car bindings)
                   (cons list (map cadr bindings)))
                 body))))

(define letrec*
  (vau (bindings & body) env
    (eval env (if (null? bindings)
            (list letrec bindings body)
            (list letrec (list (car bindings))
                  (list letrec* (cdr bindings) body))))))

(define let-redirect
  (vau (exp bindings & body) env
    (eval env (cons (eval (eval env exp) (list lambda (map car bindings) body))
                (map cadr bindings)))))

(define for-each
  (wrap (vau (x f) env (apply map (list f x) env) #inert)))
  
  
(define defn
  (vau (name & body) e
    (if (pair? name)
      (let (((n & as) name))
        (eval e (list define n
                    (cons lambda (cons as body)))))
      (eval e (cons define (cons name body))))))

(defn (compose f g) (lambda (x) (f (g x))))

(define caar  (compose car  car))
(define cadr  (compose car  cdr))
(define caddr (compose cadr cdr))

(define get-current-environment (wrap (vau () e e)))

; Closing comment