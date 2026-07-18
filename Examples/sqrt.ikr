
; Square root algorithm
;

(define abs (vau (x) _ (begin (define xv (eval _ x)) (if (< 0 xv) xv (- 0 xv)))))
(define square (vau (x) _ (begin (define xv (eval _ x)) (* xv xv))))
(define average (vau (x y) _ (* 0.5 (+ (eval _ x) (eval _ y)))))

(define good-enough? 
    (vau (guess x) _ 
        (< (abs (- (eval _ x) (square (eval _ guess)))) 0.00001)))

(define improve (vau (guess x) _
    (begin
        (define gv (eval _ guess))
        (average gv (/ (eval _ x) gv)))))

(define sqrt-iter 
    (vau (guess x) _ 
        (begin
            (define gv (eval _ guess))
            (define xv (eval _ x))
            (if (good-enough? gv xv) gv
                (sqrt-iter (improve gv xv) xv)))))

(define sqrt (vau (x) _ (sqrt-iter 1.0 (eval _ x))))