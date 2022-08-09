(define (subtract list1 list2)
  (if (null? list2) list1
      (cons (- (car list1) (car list2)) (subtract (cdr list1) (cdr list2)))))

(define (multiply-once n list)
  (map (lambda (x) (* n x)) list))

(define (divide-once list1 list2 n)
  (subtract list1 (multiply-once n list2)))

(define (divide list1 list2)
  (divide-b list1 list2 '()))

(define (divide-b list1 list2 result)
  (if (< (length list1) (length list2)) (list result (compressor list1))
      (let ((a (/ (car list1) (car list2))))
        (divide-b (cdr (divide-once list1 list2 a)) list2 (append result (list a))))))

(define (pad list1 n)
  (if (= n 0) list1
      (pad (append list1 '(0)) (- n 1))))
  
(define (add list1 list2)
  (cond ((null? list1) list2)
        ((> (length list2) (length list1)) (cons (car list2) (add list1 (cdr list2))))
        ((> (length list1) (length list2)) (add list2 list1))
        (else (cons (+ (car list1) (car list2)) (add (cdr list1) (cdr list2))))))

(define (multiply list1 list2)
  (if (null? list2) '()
      (add (pad (multiply-once (car list2) list1) (- (length list2) 1)) (multiply list1 (cdr list2)))))

(define (compressor list1)
  (cond ((null? list1) '(0))
        ((= (car list1) 0) (compressor (cdr list1)))
        (else list1)))

(define (derivative list1)
  (if (= (length list1) 1) '()
      (cons (* (car list1) (- (length list1) 1)) (derivative (cdr list1)))))

(define vec1 (make-vector 5))

(define (make-poly . coef)
  (let ((local-vec (make-vector (+ (apply max (map (lambda (list1) (cadr list1)) coef)) 1))))
    (reverse (vector->list (make-poly-helper local-vec coef)))))

(define (make-poly-helper vec list1)
  (if (null? list1) vec
      (make-poly-helper (vector-set vec (cadar list1) (caar list1)) (cdr list1))))

(define (vector-set vec pos v)
  (vector-set! vec pos v)
  vec)

(define (round-off n tol)
  (* 1.0 (* (round (/ n tol)) tol)))

;;;;;;;;;;;;;;;;;;;;;

(define (eval-poly-helper x list1 result)
  (if (null? list1) result
      (eval-poly-helper x (cdr list1) (+ (* result x) (car list1)))))

(define (eval-poly x list1)
  (eval-poly-helper x list1 0))

(define (sturm-chain-helper chain)
  (if (= (length (car chain)) 1) chain
      (sturm-chain-helper (cons (multiply-once -1 (cadr (divide (cadr chain) (car chain)))) chain))))

(define (sturm-chain list1)
  (sturm-chain-helper (list (derivative list1) list1)))

(define (xor a b)
  (and (not (and a b)) (or a b)))

(define (sign-change-helper list1 result)
  (if (= (length list1) 1) result
      (sign-change-helper (cdr list1) (+ (if (xor (negative? (car list1)) (negative? (cadr list1))) 1 0) result))))

(define (sign-change list1)
  (sign-change-helper list1 0))

(define (sturm-eval x chain)
  (map (lambda (list1) (eval-poly x list1)) chain))

(define (sturm-change x chain)
  (sign-change (sturm-eval x chain)))

(define (sturm-roots a b list1)
  (let ((chain (sturm-chain list1)))
    (- (sturm-change a chain) (sturm-change b chain))))

(define (find-zero-helper a b sturm-a sturm-b chain accuracy)
  (let ((m (/ (+ a b) 2))
        (sturm-m (sturm-change (/ (+ a b) 2) chain)))
    (cond ((<= (- b a) accuracy) m)
          ((= sturm-a sturm-m) (find-zero-helper m b sturm-m sturm-b chain accuracy))
          (else (find-zero-helper a m sturm-a sturm-m chain accuracy)))))

;(trace find-zero-helper)

(define tolerance (expt 10 -8))

(define (find-one-zero a b list1)
  (if (= (eval-poly a list1) 0) a
   (let ((chain (sturm-chain list1)))
    (round-off (find-zero-helper a b (sturm-change a chain) (sturm-change b chain) chain (/ tolerance 100)) tolerance))))

(define (bound list1)
  (+ (abs (/ (apply max (map abs (cdr list1))) (car list1))) 1))

(define (find-all-zeros-helper a b list1 result)
  (if (= (sturm-roots a b list1) 0) result
      (let ((root (find-one-zero a b list1)))
        (find-all-zeros-helper (+ root tolerance) b list1 (cons root result)))))

;(trace find-all-zeros)

(define (find-all-zeros list1)
  (find-all-zeros-helper (* -1 (bound list1)) (bound list1) list1 '()))
























;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define poly1 (make-poly '(1 4) '(1 3) '(-1 1) '(-1 0)))
(define poly2 (make-poly '(1 5) '(-3 1) '(1 0)))
(define poly3 (make-poly '(1 6) '(-4 3) '(1 1) '(-2 0)))
(define polyd (multiply (multiply (multiply '(1 -2) '(1 3)) '(1 4)) '(1 4)))
(define chain1 (sturm-chain polyd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;