
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;; 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; 2
(define string-append-map
  (lambda (xs suffix)
    (map (lambda (str)
           (string-append str suffix)) 
         xs)))

;; 3
(define (list-nth-mod xs n)
  (cond 
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs  (remainder n (length xs))))]))

;; 4
(define (stream-for-n-steps s n)
  (let ([sel (s)])
    (if (= n 0) null
        (cons (car sel) (stream-for-n-steps (cdr sel) (- n 1))))))
;; 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 5) 0)
                    (cons (- 0 x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; 6
(define (dan-then-dog)
  (letrec
      ((dan (lambda () (cons "dan.jpg" dog)))
       (dog (lambda () (cons "dog.jpg" dan))))
    (dan)))

;; 7
(define (stream-add-zero s)
  (letrec 
      ([f (lambda (x) (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

;; 8
(define (cycle-lists xs ys)
  (letrec 
      ([f (lambda (x y n) 
            (cons (cons (list-nth-mod x n) (list-nth-mod y n))
                  (lambda () (f x y (+ 1 n)))))])
    (lambda () (f xs ys 0))))

;; 9
(define (vector-assoc v vec)         
  (letrec        
      ([loop (lambda(n)        
          (if(= n (vector-length vec)) #f        
             (let ([cur (vector-ref vec n)])        
               (cond        
                 [(not( pair? cur)) (loop (+ n 1))]
                 [(equal? (car cur) v)  cur]
                 [#t (loop (+ n 1))]))))])        
      (loop 0)))

;; 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [index 0]
           [f (lambda (v)
                     (if (vector-assoc v cache)
                         (vector-assoc v cache)
                         (if (assoc v xs) 
                             (begin
                               (vector-set! cache index (assoc v xs))
                               (set! index (remainder (+ index 1) n))
                               (assoc v xs))
                             #f)))])
    f))

;; 11
(define-syntax while-less
  (syntax-rules (do) 
  [(while-less2 e1 do e2) 
   (letrec ([val1 e1]
            [loop (lambda () 
                    (let ([val2 e2])
                    (if (<= val1 val2) #t 
                        (loop) )))]) 
     (loop))]))

