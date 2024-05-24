#lang racket

(provide (all-defined-out))

(define sequence
  (lambda (low high stride)
    (if (> low high)
        '()
        (cons low (sequence (+ low stride) high stride)))))

(define string-append-map
  (lambda (xs suffix)
    (map (lambda (x) (string-append x suffix)) xs)))

(define list-nth-mod
  (lambda (xs n)
    (cond
      [(< n 0) (error "list-nth-mod: negative number")]
      [(null? xs) ((error "list-nth-mod: empty list"))]
      [(let ([index (remainder n (length xs))])
         (car (list-tail xs index)))])))

(define stream-for-n-steps
  (lambda (s n)
    (cond
      [(= n 0) '()]
      [else (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))])))


(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (if x
                              (cons "dan.jpg" (lambda () (f #f)))
                              (cons "dog.jpg" (lambda () (f #t)))))])
    (lambda () (f #t))))

(define stream-add-zero
  (lambda (s)
    (lambda () (let ([pr (s)]) (cons (cons 0 (car pr)) (stream-add-zero (cdr pr)))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (let ([x (list-nth-mod xs n)]
                      [y (list-nth-mod ys n)])
                  (cons (cons x y) (lambda () (f (+ n 1))))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (>= n (vector-length vec))
                    #f
                    (let ([ith (vector-ref vec n)])
                      (if (and (pair? ith) (equal? (car ith) v))
                          ith
                          (f (+ n 1))))))])
    (f 0)))


(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [pos 0])
    (lambda (v)
      (or (vector-assoc v memo)
          (let ([new-ans (assoc v xs)])
            (and new-ans
                 (begin
                   (vector-set! memo pos new-ans)
                   (set! pos (remainder (+ pos 1) n))
                   new-ans)))))))