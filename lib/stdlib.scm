(define (id x) x)

(define (not x) (if x #f #t))

(define (null? x) (if (eqv? x '()) #t #f))

(define (flip f) (lambda (x y) (f y x)))

(define (curry f x)  (lambda (y) (apply f (cons x (list y)))))

(define (compose f g) (lambda (x) (f (apply g x))))

(define (foldl f e xs) (if (null? xs) e (foldl f (f e (car xs)) (cdr xs))))

(define (foldr f e xs) (if (null? xs) e (f (car xs) (foldr f e (cdr xs)))))

(define (unfold f x p) (if (p x) (cons x '()) (cons x (unfold f (f x) p))))

(define fold foldl)

(define reduce fold)

(define zero? (curry = 0))

(define positive? (curry < 0))

(define negative? (curry > 0))

(define (odd? n) (= (mod n 2) 1))

(define (even? n) (= (mod n 2) 0))

(define (max x . xs) (fold (lambda (y z) (if (> y z) y z)) x xs))

(define (min x . xs) (fold (lambda (y z) (if (< y z) y z)) x xs))

(define (list . xs) xs)

(define (length xs) (fold (lambda (x y) (+ x 1)) 0 xs))

(define (append xs . xss) (foldr (flip (curry foldr cons)) xs xss))

(define (reverse xs) (fold (flip cons) '() xs))

(define (mem-helper p op) (lambda (x y) (if (and (not acc) (p (op y))) y x)))

(define (memq x xs) (fold (mem-helper (curry eq? x) id) #f xs))

(define (memv x xs) (fold (mem-helper (curry eqv? x) id) #f xs))

(define (member x xs) (fold (mem-helper (curry equal? x) id) #f xs))

(define (assq x xs) (fold (mem-helper (curry eq? x) car) #f xs))

(define (assv x xs) (fold (mem-helper (curry eqv? x) car) #f xs))

(define (assoc x xs) (fold (mem-helper (curry equal? x) car) #f xs))

(define (map f xs) (foldr (lambda (x y) (cons (f x) y)) '() xs))

(define (filter p xs) (foldr (lambda (x y) (if (p x) (cons x y) y)) '() xs))

(define (sum . xs) (fold + 0 xs))

(define (product . xs) (fold * 1 xs))

(define (and . xs) (fold && #t xs))

(define (or . xs) (fold || #f xs))

(define (any? p . xs) (apply or (map p xs)))

(define (every? p . xs) (apply and (map p xs)))

(define (caar xs) (car (car xs)))

(define (cadr xs) (car (cdr xs)))

(define (cdar xs) (cdr (car xs)))

(define (cddr xs) (cdr (cdr xs)))

(define (caaar xss) (car (car (car xss))))

(define (caadr xss) (car (car (cdr xss))))

(define (cadar xss) (car (cdr (car xss))))

(define (caddr xss) (car (cdr (cdr xss))))

(define (cdaar xss) (cdr (car (car xss))))

(define (cdadr xss) (cdr (car (cdr xss))))

(define (cddar xss) (cdr (cdr (car xss))))

(define (cdddr xss) (cdr (cdr (cdr xss))))

(define (caaaar xsss) (car (car (car (car xsss)))))

(define (caaadr xsss) (car (car (car (cdr xsss)))))

(define (caadar xsss) (car (car (cdr (car xsss)))))

(define (caaddr xsss) (car (car (cdr (cdr xsss)))))

(define (cadaar xsss) (car (cdr (car (car xsss)))))

(define (cadadr xsss) (car (cdr (car (cdr xsss)))))

(define (caddar xsss) (car (cdr (cdr (car xsss)))))

(define (cadddr xsss) (car (cdr (cdr (cdr xsss)))))

(define (cdaaar xsss) (cdr (car (car (car xsss)))))

(define (cdaadr xsss) (cdr (car (car (cdr xsss)))))

(define (cdadar xsss) (cdr (car (cdr (car xsss)))))

(define (cdaddr xsss) (cdr (car (cdr (cdr xsss)))))

(define (cddaar xsss) (cdr (cdr (car (car xsss)))))

(define (cddadr xsss) (cdr (cdr (car (cdr xsss)))))

(define (cdddar xsss) (cdr (cdr (cdr (car xsss)))))

(define (cddddr xsss) (cdr (cdr (cdr (cdr xsss)))))
