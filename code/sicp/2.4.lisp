;; 2.41
(defun add-complex (z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(defun sub-complex (z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(defun mul-complex (z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(defun div-complex (z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(defun square (x) (* x x))

;; Ben
(defun real-part (z) (car z))
(defun imag-part (z) (cdr z))
(defun magnitude (z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(defun angle (z)
  (atan (imag-part z) (real-part z)))
(defun make-from-real-imag (x y) (cons x y))
(defun make-from-mag-ang (r a) 
  (cons (* r (cos a)) (* r (sin a))))

;; Alyssa
(defun real-part (z)
  (* (magnitude z) (cos (angle z))))
(defun imag-part (z)
  (* (magnitude z) (sin (angle z))))
(defun magnitude (z) (car z))
(defun angle (z) (cdr z))
(defun make-from-real-imag (x y) 
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(defun make-from-mag-ang (r a) (cons r a))

;; 2.42
(defun attach-tag (type-tag contents)
  (cons type-tag contents))
(defun type-tag (datum)
  (if (consp datum)
      (car datum)
      (error "Bad tagged ~A -- TYPE-TAG" datum)))
(defun contents (datum)
  (if (consp datum)
      (cdr datum)
      (error "Bad tagged ~A -- CONTENTS" datum)))

(defun rectangularp (z)
  (eq (type-tag z) 'rectangular))
(defun polarp (z)
  (eq (type-tag z) 'polar))

;; Ben
(defun real-part-rectangular (z) (car z))
(defun imag-part-rectangular (z) (cdr z))
(defun magnitude-rectangular (z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(defun angle-rectangular (z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(defun make-from-real-imag-rectangular (x y)
  (attach-tag 'rectangular (cons x y)))
(defun make-from-mag-ang-rectangular (r a) 
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))
;; Alyssa
(defun real-part-polar (z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(defun imag-part-polar (z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(defun magnitude-polar (z) (car z))
(defun angle-polar (z) (cdr z))
(defun make-from-real-imag-polar (x y) 
  (attach-tag 'polar
               (cons (sqrt (+ (square x) (square y)))
                     (atan y x))))
(defun make-from-mag-ang-polar (r a)
  (attach-tag 'polar (cons r a)))

(defun real-part (z)
  (cond ((rectangularp z) 
         (real-part-rectangular (contents z)))
        ((polarp z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART ~A" z))))
(defun imag-part (z)
  (cond ((rectangularp z)
         (imag-part-rectangular (contents z)))
        ((polarp z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART ~A" z))))
(defun magnitude (z)
  (cond ((rectangularp z)
         (magnitude-rectangular (contents z)))
        ((polarp z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE ~A" z))))
(defun angle (z)
  (cond ((rectangularp z)
         (angle-rectangular (contents z)))
        ((polarp z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE ~A" z))))

(defun make-from-real-imag (x y)
  (make-from-real-imag-rectangular x y))
(defun make-from-mag-ang (r a)
  (make-from-mag-ang-polar r a))

;; 2.43
;; Ben
(defun install-rectangular-package()
  ;; internal procedures
  (labels ((real-part (z) (car z))
	   (imag-part (z) (cdr z))
	   (make-from-real-imag (x y) (cons x y))
	   (magnitude (z)
	     (sqrt (+ (square (real-part z))
		      (square (imag-part z)))))
	   (angle (z)
	     (atan (imag-part z) (real-part z)))
	   (make-from-mag-ang (r a)
	     (cons (* r (cos a)) (* r (sin a))))
	   ;; interface to the rest of the system
	   (tag (x) (attach-tag 'rectangular x)))
    (put-table 'real-part '(rectangular) #'real-part)
    (put-table 'imag-part '(rectangular) #'imag-part)
    (put-table 'magnitude '(rectangular) #'magnitude)
    (put-table 'angle '(rectangular) #'angle)
    (put-table 'make-from-real-imag 'rectangular 
	 (lambda (x y) (tag (make-from-real-imag x y))))
    (put-table 'make-from-mag-ang 'rectangular 
	 (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

;; Alyssa
(defun install-polar-package()
  ;; internal procedures
  (labels ((magnitude (z) (car z))
	   (angle (z) (cdr z))
	   (make-from-mag-ang (r a) (cons r a))
	   (real-part (z)
	     (* (magnitude z) (cos (angle z))))
	   (imag-part (z)
	     (* (magnitude z) (sin (angle z))))
	   (make-from-real-imag (x y)
	     (cons (sqrt (+ (square x) (square y)))
		   (atan y x)))
	   ;; interface to the rest of the system
	   (tag (x) (attach-tag 'polar x)))
    (put-table 'real-part '(polar) #'real-part)
    (put-table 'imag-part '(polar) #'imag-part)
    (put-table 'magnitude '(polar) #'magnitude)
    (put-table 'angle '(polar) #'angle)
    (put-table 'make-from-real-imag 'polar
	 (lambda (x y) (tag (make-from-real-imag x y))))
    (put-table 'make-from-mag-ang 'polar 
	 (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar type-tag args)))
    (let ((proc (get-table op type-tags)))
      (if proc
          (apply proc (mapcar contents args))
          (error
            "No method for these types ~A -- APPLY-GENERIC"
            (list op type-tags))))))

(defun real-part (z) (apply-generic 'real-part z))
(defun imag-part (z) (apply-generic 'imag-part z))
(defun magnitude (z) (apply-generic 'magnitude z))
(defun angle (z) (apply-generic 'angle z))

(defun make-from-real-imag (x y)
  ((get-table 'make-from-real-imag 'rectangular) x y))
(defun make-from-mag-ang (r a)
  ((get-table 'make-from-mag-ang 'polar) r a))

;; 2.73
(defun deriv (exp var)
  (cond ((numberp exp) 0)
	((variablep exp)
	 (if (same-variable-p exp var) 1 0))
	((sump exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((productp exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiationp exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation
			 (base exp)
			 (make-sum (exponent exp) -1)))
	  (deriv (base exp) var)))
	(t (error "unknown expression type -- DERIV ~A" exp))))

(defun deriv (exp var)
   (cond ((numberp exp) 0)
         ((variablep exp) (if (same-variable-p exp var) 1 0))
         (t (funcall (get-table 'deriv (operator exp))
		     (operands exp) var))))
(defun operator (exp) (car exp))
(defun operands (exp) (cdr exp))

;; b
(defun install-sum-package ()
  ;; internal procedures
  (labels ((addend (s))
	   (augend (s))
	   (make-sum (a1 a2))
	   (deriv-sum (s var)
	     (make-sum (deriv (addend s) var)
		       (deriv (augend s) var))))
    (put-table 'make-sum '+ #'make-sum)
    (put-table 'deriv '+ #'deriv-sum)
    'done))

(defun make-sum (a1 a2)
  (funcall (get-table 'make-sum '+) a1 a2))

(defun install-product-package ()
  ;; internal procedures
  (labels ((multiplier (s))
	   (multiplicand (s))
	   (make-product (m1 m2))
	   (deriv-product (s var)
	     (make-sum
	      (make-product (multiplier s)
			    (deriv (multiplicand s) var))
	      (make-product (deriv (multiplier s) var)
			    (multiplicand s)))))
    (put-table 'make-product '* #'make-product)
    (put-table 'deriv '* #'deriv-product)
    'done))

(defun make-product (m1 m2)
  (funcall (get-table 'make-product '*) m1 m2))

;; c
(defun install-exponentiation-package ()
  (labels ((exponent (e))
	   (base (e))
	   (make-exponentiation (base e))
	   (deriv-exponentiation (s var)
	     (make-product
	      (make-product (exponent s)
			    (make-exponentiation
			     (base s)
			     (make-sum (exponent s) -1)))
	      (deriv (base s) var))))
    (put-table 'make-exponentiation '** #'make-exponentiation)
    (put-table 'deriv '** #'deriv-exponentiation)
    'done))

(defun make-exponentiation (base e)
  (funcall (get-table 'make-exponentiation '**) base e))

;; 2.74
(defun install-division-1-package ()
  (labels ((get-record (name file)
	     ;; division-1 implement
	     'record))
    (put-table 'get-record 'division-1 #'get-record)))

(defun install-division-2-package ()
  (labels ((get-record (name file)
	     ;; division-2 implement
	     'record))
    (put-table 'get-record 'division-2 #'get-record)))

(defun apply-generic (op file $rest args)
  (let ((division (car file)))
    (let ((proc (get-table op divisioin)))
      (if proc
          (apply proc args)
          (error
            "No method for the division ~A -- APPLY-GENERIC"
            (list op division))))))

