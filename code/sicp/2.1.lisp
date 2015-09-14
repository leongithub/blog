(defun par1 (r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(defun par2 (r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

(defun make-center-percent (c p)
  (let ((tolerance (* c p)))
    (make-interval (- c tolerance)
		   (+ c tolerance))))

(defun percent (i)
  (/ (width i) (center i)))

(defun make-center-width (c w)
  (make-interval (- c w) (+ c w)))
(defun center (i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defun width (i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defun mul-interval (x y)
  (let ((xl (lower-bound x))
	(xu (upper-bound x))
	(yl (lower-bound y))
	(yu (upper-bound y)))
    ;; xl和xu与yl和yu 同号的四种情况
    ;; x++ y++; x++ y--; x-- y++; x-- y--
    (cond ((and (>= xl 0) ; x++ y++
		(>= yl 0))
	   (make-interval (* xl yl) (* xu yu)))
	  ((and (>= xl 0) ; x++ y--
		(<= yu 0))
	   (make-interval (* xu yl) (* xl yu)))
	  ((and (<= xu 0) ; x-- y++
		(>= yl 0))
	   (make-interval (* xl yu) (* xu yl)))
	  ((and (<= xu 0) ; x-- y--
		(<= yu 0))
	   (make-interval (* xl yl) (* xu yu)))
    ;; 单异：xl和xu异号，y 同号。或者相反。四种情况
    ;; x-+ y++; x-+ y--; x++ y-+; x-- y-+
	  ((and (<= xl 0) ; x-+ y++
		(>= xu 0)
		(>= yl 0))
	   (make-interval (* xl yu) (* xu yu)))
	  ((and (<= xl 0) ; x-+ y--
		(>= xu 0)
		(<= yu 0))
	   (make-interval (* xu yl) (* xl yl)))
	  ((and (>= xl 0) ; x++ y-+
		(<= yl 0)
		(>= yu 0))
	   (make-interval (* xu yl) (* xu yu)))
	  ((and (<= xu 0) ; x-- y-+
		(<= yl 0)
		(>= yu 0))
	   (make-interval (* xl yu) (* xl yl)))
    ;; 双异，这种就是多余两次乘
	  ((and (<= xl 0) ; x-+ y-+
		(>= xu 0)
		(<= yl 0)
		(>= yu 0))
	   (let ((p1 (* xl yl))
		 (p2 (* xl yu))
		 (p3 (* xu yl))
		 (p4 (* xu yu)))
	     (make-interval (min p2 p3)
			    (max p1 p4)))))))

(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

;; (defun sub-interval (x y)
;;   (add-interval x
;; 		(make-interval (- (upper-bound y))
;; 			       (- (lower-bound y)))))

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (if (and (>= (upper-bound y) 0)
	   (<= (lower-bound y) 0))
      (error "interval spans zero")
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

(defun make-interval (l u) (cons l u))
(defun lower-bound (x) (car x))
(defun upper-bound (x) (cdr x))

(defun our-+ (&rest ns)
  (labels ((rec (n lst f x)
	     (if (null lst)
		 (funcall (funcall (funcall n) f) x)
		 (funcall (funcall (funcall n) f)
			  (rec (car lst)
			       (cdr lst)
			       f
			       x)))))
    (lambda (f)
      (lambda (x)
	(rec (car ns) (cdr ns) f x)))))

(defun zero ()
  (lambda (f) (lambda (x) x)))

(defun one ()
  (lambda (f) (lambda (x) (funcall f x))))

(defun two ()
  (lambda (f)
    (lambda (x)
      (funcall f (funcall f x)))))

(defun our-+ (n1 n2)
  (lambda (f)
    (lambda (x)
      (funcall (funcall (funcall n1) f)
	       (funcall (funcall (funcall n2) f) x)))))


;; (lambda (f) (lambda (x) x)) -- zero

;; (add-1 zero)
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; (lambda (f) (lambda (x) (f x))) -- one
;; (lambda (f) (lambda (x) (f (f x)))) -- two

(defun our-cons (a b)
  (* (expt 2 a)
     (expt 3 b)))

(defun our-car (z)
  (divide-times z 2))

(defun our-cdr (z)
  (divide-times z 3))

(defun divide-times (number divisor)
  (labels ((iter (n result)
	     (let ((tmp (/ n divisor)))
	       (if (integerp tmp)
		   (iter tmp (1+ result))
		   result))))
    (iter number 0)))

;; (defun our-cons (x y)
;;   (labels ((dispatch (m)
;; 	     (cond ((= m 0) x)
;; 		   ((= m 1) y)
;; 		   (t (error "Argument not 0 or 1" m)))))
;;     #'dispatch))

;; (defun our-car (z)
;;   (funcall z 0))

;; (defun our-cdr (z)
;;   (funcall z 1))

;; 第三种表示法
(defun make-rect (p1 p2 p3)
  (cons (make-segment p1 p2)
	(make-segment p2 p3)))

(defun width-rect (rect)
  (if (> (length-segment (car rect))
	 (length-segment (cdr rect)))
      (cdr rect)
      (car rect)))

(defun height-rect (rect)
  (if (> (length-segment (car rect))
	 (length-segment (cdr rect)))
      (car rect)
      (cdr rect)))

;; 第二种表示法：用相邻的三点来构造
;; (defun make-rect (p1 p2 p3)
;;   (let ((seg12 (make-segment p1 p2))
;; 	(seg23 (make-segment p2 p3)))
;;     (if (> (length-segment seg12)
;; 	   (length-segment seg23))
;; 	(cons seg23 seg12)
;; 	(cons seg12 seg23))))

;; (defun width-rect (rect)
;;   (car rect))

;; (defun height-rect (rect)
;;   (cdr rect))

(defun perimeter-rect (rect)
  (* 2 (+ (length-width-rect rect)
	  (length-height-rect rect))))

(defun area-rect (rect)
  (* (length-width-rect rect))
     (length-height-rect rect))))

(defun length-width-rect (rect)
  (length-segment (width-rect rect)))

(defun length-height-rect (rect)
  (length-segment (height-rect rect)))

;; 第一种表示法：用segment宽和segment高来构造
;; (defun make-rect (width height)
;;   (cons width height))

;; (defun width-rect (rect)
;;   (car rect))

;; (defun height-rect (rect)
;;   (cdr rect))

(defun length-segment (segment)
  (let ((start-p (start-segment segment))
	(end-p (end-segment segment)))
    (expt (+ (expt (- (x-point start-p)
		      (x-point end-p))
		   2)
	     (expt (- (y-point start-p)
		      (y-point end-p))
		   2))
	  .5)))  

(defun midpoint-segment (segment)
  (let ((start-p (start-segment segment))
	(end-p (end-segment segment)))
    (make-point (average (x-point start-p)
			 (x-point end-p))
		(average (y-point start-p)
			 (y-point end-p)))))

(defun average (x y)
  (/ (+ x y) 2.0))

(defun make-segment (start-p end-p)
  (cons start-p end-p))

(defun start-segment (segment)
  (car segment))

(defun end-segment (segment)
  (cdr segment))

(defun print-point (p)
  (format t "~&(~A,~A)" (x-point p) (y-point p)))
  ;; (fresh-line)
  ;; (princ "(")
  ;; (princ (x-point p))
  ;; (princ ",")
  ;; (princ (y-point p))
  ;; (princ ")"))

(defun make-point (x y)
  (cons x y))

(defun x-point (p)
  (car p))

(defun y-point (p)
  (cdr p))

(defun make-rat (n d)
  (labels ((make-rat-inner (n d)
	     (let ((g (gcd n d)))
	       (cons (/ n g) (/ d g)))))
    (if (minusp d)
	(make-rat-inner (- n) (- d))
	(make-rat-inner n d))))

(defun numer (x)
  (car x))

(defun denom (x)
  (cdr x))

(defun print-rat (x)
  (fresh-line)
  (princ (numer x))
  (princ "/")
  (princ (denom x)))
; (format t "~&~A/~A" (numer x) (denom x))

(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(defun equal-rat-p (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
