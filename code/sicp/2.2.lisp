(defun below (painter1 painter2)
  (rotate90
   (beside
    (rotate270 painter1)
    (rotate270 painter2))))

  

(defun below (painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))
	  (paint-top
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0))))
      (lambda (frame)
	(funcall paint-bottom frame)
	(funcall paint-top frame)))))

(defun beside (painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (funcall paint-left frame)
        (funcall paint-right frame)))))

(defun flip-horiz (painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(defun rotate270 (painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(defun rotate180 (painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))

(defun flip-vert (painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(defun rotate90 (painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defun transform-painter (painter origin corner1 corner2)
  #'(lambda (frame)
      (let ((m (frame-coord-map frame)))
	(let ((new-origin (funcall #'m origin)))
	  (funcall
	   painter
	   (make-frame new-origin
		       (sub-vect (funcall #'m corner1) new-origin)
		       (sub-vect (funcall #'m corner2) new-origin)))))))



;; a outline
(let ((bl (make-vect 0 0))
      (br (make-vect 1 0))
      (tl (make-vect 0 1))
      (tr (make-vect 1 1)))
  ;; a outline
  (segments->painter (list
		      (make-segment bl tl)
		      (make-segment tl tr)
		      (make-segment tr br)
		      (make-segment br bl)))
  ;; b corner
  (segments->painter (list
		      (make-segment bl tr)
		      (make-segment br tl))))

;; c diamond
(let ((lm (make-vect 0 .5))
      (tm (make-vect .5 1))
      (rm (make-vect 1 .5))
      (bm (make-vect .5 0)))
  (segments->painter (list
		      (make-segment lm tm)
		      (make-segment tm rm)
		      (make-segment rm bm)
		      (make-segment bm lm))))

;; d wave
		      

(defun segments->painter (segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(defun frame-coord-map (frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(defun origin-frame (frame)
  (car frame))

(defun edge1-frame (frame)
  (cadr frame))

(defun edge2-frame (frame)
  (caddr frame))

(defun make-frame (origin edge1 edge2)
  (list origin edge1 edge2))

(defun origin-frame (frame)
  (car frame))

(defun edge1-frame (frame)
  (cadr frame))

(defun edge2-frame (frame)
  (cddr frame))

(defun make-frame (origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(defun add-vect (v1 v2)
  (make-vect (+ (xcor-vect v1)
		(xcor-vect v2))
	     (+ (ycor-vect v1)
		(ycor-vect v2))))

(defun sub-vect (v1 v2)
  (make-vect (- (xcor-vect v1)
		(xcor-vect v2))
	     (- (ycor-vect v1)
		(ycor-vect v2))))

(defun scale-vect (v s)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))

(defun make-vect (x y)
  (cons x y))

(defun xcor-vect (v)
  (car v))

(defun ycor-vect (v)
  (cdr v))

(defun split (combine-main combine-smaller)
  (labels ((rec (painter n)
	     (if (= n 0)
		 painter
		 (let ((smaller (rec painter (- n 1))))
		   (funcall combine-main
			    (funcall combine-smaller smaller smaller))))))
    #'rec))

(defun square-of-four (tl tr bl br)
  #'(lambda (painter)
      (let ((top (beside (funcall tl painter) (funcall tr painter)))
	    (bottom (beside (funcall bl painter) (funcall br painter))))
	(below bottom top))))

;; Then flipped-pairs can be defined in terms of square-of-four as follows:
(defun flipped-pairs (painter)
  (let ((combine4 (square-of-four #'identity #'flip-vert
                                  #'identity #'flip-vert)))
    (funcall combine4 painter)))

;; and square-limit can be expressed as
(defun square-limit (painter n)
  (let ((combine4 (square-of-four #'flip-horiz #'identity
                                  #'rotate180 #'flip-vert)))
    (funcall combine4 (corner-split painter n))))

(defun up-split (painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

(defun right-split (painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(defparameter *wave* '(wave))
(defparameter *rogers* '(regers))

(defun flip-vert (painter)
  (cons 'flip-vert painter))

(defun flip-horiz (painter)
  (cons 'flip-horiz painter))

(defun beside (p1 p2)
  (list (cons 'left (list p1)) (cons 'right (list p2))))
(defun below (p1 p2)
  (list (cons 'bottom (list p1)) (cons 'top (list p2))))

(defparameter *wave2* (beside *w

(defun queens (board-size)
  (labels ((queen-cols (k)
	     (if (= k 0)
		 (list *empty-board*)
		 (filter
		  #'(lambda (positions) (safep k positions))
		  (flatmap
		   #'(lambda (rest-of-queens)
		       (mapcar1 #'(lambda (new-row)
				    (adjoin-position new-row k rest-of-queens))
				(enumerate-interval 1 board-size)))
		   (queen-cols (- k 1)))))))
    (queen-cols board-size)))

(defparameter *empty-board* nil)

; 简单的把new-row加入到列表尾部，没有用到k
(defun adjoin-position (new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

; 判断同行和对角线是否safe
(defun safep (k positions)
  (let* ((k-value (car (last positions)))
	 (lst (remove k-value positions :from-end t :count 1)))
    (labels ((iter (rest i)
	       (let ((item (car rest)))
		 (cond ((null rest) t)
		       ((or (= item k-value) ; 判断同一行和对角线
			    (= item (- k-value (- k i)))
			    (= item (+ k-value (- k i))))
			nil)
		       (t (iter (cdr rest) (1+ i)))))))
      (iter lst 1))))

(defun s-sum-triples (n s)
  (filter #'(lambda (lst)
	      (= (accumulate #'+ 0 lst) s))
	  (unique-triples n)))

(defun unique-triples (n)
  (permutations-n (enumerate-interval 1 n) 3))

(defun permutations-n (s n)
  (if (or (null s) (zerop n))
      (list nil)
      (flatmap #'(lambda (x)
		   (mapcar1 #'(lambda (p) (cons x p))
			    (permutations-n (our-remove x s)
					    (1- n))))
	       s)))

(defun unique-pairs (n)
  (flatmap
   #'(lambda (i)
       (mapcar1 #'(lambda (j) (list i j))
		(enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(defun flatmap (proc seq)
  (accumulate #'append nil (mapcar1 proc seq)))

(defun prime-sum-p (pair)
  (primep (+ (car pair) (cadr pair))))

(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defun prime-sum-pairs (n)
  (mapcar1 #'make-pair-sum
	   (filter #'prime-sum-p
		   (unique-pairs n))))

(defun permutations (s)
  (if (null s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap #'(lambda (x)
		   (mapcar1 #'(lambda (p) (cons x p))
			    (permutations (our-remove x s))))
               s)))

(defun our-remove (item sequence)
  (filter #'(lambda (x) (not (= x item)))
          sequence))

;; 素数检测
(defun primep (n)
  (= n (smallest-divisor n)))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((dividesp test-divisor n) test-divisor)
	(t (find-divisor n (1+ test-divisor)))))

(defun dividesp (a b)
  (zerop (rem b a)))

;; 素数检测，费马检查
(defun fast-prime-p (n times)
  (cond ((= times 0) t)
	((fermat-test n) (fast-prime-p n (1- times)))
	(t nil)))

(defun fermat-test (n)
  (labels ((try-it (a)
	     (= (expmod a n n) a)))
    (try-it (1+ (random (1- n))))))

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((evenp exp) (rem (square (expmod base (/ exp 2) m)) m))
	(t (rem (* base (expmod base (- exp 1) m)) m))))

(defun our-reverse (sequence)
  (fold-right #'(lambda (x y) 
		  (append y (list x))) 
	      nil 
	      sequence))

(defun our-reverse (sequence)
  (fold-left #'(lambda (x y) 
		 (cons y x)) 
	     nil 
	     sequence))

(defun fold-right (op initial sequence)
  (accumulate op initial sequence))

(defun fold-left (op initial sequence)
  (labels ((iter (result rest)
	     (if (null rest)
		 result
		 (iter (funcall op result (car rest))
		       (cdr rest)))))
    (iter initial sequence)))

(defun dot-product (v w)
  (accumulate #'+ 0 (mapcar #'* v w)))

(defun matrix-*-vector (m v)
  (mapcar1 #'(lambda (row)
	       (dot-product row v))
	   m))

(defun transpose (mat)
  (accumulate-n #'cons nil mat))

(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (mapcar1 #'(lambda (row)
		 (matrix-*-vector cols row))
	     m)))

(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      nil
      (cons (accumulate op init (mapcar1 #'(lambda (x) (car x)) seqs))
            (accumulate-n op init (mapcar1 #'(lambda (x) (cdr x)) seqs)))))

(defun count-leaves (tree)
  (accumulate #'(lambda (x y)
		  (+ (length x) y))
	      0
	      (mapcar1 #'enumerate-tree tree)))

(defun length1 (sequence)
  (accumulate #'(lambda (x y)
                  (1+ y))
              0 
              sequence))

(defun append1 (seq1 seq2)
  (accumulate #'cons seq2 seq1))

(defun mapcar1 (p sequence)
  (accumulate #'(lambda (x y) 
		  (cons (funcall p x) y)) 
	      nil 
	      sequence))

(defun salary-of-highest-paid-programmer (records)
  (accumulate #'max
	      0
	      (mapcar1 #'salary
		       (filter #'programmerp records))))

(defun product-of-squares-of-odd-elements (sequence)
  (accumulate #'*
	      1
	      (mapcar1 #'square
		       (filter #'oddp sequence))))

(defun list-fib-squares (n)
  (accumulate #'cons
	      nil
	      (mapcar1 #'square
		       (mapcar1 #'fib
				(enumerate-interval 0 n)))))

(defun even-fibs (n)
  (accumulate #'cons
	      nil
	      (filter #'evenp
		      (mapcar1 #'fib
			       (enumerate-interval 0 n)))))

(defun sum-odd-squares (tree)
  (accumulate #'+
	      0
	      (mapcar1 #'square
		       (filter #'oddp
			       (enumerate-tree tree)))))

(defun enumerate-tree (tree)
  (cond ((null tree) nil)
	((atom tree) (list tree))
	(t (append (enumerate-tree (car tree))
		   (enumerate-tree (cdr tree))))))

(defun enumerate-interval (low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (1+ low) high))))

(defun accumulate (fn initial lst)
  (if (null lst)
      initial
      (funcall fn (car lst)
	       (accumulate fn initial (cdr lst)))))

(defun filter (predicate lst)
  (cond ((null lst) nil)
	((funcall predicate (car lst))
	 (cons (car lst) (filter predicate (cdr lst))))
	(t (filter predicate (cdr lst)))))

(defun sum-odd-squares (tree)
  (cond ((null tree) 0)  
        ((not (consp tree))
         (if (oddp tree) (square tree) 0))
        (t (+ (sum-odd-squares (car tree))
	      (sum-odd-squares (cdr tree))))))

(defun even-fibs (n)
  (labels ((next (k)
	     (if (> k n)
		 nil
		 (let ((f (fib k)))
		   (if (evenp f)
		       (cons f (next (+ k 1)))
		       (next (+ k 1)))))))
    (next 0)))

(defun square (x) (* x x))

(defun subsets (s)
  (if (null s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest 
		(mapcar1 #'(lambda (x)
			     (cons (car s) x))
			 rest)))))

(defun square-tree (tree)
  (tree-map #'(lambda (x) (* x x))
	    tree))

(defun tree-map (fn tree)
  (mapcar1 #'(lambda (sub-tree)
	       (if (consp sub-tree)
		   (tree-map fn sub-tree)
		   (funcall fn sub-tree)))
	   tree))

;; (defun square-tree (tree)
;;   (mapcar1 #'(lambda (sub-tree)
;; 	       (if (consp sub-tree)
;; 		   (square-tree sub-tree)
;; 		   (* sub-tree sub-tree)))
;; 	   tree))

;; (defun square-tree (tree)
;;   (cond ((null tree) nil)
;; 	((atom tree) (* tree tree))
;; 	(t (cons (square-tree (car tree))
;; 		 (square-tree (cdr tree))))))

(defun scale-tree (tree factor)
  (mapcar1 #'(lambda (sub-tree)
	       (if (consp sub-tree)
		   (scale-tree sub-tree factor)
		   (* sub-tree factor)))
	   tree))

;; (defun scale-tree (tree factor)
;;   (cond ((null tree) nil)
;; 	((not (consp tree)) (* tree factor))
;; 	(t (cons (scale-tree (car tree) factor)
;; 		 (scale-tree (cdr tree) factor)))))

(defun right-branch (mobile) (cdr mobile))

(defun branch-structure (branch) (cdr branch))

(defun make-mobile (left right)
  (cons left right))

(defun make-branch (length structure)
  (cons length structure))

(defun mobile-balanced-p (mobile)
  (if (numberp mobile)
      mobile
      (let* ((lbw (mobile-balanced-p (branch-structure (left-branch mobile))))
	     (rbw (and lbw
		       (mobile-balanced-p (branch-structure (right-branch mobile))))))
	(and rbw
	     (= (* (branch-length (left-branch mobile)) lbw)
		(* (branch-length (right-branch mobile)) rbw))
	     (+ lbw rbw)))))

(defun mobile-balanced-p (mobile)
  (or (numberp mobile)
      (and (= (* (branch-length (left-branch mobile))
		 (total-weight (branch-structure (left-branch mobile))))
	      (* (branch-length (right-branch mobile))
		 (total-weight (branch-structure (right-branch mobile)))))
	   (mobile-balanced-p (branch-structure (left-branch mobile)))
	   (mobile-balanced-p (branch-structure (right-branch mobile))))))

(defun total-weight (mobile)
  (labels ((rec (m acc)
	     (if (numberp m)
		 (+ m acc)
		 (rec (branch-structure (left-branch m))
		      (rec (branch-structure (right-branch m))
			   acc)))))
    (rec mobile 0)))

(defun total-weight (mobile)
  (labels ((branch-weight (branch)
	     (total-weight (branch-structure branch))))
    (if (numberp mobile)
	mobile
	(+ (branch-weight (left-branch mobile))
	   (branch-weight (right-branch mobile))))))

(defun left-branch (mobile)
  (car mobile))

(defun right-branch (mobile)
  (car (cdr mobile)))

(defun branch-length (branch)
  (car branch))

(defun branch-structure (branch)
  (car (cdr branch)))

(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))

(defun fringe (tree)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x)
			   (rec (cdr x) acc))))))
    (rec tree nil)))

;; (defun fringe (tree)
;;   (cond ((null tree) nil)
;; 	((atom tree) (list tree))
;; 	(t (append (fringe (car tree))
;; 		   (fringe (cdr tree))))))

(defun count-leaves-iter (x)
  (labels ((iter (tree acc)
	     (cond ((null tree) acc)
		   ((atom tree) (1+ acc))
		   (t (iter (car tree) 
			    (iter (cdr tree) acc))))))
    (iter x 0)))

(defun count-leaves-iter1 (x)
  (labels ((iter (tree acc)
	     (cond ((null tree) acc)
		   ((atom tree) (1+ acc))
		   (t (iter (cdr tree) 
			    (iter (car tree) acc))))))
    (iter x 0)))

(defun count-leaves (x)
  (cond ((null x) 0)
	((atom x) 1)
	(t (+ (count-leaves (car x))
	      (count-leaves (cdr x))))))

(defun for-each (proc items)
  (if (null items)
      nil
      (progn
	(funcall proc (car items))
	(for-each proc (cdr items)))))

(defun for-each (proc items)
  (dolist (item items)
    (funcall proc item)))

(defun scale-list (items factor)
  (mapcar1 #'(lambda (x) (* x factor))
	   items))

(defun mapcar1 (proc items)
  (if (null items)
      nil
      (cons (funcall proc (car items))
	    (mapcar1 proc (cdr items)))))

;; (defun scale-list (items factor)
;;   (if (null items)
;;       nil
;;       (cons (* (car items) factor)
;; 	    (scale-list (cdr items) factor))))

(defun same-parity (elem &rest args)
  (labels ((iter (predicate lst result)
	     (if (null lst)
		 (nreverse result)
		 (iter predicate
		       (cdr lst)
		       (if (funcall predicate (car lst))
			   (cons (car lst) result)
			   result)))))
    (cons elem
	  (iter (if (oddp elem)
		    #'oddp
		    #'evenp)
		args
		nil))))

(defun same-parity (elem &rest args)
  (labels ((rec (predicate lst)
	     (if (null lst)
		 nil
		 (if (funcall predicate (car lst))
		     (cons (car lst) (rec predicate (cdr lst)))
		     (rec predicate (cdr lst))))))
    (cons elem
	  (rec (if (oddp elem)
		   #'oddp
		   #'evenp)
	       args))))
		 
(defun timed-cc (amount coin-values)
  (let ((start-time (get-internal-real-time)))
    (cc amount coin-values)
    (- (get-internal-real-time) start-time)))

(defparameter *us-coins* (list 50 25 10 5 1))
(defparameter *uk-coins* (list 100 50 20 10 5 2 1 .5))

(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more-p coin-values)) 0)
	(t (+ (cc amount
		  (except-first-denomination coin-values))
	      (cc (- amount
		     (first-denomination coin-values))
		  coin-values)))))

(defun no-more-p (lst) (null lst))

(defun except-first-denomination (lst) (cdr lst))

(defun first-denomination (lst) (car lst))

(defun deep-reverse (lst)
  (labels ((iter (lst result)
	     (if (null lst)
		 result
		 (iter (cdr lst)
		       (cons
			(if (consp (car lst))
			    (deep-reverse (car lst))
			    (car lst))
			result)))))
    (iter lst nil)))

(defun deep-reverse (lst)
  (if (null lst)
      nil
      (append (deep-reverse (cdr lst))
	      (list 
	       (if (consp (car lst))
		   (deep-reverse (car lst))
		   (car lst))))))

(defun our-reverse (lst)
  (if (null lst)
      nil
      (append (our-reverse (cdr lst))
	      (list (car lst)))))

;; (defun our-reverse (lst)
;;   (labels ((iter (lst result)
;; 	     (if (null lst)
;; 		 result
;; 		 (iter (cdr lst)
;; 		      (cons (car lst)
;; 			    result)))))
;;     (iter lst nil)))

(defun last-pair (lst)
  (let ((rest (cdr lst)))
    (if (null rest)
	lst
	(last-pair rest))))
