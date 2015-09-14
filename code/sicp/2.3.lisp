;; 2.65
(defun tree->list (tree)
  (labels ((copy-to-list (tree result-list)
	     (if (null tree)
		 result-list
		 (copy-to-list (left-branch tree)
			       (cons (entry tree)
				     (copy-to-list (right-branch tree)
						   result-list))))))
    (copy-to-list tree '())))

(defun union-set (tree1 tree2)
  (labels ((union-set-list (set1 set2)
	     (cond ((null set1) set2)
		   ((null set2) set1)
		   (t
		    (let ((x1 (car set1)) (x2 (car set2)))
		      (cond ((= x1 x2)
			     (cons x1
				   (union-set-list (cdr set1) (cdr set2))))
			    ((< x1 x2)
			     (cons x1
				   (union-set-list (cdr set1) set2)))
			    ((> x1 x2)
			     (cons x2
				   (union-set-list set1 (cdr set2))))))))))
    (list->tree (union-set-list (tree->list tree1)
				(tree->list tree2)))))

;; 2.64
(defun list->tree (elements)
  (car (partial-tree elements (length elements))))

(defun partial-tree (elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (floor (/ (- n 1) 2))))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; sets as binary trees
(defun entry (tree) (car tree))
(defun left-branch (tree) (cadr tree))
(defun right-branch (tree) (caddr tree))
(defun make-tree (entry left right)
  (list entry left right))

(defun element-of-set-p (x set)
  (cond ((null set) nil)
	((= x (entry set)) t)
	((< x (entry set))
	 (element-of-set-p x (left-branch set)))
	((> x (entry set))
	 (element-of-set-p x (right-branch set)))))

(defun adjoin-set (x set)
  (cond ((null set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

;; sets as ordered lists
(defun element-of-set-p (x set)
  (cond ((null set) nil)
	((= x (car set)) t)
	((< x (car set)) nil)
	(t (element-of-set-p x (cdr set)))))

(defun intersection-set (set1 set2)
  (if (or (null set1) (null set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1
		     (intersection-set (cdr set1)
				       (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((> x1 x2)
	       (intersection-set set1 (cdr set2)))))))

(defun adjoin-set (x set)
  (cond ((null set) nil)
	((= x (car set)) (list x))
	((< x (car set)) (cons x set))
	(t (cons (car set)
		 (adjoin-set x (cdr set))))))

(defun union-set (set1 set2)
  (cond ((null set1) set2)
	((null set2) set1)
	(t
	 (let ((x1 (car set1)) (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1
			(union-set (cdr set1) (cdr set2))))
		 ((< x1 x2)
		  (cons x1
			(union-set (cdr set1) set2)))
		 ((> x1 x2)
		  (cons x2
			(union-set set1 (cdr set2)))))))))

;; duplicates
(defun adjoin-set (x set)
  (cons x set))

(defun intersection-set (set1 set2)
  (labels ((rec (set1 set2)
	     (cond ((or (null set1) (null set2)) '())
		   ((element-of-set-p (car set1) set2)
		    (cons (car set1)
			  (rec (cdr set1) set2)))
		   (t (rec (cdr set1) set2)))))
    (append (rec set1 set2) (rec set2 set1))))

(defun union-set (set1 set2)
  (append set1 set2))

;; no duplicates
(defun element-of-set-p (x set)
  (cond ((null set) nil)
	((equal? x (car set)) t)
	(t (element-of-set-p x (cdr set)))))

(defun adjoin-set (x set)
  (if (element-of-set-p x set)
      set
      (cons x set)))

(defun intersection-set (set1 set2)
  (cond ((or (null set1) (null set2)) '())
	((element-of-set-p (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(t (intersection-set (cdr set1) set2))))

(defun union-set (set1 set2)
  (cond ((null set1) set2)
	((null set2) set1)
	((element-of-set-p (car set1) set2)
	 (union-set (cdr set1) set2))
	(t (cons (car set1)
		 (union-set (cdr set1) set2)))))


;; a
(defun sump (x)
  (and (listp x) (eq (cadr x) '+)))

(defun addend (s) (car s))
(defun augend (s) (caddr s))

(defun make-sum (a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (numberp a1) (numberp a2)) (+ a1 a2))
	(t (list a1 '+ a2))))

(defun productp (x)
  (and (listp x) (eq (cadr x) '*)))

(defun multiplier (p) (car p))
(defun multiplicand (p) (caddr p))

(defun make-product (m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (numberp m1) (numberp m2)) (* m1 m2))
	(t (list m1 '* m2))))
;; b

;; 是增加了一点难度，主要是要满足先算乘法，并且没有括号（我们自己加括号）：
;; 1,(x * y + 3 * (x + y + 2))
;; 2,((x * y) + (3 * (x + y + 2)))
;; b 中需要得是［1］表达式，我们加上括号转化乘［2］表达式后发现：
;; 1，判断一个表达式是乘法还是加法得方法：如果表达式中含有'+，就为加法，如果都是'*，就为乘法。（谓词算法出来了）
;; 2，如果为加法，那么addend为第一个'+之前得项（如果为多项就是list，单项就是atom），augend为第一个'+之后得项（如果为多项就是list，单项就是atom）
;; 3，如果为乘法，multiplier就是第一项（car），multiplicand就是第二项之后得项（如果为多项就是list，单项就是atom）。
;; 至此，已经明了，相对a得环境，我们只需改谓词sump,productp，选择函数addend,augend,multiplicand，就可以了。

(defun productp (x)
  (and (consp x)
       (eq (cadr x) '*)
       (productp (cddr x))))

(defun sump (x)
  （or (and (consp x)
	    (eq (cadr x) '+))
       (sump (cddr x))))

(defun addend (s)
  (labels ((rec (s)
	     (if (eq '+ (car s))
		 nil
		 (cons (car s) (rec (cdr s))))))
    (let ((addend (rec s)))
      (if (cdr addend)
	  addend
	  (car addend)))))
	     
(defun augend (s)
  (if (eq '+ (cadr s))
      (if (cdddr s) (cddr s) (caddr s))
      (augend (cddr s))))

(defun multiplicand (p)
  (if (cdddr p)
      (cddr p)
      (caddr p)))
;; b--------

(defun augend (s)
  (if (cdddr s)
      (cons '+ (cddr s))
      (caddr s)))

(defun multiplicand (p)
  (if (cdddr p)
      (cons '* (cddr p))
      (caddr p)))

(defun make-exponentiation (base e)
  (cond ((=number? base 1) 1)
	((=number? e 0) 1)
	((=number? e 1) base)
	((and (numberp base) (numberp e)) (expt base e))
	(t (list '** base e))))
(defun base (e) (cadr e))
(defun exponent (e) (caddr e))
;; 第一个元素为**的表
(defun exponentiationp (x)
  (and (consp x) (eq (car x) '**)))

;; 0与任何东西的乘积都是0，1与任何东西的乘积总是那个东西
(defun make-product (m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (numberp m1) (numberp m2)) (* m1 m2))
	(t (list '* m1 m2))))

;; 当两个求和对象都是数时，返回它们的和。如果其中一个求和对象为0，就返回另一个对象
(defun make-sum (a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (numberp a1) (numberp a2)) (+ a1 a2))
	(t (list '+ a1 a2))))

;; 检查某个表达式是否等于一个给定的数
(defun =number? (exp num)
  (and (numberp exp) (= exp num)))

;; 变量就是符号
(defun variablep (x) (symbolp x))
;; 变量相同就表示它们的符号相互eq
(defun same-variable-p (v1 v2)
  (and (variablep v1) (variablep v2) (eq v1 v2)))
;; 和式与乘式都构造为表
(defun make-sum (a1 a2) (list '+ a1 a2))
(defun make-product (m1 m2) (list '* m1 m2))
;; 和式就是第一个元素为符号＋的表
(defun sump (x)
  (and (listp x) (eq (car x) '+)))
;; 被加数表示和式里的第二个元素
(defun addend (s) (cadr s))
;; 加数表示和式里的第三个元素
(defun augend (s) (caddr s))
;; 乘式为第一个元素为符号*的表
(defun productp (x)
  (and (listp x) (eq (car x) '*)))
;; 被乘数表示乘式里的第二个元素
(defun multiplier (p) (cadr p))
;; 乘数表示乘式里的第三个元素
(defun multiplicand (p) (caddr p))

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

(defun equal? (lst1 lst2)
  (cond ((and (atom lst1) (atom lst2))
	 (eq lst1 lst2))
	((and (listp lst1) (listp lst2))
	 (and (eq (car lst1) (car lst2))
	      (equal? (cdr lst1) (cdr lst2))))))

(defun memq (item x)
  (cond ((null x) nil)
	((eq item (car x)) x)
	(t (memq item (cdr x)))))

;;; 集合与信息检索
;; implemented as an unordered list
(defun lookup (given-key set-of-records)
  (cond ((null set-of-records) nil)
	((equal? given-key (key (car set-of-records)))
	 (car set-of-records))
	(t (lookup given-key (cdr set-of-records)))))

;; implemented as an binary tree
(defun lookup (given-key set-of-records)
  (if (null set-of-records)
      nil
      (let ((record-key (key (car set-of-records))))
	(cond ((= given-key record-key)
	       (car set-of-records))
	      ((> given-key record-key)
	       (lookup given-key (right-branch set-of-records)))
	      ((< given-key record-key)
	       (lookup given-key (left-branch set-of-records)))))))

;; Huffman树的表示
(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun leafp (object)
  (eq (car object) 'leaf))

(defun symbol-leaf (x) (cadr x))
(defun weight-leaf (x) (caddr x))

(defun make-code-tree (left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(defun left-branch (tree) (car tree))
(defun right-branch (tree) (cadr tree))
(defun symbols (tree)
  (if (leafp tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(defun weight (tree)
  (if (leafp tree)
      (weight-leaf tree)
      (cadddr tree)))

;; 解码过程
(defun decode (bits tree)
  (labels ((decode-1 (bits current-branch)
	     (if (null bits)
		 '()
		 (let ((next-branch
			(choose-branch (car bits) current-branch)))
		   (if (leafp next-branch)
		       (cons (symbol-leaf next-branch)
			     (decode-1 (cdr bits) tree))
		       (decode-1 (cdr bits) next-branch))))))
    (decode-1 bits tree)))

(defun choose-branch (bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(t (error "bad bit -- CHOOSE-BRANCH ~A" bit))))

;; 带权重元素的集合
(defun adjoin-set (x set)
  (cond ((null set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(t (cons (car set)
		 (adjoin-set x (cdr set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

;; 2.67
(defun sample-tree ()
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(defparameter sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; 2.68
(defun encode (message tree)
  (if (null message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(defun encode-symbol (symbol tree)
  (labels ((encode-symbol-1 (symbol tree bits)
	     (if (leafp tree)
		 (and (eq symbol (symbol-leaf tree)) (nreverse bits))
		 (let ((left-bits (encode-symbol-1 symbol (left-branch tree) (cons 0 bits))))
		   (if left-bits
		       left-bits
		       (encode-symbol-1 symbol (right-branch tree) (cons 1 bits)))))))
    (if (element-of-set-p symbol (symbols tree))
	(encode-symbol-1 symbol tree nil)
	(error "symbol is not in the tree ~A" symbol))))

;; 2.69
(defun generate-huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))

(defun successive-merge (lst)
  (cond ((< (length lst) 2) (car lst))
	(t (successive-merge
	    (adjoin-set (make-code-tree (first lst)
					(second lst))
			(cddr lst))))))

;; 2.70
(defparameter *songs-pairs*
  '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1)))

(defparameter *songs-message*
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom))

;; 2.71
n = 5 ; 1 2 4 8 16
n = 10; 1 2 4 8 16 32 64 128 256 512
