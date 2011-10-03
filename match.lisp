(defun carmatch (x y)
  (eq (car x) (car y))
  )

;read-from-string

(defun getpred (x)
  (cons (subseq (symbol-name x) 0 1)
;	(cons (subseq (symbol-name x) 0) nil))
	(cons (subseq (symbol-name x) 1) nil))
  )

(defun handle-gt (x y)
;  (princ (< (cdr (assoc x lst)) (eval y)))
  (cond
    ((assoc x lst) (cond
		  ((< (cdr (assoc x lst)) (eval y)) y)
		  (t (setq broken t))))
    (t (setq broken t)))
  )

(defun handle-lt (x y)
;  (princ (assoc x lst))

  (cond
    ((assoc x lst)
;     (princ (eval y))
;     (princ x)

     (cond
		  ((> (cdr (assoc x lst)) y) y)
		  (t (setq broken t))))
    (t (setq broken t)))
  )

(defun handle-eq (x y)
;  (princ (assoc x lst))
  (cond
    ((assoc x lst)
     (cond
		  ((eq (cdr (assoc x lst)) y) y)
		  (t (setq broken t))))
    (t (push (cons x y) lst)))
  )

(defun handle-neq (x y)
  (cond
    ((assoc x lst) (cond
		  ((not (eq (cdr (assoc x lst)) y)) y)
		  (t (setq broken t))))
    (t (setq broken t)))
  )

(defun act-on-pred (x y)
  (cond
    ((symbolp x)
     (cond
       ((string-equal (car (getpred x)) ">") (handle-gt (intern (cadr (getpred x))) y))
       ((string-equal (car (getpred x)) "<") (handle-lt (intern (cadr (getpred x))) y))
       ((string-equal (car (getpred x)) "!") (handle-neq (intern (cadr (getpred x))) y))
       ((string-equal (car (getpred x)) "=") (handle-eq (intern (cadr (getpred x))) y))
       ((eq x y) y)
       (t (setq broken t))))
    ((eq x y) y)
    (t (setq broken t)))
)

(defun cadr-recurse-match (x y )
  (cond
    ((and (endp (cdr x)) (endp (cdr y))) y)
    ((endp (ur-match (cdr x) (cdr y))) ())
    (t y)))

(defun and-pred (x y)
  (cond
    ((endp x) ())
    (t (ur-match (car x) y) (and-pred (cdr x) y)))
)

(defun list-match (x y)
  (cond
    ((endp x) ())
    ((eq (car x) `&) (and-pred (cdr x) y))
    (t (cons (ur-match (car x) (car y)) (ur-match (cdr x) (cdr y)))))
)


(defun other-match (x y)
  (cond
        ((listp (car x))
     (cond ((endp (ur-match (car x) (car y))) (cadr-recurse-match x y))
	   (t y)))

    ((listp (cadr x))
     (cond
       ((carmatch x y) (cadr-recurse-match x y))
       (t (setq broken t))))
    (t
     (cond
       ((carmatch x y) (ur-match (cadr x) (cadr y)))
       (t (setq broken t))))
    )
)

(defun ur-match (x y)
  (cond
    ((listp x) (list-match x y))

    (t (act-on-pred x y))))

(defun printer (list)
  (cond
    ((endp list) ())
    (t (princ (car list))
       (printer (cdr list))))
  )

(defun auto-format (list)
  (cond
    ((endp list) ())
    (t (cons (list (intern (concatenate `string "=" (symbol-name (car (car list))))) (cdr (car list))) (auto-format (cdr list)))))
)

(defun match (x y)
  (setq lst ())
  (setq broken ())
  (ur-match x y)
;  (princ broken)
  (cond
    (broken ())
    (t (auto-format lst)))
)

;(princ (match () ()))
;(princ (match `(bar =c) `(bar fight)))
;(princ (match '(elephant (color =c) (size =s)) '(elephant (color grey) (size 12))))

;(princ (match `(elephant (color =c) (size <c)) `(elephant (color 11) (size 8))))

;(princ (match `(elephants (sally (color =c) (size =s) (mood =m)) (rose (color =c) (size <s) (mood !m))) `(elephants (sally (color purple) (size 10) (mood happy)) (rose (color purple) (size 8) (mood happy)))))

;(princ (match `(elephant =c) `(elephant color)))

;(princ (match `(elephant (color =c)) `(elephant `(color white))))
;(princ (match `(3 ()) `(3 ())))
;(princ (match `(3 ()) `(4 ())))

;(princ (act-on-pred `=c))

;(princ (handle-eq `x 7))
;(princ (handle-eq `x 8))
;(princ (handle-gt `x 6))
;(princ (handle-lt `x 9))
;(princ (handle-neq `x 8))

;(princ (act-on-pred `=c 3))
;(princ (act-on-pred `>c 4))
;(princ (act-on-pred `<c 2))
;(princ (act-on-pred `!c 8))

;(princ (match `(=x =y (& =z >x <y)) `(1 3 2)))

;(princ (match
;                '(elephants ((=name (color =c) (size =s) (mood =m))
;                       (=anothername (color =c) (size <s) (mood !m))))
;               '(elephants ((sally (color purple) (size 10) (mood happy))
;                       (rose (color purple) (size 8) (mood sad))))
;))