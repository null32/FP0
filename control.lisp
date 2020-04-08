;;; Задание 1
;;; Определитель матрицы
(defun det (matrix)
    (cond
        ((null matrix) 1)
        (t
            (*
                (caar matrix)
                (det
                    (cdr (remove-first-column (normalyze matrix)))
                )
            )
        )
    )
)

;;; Сложение
(defun add-matrix (a b)
    (cond
        ((and (null a) (null b)) nil)
        (t
            (cons
                (add-row (car a) (car b))
                (add-matrix (cdr a) (cdr b))
            )
        )
    )
)

;;; Умножение
(defun multiply-matrix (a b)
    (cond
        ((null a) nil)
        (t
            (cons
                (calculate-row (car a) b)
                (multiply-matrix (cdr a) b)
            )
        )
    )
)


;;; ---------------------------------------------------------------
;;; Вспомогательные функции
(defun remove-first-column (m)
    (cond
        ((null m) nil)
        (t
            (cons
                (cdar m)
                (remove-first-column (cdr m))
            )
        )
    )
)

(defun get-first-column (m)
    (cond
        ((null m) nil)
        (t
            (cons
                (caar m)
                (get-first-column (cdr m))
            )
        )
    )
)


(defun normalyze (m)
    (cons
        (car m)
        (mapcar
            (lambda (row)
                (subtract-row row
                    (multiply-row-by (car m)
                        (/ (car row) (caar m))
                    )
                )
            )
            (cdr m)
        )
    )
)


(defun subtract-row (a b)
    (add-row a (mapcar (lambda (x) (* x -1)) b))
)

(defun multiply-row-by (row by)
    (mapcar (lambda (x) (* x by)) row)
)

(defun add-row (a b)
    (mapcar '+ a b)
)

(defun calculate-row (row m)
    (cond
        ((null (car m)) nil)
        (t
            (cons
                (scalar-multiply row (get-first-column m))
                (calculate-row row (remove-first-column m))
            )
        )
    )
)

(defun scalar-multiply (a b)
    (apply '+ (mapcar '* a b))
)


;;; Debug
; (print (apply-to-each (lambda (x) (* x 2)) '(1 2 3)))
; (print (subtract-row '(6 7 8) '(1 2 3)))
; (print (multiply-row-by '(1 2 3) '2))
; (print (normalyze '((1 2) (3 4))))
; (print (get-first-column '((1 2) (3 4))))
; (print (remove-first-column '((1 2) (3 4))))
; (print (scalar-multiply '(1 2) '(3 4)))
; (print (calculate-row '(1 2) '((5 6) (7 8))))


;;; ---------------------------------------------------------------
;;; Tests
(print (det '(
    (1 2 3)
    (6 5 4)
    (8 9 7)
)))

(print (add-matrix '(
    (1 2)
    (3 4)
) '(
    (5 6)
    (7 8)
)))

(print (multiply-matrix '(
    (1 2)
    (3 4)
) '(
    (5 6)
    (7 8)
)))

;;; ---------------------------------------------------------------
;;; Задание 2
(defun split-list (l)
    (cond
        ((null l) (cons nil nil))
        (t
            (
                (lambda (res)
                    (cond
                        ((or (null (car res)) (< (car l) (caar res)))
                            (cons
                                (cons
                                    (car l)
                                    (car res)
                                )
                                (cdr res)
                            )
                        )
                        (t
                            (cons
                                (cons
                                    (car l)
                                    nil
                                )
                                res
                            )
                        )
                    )
                )
                (split-list (cdr l))
            )
        )
    )
)

(write-line "")
(print (split-list '(2 7 10 8 3 4 9 1 2 0 8 3 2 5)))
