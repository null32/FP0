;;; #9
;;; Разделить список на два списка:
;;;   список элементов с четным номером
;;;   список элементов с нечетным номером
;;; [9, 1, 8, 2] -> [9, 8], [1, 2]

(defun split (lst)
    (cond                   ; if
        ((null lst)         ; list.length === 0
            '(nil nil)      ; return ((), ())
        )
        (t                  ; else
            ((lambda (l f s)                        ; (l, f, s) =>
                (cons                               ; join results [
                    (cons f (car l))                ;   [f, ...l[1]]
                    (cons                           ;   wrap
                        (cond                       ;   if
                            ((null s)               ;   s === null
                                (cadr l)            ;     l[2]
                            )
                            (t                      ;   else
                                (cons s (cadr l))   ;   [s, ...l[2]]
                            )
                        )                           ;   #if
                    nil)                            ;   #wrap
                )                                   ; ]
            ) (split (cddr lst)) (car lst) (cadr lst))
        )
    )
)

;;; Test 1
(write-line "Test 1")
(princ " >> (1 2 3 4)")
(print (split '(1 2 3 4)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ " >> (1 2 3)")
(print (split '(1 2 3)))
(write-line "")



(write-line "")
;;; #19
;;; Написать ф-ю враппер в список n уровня
;;; (луковица 3) -> (((3)))

(defun луковица (n)
    (_луковица n n)
)
(defun _луковица (n v)
    (cond
        ((= n 0)
            v
        )
        (t
            (cons (_луковица (- n 1) v) nil)
        )
    )
)

;;; Test 1
(write-line "Test 1")
(princ ">> (луковица 3)")
(print (луковица 3))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ ">> (луковица 10)")
(print (луковица 10))
(write-line "")



(write-line "")
;;; #25
;;; Убрать из списка каждый второй элемент
;;; [1, 9, 2, 8] -> [1, 2]

(defun filter (lst)
    (cond
        ((null lst)
            nil
        )
        (t
            (cons
                (car lst)
                (filter (cddr lst))
            )
        )
    )
)

;;; Test 1
(write-line "Test 1")
(princ ">> (1 2 3 4)")
(print (filter '(1 2 3 4)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ ">> (1 2 3)")
(print (filter '(1 2 3)))
(write-line "")



(write-line "")
;;; #26
;;; Разбить список по парам
;;;   А что если элементов нечетное количество?
;;; [1, 2, 3, 4, 5, 6] -> [[1, 2], [3, 4], [5, 6]]

(defun pairs (lst)
    (cond
        ((null lst)
            nil
        )
        (t
            (cons
                (cons (car lst) (cons (cadr lst) nil))
                (pairs (cddr lst))
            )
        )
    )
)

;;; Test 1
(write-line "Test 1")
(princ ">> (1 2 3 4 5 6)")
(print (pairs '(1 2 3 4 5 6)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ ">> (1 2 3)")
(print (pairs '(1 2 3)))
(write-line "")



(write-line "")
;;; #29
;;; Определить глубину списка
;;; [[] [[]] []] -> 3

(defun depth (lst)
    (cond
        ((null lst)
            0
        )
        (t
            (max
                (+ (depth (car lst)) 1)
                (depth (cdr lst))
            )
        )
    )
)

;;; Test 1
(write-line "Test 1")
(princ ">> (nil (nil) nil)")
(print (depth '(nil (nil) nil)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ ">> (nil (nil) ((nil)))")
(print (depth '(nil (nil) ((nil)))))
(write-line "")



(write-line "")
;;; #34
;;; Определить равенство множеств
;;; [1 2 3] == [3 2 1] -> true
;;; [1 2 3] == [4 5 6] -> false

(defun equal-set (lst-a lst-b)
    (cond
        ((and (null lst-a) (null lst-b))
            T
        )
        ((or (null lst-a) (null lst-b))
            nil
        )
        (t
            (equal-set (cdr lst-a) (remove-value lst-b (car lst-a)))
        )
    )
)
(defun remove-value (lst v)
    (cond
        ((null lst)
            nil
        )
        (t
            ((lambda (e)
                (cond
                    ((= (car lst) v)
                        e
                    )
                    (t
                        (cons (car lst) e)
                    )
                )
            ) (remove-value (cdr lst) v))
        )
    )
)

;;; Test 1
(write-line "Test 1")
(princ ">> (1 2 3) == (3 2 1)")
(print (equal-set '(1 2 3) '(3 2 1)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ ">> (1 2 4) == (4 5 6)")
(print (equal-set '(1 2 3) '(4 5 6)))
(write-line "")

;;; Test 3
(write-line "Test 3")
(princ ">> (1 2) == (1 2 3)")
(print (equal-set '(1 2) '(1 2 3)))
(write-line "")

;;; Test 4
(write-line "Test 4")
(princ ">> () == ()")
(print (equal-set '() '()))
(write-line "")



(write-line "")
;;; #36
;;; Определить наличие пересечения у множеств
;;; [1 2 3] & [3 4 5] -> true
;;; [1 2 3] & [4 5 6] -> false

(defun intersect (lst-a lst-b)
    (cond
        ((null lst-a)
            nil
        )
        ((list-contains lst-b (car lst-a))
            t
        )
        (t
            (intersect (cdr lst-a) lst-b)
        )
    )
)
(defun list-contains (lst v)
    (cond
        ((null lst)
            nil
        )
        ((= (car lst) v)
            t
        )
        (t
            (list-contains (cdr lst) v)
        )
    )
)

;;; Test 1
(write-line "Test 1")
(princ ">> (1 2 3) & (3 4 5)")
(print (intersect '(1 2 3) '(3 4 5)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ ">> (1 2 3) & (4 5 6)")
(print (intersect '(1 2 3) '(4 5 6)))
(write-line "")



(write-line "")

;;; #40
;;; Вычесть из одного множества другое
;;; [1 2 3] / [2 3 4] -> [1]
;;; [1 2 3] / [4 5 6] -> [1 2 3]
;;;   Использует remove-value описаный выше ⬆

(defun subtract (lst-a lst-b)
    (cond
        ((null lst-b)
            lst-a
        )
        (t
            (subtract (remove-value lst-a (car lst-b)) (cdr lst-b))
        )
    )
)

;;; Test 1
(write-line "Test 1")
(princ ">> (1 2 3) / (2 3 4)")
(print (subtract '(1 2 3) '(2 3 4)))
(write-line "")

;;; Test 2
(write-line "Test 2")
(princ ">> (1 2 3) / (4 5 6)")
(print (subtract '(1 2 3) '(4 5 6)))
(write-line "")



(write-line "")

;;; #45
;;; Определить расстояние между двумя точками.
;;; У символа (названия города) определены свойства x и y,
;;; необходимо вычислить расстояние между двумя городами.
;;; struct city {
;;;   float x, y;
;;; }
;;; distance(city, city) -> float
(defun distance (city-a city-b)
    (sqrt (+
        (expt
            (- (get city-a 'x) (get city-b 'x))
        2)
        (expt
            (- (get city-a 'y) (get city-b 'y))
        2)
    ))
)
(defun make-city (name x y)
	(setf (get name 'x) x)
	(setf (get name 'y) y)
)

;;; Test 1
(write-line "Test 1")
(princ ">> {0, 3} {4, 0}")
(make-city 'city1 0 3)
(make-city 'city2 4 0)
(print (distance 'city1 'city2))
(write-line "")

;; Test 2
(write-line "Test 2")
(princ ">> {34, 56} {71, 66}")
(make-city 'city1 34 56)
(make-city 'city2 71 66)
(print (distance 'city1 'city2))
(write-line "")



(write-line "")

;;; #46
;;; Определить являются ли два человека сестрами или брятьями
;;; Родители заданы как свойства символа-человека
;;;
;;; parents
;;; A    B    C    D
;;;
;;;   \/   \/   \/
;;;
;;;  VOVA DANYA ILYA
;;; children

(defun make-child (name p1 p2)
	(setf (get name 'p1) p1)
	(setf (get name 'p2) p2)
)
(defun parents (x)
	(cons
		(get x 'p1)
		(cons
			(get x 'p2)
			nil
		)
	)
)
(defun relatives (x1 x2)
	((lambda (e1 e2)
		(or
			(string= (car e1) (car e2))
			(string= (cadr e1) (car e2))
			(string= (car e1) (cadr e2))
			(string= (cadr e1) (cadr e2))
		)
	) (parents x1) (parents x2))
)

(make-child 'VOVA 'A 'B)
(make-child 'DANYA 'B 'C)
(make-child 'ILYA 'C 'D)
;; Test 1
(write-line "Test 1")
(princ ">> VOVA and DANYA")
(print (relatives 'VOVA 'DANYA))
(write-line "")

;; Test 2
(write-line "Test 2")
(princ ">> VOVA and ILYA")
(print (relatives 'VOVA 'ILYA))
(write-line "")
