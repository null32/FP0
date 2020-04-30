;;; #5
;;; Определить функциональный предикат (any pred list),
;;; который истенен, если pred истенен для хотя бы
;;; одного элемента списка list
;;; (any (lambda (x) (> x 3)) '(1 2 3)) => nil
;;; (any (lambda (x) (> x 3)) '(1 5 3)) => T

; Version 1
;(defun any (pred lst)
;    (reduce (lambda (p x) (or p (funcall pred x))) lst :initial-value nil)
;)

; Version 3
; -- "без reduce и init..."
(defun any (pred lst)
    (not (null (mapcan (lambda (x) (cond ((funcall pred x) (list x)) (t nil))) lst)))
)

;;; Test #1
(write-line "Test 1")
(princ " >> (any (lambda (x) (> x 3)) '(1 2 3))")
(print (any (lambda (x) (> x 3)) '(1 2 3)))
(write-line "")

;;; Test #2
(write-line "Test 2")
(princ " >> (any (lambda (x) (> x 3)) '(1 5 3))")
(print (any (lambda (x) (> x 3)) '(1 5 3)))
(write-line "")



(write-line "")
;;; #9
;;; Напишите генератор чисел Фибоначчи

; Version 1
;(defun fib-gen ()
;    (let
;        ((a 1) (b 0) (temp 0))
;        (lambda ()
;            (progn
;                (setq temp b)
;                (setq b (+ a b))
;                (setq a temp)
;                b
;            )
;        )
;    )
;)

; Version 2
;
; -- "желательно без prongn"
; -- да легко
;
(defun fib-gen ()
    (let
        ((a 1) (b 0))
        (lambda ()
            ((lambda (v1 v2)
                (cond
                    ((psetq a v1 b v2) nil) ; psetq always returns nil
                    (t v2)
                )
            ) b (+ a b))
        )
    )
)

;;; Test #1
(write-line "Test 1")
(princ " first fib generator")
(setq f1 (fib-gen))
(print (funcall f1))
(print (funcall f1))
(print (funcall f1))
(write-line "")
(princ " second fib generator")
(setq f2 (fib-gen))
(print (funcall f2))
(print (funcall f2))
(print (funcall f2))
(print (funcall f2))
(write-line "")



(write-line "")
;;; #13
;;; Определите функцию, которая возвращает своё определение
(setq getme
    '(
        (lambda (x)
            (list x (list 'quote x))
        )
        '(lambda (x)
            (list x (list 'quote x))
        )
    )
)

;;; Test #1
(write-line "Test 1")
(princ " eval function")
(print (eval getme))
(write-line "")

;;; Test #2
(write-line "Test 2")
(princ " recursively eval function 3 times")
(print (eval (eval (eval getme))))
(write-line "")
