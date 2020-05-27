(load "./lang.lisp")

(foreach '(1 2 3) do print)

;(print (macroexpand '(join '(1 2 3) with '0)))
(print (join '(1 2 3) with ","))
(write-line "")

(myprint "foo" "bar" 1 2 3)

; (print (macroexpand '(rangegen :from 0 :to 10 :step 1)))
; (setq l1 (rangegen :from 0 :to 10 :step 1))
; (print (funcall l1))
; (print (funcall l1))
; (print (funcall l1))
