(setq trip '(- 0 1))
;(untrace simplify)
;(untrace simplify-add)
;(untrace simplify-sub)
;(untrace simplify-triple)
(load "Main.lisp")
(setq l '(+ x (- 0 x)))
(setq p1 '(+ x (* x (- y (/ z 2)))))
(setq p2 '(+ (- z 2) (* x 5)))
(setq p3 '(+ 1 a))
