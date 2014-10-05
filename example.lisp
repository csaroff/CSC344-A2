(defun omit (x l)
	(cond
		((null l) '())
		(eq x (car l) (omit x (cdr l))
		(T (cons (car l) (omit x (crd l))))
	)
)

(defun factorial (x sofar)
	(if (= x 0) sofar
	(* x ( fact (- x 1))
)
