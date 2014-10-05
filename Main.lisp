;;let defines a scoped local variable.  
(defun addexp (e1 e2) (list '+ e1 e2))
(defun subexp (e1 e2) (list '- e1 e2))
(defun mulexp (e1 e2) (list '* e1 e2))
(defun divexp (e1 e2) (list '/ e1 e2))



(defun deep-subst (old new l)			;Traverse the tree and replace all instances of old with new.
  (cond
    ((null l) 							;if the list is null
     nil								;return an empty list
     )
    ((listp (car l))						;listp returns true if listp is of type list
     ;;cons constructs a list using (deep-subst old new(car l)) as the head
     ;;cons constructs a list using (deep-subst old new(cdr l)) as the tail
     (cons (deep-subst old new (car l)) (deep-subst old new(cdr l)))
     )
    ((eq old (car l)) 
     (cons new (deep-subst old new (cdr l)))
     )
    (T   
      (cons (car l) (deep-subst old new (cdr l)))
      )
    )
  )

(defun subst-bindings (binding-list exp) ;Takes a binding list and one expression.  
  ;;binding list is a list of (old, new) pairs
  (cond 
    ( (null bindinglist)				
     exp )
    (T
      (subst-bindings (cdr binding-list) (deep-subst (caar binding-list) (cdar binding-list) exp)
                      ))))



(defun is-elem-op (op)
  ;;Returns true if op is an elementary operator
  ;;ie +, -, *, /
  (cond 
    ((eq op '+) T)
    ((eq op '-) T)
    ((eq op '*) T)
    ((eq op '/) T)
    (T nil)
    )
  )
(defun is-elem-triple (l)
  (let 
    (op car l)
    (left-arg (cadr l))
    (right-arg (cddr l))
    (and (is-elem-op op) (numberp left-arg) (numberp right-arg))
    )
  )
(defun is-negative (l)
  ;;Returns the value of x iff the following conditions are met:
  ;;[1] (numberp x) returns true ie x is a number
  ;;[2] the list l comes in the form (- 0 x)
  ;;Otherwise, returns false ie nil
  (let 
    (op car l)
    (left-arg (cadr l))
    (right-arg (cddr l))
    (cond
      ((and (is-elem-triple l) (eq op '-) (= left-arg 0) (numberp right-arg))
       right-arg
       )
      (T nil)
      )
    )
  )
(defun simplify-add (left-arg right-arg)
  (cond
    ((and (numberp left-arg) (= right-arg 0))
     left-arg)
    ((and (= left-arg 0) (numberp right-arg) )
     right-arg)
    ;The case of (+ x (- 0 x)) = 0
    ((and (numberp left-arg) (eq left-arg (is-negative right-arg))) 0)
    ((and (eq right-arg (is-negative left-arg)) (numberp right-arg)) 0)
    (T (error "Simplify-add did not come in the standard form."))
    )
  )
(defun simplify-sub (left-arg right-arg)
  (cond
    ((and (numberp left-arg) (= right-arg 0))
     left-arg)
    ((= left-arg right-arg) 0)
    (T (error "Simplify-subtract did not come in the standard form."))
    )
  )
(defun simplify-mul (left-arg right-arg)
  (cond
    ((and (numberp left-arg) (= right-arg 0))
     0)
    ((and (= left-arg 0) (numberp right-arg))
     0)
    ((and (numberp left-arg) (= right-arg 1))
     left-arg)
    ((and (= left-arg 1) (numberp right-arg))
     right-arg)
    )
  )
(defun simplify-div (left-arg right-arg)
  (cond
    ((= left-arg 0) 0)
    ((and (numberp left-arg) (= right-arg 1))
     left-arg)
    ((= left-arg right-arg) 1)
    )
  )
(defun simplify-triple(op left-arg right-arg) ;Simplifies a triple ie elementary operation.
  ;;Remember to consider nested triples.  
  (if (listp left-arg)
    (setq left-arg (simplify left-arg))
    )
  (if (listp right-arg)
    (setq right-arg (simplify right-arg))
    )
  (cond
    ( (eq op '+) (simplify-add left-arg right-arg))
    ( (eq op '-) (simplify-sub left-arg right-arg))
    ( (eq op '*) (simplify-mul left-arg right-arg))
    ( (eq op '/) (simplify-div left-arg right-arg))
    ;( (and (numberp left-arg) (numberp right-arg))
    ; (eval (list op left-arg right-arg)))
    ;( ((eq op '+) ()) ... )
    (T
      (list op left-arg right-arg))))

(defun simplify (exp)
  (cond
    ( (listp exp)						;either a list
     (simplify-triple (car exp) (cadr exp (cddr exp)))
     (T 									;or an atom
       exp))))

(defun evalexp (binding-list exp)
  ;;Calls substitute-bindings and simplifies the result.
  (simplify (subst-bindings bindinglist exp))
  )
