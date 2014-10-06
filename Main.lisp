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
  ( (null binding-list)				
	exp )
  (T
   (subst-bindings (cdr binding-list) (deep-subst (caar binding-list) (cadar binding-list) exp)
   ))))



(defun is-elem-op (op);Unit test successful
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
  ((op (car l)))
  (let 
   ((left-arg (cadr l)))
   (let
	((right-arg (caddr l)))
	(and (is-elem-op op) (atom left-arg) (atom right-arg))
   )))
 )
(defun is-negative (l)
 ;;Returns the value of x iff the following conditions are met:
 ;;[1] (atom x) returns true ie x is a number
 ;;[2] the list l comes in the form (- 0 x)
 ;;Otherwise, returns false ie nil
 (let 
  ((op (car l)))
  (let
   ((left-arg (cadr l)))
   (let
	((right-arg (caddr l)))
	(cond
	 ((and (is-elem-triple l) (eq op '-) (eq left-arg 0) (atom right-arg))
	  right-arg
	 )
	 (T nil)
	)
   )))
 )
(defun simplify-add (left-arg right-arg)
 (cond;Unit test successful
  ((eq right-arg 0) left-arg)
  ((eq left-arg 0) right-arg)
  ;The case of (+ x (- 0 x)) eq 0
  ((and (numberp left-arg) (numberp right-arg))(+ left-arg right-arg))
  ((and (atom left-arg) (eq left-arg (is-negative right-arg))) 0)
  ((and (atom right-arg) (eq right-arg (is-negative left-arg))) 0)
  (T (list '+ left-arg right-arg))
 )
 )
(defun simplify-sub (left-arg right-arg)
 (cond
  ((eq right-arg 0) left-arg)
  ((eq left-arg right-arg) 0)
  ((and (numberp left-arg) (numberp right-arg))(- left-arg right-arg))
  (T (list '- left-arg right-arg))
 )
 )
(defun simplify-mul (left-arg right-arg)
 (cond
  ((eq left-arg 0) 0)
  ((eq right-arg 0) 0)
  ((eq left-arg 1) right-arg)
  ((eq right-arg 1) left-arg)
  ((and (numberp left-arg) (numberp right-arg))(* left-arg right-arg))
  (T (list '* left-arg right-arg))
 )
 )
(defun simplify-div (left-arg right-arg)
 (cond
  ((eq left-arg 0) 0)
  ((eq right-arg 1) left-arg)
  ((eq left-arg right-arg) 1)
  ((and (numberp left-arg) (numberp right-arg))(/ left-arg right-arg))
  (T (list '/ left-arg right-arg))
 )
 )
(defun simplify-triple(op left-arg right-arg)
 ;;Simplifies a triple ie elementary operation.
 ;;Remember to consider nested triples.  
 (cond
  ( (eq op '+) (simplify-add left-arg right-arg))
  ( (eq op '-) (simplify-sub left-arg right-arg))
  ( (eq op '*) (simplify-mul left-arg right-arg))
  ( (eq op '/) (simplify-div left-arg right-arg))
  ;( (and (atom left-arg) (numberp right-arg))
	  ; (eval (list op left-arg right-arg)))
  ;( ((eq op '+) ()) ... )
  (T
   (list op left-arg right-arg))))

(defun simplify (exp)
 (cond
  ( (listp exp)						;either a list
	(simplify-triple (car exp) (simplify (cadr exp)) (simplify (caddr exp)))
	)
	(T 									;or an atom
	 exp)))

(defun evalexp (binding-list exp)
 ;;Calls substitute-bindings and simplifies the result.
 (simplify (subst-bindings binding-list exp))
 )
 (trace simplify)
 (trace simplify-add)
 (trace simplify-sub)
 (trace simplify-triple)
 (trace deep-subst)
 (trace subst-bindings)

