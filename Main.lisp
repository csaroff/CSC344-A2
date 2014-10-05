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
(defun subst-bindings (bindinglist exp) ;Takes a binding list and one expression.  
  (cond 
    ( (null bindinglist)				;binding list is a list of (old, new) pairs
     exp )
    (T									;else
      (deep-subst ... 
                  ))))

(defun simplify-add (left-arg right-arg)
  (cond
    ((and (numberp left-arg) (= right-arg 0))
      left-arg)
    ((and (= left-arg 0) (numberp right-arg) )
      right-arg)
    (())
    ()
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
     (simplify-triple (car exp) (cadr exp (cdr exp)))
     (T 									;or an atom
       exp))))

(defun evalexp (binding-list exp)
  ;;Calls substitute-bindings and simplifies the result.
  (simplify (subst-bindings bindinglist exp))
  )
