; vim: set ft=lisp:

(define-structure steerio.core
  (export if-let)
  (open rep)
  
  (defmacro if-let (bindings #!rest body)
    `(let ,bindings
       (if ,(if (cdr bindings)
              `(and ,@(mapcar car bindings))
              (caar bindings))
         ,@body))))
