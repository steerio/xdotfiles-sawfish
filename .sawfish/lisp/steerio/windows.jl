; vim: set ft=lisp:

(define-structure steerio.windows
  (export range-limit pairs window-frame-overhead outer-resize-move)
  (open rep sawfish.wm)

  (defun range-limit (maximum val)
    (max 0 (min maximum val)))

  (defmacro pairs (fun #!rest conses)
    (let* ((lets (if (symbolp fun)
                   (list)
                   `((,(gensym) ,fun))))
           (fun (if (symbolp fun)
                  fun
                  (caar lets)))
           (syms (mapcar
                   (lambda (c)
                     (if (consp c)
                       (let ((s (gensym)))
                         (setq lets (cons (list s c) lets))
                         s)
                       c))
                   conses))
           (body `(cons
                    (,fun ,@(mapcar (lambda (sym) (list `car sym)) syms))
                    (,fun ,@(mapcar (lambda (sym) (list `cdr sym)) syms)))))
      (if (= lets '())
        body
        `(let ,(reverse lets) ,body))))
  
  (defun window-frame-overhead (win)
    (pairs -
           (window-frame-dimensions win)
           (window-dimensions win)))

  (defun outer-resize-move (win dims pos)
    (let ((dims (pairs - dims (window-frame-overhead win))))
      (move-resize-window-to
         win
         (car pos) (cdr pos)
         (car dims) (cdr dims)))))
