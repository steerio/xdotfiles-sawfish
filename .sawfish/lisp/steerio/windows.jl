; vim: set ft=lisp:

(define-structure steerio.windows
  (export range-limit pairs window-frame-overhead outer-resize-move head-move-resize head-move-to)
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

  (defun head-move-resize (win x y w h)
    (let ((ofs (current-head-offset)))
      (move-resize-window-to
        win
        (+ (car pos) x)
        (+ (cdr pos) y)
        w h)))

  (defun head-move-to (win x y)
    (let ((ofs (current-head-offset)))
      (move-window-to
        win
        (+ (car ofs) x)
        (+ (cdr ofs) y))))

  (defun outer-resize-move (win dims pos)
    (head-move-resize win pos (pairs - dims (window-frame-overhead win)))))
