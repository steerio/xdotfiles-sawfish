; vim: set ft=lisp:

(define-structure steerio.grid
  (export grid-half grid-quarter
          grid-narrow-terminal grid-side-terminal)
  (open rep sawfish.wm
        steerio.core steerio.windows)

  (declare (inline mul div))

  ; Settings

  (defgroup grid "Grid")

  (defcustom grid-horizontal-cells 6
    "Number of horizontal cells"
    :group grid
    :after-set (lambda () (rplaca current-grid grid-horizontal-cells))
    :type (number 2 12))

  (defcustom grid-vertical-cells 4
    "Number of vertical cells"
    :group grid
    :after-set (lambda () (rplacd current-grid grid-vertical-cells))
    :type (number 2 12))

  (defcustom grid-term-columns 80
    "Preferred narrow terminal columns"
    :group grid
    :type (number 70 120))

  (define current-grid (cons grid-horizontal-cells grid-vertical-cells))

  ; Helpers

  (defun mul (a b)
    (truncate (* a b)))

  (defun div (a b)
    (truncate (/ a b)))

  (defun window-center (win #!optional pos)
    (pairs
      (lambda (pos size)
        (round (+ pos (/ size 2))))
      (or pos (window-position win))
      (window-frame-dimensions win)))

  (defun target (win ratios #!optional pos)
    (pairs
      (lambda (grid screen center ratio)
        (* (range-limit
             (1- ratio)
             (mul (/ center screen) ratio))
           (/ grid ratio)))
      current-grid (current-head-dimensions) (window-center win pos) ratios))

  (defun cell-size ()
    (pairs / (current-head-dimensions) current-grid))

  (defun cell-width ()
    (/ (car (current-head-dimensions)) grid-horizontal-cells))

  (defun cell-height ()
    (/ (cdr (current-head-dimensions)) grid-vertical-cells))

  (defun in-cell-x (win)
    (div (window-x win) (truncate (cell-width))))

  (defun in-cell-y (win)
    (div (window-y win) (truncate (cell-height))))

  (defun in-cell (win)
    (pairs
      (lambda (pos size) (div pos (truncate size)))
      (window-position win) (cell-size)))

  (defun cell-x (pos)
    (mul pos (cell-width)))

  (defun cell-y (pos)
    (mul pos (cell-height)))

  (defun cell-at (pos)
    (pairs mul pos (cell-size)))

  ; Tiling

  (defun tile (win dims pos)
    (let ((cell (cell-size)))
      (outer-resize-move
        win
        (pairs mul dims cell)
        (pairs mul pos cell))))

  (defun auto-tile (win ratios)
    (tile
      (if (cdr (workspace-windows))
        ((placement-mode 'best-fit) win)
        win)
      (pairs / current-grid ratios)
      (target win ratios)))

  ; Commands

  (defun grid-half (win)
    (interactive "%f")
    (auto-tile win '(2 . 1)))

  (defun grid-quarter (win)
    (interactive "%f")
    (auto-tile win '(2 . 2)))

  (defun grid-narrow-terminal (win #!optional height)
    (interactive "%f")
    (let ((hints (window-size-hints win)))
      (if-let ((inc (assoc 'width-inc hints)))
        (resize-window-to
          win
          (+ (* grid-term-columns (cdr inc))
             (or (cdr (assoc 'base-width hints))
                 0))
          (or height (cdr (window-dimensions win)))))))

  (defun grid-side-terminal (win)
    (interactive "%f")
    (let ((scr (current-head-dimensions)))
      (if (grid-narrow-terminal
            win
            (- (cdr scr)
               (cdr (window-frame-overhead win))))
        (let ((width (car (window-frame-dimensions win))))
          (head-move-to
            win
            (if (< (+ (window-x win) (/ width 2))
                   (/ (car scr) 2))
              0
              (- (car scr) width))
            0))))))
