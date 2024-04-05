(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

(global-set-key (kbd "C-c v") 'halve-other-window-height)

;; These are defaults:
(global-set-key (kbd "C-x -") 'shrink-window-if-larger-than-buffer)
(global-set-key (kbd "C-x +") 'balance-windows)
(global-set-key (kbd "C-x ^") 'enlarge-window)
(global-set-key (kbd "C-x {") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x }") 'shrink-window-horizontally)

(global-set-key (kbd "C-x <up>") (lambda () 
  (interactive)
  (enlarge-window 5)))
(global-set-key (kbd "C-x <down>") (lambda ()
  (interactive)
  (shrink-window 5)))
(global-set-key (kbd "C-x <left>") (lambda ()
  (interactive)
  (shrink-window-horizontally 5)))
(global-set-key (kbd "C-x <right>") (lambda ()
  (interactive)
enlarge-window-horizontally 5))
