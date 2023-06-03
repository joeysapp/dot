(defun osascript-run-file (osascript-file)
  "Run an oascript file"
  (interactive "fApplescript file: ")
  (shell-command (concat "osascript " osascript-file )))

; (defun osascript-eval-region-as-script-and-block-emacs (s e)
;   "Evaluate your region as osascript (so with quotations) and will block Emacs until the script is.. addressed?"
;   (interactive "r")
;   (let ((script (buffer-substring s e)))
;     (shell-command (concat "osascript " "-e " script))))

(defun osascript-eval-region (s e)
  "Evaluates a region as an osascript file while not blocking emacs"
  (interactive "r")
  (let ((script (buffer-substring s e))
        (osascript-file "/tmp/emacs-eval-region.osascript"))
    (find-file osascript-file)
    (with-current-buffer (get-file-buffer osascript-file)
      (kill-region (point-min) (point-max))
      (insert script)
      (save-buffer)
      (kill-buffer))
    (osascript-run-file osascript-file)))

(defun osascript-eval-current-buffer ()
  (interactive)
  (osascript-eval-region-as-file ((point-min) (point-max))))
