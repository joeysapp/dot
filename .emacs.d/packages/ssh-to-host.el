(defun ssh-hotkey (host)
  (interactive (list (read-string 
     "Enter: /ssh: user@host:path"
     (concat "/ssh:" (exec-path-from-shell-copy-env "SSH_DIGITALOCEAN_000")))))
  (switch-to-buffer (find-file-noselect host)))

(defun ssh-to-digitalocean-000 () (switch-to-buffer
 (find-file-noselect
  (concat "/ssh:" (exec-path-from-shell-copy-env "SSH_DIGITALOCEAN_000")))))
; (setq foo (let ((current-prefix-arg 4)) (call-interactively 'ssh-to-digitalocean-000))))))
