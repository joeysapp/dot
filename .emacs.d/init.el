;                                        __    _____         __ __
; .-----.--------.---.-.----.-----.  .--|  |  /  /__|.-----.|__|  |_   .-----.
; |  -__|        |  _  |  __|__ --|__|  _  |,' ,'|  ||     ||  |   _|__|  -__|
; |_____|__|__|__|___._|____|_____|__|_____/__/  |__||__|__||__|____|__|_____|
; ----------------------------------------------------------------------
; [ref] https://github.com/Fanael/init.el/blob/master/init.el
; [ref] https://github.com/yPhil-gh/kituu/blob/master/.emacs

(setq warning-minimum-level :emergency)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
; (eval 'load-path)

(save-place-mode 1)
(electric-indent-mode nil)

(setq-default indent-tabs-mode nil) ; indent will only insert spaces now
(put 'set-goal-column 'disabled nil) ; what happens on <enter>, basically auto-indenting (?)

(show-paren-mode t)
(setq show-paren-style 'parenthesis) ; expression for entire highlight

; [info] Prevent random # ~ / garbage all over remotes/local
(setq create-lockfiles nil)
(setq make-backup-files nil)
; (unless (file-directory-p "~/.emacs.d/backup")
;   (make-directory "~/.emacs.d/backup"))
; (setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

; macOS doesn't support --dired in ls call
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

; (set #'display-startup-echo-area-message #'ignore)
; (setq initial-scratch-message "foo") ; (figlet-preview-fonts)
; (add-hook 'after-init-hook (lambda () (figlet-preview-fonts)))

; (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
;   "Create parent directory if not exists while visiting file."
;   (unless (file-exists-p filename)
;     (let ((dir (file-name-directory filename)))
;       (unless (file-exists-p dir)
;         (make-directory dir t)))))
;                   __
; .-----.---.-.----|  |--.---.-.-----.-----.-----.
; |  _  |  _  |  __|    <|  _  |  _  |  -__|__ --|
; |   __|___._|____|__|__|___._|___  |_____|_____|
;-|__|-------------------------|_____|----------------------------------------

; (add-to-list 'load-path "/home/zooey/.emacs.d/packages")
(add-to-list 'load-path "~/.emacs.d/packages")

(load "exec-path-from-shell.el")
(load "ssh-to-host.el")
(global-set-key (kbd "C-c C-c") 'ssh-hotkey)
; I'm fine with having interactive functions as above; just know we can ssh-to-... from --funcall in cli.
; (keymap-global-set "C-c C-0" 'ssh-to-digitalocean-000)

(load "nyan-mode-1.1.3/nyan-mode-autoloads.el")
(load "sanityinc-tomorrow-night-theme.el")
; (load "color-theme-sanityinc-tomorrow.el")
; [ref] If you open emacs via Finder, your $PATH will be wrong & no fonts! 
(load "figlet.el") ; (figlet-get-font-list)
(global-set-key (kbd "M-t") 'figlet-figletify-region-comment)

; (load "fmt.el") ; todo - fitting paragraphs to ~60char width

; [note] requires https://github.com/arduino/arduino-cli / https://arduino.github.io/arduino-cli/
; (load "arduino-mode.el")
; (autoload 'arduino-mode "arduino-mode" "Major mode for editing Arduino code." t)
; (autoload 'ede-arduino-preferences-file "ede-arduino" "Preferences file of Arduino." t)
; (add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))
; (add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))

; (load "osascript.el") 
; (load "applescript-mode.el")
; (add-to-list 'auto-mode-alist '("\\.scpt\\'" . applescript-mode-mode))

; [ref] https://github.com/jacktasia/dumb-jump
; [ref] M-. to jump to function def
; (load "dumb-jump.el")
; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
; (require 'package)
; (setq package-list
;       '(
;         moe-theme
;         color-theme-sanityinc-tomorrow
;         nyan-mode
; 
;         ; web-mode
;         ; markdown-mode
;         ; python-mode
;         ; clojure-mode
; 
;         ; dumb-jump
;         ; arduino-mode
; ))
; (package-initialize)
; (unless package-archive-contents
;   (package-refresh-contents))
; ; If we don't have all the above packages, install them (but need to add melpa!)
; (dolist (package package-list)
;   (unless (package-installed-p package)
;     (package-install package)))


;  __                 __                        __
; |  |--.-----.--.--.|  |--.-----.---.-.----.--|  |
; |    <|  -__|  |  ||  _  |  _  |  _  |   _|  _  |
; |__|__|_____|___  ||_____|_____|___._|__| |_____|
;-------------|_____|----------------------------------------------------------
; [ref]: https://www.masteringemacs.org/article/mastering-key-bindings-emacs
; [ref] https://stackoverflow.com/questions/5682631/what-are-good-custom-keybindings-in-emacs

(global-set-key (kbd "C-z") nil) ; (suspend-frame), minimizes frame on macOS
(global-set-key (kbd "C-x C-z") nil) ; (suspend-frame), minimizes frame on macOS

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
; (global-set-key (kbd "C-x C-u") nil) ; (upcase-region)
; (global-set-key (kbd "C-x C-l") nil) ; (downcase-region)

; resettting this with figlet.
; (global-set-key (kbd "M-t") nil) ; (transpose-chars) ; swaps chars around, but I like figlet :^)
(global-set-key (kbd "C-T") nil) ; (transpose-chars) ; swaps chars around, but I like figlet :^)
(global-set-key (kbd "C-t") nil) ; (transpose-chars) ; swaps chars around, but I like figlet :^)

(global-set-key (kbd "C-q") nil) ; (quoted-insert), I think this puts quotes to M-x <here> ?
(global-set-key (kbd "C-q") 'eval-last-sexp) ; but I'm used to this now :^)

;[todo] Not sure if we can disable this. fold all headers in modes using outline-mode (org, markdown)
(global-set-key (kbd "<tab>") nil)
; (global-set-key (kbd "<S-tab>") nil)
; (global-set-key (kbd "<backtab>") nil) ; equivalent of above

; https://stackoverflow.com/questions/2627289/how-to-replace-a-region-in-emacs-with-yank-buffer-contents
; [note] This is probably different than how the original mark/point usage of emacs was intended, but this is nice for now
(delete-selection-mode 1)

(global-set-key (kbd "C-c C-c") nil)
;  _______
; |_     _|.----.---.-.--------.-----.
;   |   |  |   _|  _  |        |  _  |
;   |___|  |__| |___._|__|__|__|   __|
;------------------------------|__|--------------------------------------------
; [ref] Usage notes. Know ssh will take a while if your init.el is enormous
;    C-x C-f   /ssh:user@host:/Users/zooey/Documents/site
;    C-x C-f   /sudo:root@host[#port]:/path/to/file

; [TROUBLE??]I did unset the PS1 in .zshrc (here, idk) and the .bashrc on development..
; [TROUBLE??] If you run into more problems, check to see if PATH is changed in your bashrc or something

; [ref] https://stackoverflow.com/questions/3465567/how-to-use-ssh-and-sudo-together-with-tramp-in-emacs#4725727
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

; [ref] https://www.gnu.org/software/emacs/manual/html_node/tramp/Quick-Start-Guide.html
; [ref] https://emacs.stackexchange.com/questions/13797/tramp-dired-transfers-files-inline-over-ssh-instead-of-using-scp-externaly
; [ref] https://stackoverflow.com/questions/6954479/emacs-tramp-doesnt-work
; [ref] https://emacs.stackexchange.com/questions/21636/tramp-hangs-after-timeout-on-trying-to-connect

;         __          __
; .-----.|  |_.--.--.|  |.-----.
; |__ --||   _|  |  ||  ||  -__|
; |_____||____|___  ||__||_____|
;-------------|_____|----------------------------------------------------------

; (load-theme 'moe-dark t)
; (load-theme 'adwaita)
; (add-to-list 'load-path "~/Documents/code/emacs/nextstep/Emacs.app/Contents/Resources/etc/themes")
; (load-theme 'sanityinc-tomorrow-night t)
; (load "adwaita-theme.el")
; (require 'moe-theme)
; (setq moe-theme-highlight-buffer-id 1)
; (setq moe-theme-modeline-color 'cyan)

(nyan-mode t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(line-number-mode 1)
(column-number-mode 1)

; [ref] See font fullnames: (font-family-list)
; (set-face-attribute 'default nil :family "CtrlD" :height 130) ; height is 100 = 1*10pt, so 130 is 13pt
(set-face-attribute 'default nil :family "Essential PragmataPro" :height 135) ; height is 100 = 1*10pt, so 130 is 13pt
(setq default-frame-alist '((width . 140) (height . 120)))
; frame size / opacity
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

; Borders of window

(add-to-list 'default-frame-alist '(left-fringe . 4))
(add-to-list 'default-frame-alist '(right-fringe . 0)) ; aright side of window
(setq linum-format "%4d") ; "%4d \u2502 "  is  pipe to right side of 00 | on the left side
(fringe-mode '(3 . 0)) ; 6px border of gray60..?
(global-display-line-numbers-mode 1) ; <-- used to display fringe (w/ fringe-mode/linum-format
; far-left frame fringe lol
; (set-face-attribute 'fringe nil :background "white" :foreground nil)
;  __                 __
; |  |--.-----.-----.|  |--.-----.
; |     |  _  |  _  ||    <|__ --|
;-|__|__|_____|_____||__|__|_____|-------------------------------------------------------
; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary
; 2024-04-04: None of these work >_> Trying to figure out how to diff. tramp frames automatically
; (add-hook 'after-init-hook (lambda () (local-set-key [f1] 'sick-function-thing)))
; (defvar after-load-theme-hook nil
;   "... this might actually be firing? Hook run after a color theme is loaded using `load-theme'."
;   (if (not (eq 'user-init-file "/home/zooey/.emacs.d/init.el"))
;       (face-remap-add-relative 'default :background "darkgreen" :foreground "lightblue")
;     nil)
                                        ;   )
; Throwing errors on deployments:
; (add-hook 'after-make-frame-functions
; 	  (lambda (frame)
; 	    ; (set-variable 'color-theme-is-global nil)
; 	    (select-frame frame)
; 	    (if (string= (window-system) "ns")
; 		(color-theme-sanityinc-tomorrow-blue)
; 	      nil)))
; 
; (add-to-list 'load-path "~/.emacs.d/hooks")
; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

; [ref] Future usage help:
; (expand-file-name)
; (regexp-quote buffer-file-name)
; (defun turn-on-auto-fill-hook ()
;   (cond ((string-match "^/home/foo/bar/rawirousdijf/" buffer-file-name)
;          (auto-fill-mode 1))))


;                    __
; .--------.-----.--|  |.-----.
; |        |  _  |  _  ||  -__|
;-|__|__|__|_____|_____||_____|-------------------------------------------------
; (require 'shell-script-mode)

;  ~/Documents/code/emacs/nextstep/Emacs.app/Contents/Resources/lisp/progmodes/
(require 'js)
(require 'conf-mode)
(add-to-list 'auto-mode-alist  (cons (concat "^" (getenv "HOME") "/[.]zsh") 'shell-script-mode))

; Something's weird up with tramp regex stuff.. for now I think it has to all be evaluated manually:
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))
(add-to-list 'auto-mode-alist '("*\\.js" . js-mode))
(add-to-list 'auto-mode-alist '("*\\.jsx" . js-mode))
(add-to-list 'auto-mode-alist '("*\\.mjs" . js-mode))
(add-to-list 'auto-mode-alist '("\\.mjs" . js-mode))
(add-to-list 'auto-mode-alist '("*\\.cjs" . js-mode))
(add-to-list 'auto-mode-alist '("*\\.ts" . js-mode))

; (add-to-list 'auto-mode-alist '("\\.plist" . xml-mode))
(add-to-list 'auto-mode-alist '("/\\.bin*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("*\\.conf" . conf-mode))
(add-to-list 'auto-mode-alist '("/etc/nginx/*" . conf-mode))
; (require 'web-mode)
; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

; [ref] https://orgmode.org/worg/orgcard.html
; [ref] https://zzamboni.org/post/beautifying-org-mode-in-emacs/
; (setq org-hide-emphasis-markers t)
; (add-hook 'python-mode-hook
;   (function (lambda ()
;      (setq indent-tabs-mode nil tab-width 2))))

;  __                 __
; |  |--.-----.-----.|  |_.--.--.-----.
; |  _  |  _  |  _  ||   _|  |  |  _  |
; |_____|_____|_____||____|_____|   __|
;-------------------------------|__|-------------------------------------------

; (split-window-below true)
; (other-window 1 nil)
; [todo] https://github.com/lewang/command-log-mode/blob/master/command-log-mode.el
; (command-history)
; (view-lossage)

; [todo] https://www.masteringemacs.org/article/demystifying-emacs-window-manager
; (add-hook 'server-switch-hook #'raise-frame)

; [todo] https://stackoverflow.com/questions/10171280/how-to-launch-gui-emacs-from-command-line-in-osx
; [todo] https://emacs.stackexchange.com/questions/34737/start-emacsclient-with-focus-from-command-line

; (raise-frame)

;; ================================================================================
;; ================================================================================
;; WARNING
;; NONE OF THIS WILL BE SET IF YOU HAVE TO MANUALLY SET THEM ON
;; BOTH TRAMP and SUDOER EMACS SESSIONS!!!!!!!!!!!!!!!
;; ================================================================================
;;================================================================================
(custom-set-variables
 ; custom-set-variables was added by Custom.
 ; If you edit it by hand, you could mess it up, so be careful.
 ; Your init file should contain only one such instance.
 ; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(custom-safe-themes '(default))
 '(dired-listing-switches "-alFhs")
 '(electric-indent-mode nil)
 '(js-expr-indent-offset 2)
 '(js-indent-align-list-continuation t)
 '(js-indent-level 2)
 '(js-js-switch-tabs nil t)
 '(js-jsx-align->-with-< t)
 '(js-jsx-detect-syntax nil)
 '(js-jsx-syntax t)
 '(list-colors-sort '(hsv-dist . "gray100"))
 '(outline-minor-mode-cycle t)
 '(py-outline-minor-mode-p nil)
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")))
 '(standard-indent 2)
 '(tramp-verbose 6)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-auto-opening t)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 2))
