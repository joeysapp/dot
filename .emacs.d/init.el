;;                                        __    _____         __ __
;; .-----.--------.---.-.----.-----.  .--|  |  /  /__|.-----.|__|  |_   .-----.
;; |  -__|        |  _  |  __|__ --|__|  _  |,' ,'|  ||     ||  |   _|__|  -__|
;; |_____|__|__|__|___._|____|_____|__|_____/__/  |__||__|__||__|____|__|_____|
;; ----------------------------------------------------------------------
;; [init inspirations]
;; - https://github.com/Fanael/init.el/blob/master/init.el

;;         __               __
;; .-----.|  |_.---.-.----.|  |_.--.--.-----.
;; |__ --||   _|  _  |   _||   _|  |  |  _  |
;; |_____||____|___._|__|  |____|_____|   __|
;;                                    |__|
;; ----------------------------------------------------------------------
;; (eval 'load-path)

(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
; (set #'display-startup-echo-area-message #'ignore)
; (setq initial-scratch-message "foo") ; (figlet-preview-fonts)
; (add-hook 'after-init-hook (lambda () (figlet-preview-fonts)))

(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))) ; for when we do use above

 ; macOS doesn't support --dired in ls call
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(save-place-mode 1) ; saves cursor location in frames (e.g. files/dired)

(setq-default indent-tabs-mode nil) ; indent will only insert spaces now
(put 'set-goal-column 'disabled nil) ; what happens on <enter>, basically auto-indenting (?)

(show-paren-mode t)
(setq show-paren-style 'parenthesis) ; expression for entire highlight



;;  __                 __                        __
;; |  |--.-----.--.--.|  |--.-----.---.-.----.--|  |
;; |    <|  -__|  |  ||  _  |  _  |  _  |   _|  _  |
;; |__|__|_____|___  ||_____|_____|___._|__| |_____|
;;             |_____|
;; ----------------------------------------------------------------------
; [todo]: https://www.masteringemacs.org/article/mastering-key-bindings-emacs

(global-set-key (kbd "C-z") nil) ;; (suspend-frame), minimizes frame on macOS
(global-set-key (kbd "C-x C-z") nil) ;; (suspend-frame), minimizes frame on macOS

; (global-set-key (kbd "C-x C-u") nil) ;; (upcase-region)
; (global-set-key (kbd "C-x C-l") nil) ;; (downcase-region)
(put 'downcase-region 'disabled nil) ;; remove warnings for above keybinds

(global-set-key (kbd "C-t") nil) ;; (transpose-chars) ; swaps chars around, but I like figlet :^)

(global-set-key (kbd "C-q") nil) ;; (quoted-insert), I think this puts quotes to M-x <here> ?
(global-set-key (kbd "C-q") 'eval-last-sexp) ; but I'm used to this now :^)

;; [todo] I don't think this works; fold all headers in modes using outline-mode (org, markdown)
; (global-set-key (kbd "<S-tab>") nil)
; (global-set-key (kbd "<backtab>") nil) ; equivalent of above


;;         __          __
;; .-----.|  |_.--.--.|  |.-----.
;; |__ --||   _|  |  ||  ||  -__|
;; |_____||____|___  ||__||_____|
;;             |_____|
;; ----------------------------------------------------------------------
(load-theme 'moe-dark t)
;; Function/minibuf prompt has wrong face with this...
; (require 'moe-theme)
; (setq moe-theme-highlight-buffer-id 1)
; (setq moe-theme-modeline-color 'cyan)
; (moe-light)

(nyan-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; frame size / opacity
; (font-family-list)
; (set-face-attribute 'default nil :family "CtrlD" :height 130) ; height is 100 = 1*10pt, so 130 is 13pt
(set-face-attribute 'default nil :family "Essential PragmataPro" :height 135) ; height is 100 = 1*10pt, so 130 is 13pt
(setq default-frame-alist '((width . 110) (height . 70)))
(set-frame-parameter (selected-frame) 'alpha '(98 98))
(add-to-list 'default-frame-alist '(alpha 98 98))

;; Borders of window
;(global-linum-mode 1)
(global-display-line-numbers-mode 1)

(setq linum-format "%4d") ; "%4d \u2502 "  is  pipe to right side of 00 | on the left side
(fringe-mode '(3 . 0)) ; 6px border of gray60..?
; (add-to-list 'default-frame-alist '(left-fringe . 8))
; (add-to-list 'default-frame-alist '(right-fringe . 0)) ; right side of window
;; line separating fringe and content
; (set-face-attribute 'fringe nil :background "white" :foreground nil)

;;  __                 __
;; |  |--.-----.-----.|  |--.-----.
;; |     |  _  |  _  ||    <|__ --|
;; |__|__|_____|_____||__|__|_____|
;; ------------------------------------------------------------
;; (add-to-list 'load-path "~/.emacs.d/hooks")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

; [dot] for when I forget to add a shebang <_<
(add-to-list 'auto-mode-alist '("/\\.bin" . shell-script-mode))
;; [ref] Future usage help:
; (expand-file-name)
; (regexp-quote buffer-file-name)
; (defun turn-on-auto-fill-hook ()
;   (cond ((string-match "^/home/foo/bar/rawirousdijf/" buffer-file-name)
;          (auto-fill-mode 1))))

;;                   __
;; .-----.---.-.----|  |--.---.-.-----.-----.-----.
;; |  _  |  _  |  __|    <|  _  |  _  |  -__|__ --|
;; |   __|___._|____|__|__|___._|___  |_____|_____|
;; |__|                         |_____|
;;
;; ----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/packages")
(load "osascript.el") ; (osascript-eval-region), (osascript-eval-current-buffer) ; (osascript-run-file) a file
(load "applescript-mode.el")
(add-to-list 'auto-mode-alist '("\\.scpt\\'" . applescript-mode-mode))
;; If you open this via finder (e.g. not through a tty/shell session) your $PATH will not point
;; to where it normally does, and figlet won't work.
(load "figlet.el") ; (figlet-get-font-list)
(global-set-key (kbd "C-t") 'figlet-figletify-region-comment)

;; (load "fmt.el") ;; todo - fitting paragraphs to ~60char width

(load "arduino-mode.el")
; [note] requires https://github.com/arduino/arduino-cli / https://arduino.github.io/arduino-cli/
; (autoload 'arduino-mode "arduino-mode" "Major mode for editing Arduino code." t)
; (autoload 'ede-arduino-preferences-file "ede-arduino" "Preferences file of Arduino." t)
; (add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))
; (add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
; C-c C-c ; upload to arduino
; C-c C-v ; verify
; C-c C-m ; serial monitor
; C-c C-x ; open w arduino IDE

; (load "dumb-jump.el")
; [ref] https://github.com/jacktasia/dumb-jump
; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
; M-. to jump to function def

; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(require 'package)
(setq package-list
      '(
        moe-theme
        color-theme-sanityinc-tomorrow

        nyan-mode
        web-mode
        markdown-mode
        python-mode
        clojure-mode

        ;; dumb-jump
        ;; arduino-mode
        ))

;; Fetch list of available packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; If we don't have all the above packages, install them (but need to add melpa!)
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;                    __
;; .--------.-----.--|  |.-----.
;; |        |  _  |  _  ||  -__|
;; |__|__|__|_____|_____||_____|
;; ----------------------------------------------------------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))

;; plists
(add-to-list 'auto-mode-alist '("\\.plist\\'" . xml-mode))

;; org-mode
;; [ref] https://orgmode.org/worg/orgcard.html
;; [ref] https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(setq org-hide-emphasis-markers t)

(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))


;;                __                          __
;; .-----.-----. |  |.---.-.--.--.-----.----.|  |--.
;; |  _  |     | |  ||  _  |  |  |     |  __||     |
;; |_____|__|__| |__||___._|_____|__|__|____||__|__|
;; ============================================================
; [todo] https://www.masteringemacs.org/article/demystifying-emacs-window-manager

; (split-window-below true)
; (command-history)
; (other-window 1 nil)

; to read: https://github.com/lewang/command-log-mode/blob/master/command-log-mode.el
; (view-lossage)
; (command-history)


;; [todo] I am lazy
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("58c996beb973f7e988ee4fd21c367b7a5bbdb0622ddfbbd112672a7b4e3d3b81" "a68624bd5c4bec879ee59cd3039531b6229766a8b8ed0e79eef2642f14dbda32" "afeb7b07dbc1a4cfadb24f3ef6c8cf5e63051bf76411779f03a0fe3aadc07768" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default))
 '(electric-indent-mode nil)
 '(global-display-line-numbers-mode t)
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
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-auto-opening t)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-current-line ((t nil))))

(put 'upcase-region 'disabled nil)
