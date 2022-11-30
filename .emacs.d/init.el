
;;                                        __    _____         __ __
;; .-----.--------.---.-.----.-----.  .--|  |  /  /__|.-----.|__|  |_   .-----.
;; |  -__|        |  _  |  __|__ --|__|  _  |,' ,'|  ||     ||  |   _|__|  -__|
;; |_____|__|__|__|___._|____|_____|__|_____/__/  |__||__|__||__|____|__|_____|


;; * C-x <tab> <arrow-keys> lets you indent a region easer than C-x t


;; * M-x describe-variable <name> shows you all of the environment variables and info.
;; * C-h l shows you a log of (keystroke . command) executed in your buffer
;; * C-x a : 


;; init inspirations:
;; - https://github.com/Fanael/init.el/blob/master/init.el
;; 
;; packages to learn off of:
;; - https://repo.or.cz/ShellArchive.git/tree
;;
;; [ ] Put CtrlD in dot somehow, or git-annex
;; [ ] Upgrade eval-last-sexp to eval-region-or-last-sexp for C-q
;; [ ] Improve custom/figlet.el to allow unselected lines - 2022-09-04


;; elisp stuffs
;;   (message "foo")
;;   (insert "foo")
;;   (defcustom foo 0 "testing")
;;   (custom-set-variables '(foo 1))
;;   (setq foo 2)
;;   (customize-mark-as-set 'foo)
;;   (setq foo 3)
;;   (car (get 'foo 'standard-value))   ;; evaluates to 0
;;   (car (get 'foo 'saved-value))      ;; evaluates to 1
;;   (car (get 'foo 'customized-value)) ;; evaluates to 2
;;   foo                                ;; evaluates to 3
;;  
;;   (message (if (use-region-p)
;;                (string (region-beginning))
;;                (string (line-beginning-position))))
;;
;;    (add-hook 'after-init-hook (lambda () (append-to-file "after-init-hook\n" nil  "log.txt")))



;;         __               __
;; .-----.|  |_.---.-.----.|  |_.--.--.-----.
;; |__ --||   _|  _  |   _||   _|  |  |  _  |
;; |_____||____|___._|__|  |____|_____|   __|
;;                                    |__|
; (set #'display-startup-echo-area-message #'ignore)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
; (setq initial-scratch-message "foo") ; (figlet-preview-fonts)
; (add-hook 'after-init-hook (lambda () (figlet-preview-fonts)))
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(when (string= system-type "darwin")   ; macOS doesn't support --dired in ls call
  (setq dired-use-ls-dired nil))
;; saves cursor location in file on close for next open
(save-place-mode 1) ; saves cursor location in files / directory




;;  __                 __                        __
;; |  |--.-----.--.--.|  |--.-----.---.-.----.--|  |
;; |    <|  -__|  |  ||  _  |  _  |  _  |   _|  _  |
;; |__|__|_____|___  ||_____|_____|___._|__| |_____|
;;             |_____|

;; (suspend-frame), minimizes frame on macOS.
(global-set-key (kbd "C-z") nil)

;; (upcase-region), annoying
(global-set-key (kbd "C-x C-u") nil)

;; collapses all headers in markdown, annoying when shift-tab
(global-set-key (kbd "<S-tab>") nil)
(global-set-key (kbd "<backtab>") nil) ; equivalent of above

;; (transpose-chars) ; swaps chars around,"useless" (lol)
(global-set-key (kbd "C-t") nil)
(global-set-key (kbd "C-t") 'figlet-figletify-region-comment)

;; (quoted-insert), i think this puts quotes to M-x <here> ?
(global-set-key (kbd "C-q") nil)
(global-set-key (kbd "C-q") 'eval-last-sexp)

(setq-default indent-tabs-mode nil) ; indent will only insert spaces now
; what happens on <enter>, basically auto-indenting
(put 'set-goal-column 'disabled nil)



;;   ___
;; .'  _|.----.---.-.--------.-----.
;; |   _||   _|  _  |        |  -__|
;; |__|  |__| |___._|__|__|__|_____|
;;
(load-theme 'moe-light t)
(nyan-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; frame size / opacity
(set-face-attribute 'default nil :family "CtrlD" :height 130) ; height is 100 = 1*10pt, so 130 is 13pt
(setq default-frame-alist '((width . 110) (height . 70)))
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

;; linum and fringe
(global-linum-mode 1)
(setq linum-format "%4d") ; "%4d \u2502 "  is  pipe to right side of 00 | on the left side
; (add-to-list 'default-frame-alist '(left-fringe . 8))
; (add-to-list 'default-frame-alist '(right-fringe . 0)) ; right side of window
(fringe-mode '(3 . 0)) ; 6px border of gray60..?
 ; line separating fringe and content
; (set-face-attribute 'fringe nil :background "white" :foreground nil)


;;                   __
;; .-----.---.-.----|  |--.---.-.-----.-----.-----.
;; |  _  |  _  |  __|    <|  _  |  _  |  -__|__ --|
;; |   __|___._|____|__|__|___._|___  |_____|_____|
;; |__|                         |_____|
;;
(add-to-list 'load-path "~/.emacs.d/custom")
(load "figlet.el") ; (figlet-get-font-list) to see all fonts
; (load "indent-region-example.el")  ; look at a nice emacs-list example :^)
; (load "open-file-with-program.el") ; currently not working?

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-list
      '(
        ;; dumb-jump ; M-. to jump to thing def
        moe-theme
        color-theme-sanityinc-tomorrow
        nyan-mode        
        web-mode
        markdown-mode))

(package-initialize)
; fetch list of available packages
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
; M-. to jump to def, https://github.com/jacktasia/dumb-jump



;;                    __
;; .--------.-----.--|  |.-----.
;; |        |  _  |  _  ||  -__|
;; |__|__|__|_____|_____||_____|

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))


;; open-with (broken?)
;; https://github.com/garberw/openwith
;; if this one is still broken, look @ https://github.com/jpkotta/openwith/blob/master/openwith.el
;; Open files from dired with other applications
; (when (require 'openwith nil 'noerror)
;   (setq openwith-associations
;         (list
;          (list (openwith-make-extension-regexp
;                 '("mpg" "mpeg" "mp3" "mp4"
;                   "avi" "wmv" "wav" "mov" "flv"
;                   "ogm" "ogg" "mkv"))
;                "vlc"
;                '(file))
;          (list (openwith-make-extension-regexp
;                 '("xbm" "pbm" "pgm" "ppm" "pnm"
;                   "png" "gif" "bmp" "tif" "jpeg" "jpg"))
;                "Preview"
;                '(file))
;          (list (openwith-make-extension-regexp
;                 '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
;                "textedit"
;                '(file))
;          '("\\.lyx" "lyx" (file))
;          '("\\.chm" "kchmviewer" (file))
;          (list (openwith-make-extension-regexp
;                 '("pdf" "ps" "ps.gz" "dvi"))
;                "preview"
;                '(file))
;          ))
;   (openwith-mode 1))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("58c996beb973f7e988ee4fd21c367b7a5bbdb0622ddfbbd112672a7b4e3d3b81" "a68624bd5c4bec879ee59cd3039531b6229766a8b8ed0e79eef2642f14dbda32" "afeb7b07dbc1a4cfadb24f3ef6c8cf5e63051bf76411779f03a0fe3aadc07768" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default))
 '(electric-indent-mode nil)
 '(js-expr-indent-offset 2)
 '(js-indent-align-list-continuation t)
 '(js-indent-level 2)
 '(js-js-switch-tabs nil)
 '(js-jsx-align->-with-< t)
 '(js-jsx-detect-syntax nil)
 '(js-jsx-syntax t)
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
 )
(put 'downcase-region 'disabled nil)
