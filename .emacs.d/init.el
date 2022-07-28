;; todo(joeysapp): read over these:
; * https://github.com/Fanael/init.el/blob/master/init.el
; *


;; visual config
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; (set-face-attribute 'default nil :background "black"
;  :foreground "white" :font "Courier" :height 180)

;; Place all backup files (files~) in this location instead of in pwd
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; https://github.com/jacktasia/dumb-jump
;; M-. to jump to definitions
;; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

; save cursor in buffer
;(use-package saveplace
;  :init (save-place-mode))

(add-to-list 'load-path "~/.emacs.d/custom")
(load "figlet.el")

;(setq-default indent-tabs-mode 0)
;(setq-default global-linum-mode 1)

;; modes

;; https://melpa.org/#/js2-mode
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js-mode-hook 'js2-minor-mode)
;; You may also want to hook it in for shell scripts running via node.js:
;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-mode))

;; (setq web-mode-enable-auto-pairing t)
; (add-hook 'web-mode-hook
;           (lambda ()
; 	    (setq indent-tabs-mode nil)
;             ;; short circuit js mode and just do everything in jsx-mode
;             (if (equal web-mode-content-type "javascript")
;                 (web-mode-set-content-type "jsx")
;               (message "now set to: %s" web-mode-content-type))))
; 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(custom-safe-themes
   '("58c996beb973f7e988ee4fd21c367b7a5bbdb0622ddfbbd112672a7b4e3d3b81" "a68624bd5c4bec879ee59cd3039531b6229766a8b8ed0e79eef2642f14dbda32" "afeb7b07dbc1a4cfadb24f3ef6c8cf5e63051bf76411779f03a0fe3aadc07768" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default))
 '(electric-indent-mode nil)
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(js-expr-indent-offset 2)
 '(js-indent-align-list-continuation nil)
 '(js-indent-level 2)
 '(js-js-switch-tabs nil)
 '(js-jsx-align->-with-< nil)
 '(js-jsx-detect-syntax nil)
 '(package-selected-packages
   '(dumb-jump moe-theme color-theme-sanityinc-tomorrow nyan-mode web-mode markdown-mode cider clojure-mode))
 '(standard-indent 2)
 '(tool-bar-mode nil)
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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "CtrlD")))))
;; height is 100 = 1/10pt, so 140 is 14pt
