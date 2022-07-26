;; todo(joeysapp): read over these:
; * https://github.com/Fanael/init.el/blob/master/init.el
; *


;; visual config
(load-theme 'kaolin-valley-light t) ; t is 'NO-CONFIRM' flag
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
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

; save cursor in buffer
(use-package saveplace
  :init (save-place-mode))

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
 '(custom-safe-themes
   '("5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" default))
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
   '(kaolin-themes dumb-jump nyan-mode use-package web-mode markdown-mode cider clojure-mode))
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
 '(default ((t (:inherit nil :stipple nil :background "#fdfde7" :foreground "#5f5f5f" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Fira Code")))))

;; height is 100 = 1/10pt, so 140 is 14pt
