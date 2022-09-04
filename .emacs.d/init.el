;;
;; ================================================================================
;;    .                     .         s                            ..
;;   @88>                  @88>      :8                      x .d88"
;;   %8P      u.    u.     %8P      .88                       5888R
;;    .     x@88k u@88c.    .      :888ooo             .u     '888R
;;  .@88u  ^"8888""8888"  .@88u  -*8888888          ud8888.    888R
;; ''888E`   8888  888R  ''888E`   8888           :888'8888.   888R
;;   888E    8888  888R    888E    8888           d888 '88%"   888R
;;   888E    8888  888R    888E    8888           8888.+"      888R
;;   888E    8888  888R    888E   .8888Lu=    .   8888L        888R
;;   888&   "*88*" 8888"   888&   ^%888*    .@8c  '8888c. .+  .888B .
;;   R888"    ""   'Y"     R888"    'Y"    '%888"  "88888%    ^*888%
;;    ""                    ""               ^*      "YP'       "%
;; ================================================================================
;;  __            __
;; |  |_.-----.--|  |.-----.
;; |   _|  _  |  _  ||  _  |
;; |____|_____|_____||_____|
;;
;; [ ] Improve custom/figlet.el to allow unselected lines - 2022-09-04
;;     notes/elisp stuff for that:
;;
;;           (message "foo")
;;           (insert "foo")
;;           (defcustom foo 0 "testing")
;;           (custom-set-variables '(foo 1))
;;           (setq foo 2)
;;           (customize-mark-as-set 'foo)
;;           (setq foo 3)
;;           (car (get 'foo 'standard-value))   ;; evaluates to 0
;;           (car (get 'foo 'saved-value))      ;; evaluates to 1
;;           (car (get 'foo 'customized-value)) ;; evaluates to 2
;;           foo                                ;; evaluates to 3
;;          
;;           (message (if (use-region-p)
;;                        (string (region-beginning))
;;                        (string (line-beginning-position))))
;;


;;               __
;; .-----.-----.|  |_.-----.-----.
;; |     |  _  ||   _|  -__|__ --|
;; |__|__|_____||____|_____|_____|
;;
;; * C-x <tab> <arrow-keys> lets you indent a region easer than C-x t
;;
;; cool init:          https://github.com/Fanael/init.el/blob/master/init.el
;; cool packages:      https://repo.or.cz/ShellArchive.git/tree


;;                  __            __   __
;; ?????? .--.--.--.---.-.|  |_  .----. |  |_|  |--.-----.-----.   ???
;; ?????? |  |  |  |  _  ||   _| |   _| |   _|     |  -__|__ --|   ???
;; ?????? |________|___._||____| |__|   |____|__|__|_____|_____|   ???
;; ================================================================================

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)



;;         __               __
;; .-----.|  |_.---.-.----.|  |_.--.--.-----.
;; |__ --||   _|  _  |   _||   _|  |  |  _  |
;; |_____||____|___._|__|  |____|_____|   __|
;;                                    |__|
(set #'display-startup-echo-area-message #'ignore)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
; (figlet-preview-fonts)

; (setq initial-scratch-message "foo")
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;;  __                 __                        __
;; |  |--.-----.--.--.|  |--.-----.---.-.----.--|  |
;; |    <|  -__|  |  ||  _  |  _  |  _  |   _|  _  |
;; |__|__|_____|___  ||_____|_____|___._|__| |_____|
;;             |_____|
(global-set-key (kbd "<S-tab>") nil) ; collapses all headers in markdown
; (global-set-key (kbd "<backtab>") nil)
(global-set-key (kbd "C-z") #'figlet-figletify-region-comment)
; (global-set-key (kbd "C-u z") #'figlet-figletify-region-comment) ; how to "prefix arg"?
(setq-default indent-tabs-mode -1) ; indent with spaces vs tab


;;   ___
;; .'  _|.----.---.-.--------.-----.
;; |   _||   _|  _  |        |  -__|
;; |__|  |__| |___._|__|__|__|_____|
;;
(nyan-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(set-face-attribute 'default nil :family "CtrlD" :height 130) ; height is 100 = 1*10pt, so 130 is 13pt
(setq default-frame-alist '((width . 120) (height . 45)))
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))
(setq-default global-linum-mode 1)
(setq linum-format "%3d") ; "%4d \u2502 "  is  pipe to right side of 00 | on the left side
(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-to-list 'default-frame-alist '(right-fringe . 0))
; (setq-default left-fringe-width 11) "set fringes globally"
; (setq-default right-fringe-width 0)


;;                   __
;; .-----.---.-.----|  |--.---.-.-----.-----.-----.
;; |  _  |  _  |  __|    <|  _  |  _  |  -__|__ --|
;; |   __|___._|____|__|__|___._|___  |_____|_____|
;; |__|                         |_____|
;;
(add-to-list 'load-path "~/.emacs.d/custom")
(load "figlet.el") ; (figlet-get-font-list) to see all fonts
(load "indent-region-example.el")  ; look at a nice emacs-list example :^)
(load "open-file-with-program.el") ; currently not working?
(eval-and-compile
  (require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
  (setq package-enable-at-startup nil)
  (setq package-native-compile t)
  (setq native-comp-async-report-warnings-errors nil)
  (setq async-bytecomp-allowed-packages nil)
  (package-initialize)

  ;; The below stuff isn't working, from https://github.com/Fanael/init.el/blob/master/init.el
  (defvar init-el-package-archives-refreshed nil)
  ; handle uninstalled packages
  (defun init-el-install-package (package-name)
    (unless (package-installed-p package-name)
      (unless init-el-package-archives-refreshed
        (package-refresh-contents)
        (setq init-el-package-archives-refreshed t))
      (package-install package-name)))

  (defmacro init-el-with-eval-after-load (feature &rest body)
    (declare (indent 1) (debug t))
    (require feature)
    `(with-eval-after-load ',feature ,@body))

  (defmacro init-el-require-package (package-name &optional feature-name)
    (init-el-install-package package-name)
    (require (or feature-name package-name))
    `(init-el-install-package ',package-name)))


;;               __          __     __          __ __ __
;; .-----.--.--.|  |_.-----.|__|.--|  |.-----. |  |__|  |--.-----.
;; |  _  |  |  ||   _|__ --||  ||  _  ||  -__| |  |  |  _  |__ --|
;; |_____|_____||____|_____||__||_____||_____| |__|__|_____|_____|
;;
;; saves cursor location in file on close for next open
;; (init-el-require-package 'save-place-mode)
;; (require 'save-place-mode)
;; (save-place-mode 1)

; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
; M-. to jump to def, https://github.com/jacktasia/dumb-jump



;;                    __
;; .--------.-----.--|  |.-----.-----.
;; |        |  _  |  _  ||  -__|__ --|
;; |__|__|__|_____|_____||_____|_____|

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-mode))


;; open-with (broken?)
;; https://github.com/garberw/openwith
;; if this one is still broken, look @ https://github.com/jpkotta/openwith/blob/master/openwith.el
;; Open files from dired with other applications
(when (require 'openwith nil 'noerror)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "vlc"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg" "jpg"))
               "Preview"
               '(file))
         (list (openwith-make-extension-regexp
                '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
               "textedit"
               '(file))
         '("\\.lyx" "lyx" (file))
         '("\\.chm" "kchmviewer" (file))
         (list (openwith-make-extension-regexp
                '("pdf" "ps" "ps.gz" "dvi"))
               "preview"
               '(file))
         ))
  (openwith-mode 1))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(moe-light))
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
   '('save-place-mode 'save-place-mode 'save-place-mode dumb-jump moe-theme color-theme-sanityinc-tomorrow nyan-mode web-mode markdown-mode cider clojure-mode))
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
 )
