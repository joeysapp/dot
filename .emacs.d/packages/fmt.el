; https://stackoverflow.com/questions/43352006/split-lines-of-current-paragraph-in-emacs
(defun paragraph-to-lines ()
  "Format current paragraph into single lines."
  (interactive "*")
  (save-excursion
    (set-mark (point))
    (let (
          (start (point))
          (window-size (window-total-width))
          )
         (forward-paragraph)
         (let (
               (end (point))
               (length (abs (- (mark) (point))))
               )
            (exchange-point-and-mark)
            (if (> length window-size)
                (print "foo")
              (print "bar"))))))

(paragraph-to-lines)



              ;(backward-paragraph)
              (forward-char (window-total-width))
              ;; Replace all \n line breaks in paragraph with spaces. 
              (replace-regexp "\n" " " nil (1+ (point)) end)
      
              (backward-paragraph)
              (replace-regexp "\\. ?" ".\n" nil (point) end)))))


(count-words (point) 1)
(exchange-point-and-mark) ; turns on/off mark



   ;; https://old.reddit.com/r/emacs/comments/9c0a4d/tip_setting_initial_frame_size_and_position/ ; ;; Set initial frame size and position ; (defun my/set-initial-frame () ;   (let* ((base-factor 0.70) ;     (a-width (* (display-pixel-width) base-factor)) ;         (a-height (* (display-pixel-height) base-factor)) ;         (a-left (truncate (/ (- (display-pixel-width) a-width) 2))) ;     (a-top (truncate (/ (- (display-pixel-height) a-height) 2)))) ;     (set-frame-position (selected-frame) a-left a-top) ;     (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t))) ; (setq frame-resize-pixelwise t) ; (my/set-initial-frame) ;  ;     (defun set-frame-size-according-to-resolution () ;       (interactive) ;       (if window-system ;               (progn ;                     ;; Setting font size will affect frame size, so set font size first ;                     ;; But it still slightly affect the frame size even using this order ;                     ;; or Monaco, Bitstream Vera Sans Mono, Liberation Mono ;                     (if (> (x-display-pixel-width) 1500) ;                             (cond ;                              ((find-font (font-spec :name "PragmataPro")) ;                               (set-frame-font "PragmataPro-13.5")) ;                              ((find-font (font-spec :name "Input Mono Compressed")) ;                               (set-frame-font "Input Mono Compressed-13.5"))) ;                       (cond ;                        ((find-font (font-spec :name "PragmataPro")) ;                             (set-frame-font "PragmataPro-12")) ;                        ((find-font (font-spec :name "Input Mono Compressed")) ;                             (set-frame-font "Input Mono Compressed-12")))) ;                     (setq width-chars (/ (/ (x-display-pixel-width) (frame-char-width)) 2)) ;                     (setq height-lines (- (/ (x-display-pixel-height) (frame-char-height)) 4)) ;                     (setq default-frame-alist ;                               `((top . 0) (left . 0) ;                                     (width . ,width-chars) ;                                     (height . ,height-lines))) ;                     ))) ; (set-frame-size-according-to-resolution) ;  
