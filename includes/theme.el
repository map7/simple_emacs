;;; -*- Mode: Emacs-Lisp -*-
;;; my color theme and other fancy stuff
;;;

(require 'color-theme)

(defun my-color-theme-dark ()
  (interactive)
  ;; main theme
  (color-theme-install
   '(my-color-theme-dark
     ((background-color . "#161616")
      (foreground-color . "#D3D7CF")
      (cursor-color . "white")
      (mouse-color . "white")
      (background-mode . dark))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (italic ((t (:italic t))))
     (region ((t (:background "#004BAD"))))
     ;; orange: #E67321
     ;; purple: #8722C9 #9D62BB
     ;; green: #00B200
     ;; leaf: #65A63A
     ;; blue: #398EE6
     ;; chalk: #D3D7CF
     (font-lock-builtin-face ((t (:foreground "#E67321"))))
     (font-lock-comment-face ((t (:foreground "#9D62BB"))))
     (font-lock-constant-face ((t (:foreground "#D3D7CF"))))
     (font-lock-doc-string-face ((t (:bold t :foreground "#6ABF14"))))
     (font-lock-function-name-face ((t (:foreground "white"))))
     (font-lock-keyword-face ((t (:foreground "#E67321"))))
     (font-lock-preprocessor-face ((t (:foreground "#8722C9"))))
     (font-lock-reference-face ((t (:foreground "red3"))))
     (font-lock-string-face ((t (:foreground "#65A63A"))))
     (font-lock-type-face ((t (:foreground "#D3D7CF"))))
     (font-lock-variable-name-face ((t (:foreground "#D3D7CF"))))
     (font-lock-warning-face ((t (:bold t :foreground "red"))))
     (py-builtins-face ((t (:foreground "#D3D7CF"))))
     (py-pseudo-keyword-face ((t (:foreground "#398EE6"))))
     (rst-level-1-face ((t (:bold t :foreground "snow1"))))
     (rst-level-2-face ((t (:bold t :foreground "snow2"))))
     (rst-level-3-face ((t (:bold t :foreground "snow3"))))
     (rst-level-4-face ((t (:bold t :foreground "snow4"))))
     (erc-action-face ((t (nil))))
     (erc-notice-face ((t (:foreground "#878899"))))
     (erc-bold-face ((t (:bold t :weight bold))))
     (erc-command-indicator-face ((t (:bold t :weight bold))))
     (erc-dangerous-host-face ((t (:foreground "red"))))
     (erc-default-face ((t (nil))))
     (erc-timestamp-face ((t (:bold nil :foreground "gray45" :weight normal))))
     (erc-underline-face ((t (:underline t))))
     (erc-prompt-face ((t (:bold t :foreground "GoldenRod3" :weight bold))))
     (trailing-whitespace ((t (:background "gray30")))))))

(defun my-color-theme-light ()
  (interactive)
  ;; main theme
  (color-theme-install
   '(my-color-theme-light
     ((background-color . "white")
      (foreground-color . "black")
      (cursor-color . "black")
      (mouse-color . "white")
      (background-mode . light))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (italic ((t (:italic t))))
     (region ((t (:background "moccasin"))))
     ;; orange: #E67321
     ;; purple: #8722c9
     ;; green: #00b200
     ;; blue: #398EE6
     (font-lock-builtin-face ((t (:bold t :foreground "#E67321"))))
     (font-lock-comment-face ((t (:italic t :bold t :foreground "#8722c9"))))
     (font-lock-constant-face ((t (:bold t :foreground "#398EE6"))))
     (font-lock-doc-string-face ((t (:bold t :foreground "#51B200"))))
     (font-lock-function-name-face ((t (:bold t))))
     (font-lock-keyword-face ((t (:bold t :foreground "#E67321"))))
     (font-lock-preprocessor-face ((t (:foreground "#8722c9" :bold t))))
     (font-lock-reference-face ((t (:foreground "red3"))))
     (font-lock-string-face ((t (:bold t :foreground "#51B200"))))
     (font-lock-type-face ((t (:bold t :foreground "#398EE6"))))
     (font-lock-variable-name-face ((t (:italic t :bold t :foreground "magenta3"))))
     (font-lock-warning-face ((t (:bold t :foreground "red"))))
     (py-builtins-face ((t (:bold t :foreground "#398EE6"))))
     (py-pseudo-keyword-face ((t (:bold t :foreground "#398EE6"))))
     (rst-level-1-face ((t (:bold t :foreground "snow1"))))
     (rst-level-2-face ((t (:bold t :foreground "snow2"))))
     (rst-level-3-face ((t (:bold t :foreground "snow3"))))
     (rst-level-4-face ((t (:bold t :foreground "snow4"))))
     (erc-action-face ((t (nil))))
     (erc-notice-face ((t (:foreground "#878899"))))
     (erc-bold-face ((t (:bold t :weight bold))))
     (erc-command-indicator-face ((t (:bold t :weight bold))))
     (erc-dangerous-host-face ((t (:foreground "red"))))
     (erc-default-face ((t (nil))))
     (erc-timestamp-face ((t (:bold nil :foreground "gray45" :weight normal))))
     (erc-underline-face ((t (:underline t))))
     (erc-prompt-face ((t (:bold t :foreground "GoldenRod3" :weight bold))))
     (trailing-whitespace ((t (:background "gray90")))))))

(defun my-color-theme-dull ()
  (interactive)
  ;; main theme
  (color-theme-install
   '(my-color-theme-light
     ((background-color . "white")
      (foreground-color . "black")
      (cursor-color . "black")
      (mouse-color . "white")
      (background-mode . light))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (italic ((t (:italic t))))
     (region ((t (:background "moccasin"))))
     ;; red earth: #A73232
     ;; purple: #8722c9
     ;; light green: #228C00
     ;; teal: #008080
     (font-lock-builtin-face ((t (:bold t :foreground "#A73232"))))
     (font-lock-comment-face ((t (:foreground "#8722c9"))))
     (font-lock-constant-face ((t (:bold t :foreground "#398EE6"))))
     (font-lock-doc-string-face ((t (:bold t :foreground "#51B200"))))
     (font-lock-function-name-face ((t (:foreground "black"))))
     (font-lock-keyword-face ((t (:bold t :foreground "#A73232"))))
     (font-lock-preprocessor-face ((t (:foreground "#8722c9" :bold t))))
     (font-lock-reference-face ((t (:foreground "red3"))))
     (font-lock-string-face ((t (:foreground "#228C00"))))
     (font-lock-type-face ((t (:bold t :foreground "#008080"))))
     (font-lock-variable-name-face ((t (:foreground "black"))))
     (font-lock-warning-face ((t (:bold t :foreground "red"))))
     (py-builtins-face ((t (:bold t :foreground "#398EE6"))))
     (py-pseudo-keyword-face ((t (:bold t :foreground "#398EE6"))))
     (rst-level-1-face ((t (:bold t :foreground "snow1"))))
     (rst-level-2-face ((t (:bold t :foreground "snow2"))))
     (rst-level-3-face ((t (:bold t :foreground "snow3"))))
     (rst-level-4-face ((t (:bold t :foreground "snow4"))))
     (erc-action-face ((t (nil))))
     (erc-notice-face ((t (:foreground "#878899"))))
     (erc-bold-face ((t (:bold t :weight bold))))
     (erc-command-indicator-face ((t (:bold t :weight bold))))
     (erc-dangerous-host-face ((t (:foreground "red"))))
     (erc-default-face ((t (nil))))
     (erc-timestamp-face ((t (:bold nil :foreground "gray45" :weight normal))))
     (erc-underline-face ((t (:underline t))))
     (erc-prompt-face ((t (:bold t :foreground "GoldenRod3" :weight bold))))
     (trailing-whitespace ((t (:background "red")))))))

(defun my-color-theme-gold ()
  (interactive)
  ;; main theme
  (color-theme-install
   '(my-color-theme-light
     ((background-color . "#CCCB88")
      (foreground-color . "black")
      (cursor-color . "black")
      (mouse-color . "white")
      (background-mode . light))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (italic ((t (:italic t))))
     ;; red earth: #A73232
     ;; purple: #8722c9
     ;; light green: #228C00
     ;; teal: #008080
     ;; light cyan: #EAFFFF
     (font-lock-builtin-face ((t (:bold t :foreground "#A73232"))))
     (font-lock-comment-face ((t (:foreground "#8722c9"))))
     (font-lock-constant-face ((t (:bold t :foreground "#398EE6"))))
     (font-lock-doc-string-face ((t (:bold t :foreground "#51B200"))))
     (font-lock-function-name-face ((t (:foreground "black"))))
     (font-lock-keyword-face ((t (:bold t :foreground "#A73232"))))
     (font-lock-preprocessor-face ((t (:foreground "#8722c9" :bold t))))
     (font-lock-reference-face ((t (:foreground "red3"))))
     (font-lock-string-face ((t (:foreground "#228C00"))))
     (font-lock-type-face ((t (:bold t :foreground "#008080"))))
     (font-lock-variable-name-face ((t (:foreground "black"))))
     (font-lock-warning-face ((t (:bold t :foreground "red"))))
     (py-builtins-face ((t (:bold t :foreground "#398EE6"))))
     (py-pseudo-keyword-face ((t (:bold t :foreground "#398EE6"))))
     (rst-level-1-face ((t (:bold t :foreground "snow1"))))
     (rst-level-2-face ((t (:bold t :foreground "snow2"))))
     (rst-level-3-face ((t (:bold t :foreground "snow3"))))
     (rst-level-4-face ((t (:bold t :foreground "snow4"))))
     (erc-action-face ((t (nil))))
     (erc-notice-face ((t (:foreground "#878899"))))
     (erc-bold-face ((t (:bold t :weight bold))))
     (erc-command-indicator-face ((t (:bold t :weight bold))))
     (erc-dangerous-host-face ((t (:foreground "red"))))
     (erc-default-face ((t (nil))))
     (erc-timestamp-face ((t (:bold nil :foreground "gray45" :weight normal))))
     (erc-underline-face ((t (:underline t))))
     (erc-prompt-face ((t (:bold t :foreground "GoldenRod3" :weight bold))))
     (trailing-whitespace ((t (:background "red")))))))

(defun my-color-theme-acme ()
  (interactive)
  ;; main theme
  (color-theme-install
   '(my-color-theme-light
     ((background-color . "#FFFFEA")
      (foreground-color . "black")
      (cursor-color . "black")
      (mouse-color . "white")
      (background-mode . light))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (italic ((t (:italic t))))
     ;; red earth: #A73232
     ;; purple: #8722c9
     ;; light green: #228C00
     ;; teal: #008080
     ;; light cyan: #EAFFFF
     (font-lock-builtin-face ((t (:bold t :foreground "#A73232"))))
     (font-lock-comment-face ((t (:foreground "#8722c9"))))
     (font-lock-constant-face ((t (:bold t :foreground "#398EE6"))))
     (font-lock-doc-string-face ((t (:bold t :foreground "#51B200"))))
     (font-lock-function-name-face ((t (:foreground "black"))))
     (font-lock-keyword-face ((t (:bold t :foreground "#A73232"))))
     (font-lock-preprocessor-face ((t (:foreground "#8722c9" :bold t))))
     (font-lock-reference-face ((t (:foreground "red3"))))
     (font-lock-string-face ((t (:foreground "#228C00"))))
     (font-lock-type-face ((t (:bold t :foreground "#008080"))))
     (font-lock-variable-name-face ((t (:foreground "black"))))
     (font-lock-warning-face ((t (:bold t :foreground "red"))))
     (py-builtins-face ((t (:bold t :foreground "#398EE6"))))
     (py-pseudo-keyword-face ((t (:bold t :foreground "#398EE6"))))
     (rst-level-1-face ((t (:bold t :foreground "snow1"))))
     (rst-level-2-face ((t (:bold t :foreground "snow2"))))
     (rst-level-3-face ((t (:bold t :foreground "snow3"))))
     (rst-level-4-face ((t (:bold t :foreground "snow4"))))
     (erc-action-face ((t (nil))))
     (erc-notice-face ((t (:foreground "#878899"))))
     (erc-bold-face ((t (:bold t :weight bold))))
     (erc-command-indicator-face ((t (:bold t :weight bold))))
     (erc-dangerous-host-face ((t (:foreground "red"))))
     (erc-default-face ((t (nil))))
     (erc-timestamp-face ((t (:bold nil :foreground "gray45" :weight normal))))
     (erc-underline-face ((t (:underline t))))
     (erc-prompt-face ((t (:bold t :foreground "GoldenRod3" :weight bold))))
     (trailing-whitespace ((t (:background "red")))))))


;; the frame setup for all my computers
(defun setup-frame-for (name height width)
  (if (equal system-name name)
      (progn
        (set-frame-height (selected-frame) height)
        (set-frame-width (selected-frame) width))))

(defun frame-setup (list)
  (when window-system
    (dolist (conf list)
      (setup-frame-for (car conf) (cadr conf) (caddr conf)))))

(frame-setup
 '(("helios"  64 84) ;; my desktop
   ))
