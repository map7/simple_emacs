; Init.el
; Author: Michael Pope
; Desc:   Emacs v24 config file made for development (mostly Ruby on Rails)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(setq abg-required-packages
      (list 'yaml-mode 'haml-mode 'magit-simple-keys))
(dolist (package abg-required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))

; directory to put various el files into
(add-to-list 'load-path "~/.emacs.d/includes")
(add-to-list 'load-path "~/.emacs.d/includes/emacs-rails")
(add-to-list 'load-path "~/.emacs.d/includes/ruby-mode")
(add-to-list 'load-path "~/.emacs.d/includes/yasnippet")
(add-to-list 'load-path "~/.emacs.d/includes/egg") ; new git plugin
(add-to-list 'load-path "~/.emacs.d/includes/feature-mode")

; Keybinding
(global-set-key [f5] 'egg-status)
(global-set-key [f6] 'eshell)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'next-multiframe-window)
(global-set-key [f12] 'switch-full-screen)

;fullscreen mode
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;; Allow using mouse thumb button to browse URLs
(global-set-key [mouse-10] 'browse-url-at-mouse)

; stop emacs from contaminating each directory with semantic.cache
(setq semanticdb-default-save-directory "/tmp")

; General settings
(menu-bar-mode 1)         ;; enable the menu bar
(tool-bar-mode -1)        ; Disable tool-bar
(display-battery-mode)
(setq column-number-mode t)
(display-time)
(setq backup-inhibited t) ;; disable backup
(setq save-abbrevs t)

;; Org-mode options
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-files '("~/Dropbox/org/"))
(setq org-directory "~/Dropbox/org")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org");; new notes will be stored here 
(setq org-mobile-directory "~/Dropbox/MobileOrg")         ;; Set to <your Dropbox root directory>/MobileOrg.

;; Set color
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))



; Auto complete settings
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
    try-complete-file-name
    try-expand-dabbrev))


; -------------------- Custom Settings --------------------
(custom-set-variables
 '(ecb-options-version "2.32")
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/Dropbox/org")))
 '(rails-ws:default-server-type "mongrel")
 '(tooltip-mode nil))


; -------------------- File plugins --------------------
; Intelligent file opener
(ido-mode t)

; tramp - remote ssh editin
(setq tramp-default-method "ssh")

; -------------------- Rails setting files --------------------
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

; -------------------- Rails minor plugin -------------------- 
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(require 'rails)


; -------------------- Rails Views -------------------- 
; haml-sass
(require 'haml-mode nil 't)
 (add-hook 'haml-mode-hook
  '(lambda ()
     (setq tab-width 2)
     ))
(require 'sass-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

; stylesheets
(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))

; html2haml function to convert current buffer to haml
(defun haml-ify ()
  "run html2haml on current buffer"
  (interactive)
  (setf filename buffer-file-name)
  (setf newfilename (concat
		     (car (split-string filename "\\.")) ".haml"))
  (save-buffer)
  (shell-command (concat
		  "html2haml " filename " > " newfilename))
  (kill-buffer (current-buffer))
  (delete-file filename)
  (find-file newfilename))

; -------------------- Rails Testing -------------------- 
; rcov
(require 'rcov)

; Cucumber
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

; -------------------- Rails Tools -------------------- 
; Check out abbrev-mode instead as it seems lighter.
; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/includes/yasnippet/snippets/")

(require 'snippet)

; -------------------- Ruby plugins -------------------- 
; Taken from the comment section in inf-ruby.el
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

; Ruby electric
(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
;                           (delete-trailing-whitespace)
                           )))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "IMENU")
            (require 'ruby-electric)
            (ruby-electric-mode t)
            ))

; Inferior Ruby Mode
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
	     ))

; Ruby-block
(require 'ruby-block)
(ruby-block-mode t)


; -------------------- SQL --------------------
(defun my-sql-interactive-mode-hook ()
  (setq tab-width 8))
(add-hook 'sql-interactive-mode-hook 'my-sql-interactive-mode-hook)
(require 'sql)
(put 'upcase-region 'disabled nil)
