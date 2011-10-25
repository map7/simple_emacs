; Init.el
; Author: Michael Pope
; Desc:   Emacs v24 config file made for development (mostly Ruby on Rails)
;; Ref: http://itstickers.blogspot.com/2010/11/all-about-emacs.html
;; Ref: http://avdi.org/devblog/category/emacs-reboot/

(require 'package)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(setq required-packages
      (list 'highline 'apache-mode 'crontab-mode 'emms 'switch-window 'multi-term 'undo-tree 'rvm 'auto-complete 'yasnippet-bundle 'ruby-electric 'rinari 'inf-ruby 'coffee-mode 'yaml-mode 'feature-mode 'scss-mode 'haml-mode 'magit-simple-keys))
(dolist (package required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))


;; Setup external directory variable
(setq elisp-dir
      (expand-file-name "elisp" user-emacs-directory))
(setq elisp-external-dir
      (expand-file-name "external" elisp-dir))

;; Add external projects to load path
(add-to-list 'load-path elisp-external-dir)

(dolist (project (directory-files elisp-external-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))


; Keybinding
(global-set-key [f4] 'gist-region-or-buffer)
(global-set-key [f5] 'magit-status)
(global-set-key [f6] 'multi-term)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'next-multiframe-window)
(global-set-key [f12] 'switch-full-screen)

(global-set-key (kbd "C-x f") 'rinari-find-file-in-project)

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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

; Auto complete settings
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
    try-complete-file-name
    try-expand-dabbrev))

; -------------------- Custom Settings --------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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

;; https://github.com/remvee/emacs-rails
(require 'rails)			

;; Rinari
(add-to-list 'load-path "~/.emacs.d/rinari/")
(require 'rinari)
(add-hook 'ruby-mode-hook
          (lambda ()
            (defadvice ruby-mode-set-encoding
              (around ruby-mode-set-encoding-disable activate) nil)))


; -------------------- Rails Views -------------------- 
; haml-sass
(require 'haml-mode nil 't)
 (add-hook 'haml-mode-hook
  '(lambda () (setq tab-width 2)))
(setq scss-compile-at-save nil)

; stylesheets
(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))

; html2haml function to convert current buffer to haml
(defun haml-ify ()
  "run html2haml on current buffer"
  (interactive)
  (setf filename buffer-file-name)
  (setf newfilename (concat (car (split-string filename "\\.")) ".haml"))
  (save-buffer)
  (shell-command (concat "html2haml " filename " > " newfilename))
  (kill-buffer (current-buffer))
  (delete-file filename)
  (find-file newfilename))

;; -------------------- Rails Testing -------------------- 
;; Cucumber
(require 'feature-mode)

;; -------------------- Rails Tools -------------------- 
;; Check out abbrev-mode instead as it seems lighter.
(require 'snippet)

;; -------------------- Ruby plugins -------------------- 
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))

;; ;; Ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

;; Issues under some compiles of emacs
;; (require 'ruby-electric)
;; (add-hook 'ruby-mode-hook
;;           (lambda()
;;             (add-hook 'local-write-file-hooks
;;                       '(lambda()
;;                          (save-excursion
;;                            (untabify (point-min) (point-max))
;;                            ;(delete-trailing-whitespace)
;;                            )))
;;             (set (make-local-variable 'indent-tabs-mode) 'nil)
;;             (set (make-local-variable 'tab-width) 2)
;;             (imenu-add-to-menubar "IMENU")
;;             (require 'ruby-electric)
;;             (ruby-electric-mode t)))

;; Inferior Ruby Mode
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby" "" t)
(eval-after-load 'ruby-mode '(add-hook 'ruby-mode-hook 'inf-ruby-keys))

;; -------------------- SQL --------------------
(defun my-sql-interactive-mode-hook ()
  (setq tab-width 8))
(add-hook 'sql-interactive-mode-hook 'my-sql-interactive-mode-hook)
(require 'sql)
(put 'upcase-region 'disabled nil)


;; -------------------- Autocomplete --------------------
;; Use with Rsense for Ruby autocomplete:
;; http://cx4a.org/software/rsense/
;; Follow instructions on: http://itstickers.blogspot.com/2010/11/all-about-emacs.html
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "!/.emacs.d/ac-dict")
(ac-config-default)

;; Rsense
(setq rsense-home "/opt/rsense-0.3")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)
 
;; Rsense + Autocomplete
(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))


;; RVM in emacs
(require 'rvm)
(rvm-use-default) ;; use rvm’s default ruby for the current Emacs session

;; Switch windows easier when you have 3 or more.
(require 'switch-window)

;; gist
(require 'gist)
(setq gist-authentication-function 'gist-basic-authentication)

(setq the-secrets-file
      (expand-file-name "secrets.el" user-emacs-directory))
(when (file-exists-p the-secrets-file)
  (load the-secrets-file))

;; highline-mode
(require 'highline) 
(defun highline-mode-on ()
  (highline-mode 1)) 
;; Turn on local highlighting for Dired (C-x d) 
(add-hook 'dired-after-readin-hook #'highline-mode-on) 
;; Turn on local highlighting for list-buffers (C-x C-b) 
(defadvice list-buffers (after highlight-line activate) (save-excursion (set-buffer "*Buffer List*") (highline-mode-on)))
