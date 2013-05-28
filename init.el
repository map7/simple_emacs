; Init.el
; Author: Michael Pope
; Desc:   Emacs v24 config file made for development (mostly Ruby on Rails)
;; Ref: http://itstickers.blogspot.com/2010/11/all-about-emacs.html
;; Ref: http://avdi.org/devblog/category/emacs-reboot/
;; This file is constantly tested with emacs-snapshot from the following repo:
;; https://launchpad.net/~cassou/+archive/emacs


;; packages not in marmalade
;; 'ruby-electric 'rinari - These are also updated regularly so I've put them into gitmodules

;; Have to get the latest haml-mode as 3.0.14 had bugs in the colouring.
;; 'haml-mode
;; Have to use a special version of haml-mode from:
;; https://github.com/dgutov/haml-mode 

(require 'package)
;; (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(setq required-packages
      (list 'find-file-in-project 'dired-details 'ace-jump-mode 'expand-region 'mwe-log-commands 'drag-stuff 'flymake-ruby 'flymake-haml 'regex-tool 'mic-paren 'highline 'android-mode 'css-mode 'csv-mode 'apache-mode 'crontab-mode 'switch-window 'multi-term 'undo-tree 'rvm 'auto-complete 'yasnippet-bundle 'inf-ruby 'coffee-mode 'yaml-mode 'feature-mode 'scss-mode 'magit))
(dolist (package required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set up load path
(add-to-list 'load-path dotfiles-dir)

;; setup ace jump mode
(define-key global-map (kbd "C-0") 'ace-jump-mode)
(require 'setup-ace-jump-mode)

;; Expand Region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


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

; General settings
(setq-default tab-width 4)
(menu-bar-mode 1)         ;; enable the menu bar
(tool-bar-mode -1)        ; Disable tool-bar
;; (display-battery-mode)
(setq column-number-mode t)
(display-time)
(setq backup-inhibited t) ;; disable backup


; Keybinding (Keyboard shortcuts)
;; (global-set-key [f1] 'twit)
(global-set-key [f2] 'gist-region-or-buffer)
(global-set-key [f3] 'switch-window)
(global-set-key [f4] 'magit-display-log)
(global-set-key [f5] 'magit-status)
(global-set-key [f6] 'multi-term)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'next-multiframe-window)
;; f9 is taken by git-status somewhere.
(global-set-key [f10] 'undo-tree-visualize)
(global-set-key [f12] 'switch-full-screen)



;
; Examples
; http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;
; C-<f12> = Use control function keys 
; s-h     = Use windows key (super), this example is super & h
;

; Create a little function to run publish mode in a shortcut
(defun puborg ()
  (interactive)
  (org-publish-project "org")
)

;; Using 's' for work computer and 'H' for notebook as there is a 
;; difference in mapping the super key.
(global-set-key (kbd "H-e") 'eval-buffer)
(global-set-key (kbd "s-e") 'eval-buffer)
(global-set-key (kbd "H-h") 'puborg)
(global-set-key (kbd "s-h") 'puborg)
(global-set-key (kbd "H-u") 'org-mobile-push)
(global-set-key (kbd "s-u") 'org-mobile-push)
(global-set-key (kbd "H-g") 'rinari-rgrep)
(global-set-key (kbd "s-g") 'rinari-rgrep)
(global-set-key (kbd "C-x f") 'rinari-find-file-in-project)
(global-set-key (kbd "H-RET") 'google-maps)
(global-set-key (kbd "s-RET") 'google-maps)

(global-set-key (kbd "C-c I") 'irc)

;; Speed up common functions 
(global-set-key (kbd "H-2") 'split-window-vertically)
(global-set-key (kbd "H-3") 'split-window-horizontally)
(global-set-key (kbd "H-i") 'org-clock-in)
(global-set-key (kbd "H-o") 'org-clock-out)

;; Speed up finding files
;; Function to create new functions that look for a specific pattern
(defun ffip-create-pattern-file-finder (&rest patterns)
  (lexical-let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

;; Find file in project, with specific patterns
(global-unset-key (kbd "C-x C-o"))
(global-set-key (kbd "C-x C-o ja")
                (ffip-create-pattern-file-finder "*.java"))
(global-set-key (kbd "C-x C-o js")
                (ffip-create-pattern-file-finder "*.js"))
(global-set-key (kbd "C-x C-o jp")
                (ffip-create-pattern-file-finder "*.jsp"))
(global-set-key (kbd "C-x C-o r")
                (ffip-create-pattern-file-finder "*.rb"))
(global-set-key (kbd "C-x C-o c")
                (ffip-create-pattern-file-finder "*.coffee"))
(global-set-key (kbd "C-x C-o s")
                (ffip-create-pattern-file-finder "*.scss"))

; Auto revert unless there is unsaved data
(global-auto-revert-mode t)

;fullscreen mode
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;; Allow using mouse thumb button to browse URLs
(global-set-key [mouse-10] 'browse-url-at-mouse)

; stop emacs from contaminating each directory with semantic.cache
(setq semanticdb-default-save-directory "/tmp")


;; Org-mode options
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(setq org-clock-out-remove-zero-time-clocks t)
;; Problem with this is I like to see the clock and how many hours I've spent on the last
;; item. Maybe if we could show the last 5 before putting the history into the drawer.
;; (setq org-clock-into-drawer "CLOCK")

;; (add-hook 'org-mode-hook 'my-org-mode-autosave-settings)
;; (defun my-org-mode-autosave-settings ()
;;   (set (make-local-variable 'auto-save-visited-file-name) t)
;;   (setq auto-save-interval 20))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-files '("~/org/"))
(setq org-directory "~/org")
(setq org-mobile-inbox-for-pull "~/org/inbox.org");; new notes will be stored here
(setq org-support-shift-select t)
(setq org-mobile-directory "~/MobileOrg")         ;; Set to <your ownCloud root directory>/MobileOrg.


;; Set more workflow states than TODO
(setq org-todo-keywords
	  '((sequence "TODO(t)" "|" "DONE(d)" "DELEGATED(>)" )
		(sequence "GONNA(g)" "|" "DONE(d)" )))

(setq org-support-shift-select t)

;; Put email links in org mode :) - currently broken :(
;; (setq ffap-url-regexp (replace-regexp-in-string "mailto:" "thunderlink: \ \ \ \ | mailto:" ffap-url-regexp));; for ThunderLink

;;  (defun browse-url-thunderlink (url & optional new-window)
;;    (interactive (browse-url-interactive-arg "URL:"))
;;    (if (string-match "^ thunderlink ://" url)
;;        (progn
;;          (start-process (concat "thunderbird" url) nil "thunderbird" "-thunderlink" url)
;;          t)
;;      nil)
;;    )
;; (unless (listp browse-url-browser-function) (setq browse-url-browser-function (list (cons "." browse-url-browser-function))))
;; (add-to-list 'browse-url-browser-function' ("^ thunderlink:". browse-url-thunderlink))

;; (add-hook 'org-load-hook
;;             '(lambda ()
;;                (add-to-list 'org-link-types "thunderlink")
;;                (org-make-link-regexps)
;;                (add-hook 'org-open-link-functions' browse-url-thunderlink)
;;                ))


;; mobile org options
;; http://kenmankoff.com/2012/08/17/emacs-org-mode-and-mobileorg-auto-sync
;;

;; Automatically push changes
(defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

(defun org-mobile-push-with-delay (secs) 
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'org-mobile-push)))

;; TODO - put this into the background somehow.
;; (add-hook 'after-save-hook 
;;  (lambda () 
;;    (when (eq major-mode 'org-mode)
;;      (dolist (file (org-mobile-files-alist))
;;        (if (string= (expand-file-name (car file)) (buffer-file-name))
;;            (org-mobile-push-with-delay 10)))
;;    )))

;; This runs directly after you start emacs, it takes too long.
;; (run-at-time "00:05" 86400 '(lambda () (org-mobile-push-with-delay 1))) ;; refreshes agenda file each day

;; org publish options
(require 'org-publish)
(setq org-publish-project-alist
      '(
		;; ... add all the components here (see below)...

		;; All org files (notes)
		("org-notes"
		 :base-directory "~/org/"
		 :base-extension "org"
		 :publishing-directory "~/org_html/"
		 :style "<link rel=\"stylesheet\" href=\"css/stylesheet.css\" type=\"text/css\" />"
		 :recursive t
		 :publishing-function org-publish-org-to-html
		 :headline-levels 4             ; Just the default for this project.
		 :auto-preamble t
		 )

		;; Attachments
		("org-static"
		 :base-directory "~/org/"
		 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|htaccess"
		 :publishing-directory "~/org_html/"
		 :recursive t
		 :publishing-function org-publish-attachment
		 )
		
		;; Publish component
		("org" :components ("org-notes" "org-static"))

      ))



;; Set color
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

;; Auto complete settings / tab settings
;; http://emacsblog.org/2007/03/12/tab-completion-everywhere/ <-- in the comments
(global-set-key [(tab)] 'smart-tab)
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (dabbrev-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
		  (let ((yas/fallback-behavior nil))
			(unless (yas/expand)
			  (dabbrev-expand nil)))
		(indent-for-tab-command)))))

; -------------------- Custom Settings --------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(ecb-options-version "2.32")
 '(inhibit-startup-screen t)
 '(max-lisp-eval-depth 3000)
 '(max-specpdl-size 3000)
 '(org-archive-location "~/org/archive/%s_archive::")
 '(rails-ws:default-server-type "mongrel")
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.googlemail.com")
 '(smtpmail-smtp-service "smtp")
 '(tool-bar-mode nil)
 '(tooltip-mode nil))

; -------------------- File plugins --------------------
; Interactively Do Things
(ido-mode t)							

; tramp - remote ssh editing
(require 'tramp)
(setq tramp-default-method "ssh")

; -------------------- Rails setting files --------------------
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

; -------------------- Rails minor plugin -------------------- 
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; https://github.com/remvee/emacs-rails
;; Currently this interfers with auto complete, using rinari instead
;; automatically adds end to blocks.
(require 'rails)			;; Remarked out due to incompatibility with autocomplete

;; Rinari - Rails plugin
(add-to-list 'load-path "~/.emacs.d/rinari/")
(require 'rinari)
(add-hook 'ruby-mode-hook
          (lambda ()
            (defadvice ruby-mode-set-encoding
              (around ruby-mode-set-encoding-disable activate) nil)))

; When Rinari mode is loaded, add extra bindings to its key map enabling the
; use of M-R and M-r as its prefixes. The easier to type M-r is used for the
; frequently used `find' functions and M-R is used for the more infrequently
; used rake/script/web-server commands. M-r is normally bound to
; `move-to-window-line-top-bottom', which I can do without, and M-R is
; normally unbound. Examples of the changes below; it should be obvious how to
; pick your own prefixes if you don't like mine.
;
; Function                  Default Binding    New Binding
; ========                  ===============    ===========
; rinari-find-controller    C-c ; f c          M-r c
; rinari-web-server         C-c ; w            M-R w
(add-hook 'rinari-minor-mode-hook (lambda ()
	(define-prefix-command 'pd-rinari-map1)
	(define-prefix-command 'pd-rinari-map2)
	(local-set-key (kbd "M-R") 'pd-rinari-map1)
	(local-set-key (kbd "M-r") 'pd-rinari-map2)

	(define-key pd-rinari-map1 "'" 'rinari-find-by-context)
	(define-key pd-rinari-map1 ";" 'rinari-find-by-context)
	(define-key pd-rinari-map1 "c" 'rinari-console)
	(define-key pd-rinari-map1 "d" 'rinari-cap)
	(define-key pd-rinari-map1 "e" 'rinari-insert-erb-skeleton)
	(define-key pd-rinari-map1 "g" 'rinari-rgrep)
	(define-key pd-rinari-map1 "p" 'rinari-goto-partial)
	(define-key pd-rinari-map1 "q" 'rinari-sql)
	(define-key pd-rinari-map1 "r" 'rinari-rake)
	(define-key pd-rinari-map1 "s" 'rinari-script)
	(define-key pd-rinari-map1 "t" 'rinari-test)
	(define-key pd-rinari-map1 "w" 'rinari-web-server)
	(define-key pd-rinari-map1 "x" 'rinari-extract-partial)

	(define-key pd-rinari-map2 ";" 'rinari-find-by-context)
	(define-key pd-rinari-map2 "C" 'rinari-find-cells)
	(define-key pd-rinari-map2 "F" 'rinari-find-features)
	(define-key pd-rinari-map2 "M" 'rinari-find-mailer)
	(define-key pd-rinari-map2 "S" 'rinari-find-steps)
	(define-key pd-rinari-map2 "Y" 'rinari-find-sass)
	(define-key pd-rinari-map2 "a" 'rinari-find-application)
	(define-key pd-rinari-map2 "c" 'rinari-find-controller)
	(define-key pd-rinari-map2 "e" 'rinari-find-environment)
	(define-key pd-rinari-map2 "f" 'rinari-find-file-in-project)
	(define-key pd-rinari-map2 "h" 'rinari-find-helper)
	(define-key pd-rinari-map2 "i" 'rinari-find-migration)
	(define-key pd-rinari-map2 "j" 'rinari-find-javascript)
	(define-key pd-rinari-map2 "l" 'rinari-find-lib)
	(define-key pd-rinari-map2 "m" 'rinari-find-model)
	(define-key pd-rinari-map2 "n" 'rinari-find-configuration)
	(define-key pd-rinari-map2 "o" 'rinari-find-log)
	(define-key pd-rinari-map2 "p" 'rinari-find-public)
	(define-key pd-rinari-map2 "r" 'rinari-find-rspec)
	(define-key pd-rinari-map2 "s" 'rinari-find-script)
	(define-key pd-rinari-map2 "t" 'rinari-find-test)
	(define-key pd-rinari-map2 "u" 'rinari-find-plugin)
	(define-key pd-rinari-map2 "v" 'rinari-find-view)
	(define-key pd-rinari-map2 "w" 'rinari-find-worker)
	(define-key pd-rinari-map2 "x" 'rinari-find-fixture)
	(define-key pd-rinari-map2 "y" 'rinari-find-stylesheet)
	(define-key pd-rinari-map2 "z" 'rinari-find-rspec-fixture)
	))



;; ; -------------------- Rails Views -------------------- 
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
  (setf newfilename (concat filename ".haml"))
  (save-buffer)
  (shell-command (concat
    "html2haml " filename " > " newfilename))
  (kill-buffer (current-buffer))
  (delete-file filename)
  (find-file newfilename))
;; -------------------- Rails Testing -------------------- 
;; Cucumber
(require 'feature-mode)

;; -------------------- Rails Tools -------------------- 
;; Check out abbrev-mode instead as it seems lighter.
;; Doesn't exist:
;; (require 'snippet)

;; yasnippet
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"            ;; personal snippet
		))

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/yasnippets-rails")

;; -------------------- Ruby plugins -------------------- 
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("god" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.eco\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.hamlc\\'" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))

;; Ruby-electric
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
;; Run a ruby process in a buffer
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
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
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

;; Complete by C-c .
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c .") 'rsense-complete)))

;; RVM in emacs
;; (require 'rvm)
;; (rvm-use-default) ;; use rvm’s default ruby for the current Emacs session

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
(defadvice list-buffers (after highlight-line activate) (with-current-buffer (set-buffer "*Buffer List*") (highline-mode-on)))

;; mic-paren - advanced highlighting of matching parentheses
(paren-activate)

;; flyspell
(add-hook 'org-mode-hook
	  (lambda ()
	    (flyspell-mode)
	    (setq flyspell-issue-message-flag 'nil)))
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)
	    (setq flyspell-issue-message-flag 'nil)))

;; flymake haml
(add-hook 'haml-mode-hook 'flymake-haml-load)


;; undo
(add-hook 'ruby-mode-hook 'undo-tree-mode)

;; multi-term
;; (setq term-default-fg-color "#aaa")	

;; Upgrade all packages
(defun package-update-all ()
  "Update all packages"
  (interactive)
  (dolist (elt package-alist)
    (let* ((name (car elt))
           (file-name (symbol-name name))
           (available-pkg (assq name package-archive-contents))
           (available-version (and available-pkg
                                   (package-desc-vers (cdr available-pkg))))
           (current-version (package-desc-vers (cdr elt)))
           )
      (when (and available-version
                 (version-list-< current-version available-version))
        (message "Updating to: %s - %s" file-name
                 (package-version-join available-version))
        (package-install name)
        (package-delete file-name (package-version-join current-version))))))


;; Music player
(add-to-list 'load-path "~/.emacs.d/elisp/external/bongo")
(setq bongo-global-lastfm-mode t)
(autoload 'bongo "bongo"
  "Start Bongo by switching to a Bongo buffer." t)

;; Volume manager (Use 'v' in bongo music player)
(add-to-list 'load-path "~/.emacs.d/elisp/external/volume-el")
(autoload 'volume "volume"
   "Tweak your sound card volume." t)

;; Twitter mode
(add-to-list 'load-path "~/.emacs.d/elisp/external/twitter-mode")
(require 'twittering-mode)
(setq twittering-icon-mode t)  
(setq twittering-timer-interval 40) 
(setq twittering-url-show-status nil) 
(add-hook 'twittering-edit-mode-hook (lambda () (ispell-minor-mode) (flyspell-mode)))
(setq twittering-use-master-password t) ;; Don't prompt for authorisation. 

;; drag-stuff
(require 'drag-stuff)

;; coffee-mode
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))

;; (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
;; (define-key coffee-mode-map [(meta R)] 'coffee-compile-region)

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; Don't ask to save abbrevs
(setq save-abbrevs 'silently)


;; log commands.
;; M-x mwe:open-command-log-buffer
(add-hook 'LaTeX-mode-hook (function mwe:log-keyboard-commands))

;; screensaver
(setq zone-when-idle t)


;; IRC reconnect
(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
	    (port (process-contact process :service))
	    (nick (rcirc-nick process))
	    channels query-buffers)
       (dolist (buf (buffer-list))
	 (with-current-buffer buf
	   (when (eq process (rcirc-buffer-process))
	     (remove-hook 'change-major-mode-hook
			  'rcirc-change-major-mode-hook)
	     (if (rcirc-channel-p rcirc-target)
		 (setq channels (cons rcirc-target channels))
	       (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
		      rcirc-default-user-name
		      rcirc-default-full-name
		      channels))))

;; Video editor
;; (load "~/.emacs.d/elisp/external/gneve.el")


;; Google maps
(require 'google-maps)
(require 'org-location-google-maps)

;; mark-multiple
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

;; ;; Move around windows easier
;; (when (fboundp 'windmove-default-keybindings)
;;       (windmove-default-keybindings))

;; Make dired less verbose
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; rotate windows
(global-set-key (kbd "C-<tab>") 'rotate-windows)
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

;; jinja2 - used in ansible templates
(require 'jinja2-mode)
