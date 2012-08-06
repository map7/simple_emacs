;;; gneve.el - GNU Emacs video editor mode for editing video Edit Decision List or EDL

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Martin Howse (m AT 1010 DOT co DOT uk)
;; Maintainer: Martin Howse (m AT 1010 DOT co DOT uk)
;; Committers: Arnold Matyasi (arn AT webma DOT hu)
;;             Gabor Torok (tgabor AT webma DOT hu)
;; URL: http://www.1010.co.uk/gneve.html
;; Compatibility: Emacs20, Emacs21, Emacs22, Emacs23

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Use: 
;;     1. M-x gneve-mode RET to invoke in gneve-mode
;;        C-x C-w to save *GNEVE* buffer as videname.edl for later usage
;;     2. C-x C-f any .edl file to open it in gneve-mode
;;     3. C-h m to visit gneve mode help

;;; Install:

;; Put something similar to the following in your ~/.emacs to use this file:
;;
;; (load "~/path/to/gneve.el")
;;

;;; Dependency:

;; gneve.el now only requires MLT framework (subversion) 
;; Miracle server running on port 5250

;;; TODO:

; 0] render options
; 1] Add support for transitions
; 2] Subtitle editor built on same foundations
; 3] Fix SDL/X11 and size of clip
; 4] Support for org-mode linking to re-write and place here

;;; Code:

(defvar gneve-mode-map nil
  "local keymap for gneve")

(setq gneve-mode-map nil)

(defconst number-regexp
  "-?\\([0-9]+\\.?\\|\\.\\)[0-9]*\\(e[0-9]+\\)?"
  "Regular expression for recognizing numbers.")

(defconst vslot-regexp 
  "^\\([0-9]+\\):"
  "Regexp for video slot number")

(defvar gneve-buffer "*GNEVE*" "gneve-buffer")
(defvar vslots nil "video slot file names list")
(defvar vslot-n nil "video slot number")
(defvar vslot nil "active video slot")
(defvar filename nil "filename")
(defvar lastin nil "lastin")
(defvar lastout nil "lastout")
(defvar start nil "start of timecode mark")
(defvar end nil "end of timecode mark")
(defvar videoname nil "rendered videofile name")
(defvar timecode-string nil "timecode string")
(defvar gn-connection nil "connection to miracle server")

(setq gn-connection nil)

;; Keyboard shortcuts definition

(if gneve-mode-map
  nil
  (setq gneve-mode-map (make-sparse-keymap))

;; Video operation
  (define-key gneve-mode-map "V" 'open-film)
  (define-key gneve-mode-map "L" 'pause)
  (define-key gneve-mode-map "M" 'play)
  (define-key gneve-mode-map "J" 'prev-frame)
  (define-key gneve-mode-map "K" 'next-frame)
  (define-key gneve-mode-map "Q" 'one-sec-back)
  (define-key gneve-mode-map "W" 'one-sec-forward)
  (define-key gneve-mode-map "A" 'five-sec-back)
  (define-key gneve-mode-map "S" 'five-sec-forward)



;; Mark operation
  (define-key gneve-mode-map "E" 'mark-start)
  (define-key gneve-mode-map "R" 'mark-end)
  (define-key gneve-mode-map "H" 'write-marks)
  (define-key gneve-mode-map "Z" 'goto-start)
  (define-key gneve-mode-map "X" 'goto-end)
  (define-key gneve-mode-map "C" 'goto-point)
  (define-key gneve-mode-map "G" 'goto-timecode)

  ;; Render operation
  (define-key gneve-mode-map "U" 'region-render)
  (define-key gneve-mode-map "I" 'buffer-render)
  (define-key gneve-mode-map "O" 'save-rendered)
  (define-key gneve-mode-map "P" 'play-rendered)
  (define-key gneve-mode-map "[" 'preview))

(defun gneve-mode()
  "EDL and mplayer based GNU Emacs video editing mode

Video commands:
  V - Visit video file and start playing
  L - Pause
  M - Play
  J - 1 frame back and pause
  K - 1 frame forward and pause
  C - Goto timecode of current point
  G - Goto timecode of user input

  Layout summary.

  Q W 
   A S     G H J K L
          V     M

Editing commands:
  E - Mark start of a section
  R - Mark end of a section
  H - Write marked section to EDL buffer
  Z - Goto start of marked section and pause
  X - Goto end of marked section and pause

  Layout summary:
      E R 

    Z X C

Render commands:
  U - Render active region
  I - Render whole buffer
  O - Save rendered video
  P - Play rendered video
  [ - preview video (to-be-rendered)

  Layout summary:  
              U I O P ["
  (interactive)
  (kill-all-local-variables)
  (defvar vslot-p nil "vslot predicate")
  (setq vslot-p nil)
  (if (string-match "\\.edl" (buffer-name))
      (setq gneve-buffer (buffer-name))
    (if (not (member (get-buffer gneve-buffer) (buffer-list)))
        (setq vslot-p t)))
  (pop-to-buffer gneve-buffer nil)
  (if vslot-p
      (insert "(setq vslots '( ))\n\n"))
  (goto-char (point-min))
  (eval-region 0 (search-forward "))"))
  (forward-char 2)
  (setq major-mode 'gneve-mode)
  (setq mode-name "gneve")
  (use-local-map gneve-mode-map))

(defun vslot-pos (arg list)
  "video slot postion"
  (cond 
   ((endp list) nil)
   ((equal arg (car list)) (length (cdr list)))
   (t (vslot-pos arg (cdr list)))))

(defun open-film (filename)
  (interactive "ffilename:")
  (when (not (member (expand-file-name filename) vslots))
    (add-to-list 'vslots (expand-file-name filename) t)
    (switch-to-buffer gneve-buffer)
    (goto-char (point-min))
    (search-forward "))")
    (backward-char 2)
    (insert (format "\n\"%s\"" (expand-file-name filename)))
    (forward-char 2))
  (setq vslot-n (vslot-pos (expand-file-name filename) (reverse vslots)))
;; if buffer doesn't already exist
   (when (not gn-connection)
 (setq gn-connection (make-comint "boo" (cons "127.0.0.1" 5250)))
 (process-send-string gn-connection "UADD sdl\n")
 (process-send-string gn-connection "UADD avformat\n"))
   (process-send-string gn-connection (format "LOAD U%d %s\n" vslot-n (expand-file-name filename)))
 (process-send-string gn-connection (format "PLAY U%d\n" vslot-n))
  (print (expand-file-name filename)))

(defun next-frame ()
  (interactive)
   (process-send-string gn-connection (format "STEP U%d 1\n" vslot-n)))

(defun prev-frame ()
  (interactive)
   (process-send-string gn-connection (format "STEP U%d -1\n" vslot-n)))

(defun pause ()
  (interactive)
   (process-send-string gn-connection (format "PAUSE U%d\n" vslot-n)))

(defun play ()
  (interactive)
   (process-send-string gn-connection (format "PLAY U%d\n" vslot-n)))

(defun query-film ()
  (interactive)
   (process-send-string gn-connection (format "USTA U%d\n" vslot-n)))

(defun write-marks ()
  "write video slot, lastin, lastout time code into EDL buffer"
  (interactive)
  (switch-to-buffer gneve-buffer)
  (insert (format "%d:%s %s\n" vslot-n lastin lastout)))

(defun one-sec-back ()
  (interactive)
   (process-send-string gn-connection (format "STEP U%d -25\n" vslot-n)))

(defun one-sec-forward ()
  (interactive)
   (process-send-string gn-connection (format "STEP U%d 25\n" vslot-n)))

(defun five-sec-back ()
  (interactive)
   (process-send-string gn-connection (format "STEP U%d -125\n" vslot-n)))

(defun five-sec-forward ()
  (interactive)
   (process-send-string gn-connection (format "STEP U%d 125\n" vslot-n)))

(defun mark-start ()
  (interactive)
  (setq lastin (marker))
  (message "edit points %s %s\n" lastin lastout))

(defun mark-end ()
  (interactive)
  (setq lastout (marker))
  (message "edit points %s %s\n" lastin lastout))

(defun marker ()
  ;; copies latest mark to boo. copy and paste only to variable - new function write to buffer
  (interactive)
  (progn
    ;; goto end of buffer search back to equals and copy to last-in
    (query-film)
    (set-buffer "*boo*")
    (sleep-for 0.1)
    (goto-char (point-max))
    (previous-line)
    (re-search-forward "\"")
    (re-search-forward "\"")
    ;; search for last number and return this
    (re-search-forward number-regexp)
    (goto-char (match-end 0)))
  (buffer-substring (match-beginning 0) (point)))

(defun goto-point ()
  (interactive)  
  (setq timecode-string (read (current-buffer)))
   (process-send-string gn-connection (format "GOTO U%d %s\n" vslot-n timecode-string)))

(defun tc-human ()
  "calculate human readable timecode (hh:mm:ss,ms)"
  (setq tc-hour (/ (floor (string-to-number timecode-string)) 9000))
  (setq tc-min (/ (- (floor (string-to-number timecode-string)) (* 9000 tc-hour))  1500))
  (setq tc-sec (- (string-to-number timecode-string) (* 1500 tc-min) (* 9000 tc-hour)))
  (setq tc-msec (truncate (* 1000 (- tc-sec (floor tc-sec))))))


(defun goto-timecode ()
  "goto user input timecode"
  (interactive)
  (defvar timecode-newpos nil "new timecode position string")
  (setq timecode-string (marker))
  (tc-human)
  (setq timecode-newpos  (read-string "Goto timecode (min:sec) " (format "%d:%.2f" tc-min tc-sec ) nil nil))
  (setq tc-min (car (split-string timecode-newpos ":")))
  (setq tc-sec (cadr (split-string timecode-newpos ":")))
  (setq timecode-string (number-to-string (+ (* 1500 (string-to-number tc-min)) (* 25 (string-to-number tc-sec)))))
  (process-send-string gn-connection (format "GOTO U%d %s\n" vslot-n timecode-string)))

(defun goto-start ()
  "goto mark start"
  (interactive)
  (process-send-string gn-connection (format "GOTO U%d %s\n" vslot-n lastin)))

(defun goto-end ()
  "goto mark end"
  (interactive)
  (process-send-string gn-connection (format "GOTO U%d %s\n" vslot-n lastout)))

(defun buffer-render ()
  "render whole buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (narrow-to-region (+ (search-forward  "))") 2) (point-max)) 
      (render)
      (post-render))))

(defun region-render ()
  "render only active region"
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (render)
      (post-render))))

(defun save-rendered ()
  "save rendered video file"
  (interactive)
  (setq videoname (read-file-name "Save rendered video: " default-directory nil nil))
  (if (file-exists-p videoname)
  (or (y-or-n-p (format "Video `%s' already exists; overwrite? " videoname))
		 (error "Canceled")))
  (message videoname)
  (shell-command (concat "time cp -f /tmp/test.dv " (expand-file-name videoname))))

(defun preview ()
       (interactive)
  ;; write XML
  (render)
  ;; play with inigo editor
  (start-process "non-process3" nil "inigo" "/tmp/basic.westley"))

(defun play-rendered ()
  "play rendered video file"
  (interactive)
  (start-process "non-process3" nil "mplayer" "-vo" "x11" "-sub" "/tmp/test.srt" "-quiet" "/tmp/test.dv"))

(defun render()
  "render edl to westley/openMLT XML"
  (interactive)
  (defvar startframe nil "start frame")
  (defvar endframe nil "end frame")
  (defvar subcounter 1 "subtitle counter")
  (defvar subtitle nil "subtitle string")
  (setq subcounter 1)

;; counts number of edits
  (let ((old-point (point))
        (counter 0)
	(x 0))
    (goto-char (point-min))
    (while
	(re-search-forward "^[0-9]" nil t)
      (setq counter (1+ counter)))
    (goto-char old-point)
    (message "%d" counter)

    ;; header
  (switch-to-buffer (find-file-noselect "/tmp/basic.westley")) ;; should be blank
  (erase-buffer)
  (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<westley root=\"/tmp\" title=\"TMP\">\n")
  ;; producers
  (let ((xxx 0))
    (dolist (i vslots)
      (insert (format "  <producer id=\"producer%d\">\n" xxx))
      (insert (format "    <property name=\"resource\">%s</property>\n" i))
      (insert "  </producer>\n")
      (setq xxx (+ xxx 1))))

  (insert "  <playlist id=\"playlist0\">\n")

;; EDL
    (switch-to-buffer gneve-buffer)
    (goto-char (point-min))
    (while (< x counter)
      (vslot-matcher)
      (setq startframe (timecode-matcher))
      (forward-char 1)
      (setq x (1+ x))
      (setq endframe (timecode-matcher))
      (switch-to-buffer  "basic.westley")
      (insert (format  "    <entry producer=\"producer%s\" in=\"%s\" out=\"%s\"/>\n" vslot startframe endframe)) ;; vslot number???
      (switch-to-buffer gneve-buffer)
      (forward-line 1)
      (beginning-of-line))
    ;; write footer

      (switch-to-buffer "basic.westley")
      (goto-char (point-max))
      (insert "  </playlist>\n</westley>\n")

      ;; save and close temp file
      (save-buffer "/tmp/basic.westley")
      (switch-to-buffer gneve-buffer)
      (goto-char old-point)))

(defun post-render ()
  ;; render the xml to .... dv or ....?
  ;; using inigo

  (start-process "non-process3" nil "inigo" "/tmp/basic.westley" "-consumer" "avformat:/tmp/test.dv"))

(defun timecode-matcher () ;; fix to search forwards
  (let (timecode-string)
    (re-search-forward number-regexp)
	(goto-char (match-end 0))
    (setq timecode-string (buffer-substring (match-beginning 0) (point)))
    (string-to-number timecode-string)))

(defun vslot-matcher ()
  (re-search-forward vslot-regexp)
      (goto-char (match-end 0))
  (setq vslot (buffer-substring (match-beginning 1) (- (point) 1))))

;;; List functions

;;;###autoload(add-to-list 'auto-mode-alist '("\\.edl\\'" . gneve-mode))
(provide 'gneve-mode)
