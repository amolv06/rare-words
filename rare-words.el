;;; rare-words.el --- Find rare words in your prose.

;; Copyright (C) 2025 Amol Vaidya

;; Author: Amol Vaidya  
;; Version: 20240608.1444
;; Keywords: zathura, theming
;; URL: https://github.com/amolv06/zathura-sync-theme
;;; Commentary:

;; This package is inspired by the blog post at
;; https://blog.akaisuisei.org/communicating-with-zathura-via-dbus.html
;; written by mafty.

(defgroup rare-words nil
  "Customization group for rare-words.el.")

(defcustom rare-words-common-word-cutoff 1000
  "The number of most frequently used words to be considered common."
  :type '(integer :min 1 :max 100000))

(defcustom rare-words-semi-common-word-cutoff 4486
  "The number of most frequently used words to be considered semi-common."
  :type '(integer :min 1 :max 100000))

(defcustom rare-words-dictionary (expand-file-name "words.db"
						   (file-name-directory buffer-file-name))
  "Sqlite db file containing a list of words and the rankings for the most
common ones.")

(defcustom rare-words-semi-common-word-face 'warning
  "The overlay face used for semicommon words"
  :type '(face))

(defcustom rare-words-rare-word-face 'error
  "The overlay face used for rare words"
  :type '(face))

(defvar-local rare-words--active-overlays nil
  "List containing all active overlays associated with the rare-words
package.")

(defun rare-words--identify-rare-word (db word)
  (let ((rank (or (cadar (sqlite-select db (format "select * from dictionary where word='%s'" word)))
		  9999999)))
    (cond ((< rank rare-words-common-word-cutoff)
	   'common)
	  ((< rank rare-words-semi-common-word-cutoff)
	   'semicommon)
	  (t 'rare))))

(defun rare-words--next-word (&optional max)
  (interactive)
  (if (re-search-forward "[A-Za-z']+"
			 (or max (point-max))
			 t)
      (current-word nil t)
    nil))

(defun rare-words--make-rare-word-overlay (min max rarity)
  (let ((cur-overlay (make-overlay min max)))
  (push cur-overlay rare-words--active-overlays)
  (overlay-put cur-overlay 'face (cond ((eq rarity 'semicommon)
					rare-words-semi-common-word-face)
				       ((eq rarity 'rare)
					rare-words-rare-word-face)
				       (t nil)))))

(defun rare-words--error-checks ()
  (unless (sqlite-available-p)
    (error "When did you compile emacs bruh? 1983? You don't have SQLite support"))
  (unless (executable-find "sqlite3")
    (error "Bruh, do you even sqlite, bruh? apt get sqlite or something, bruh."))
  (unless (file-exists-p rare-words-dictionary)
    (error "Where's your dictionary, bruh? You think I'm Webster?")))
    
(defun rare-words-highlight ()
  (interactive)
  (rare-words--error-checks)
  (let ((highlight-zone-min (if (region-active-p)
				(region-beginning)
			      (point-min)))
	(highlight-zone-max (if (region-active-p)
				(region-end)
			      (point-max)))
	(db (sqlite-open rare-words-dictionary)))
    (goto-char highlight-zone-min)
    (while (< (point) highlight-zone-max)
      (let* ((cur-word-no-downcase (rare-words--next-word highlight-zone-max))
	     (cur-word (when cur-word-no-downcase (downcase cur-word-no-downcase)))
	     (cur-word-rarity (rare-words--identify-rare-word db cur-word)))
	(unless cur-word (goto-char highlight-zone-max))
	(when (memq cur-word-rarity '(rare semicommon))
	  (rare-words--make-rare-word-overlay (match-beginning 0)
					      (match-end 0)
					      cur-word-rarity))))))

(defun rare-words-remove-overlays ()
  (interactive)
  (dolist (overlay rare-words--active-overlays)
    (delete-overlay overlay))
  (setq rare-words--active-overlays nil))
               
