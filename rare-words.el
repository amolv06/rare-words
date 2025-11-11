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

(defcustom rare-words-common-word-cutoff 2500
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

(defcustom rare-words-search-forward-regex "[A-Za-z']+"
  "Regular expression used to define word patterns we want included."
  :type '(string))

(defvar-local rare-words--active-overlays nil
  "List containing all active overlays associated with the rare-words
package.")

(defun rare-words--identify-rare-word (db word)
  "Look up sqlite database DB for WORD and returns its rarity.

DB is expected to be a sqlite database file containing a table called
\"dictionary\" with a list of all strings identified as legitimate
words and their ranking in terms of frequency of use. Return values
can either be `rare', `semicommon' `common', or `unk' (for strings not
in the dictionary table). The ranking ranges for word classification
are defined by the custom variables. `rare-words-semi-common-word-cutoff' and `rare-words-common-word-cutoff'."

  (let* ((rank (or (cadar (sqlite-select db "select * from dictionary where word=?1" `(,word)))
		   'unknown)))
    (cond ((eq rank 'unknown)
	   'unk)
	  ((< rank rare-words-common-word-cutoff)
	   'common)
	  ((< rank rare-words-semi-common-word-cutoff)
	   'semicommon)
	  (t 'rare))))

(defun rare-words--next-word (&optional max)
  "Find the next word, until a `point' value given by MAX is reached.

What classifies as a word is given by the customizable variable
`rare-words-search-forward-regex'. Uses `re-search-forward' to find
the next word, so `match-beginning' and `match-end' are available for
subsequent use."

  (if (re-search-forward rare-words-search-forward-regex
			 (or max (point-max))
			 t)
      (current-word nil t)
    nil))

(defun rare-words--make-rare-word-overlay (min max rarity)
  (when (memq rarity '(rare semicommon))
    (let ((cur-overlay (make-overlay min max)))
      (push cur-overlay rare-words--active-overlays)
      (overlay-put cur-overlay 'face (cond ((eq rarity 'semicommon)
					    rare-words-semi-common-word-face)
					   ((eq rarity 'rare)
					    rare-words-rare-word-face)
					   (t nil))))))

(defun rare-words--error-checks ()
  "Ensure that package prerequisites are available.

Checks that sqlite functions are available in Emacs, that Sqlite is
installed on the system, and that we can find the frequency dictionary
given by `rare-words-dictionary'."
  
  (unless (sqlite-available-p)
    (error "When did you compile emacs bruh? 1983? You don't have SQLite support"))
  (unless (executable-find "sqlite3")
    (error "Bruh, do you even sqlite, bruh? apt get sqlite or something, bruh."))
  (unless (file-exists-p rare-words-dictionary)
    (error "Where's your dictionary, bruh? You think I'm Webster?")))

(defun rare-words--get-words-in-region-or-buffer (min max)
  (let ((word-list nil))
    (goto-char min)
    (while (< (point) max)
      (let* ((cur-word-no-downcase (rare-words--next-word max))
	     (cur-word (when cur-word-no-downcase (downcase cur-word-no-downcase))))
	(if cur-word
	    (push cur-word word-list)
	  (goto-char max))))
    (delete-dups word-list)))

(defun rare-words--get-word-frequency (db word-list)
  (sqlite-execute db "create temp table buf_words (word text primary key)")
  (dolist (w word-list)
    (sqlite-execute db "insert into buf_words values (?)" (list w)))
  (sqlite-select db "select a.* from dictionary as a inner join buf_words as b on a.word=b.word"))

(defun rare-words--plist-to-hash (plist)
  (let ((hash-table (make-hash-table :test 'equal)))
    (while plist
      (let ((key (car plist))
	    (value (cadr plist)))
	(puthash key value hash-table))
      (setq plist (cddr plist)))
    hash-table))		 

(defun rare-words-remove-overlays ()
  (interactive)
  (dolist (overlay rare-words--active-overlays)
    (delete-overlay overlay))
  (setq rare-words--active-overlays nil))

(defun rare-words-highlight ()
  (interactive)
  (rare-words-remove-overlays)
  (rare-words--error-checks)
  (let* ((highlight-zone-min (if (region-active-p)
 				 (region-beginning)
			       (point-min)))
	 (highlight-zone-max (if (region-active-p)
				 (region-end)
			       (point-max)))
	 (db (sqlite-open rare-words-dictionary))
	 (words-in-region (rare-words--get-words-in-region-or-buffer highlight-zone-min
								     highlight-zone-max))
	 (word-frequency-plist (flatten-list (rare-words--get-word-frequency db words-in-region)))
	 (word-frequency-hash (rare-words--plist-to-hash word-frequency-plist)))
    (goto-char highlight-zone-min)
    (while (< (point) highlight-zone-max)
      (let* ((cur-word-no-downcase (rare-words--next-word highlight-zone-max))
	     (cur-word (when cur-word-no-downcase (downcase cur-word-no-downcase))))
	(if cur-word
	    (rare-words--make-rare-word-overlay (match-beginning 0)
						(match-end 0)
						(let ((freq (gethash cur-word word-frequency-hash)))
						  (if freq
						      (cond ((< freq rare-words-common-word-cutoff)
							     'common)
							    ((< freq rare-words-semi-common-word-cutoff)
							     'semicommon)
							    (t 'rare))
						    'do-nothing)))
	  (goto-char highlight-zone-max))))))
