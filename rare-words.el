;;; rare-words.el --- Highlight your rare words!  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Amol Vaidya (BigEatie)

;; Author: Amol Vaidya <amolvaidya.signup@gmail.com>
;; Created: 4 December 2025
;; URL: https://github.com/amolv06/rare-words
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary: 

;; This package looks at the active region, or if no region is active,
;; then the entire buffer, and highlights any rare or semicommon words
;; it finds within.

;; Rare and semicommon words are defined by the sqlite words database
;; that comes along with this package.

;;; Code:

(defgroup rare-words nil
  "Customization group for the rare-words package."
  :group 'emacs)

(defcustom rare-words-common-word-cutoff 2500
  "Threshold below which all words are considered common.

If the rank of a word is between `rare-words-common-word-cutoff' and
`rare-words-semi-common-word-cutoff' then the word is considered
semicommon."
  :type '(natnum)
  :group 'rare-words)

(defcustom rare-words-semi-common-word-cutoff 4510
  "Threshold above which all words are considered rare.

If the rank of a word is between `rare-words-common-word-cutoff' and
`rare-words-semi-common-word-cutoff' then the word is considered
semicommon."
  :type '(natnum)
  :group 'rare-words)

(defcustom rare-words-dictionary (expand-file-name "words.db"
						   (file-name-directory (or load-file-name
									    (buffer-file-name))))
  "A SQLite database containing word frequency rankings.

It must contain a table named dictionary, with columns word (text) and
rank (integer). Only words present in this dictionary are eligible to
be marked as semicommon or rare."
  :group 'rare-words)

(defcustom rare-words-semi-common-word-face 'warning
  "The face used to highlight semicommon words."
  :type '(face)
  :group 'rare-words)

(defcustom rare-words-rare-word-face 'error
  "The face used to highlight rare words."
  :type '(face)
  :group 'rare-words)

(defcustom rare-words-search-forward-regex "[A-Za-z']+"
  "Regular expression delineating a word for the rare-words package."
  :group 'rare-words)

(defvar-local rare-words--overlay-list nil
  "Local list of overlays associated with the rare-words package."
 'rare-words)

(defun rare-words-kill-highlights ()
  "Deletes local overlay list associated with rare-words package."
  (interactive)
  (dolist (o rare-words--overlay-list)
    (delete-overlay o))
  (setq rare-words--overlay-list nil))

(defun rare-words--get-word-frequency (word-list)
  "Get rank of words in `WORD-LIST' from `rare-words-dictionary'."
  
  (let ((db (sqlite-open rare-words-dictionary)))
    (sqlite-execute db "create temporary table buf_words (word text)")
    (dolist (w word-list)
      (sqlite-execute db "insert into buf_words values (?)" `(,w)))
    (let ((results (sqlite-select db (format "select a.* from dictionary as a inner join buf_words as b on a.word=b.word"))))
      (sqlite-close db)
      results)))

(defun rare-words--get-next-word (&optional max)
  "Return the next word from point based on `rare-words-search-forward-regex'."
  (interactive)
  (if (re-search-forward rare-words-search-forward-regex
			 (or max (point-max))
			 t)
      (current-word nil t)
    nil))

(defun rare-words--get-words-in-region-or-buffer (min max)
  "Return a list of all words in between positions MIN and MAX."
  (let ((word-list nil))
    (goto-char min)
    (while (< (point) max)
      (let ((word (rare-words--get-next-word max)))
	(if word
	    (push (downcase word) word-list)
	  (goto-char max))))
    word-list))

(defun rare-words--make-rare-word-overlay (rarity)
  "Based on RARITY, create an overlay for the previously matched word."
  (when (memq rarity '(rare semicommon))
    (let ((cur-overlay (make-overlay (match-beginning 0) (match-end 0))))
      (push cur-overlay rare-words--overlay-list)
      (overlay-put cur-overlay 'face (cond ((eq rarity 'semicommon) rare-words-semi-common-word-face)
					   ((eq rarity 'rare) rare-words-rare-word-face))))))
					   
      
(defun rare-words-highlight ()
  "Highlight all rare and semicommon words in buffer or active region.

If a region is active, it will only highlight words within that
region, otherwise highlights words in the entire buffer."

  (interactive)
  (save-excursion
    (rare-words-kill-highlights)
    (let* ((highlight-zone-min (if (region-active-p)
				   (region-beginning)
				 (point-min)))
	   (highlight-zone-max (if (region-active-p)
				   (region-end)
				 (point-max)))
	   (word-list (rare-words--get-words-in-region-or-buffer highlight-zone-min highlight-zone-max))
	   (word-frequency (rare-words--get-word-frequency word-list))
	   (word-frequency-hash (make-hash-table :test 'equal)))
      (mapcar (lambda (x) (puthash (car x) (cadr x) word-frequency-hash)) word-frequency)
      (goto-char highlight-zone-min)
      (while (< (point) highlight-zone-max)
	(let ((word (rare-words--get-next-word)))
	  (if word
	      (setq word (downcase word))
	    (goto-char highlight-zone-max))
	  (let (( freq (gethash word word-frequency-hash)))
	    (when freq
	      (rare-words--make-rare-word-overlay
	       (cond ((< freq rare-words-common-word-cutoff) 'common)
		     ((< rare-words-common-word-cutoff freq rare-words-semi-common-word-cutoff) 'semicommon)
		     ((> freq rare-words-semi-common-word-cutoff) 'rare)
		     (t 'unk))))))))))

(provide 'rare-words)

;;; rare-words.el ends here
