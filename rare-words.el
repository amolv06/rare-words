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

(defcustom rare-words-semi-common-word-cutoff 10000
  "The number of most frequently used words to be considered semi-common."
  :type '(integer :min 1 :max 100000))

(defcustom rare-words-common-word-file (expand-file-name "frequent"
							 (file-name-directory buffer-file-name))
  "File containing a list of most common words")

(defvar rare-words--common-words-table (make-hash-table :test 'equal)
  "List containing the n most common words, where n is the value of
`rare-words-common-words-cutoff'.")

(defvar-local rare-words--active-overlays nil
  "List containing all active overlays associated with the rare-words
package.")

(defun rare-words--build-common-words-table ()
  (with-temp-buffer
    (insert-file-contents rare-words-common-word-file)
    (goto-char (point-min))
    (dotimes (i rare-words-semi-common-word-cutoff)
      (puthash (current-word) i rare-words--common-words-table)
      (forward-word 1))))

(defun rare-words--identify-rare-word (word)
  (let ((freq (or (gethash word rare-words--common-words-table) 9999999999999)))
    (cond ((< freq rare-words-common-word-cutoff)
	   'common)
	  ((< freq rare-words-semi-common-word-cutoff)
	   'semicommon)
	  (t 'rare))))

(defun rare-words--next-word (&optional max)
  (interactive)
  (re-search-forward "[A-Za-z]+"
		     (or max (point-max))
		     t)
   (backward-char)
   (message (current-word nil t)))

(defun rare-words--make-rare-word-overlay (min max rarity)
  (let ((cur-overlay (make-overlay min max)))
  (push cur-overlay rare-words--active-overlays)
  (overlay-put cur-overlay 'face (cond ((eq rarity 'semicommon)
					'warning)
				       ((eq rarity 'rare)
					'error)
				       (t nil)))))
(defun rare-words-highlight ()
  (interactive)
  (when (equal 0 (hash-table-count rare-words--common-words-table))
    (rare-words--build-common-words-table))
  (let ((highlight-zone-min (if (region-active-p)
				(region-beginning)
			      (point-min)))
	(highlight-zone-max (if (region-active-p)
				(region-end)
			      (point-max))))
    (goto-char highlight-zone-min)
    (while (< (point) highlight-zone-max)
      (let* ((cur-word (rare-words--next-word highlight-zone-max))
	     (cur-word-rarity (rare-words--identify-rare-word cur-word)))
	(rare-words--make-rare-word-overlay (match-beginning 0)
					    (match-end 0)
					    cur-word-rarity)))))

(defun rare-words-remove-overlays ()
  (interactive)
  (dolist (overlay rare-words--active-overlays)
    (delete-overlay overlay))
  (setq rare-words--active-overlays nil))
    
(rare-words-highlight)

    
