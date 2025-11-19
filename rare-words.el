
(defgroup rare-words nil
  "Customization group for the rare-words package.")

(defcustom rare-words-common-word-cutoff 2500
  "All words ranked this value or below are considered common."
  :type '(natnum))

(defcustom rare-words-semi-common-word-cutoff 4510
  "All words ranked below this value but above
 `rare-words-common-word-cutoff' are considered semi-common."
  :type '(natnum))

(defcustom rare-words-dictionary (expand-file-name "words.db"
						   (file-name-directory (or load-file-name
									    (buffer-file-name))))
  "The sqlite database where we find the rankings for common words")

(defcustom rare-words-semi-common-word-face 'warning
  "The face semi-common words will be highlighted by."
  :type '(face))

(defcustom rare-words-rare-word-face 'error
  "The face rare words will be highlighted by."
  :type '(face))

(defcustom rare-words-search-forward-regex "[A-Za-z']+"
  "Regular expression used to define what a word is for the purpose of
 the rare-words package.")

(defvar-local rare-words--overlay-list nil
  "A list of overlays associated with the rare-words package for this
 buffer.")

(defun rare-words-kill-highlights ()
  "Deletes the buffer-local overlay list associated with the rare-words
package."
  (interactive)
  (dolist (o rare-words--overlay-list)
    (delete-overlay o))
  (setq rare-words--overlay-list nil))

(defun rare-words--get-word-frequency (word-list)
  (let ((db (sqlite-open rare-words-dictionary)))
    (sqlite-execute db "create temporary table buf_words (word text)")
    (dolist (w word-list)
      (sqlite-execute db "insert into buf_words values (?)" `(,w)))
    (let ((results (sqlite-select db (format "select a.* from dictionary as a inner join buf_words as b on a.word=b.word"))))
      (sqlite-close db)
      results)))

(defun rare-words--get-next-word (&optional max)
  (interactive)
  (if (re-search-forward rare-words-search-forward-regex
			 (or max (point-max))
			 t)
      (current-word nil t)
    nil))

(defun rare-words--get-words-in-region-or-buffer (min max)
  (let ((word-list nil))
    (goto-char min)
    (while (< (point) max)
      (let ((word (rare-words--get-next-word)))
	(if word
	    (push word word-list)
	  (goto-char max))))
    word-list))

(defun rare-words--make-rare-word-overlay (rarity)
  (when (memq rarity '(rare semicommon))
    (let ((cur-overlay (make-overlay min max)))
      (push cur-overlay rare-words--overlay-list)
      (overlay-put cur-overlay 'face (cond ((eq rarity 'semicommon) rare-words-semi-common-word-face)
					   ((eq rarity 'rare) rare-words-rare-word-face))))))
					   
      
(defun rare-words-highlight ()
  (interactive)
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
	(when word (setq word (downcase word)))
	(rare-words--make-rare-word-overlay (let ((freq (gethash word word-frequency-hash)))
					      (cond ((< freq rare-words-common-word-cutoff) 'common)
						    ((< rare-words-common-word-cutoff freq rare-words-semi-common-word-cutoff) 'semicommon)
						    ((> freq rare-words-semi-common-word-cutoff 'rare))
						    (t 'unk))))))))
					       
     
    
    

The quick brown fox jumps over the lazy dog.

;; Some ideas:
;; 1. Transactions
;; 2. Indices on both the temp table and dictionary.
;; 3. Hash table

;; Bracket overlays 

