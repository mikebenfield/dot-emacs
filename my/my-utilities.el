
(provide 'my-utilities)

(defun my-swap-regions (beg1 end1 beg2 end2)
  "Caller's responsibility to ensure (<= beg1 end1 beg2 end2)"
  (let* ((s2 (delete-and-extract-region beg2 end2))
	 (s1 (delete-and-extract-region beg1 end1))
	 (diff1 (- end1 beg1))
	 (diff2 (- end2 beg2)))
    (save-excursion
      (goto-char beg1)
      (insert s2)
      (goto-char (+ beg2 (- diff1) diff2))
      (insert s1))))

(defun my-leading-space-count ()
  "How many space characters does the line begin with?"
  (save-excursion
    (beginning-of-line)
    (looking-at " *")
    (- (match-end 0) (match-beginning 0))))

(defun my-next-line-empty-p ()
  "Does the next line contain nothing but spaces?"
  (save-excursion
    (end-of-line)
    (and (not (eobp))
	 (progn
	   (forward-line)
	   (beginning-of-line)
	   (looking-at " *$")))))

(defvar my-learn-file "/Users/mike/GoogleDrive/Notes2/learn.org")

(defun my-random-choice (lst total)
  "Choose a random element from lst.
lst should be a list of the form ((value1 count1) (value2 count2) ... ).
The probability of choosing valuei is counti / Sum_j countj.
"
  (let ((max (random total))
	(so-far (cadar lst)))
    (while (< so-far max)
      (setq lst (cdr lst))
      (setq so-far (+ so-far (cadar lst))))
    (caar lst)))

(defun my-random-learn ()
  "Choose a random Q in the learn.org file."
  (interactive)
  (goto-char 1)
  (let ((buffer (find-file my-learn-file))
	(regex "^\*+ SECTION\\(\\(?:_[0-9]*\\)?\\)")
	(section-headings '())
	(total 0))
    (while (re-search-forward regex nil t)
      (let* ((num-string (seq-drop (match-string 1) 1))
	     (num (if (equal num-string "")
	     	      1
	     	    (string-to-number num-string))))
	(setq section-headings (cons (list (point) num) section-headings))
	(setq total (+ num total))))
    (let ((position (my-random-choice section-headings total)))
      (goto-char position))))


