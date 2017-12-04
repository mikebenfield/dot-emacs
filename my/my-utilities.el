
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
