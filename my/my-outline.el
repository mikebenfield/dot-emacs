
(provide 'my-outline)

(require 'my-utilities)

(defvar my-outline-comments-level-state 'special
  "Three values:
'plain: comments are considered the same as any other subtree when calculating level.
(Not generally needed by the user.)
'low: comments are one level lower than the surrounding text. This way, the
comment can be visible while the next subtree has only its heading showing - because
the comment subtree must be 'open' to show the next subtree.
'special: the leading line in a comment block is normal, but the following lines
are at level 80. This way, they can be hidden on cycle level 1.")

(make-local-variable 'my-outline-comments-special-level)

(defun my-outline-default-in-comment-line-p ()
  "t if the current line is commented out by C++ line comment syntax"
  (save-excursion
    (beginning-of-line)
    (looking-at " *//")))

(defvar my-outline-in-comment-line-p 'my-outline-default-in-comment-line-p
  "Set this for each mode to a function which returns t iff the current
line is a comment")

(make-local-variable 'my-outine-in-comment-line-p)

(defun my-outline-previous-line-comment-p ()
  (save-excursion
    (beginning-of-line)
    (and (not (bobp))
	 (progn
	   (forward-line -1)
	   (funcall my-outline-in-comment-line-p)))))

(defun my-outline-next-line-comment-p ()
  (save-excursion
    (end-of-line)
    (and (not (eobp))
	 (progn
	   (forward-line)
	   (funcall my-outline-in-comment-line-p)))))

(defun my-outline-level ()
  (let ((space-count (my-leading-space-count)))
    (cond
     ((or (not (funcall my-outline-in-comment-line-p))
	  (equalp my-outline-comments-level-state 'plain))
      (+ 2 space-count))
     ((equalp my-outline-comments-level-state 'low)
      (1+ space-count))
     ((equalp my-outline-comments-level-state 'special)
      (if (my-outline-previous-line-comment-p)
	  80
	(+ 2 space-count)))
     (t (user-error "Invalid value for my-outline-comments-level-state")))))

(defun my-outline-to-extended-beginning ()
  "Put point at the beginning of this extended subtree, which means including
  any comment blocks."
  (outline-back-to-heading)
  (beginning-of-line)
  (while (my-outline-previous-line-comment-p)
    (forward-line -1)))

(defun my-outline-to-extended-end ()
  "Put point at the end of this extended subtree"
  (let ((my-outline-comments-level-state 'plain))

    (when (funcall my-outline-in-comment-line-p)
      ;; go to the last line of this comment/attribute block
      (while (my-outline-next-line-comment-p)
    	(forward-line))

      (when (not (my-next-line-empty-p))
	(my-outline-get-next-sibling)))

    (outline-back-to-heading)
    (let ((beg (point))
	  (level (my-outline-level)))
      (outline-end-of-subtree)
      (beginning-of-line)
      	;; keep going back while we're not at the beginning of this subtree AND
      	;; our line is empty or our level is not right
      	(while (and (not (equalp (point) beg))
      		    (or (looking-at " *$")
			(> level (my-outline-level))))
      	  (forward-line -1))
      	(end-of-line))))

(defun my-outline-get-next-sibling ()
  "Like outline-get-next-sibling, but don't move if there is no next sibling"
  (let ((next-sib (save-excursion
		    (beginning-of-line)
		    (outline-get-next-sibling))))
    (when next-sib
      (goto-char next-sib)
      next-sib)))

(defun my-outline-get-previous-sibling ()
  "Like outline-get-last-sibling, but don't move if there is no last sibling"
  (interactive)
  (let ((last-sib (save-excursion
		    (beginning-of-line)
		    (outline-get-last-sibling))))
    (when last-sib
      (goto-char last-sib)
      last-sib)))

(defun my-outline-reset-visibility ()
  "After some operations, the visibility is screwed up. This will reset it."
  (setq my-outline-cycle-state (- my-outline-cycle-state 1))
  (my-outline-cycle))

(defun my-outline-move-subtree-down ()
  (interactive)
  (let ((current (point))
	(beg1 (save-excursion
		(my-outline-to-extended-beginning)
		(point)))
	(end1 (progn
		(my-outline-to-extended-end)
		(point))))
    (if (not (my-outline-get-next-sibling))
	(progn
	  (goto-char current)
	  (user-error "No next sibling"))
      (let ((beg2 (save-excursion
		    (my-outline-to-extended-beginning)
		    (point)))
	    (end2 (progn
		    (my-outline-to-extended-end)
		    (point))))
	(my-swap-regions beg1 end1 beg2 end2)

	;; now put point at the right place
	(let ((new-beg (+ beg2
			  (- beg1 end1)
			  (- end2 beg2)))
	      (diff (- current beg1)))
	  (goto-char (+ new-beg diff)))
	(my-outline-reset-visibility)))))

(defun my-outline-move-subtree-up ()
  (interactive)
  (let ((current (point))
	(beg1 (progn
		(my-outline-to-extended-beginning)
		(point)))
	(end1 (save-excursion
		(my-outline-to-extended-end)
		(point))))
    (if (not (my-outline-get-previous-sibling))
	(progn
	  (goto-char current)
	  (user-error "No previous sibling"))
      (let ((beg2 (save-excursion
		    (my-outline-to-extended-beginning)
		    (point)))
	    (end2 (save-excursion
		    (my-outline-to-extended-end)
		    (point))))
	(my-swap-regions beg2 end2 beg1 end1)

	;; now put point at the right place
	(let ((diff (- current beg1)))
	  (goto-char (+ beg2 diff)))
	(my-outline-reset-visibility)))))

(defun my-outline-install ()
  (outline-minor-mode 1)

  (set-display-table-slot standard-display-table
                        'selective-display (string-to-vector " ◦◦◦◦ "))

  (setq outline-level 'my-outline-level)

  (evil-local-set-key 'normal (kbd "\\") 'my-outline-cycle)
  ;; (evil-local-set-key 'normal outline-next-visible-heading)
  ;; (evil-local-set-key 'normal outline-previous-visible-heading)
  ;; (evil-local-set-key 'normal outline-backward-same-level)
  ;; (evil-local-set-key 'normal outline-forward-same-level)
  (evil-local-set-key 'normal (kbd "M-<down>") 'my-outline-move-subtree-down)
  (evil-local-set-key 'normal (kbd "M-<up>") 'my-outline-move-subtree-up)
  (evil-local-set-key 'normal (kbd "M-<left>") 'outline-up-heading)
  (evil-local-set-key 'normal (kbd "M-<right>") 'my-outline-toggle-comments-level-state))

;; interactive function

(defun my-outline-toggle-comments-level-state ()
  "Toggle the var my-outline-comments-level-state between 'low and
'special. No effect if it's on 'plain."
  (interactive)
  (cond
   ((equalp my-outline-comments-level-state 'low)
    (setq my-outline-comments-level-state 'special))
   ((equalp my-outline-comments-level-state 'special)
    (setq my-outline-comments-level-state 'low))))

(defvar my-outline-cycle-state 0)

(make-local-variable 'my-outline-cycle-state)

(defun my-outline-cycle ()
  (interactive)
  (let ((cycle (mod my-outline-cycle-state 3)))
    (cond
     ((equalp cycle 0) (outline-show-all) (outline-hide-sublevels 2))
     ((equalp cycle 1) (outline-show-all) (outline-hide-sublevels 40))
     (t (outline-show-all))))
  (setq my-outline-cycle-state (1+ my-outline-cycle-state)))
