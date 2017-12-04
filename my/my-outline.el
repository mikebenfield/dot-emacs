
(provide 'my-outline)

(require 'my-utilities)

(defun my-rust-outline-mode-hook ()
  (outline-minor-mode 1)

  (evil-local-set-key 'normal (kbd "\\") 'my-outline-cycle)
  ;; (evil-local-set-key 'normal outline-next-visible-heading)
  ;; (evil-local-set-key 'normal outline-previous-visible-heading)
  ;; (evil-local-set-key 'normal outline-backward-same-level)
  ;; (evil-local-set-key 'normal outline-forward-same-level)
  (evil-local-set-key 'normal (kbd "M-<down>") 'my-outline-move-subtree-down)
  (evil-local-set-key 'normal (kbd "M-<up>") 'my-outline-move-subtree-up)
  (evil-local-set-key 'normal (kbd "M-<left>") 'outline-up-heading)
  (evil-local-set-key 'normal (kbd "M-<right>") 'my-rust-toggle-special-level)

  (make-local-variable 'my-rust-special-level)
  (make-local-variable 'my-rust-plain-level)
  (make-local-variable 'my-outline-cycle-state)

  ;; (advice-add 'outline-end-of-subtree :around #'my-outline-end-of-subtree-advice)

  (setq outline-level 'my-rust-outline-level)

  (setq outline-regexp
	(rx
	 (* space)
	 (or
	  (and (char "A-Z") (* alpha) (* space) "{")
	  "#["
	  "//"
	  "mod "
	  "pub mod "
	  "enum "
	  "pub enum "
	  "struct"
	  "pub struct"
	  "fn "
	  "pub fn "
	  "impl"
	  "trait "
	  "pub trait "
	  "macro_rules! ")))) 

(add-hook 'rust-mode-hook 'my-rust-outline-mode-hook)

(set-display-table-slot standard-display-table 
                        'selective-display (string-to-vector " ◦◦◦◦ "))

(defun my-rust-in-comment-line ()
  "t if the current line is commented out"
  (save-excursion
    (beginning-of-line)
    (looking-at " *//")))

(defun my-rust-in-attribute-line ()
  "t if the current line starts with an attribute"
  (save-excursion
    (beginning-of-line)
    (looking-at " *#\\[")))

(defun my-rust-in-comment-or-attribute-line ()
  "t if the current line is commented out or starts with an attribute"
  (or (my-rust-in-comment-line)
      (my-rust-in-attribute-line)))

(defvar my-rust-special-level t)

(defvar my-rust-plain-level nil)

(defun my-rust-toggle-special-level ()
  (interactive)
  (setq my-rust-special-level (not my-rust-special-level))
  (setq my-outline-cycle-state (- my-outline-cycle-state 1))
  (my-outline-cycle))

(defun my-rust-outline-level ()
  (let ((space-count 0))
    (save-excursion
      (while (equalp (following-char) ?\ )
	(setq space-count (1+ space-count))
	(forward-char)))
    (cond
     (my-rust-plain-level (if (looking-at " *$")
			      1
			    (+ 2 space-count)))
     ((and my-rust-special-level
	   (my-rust-in-comment-or-attribute-line)
	   (not (bobp))
	   (save-excursion
	     (forward-line -1)
	     (my-rust-in-comment-or-attribute-line)))
      ;; We're in a non-leading comment or attribute, and my-rust-special-level
      ;; is on, so we want a high level 
      (+ 50 space-count))
     ((and (not my-rust-special-level)
	   (my-rust-in-comment-or-attribute-line))
      ;; We're in a comment or attribute, and my-rust-special-level is not on,
      ;; so we want one level lower than the surrounding text
      (1+ space-count))
     (t
      (+ 2 space-count)))))

(defun my-rust-previous-line-is-comment-or-attribute ()
  (save-excursion
    (and
     (not (bobp))
     (progn
       (forward-line -1)
       (my-rust-in-comment-or-attribute-line)))))

(defun my-rust-next-line-is-comment-or-attribute ()
  (save-excursion
    (and
     (not (eobp))
     (progn
       (forward-line 1)
     (my-rust-in-comment-or-attribute-line)))))

(defun my-rust-to-extended-beginning ()
  "Put point at the beginning of this extended subtree, which means including
any comments or attributes."
  (interactive)
  (outline-back-to-heading)
  (while (my-rust-previous-line-is-comment-or-attribute)
    (forward-line -1)
    (beginning-of-line)))

(defun my-previous-line-empty ()
  (save-excursion
    (beginning-of-line)
    (and
     (not (bobp))
     (progn
       (forward-line -1)
       (looking-at " *$")))))

(defun my-next-line-empty ()
  (save-excursion
    (end-of-line)
    (and
     (not (eobp))
     (progn
       (forward-line)
       (beginning-of-line)
       (looking-at " *$")))))

(defun my-previous-line-level ()
  (save-excursion
    (beginning-of-line)
    (if (bobp)
	nil
      (forward-line -1)
      (funcall outline-level))))

(defun my-outline-get-next-sibling ()
  "Like outline-get-next-sibling, but don't move if there is no next sibling"
  (let ((next-sib (save-excursion
		    (beginning-of-line)
		    (outline-get-next-sibling))))
    (when next-sib
      (goto-char next-sib)
      next-sib)))

(defun my-outline-get-last-sibling ()
  "Like outline-get-last-sibling, but don't move if there is no last sibling"
  (interactive)
  (let ((last-sib (save-excursion
		    (beginning-of-line)
		    (outline-get-last-sibling))))
    (when last-sib
      (goto-char last-sib)
      last-sib)))

(defun my-rust-to-extended-end ()
  "Put point at the end of this extended subtree"
  (interactive)
  (let ((my-rust-plain-level t))

    (when (my-rust-in-comment-or-attribute-line)
      ;; go to the last line of this comment/attribute block
      (while (my-rust-next-line-is-comment-or-attribute)
    	(forward-line))

      (when (not (my-next-line-empty))
	(my-outline-get-next-sibling)))

    (outline-back-to-heading)
    (let ((beg (point))
	  (level (funcall outline-level)))
      (outline-end-of-subtree)
      (beginning-of-line)
      	;; keep going back while we're not at the beginning of this subtree AND
      	;; our line is empty or our level is not right
      	(while (and (not (equalp (point) beg))
      		    (or (looking-at " *$")
			(> level (funcall outline-level))))
      	  (forward-line -1))
      	(end-of-line))))

(defun my-outline-move-subtree-down ()
  (interactive)
  (let ((current (point))
	(beg1 (save-excursion
		(my-rust-to-extended-beginning)
		(point)))
	(end1 (progn
		(my-rust-to-extended-end)
		(point))))
    (if (not (my-outline-get-next-sibling))
	(progn
	  (goto-char current)
	  (user-error "No next sibling"))
      (let ((beg2 (save-excursion
		    (my-rust-to-extended-beginning)
		    (point)))
	    (end2 (progn
		    (my-rust-to-extended-end)
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
		(my-rust-to-extended-beginning)
		(point)))
	(end1 (save-excursion
		(my-rust-to-extended-end)
		(point))))
    (if (not (my-outline-get-last-sibling))
	(progn
	  (goto-char current)
	  (user-error "No next sibling"))
      (let ((beg2 (save-excursion
		    (my-rust-to-extended-beginning)
		    (point)))
	    (end2 (save-excursion
		    (my-rust-to-extended-end)
		    (point))))
	(my-swap-regions beg2 end2 beg1 end1)

	;; now put point at the right place
	(let ((diff (- current beg1)))
	  (goto-char (+ beg2 diff)))
	(my-outline-reset-visibility)))))

(defvar my-outline-cycle-state 0)

(defun my-outline-cycle ()
  (interactive)
  (let ((cycle (mod my-outline-cycle-state 3)))
    (cond
     ((equalp cycle 0) (outline-show-all) (outline-hide-sublevels 2))
     ((equalp cycle 1) (outline-show-all) (outline-hide-sublevels 20))
     (t (outline-show-all))))
  (setq my-outline-cycle-state (1+ my-outline-cycle-state)))

(defun my-rust-in-comment-subtree ()
  (save-excursion
    (outline-back-to-heading)
    (looking-at " *// \*")))

(defun my-outline-reset-visibility ()
  (setq my-outline-cycle-state (- my-outline-cycle-state 1))
  (my-outline-cycle))

(defun my-outline-end-of-subtree-advice (orig-fun &rest args)
  (outline-back-to-heading)
  (let ((current-pos (point))
  	(current-level (funcall outline-level)))
    (apply orig-fun args)
    (let ((their-end (point)))
      (goto-char current-pos)
      (my-find-next-lower-sublevel current-level their-end))))

(defun my-thing ()
  (interactive)
  (my-swap-regions 1 10 12 17))
