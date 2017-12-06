
(provide 'my-rust)

(require 'my-outline)

(defun my-rust-mode-hook ()
  (my-outline-install)

  (setq my-outline-in-comment-line-p
	(lambda ()
	  (save-excursion
	    (beginning-of-line)
	    (or (looking-at " *///")
		(looking-at " *#\\[")))))

  (setq outline-regexp
	(rx
	 (* space)
	 (or
	  (and (char "A-Z") (* alpha) (* space) "{")
	  "#["
	  "///"
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

(add-hook 'rust-mode-hook 'my-rust-mode-hook)

;; Rust use formating

(defun my-rust-sort-use-line ()
  ;; the buffer has been narrowed to a 'use' line
  (save-excursion
    (save-restriction
      (let* ((beg (search-forward "{" nil t))
	     (self-beg (search-forward "self" nil t))
	     (real-beg (if (and beg self-beg)
			   self-beg
			 beg))
	    (end (search-forward "}" nil t)))
	(if (and real-beg end (< real-beg end))
	    (sort-regexp-fields nil "[a-zA-Z0-9_]+" "[a-zA-Z0-0_]+" real-beg end))))))

(defun my-rust-sort-use-lines ()
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (let ((beg t)
	    (end t))
	(while beg
	  (setq beg (re-search-forward "^\\(use\\|pub use\\)" nil t))
	  (setq end (search-forward ";" nil t))
	  (when (and beg end)
	    (save-excursion
	      (save-restriction
		(narrow-to-region beg end)
		(goto-char (point-min))
		(my-rust-sort-use-line)))))))))

;; returns beginning of block if narrowed; otherwise nil. leaves point at beginning of
;; block
(defun my-rust-narrow-to-use-block ()
  (let ((beg (re-search-forward "^\\(use\\|pub use\\)" nil t))
	(end (re-search-forward "^[[:space:]]*$" nil t)))
    (when (and beg end)
      (goto-char beg)
      (beginning-of-line)
      (narrow-to-region (point) end)
      beg)))

(defun my-rust-nextrecfn ()
  (if (not (re-search-forward "^\\(use\\|pub use\\)" nil t))
    (goto-char (point-max))
    (beginning-of-line)))

(defun my-rust-endrecfn ()
  (search-forward ";" nil t))

(defun my-rust-sort-use-line-chunks ()
  (save-excursion
    (save-restriction
      (let ((cont (point-min)))
	(while cont
	  (goto-char cont)
	  (save-restriction
	    (setq cont (my-rust-narrow-to-use-block))
	    (when cont
	      (goto-char (point-min))
	      (sort-subr nil 'my-rust-nextrecfn 'my-rust-endrecfn)
	      (setq cont (point-max)))))))))

(defun my-rust-use-format ()
  (interactive)
  (my-rust-sort-use-lines)
  (my-rust-sort-use-line-chunks))

(global-set-key [f4] 'my-rust-use-format)

