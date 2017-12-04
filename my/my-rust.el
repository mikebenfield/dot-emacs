
(provide 'my-rust)

(require 'my-outline)

(defun my-rust-mode-hook ()
  (my-outline-install)

  (setq my-outline-in-comment-line-p
	(lambda ()
	  (save-excursion
	    (beginning-of-line)
	    (or (looking-at " *//")
		(looking-at " *#\\[")))))

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

(add-hook 'rust-mode-hook 'my-rust-mode-hook)
