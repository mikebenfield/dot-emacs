
(provide 'my-keys)

(define-prefix-command 'my-keymap)

(define-key evil-window-map " " 'my-keymap)

(defun my-notes-find-file ()
  (interactive)
  (counsel-find-file "~/GoogleDrive/Notes2/"))

(define-key 'my-keymap (kbd "n") 'my-notes-find-file)

(defun my-work-filename (time)
  (concat "~/GoogleDrive/WorkJournal/"
	  (format-time-string "%Y-%m-%d.org" time)))

(defun my-journal-filename (time)
  (concat "~/GoogleDrive/Journal/"
	  (format-time-string "%Y-%m-%d.org" time)))

(defun my-time-days-ago (n)
  (- (float-time) (* n 86400)))

(defun my-default-daily-work-file (time)
  (concat (format-time-string "%a, %Y-%m-%d\n")
	  "\n"
	  "* Major tasks to accomplish\n"
	  "- [ ] First task\n"
	  "- [ ] Second task\n"
	  "\n"
	  "* Extra tasks\n"
	  "- [ ] First task\n"
	  "- [ ] Second task\n"
	  "\n"
	  "* Schedule\n"
	  "| time     | task                    |\n"
	  "|----------+-------------------------|\n"
	  "| 6        | wake, breakfast, dog    |\n"
	  "| 6:45     | gym                     |\n"
	  "| 8:15     | plan lesson             |\n"
	  "| 9        | teach                   |\n"
	  "| 12:30    | lunch, dog              |\n"
	  "| 1:15     |                         |\n"
	  "|          |                         |\n"
	  "\n"
	  "* Log\n\n"))

(defun my-open-daily-work (n)
  (interactive "P")
  (let* ((n* (if (numberp n) n 0))
	 (time (my-time-days-ago n*))
	 (filename (my-work-filename time))
	 (exists (file-exists-p filename)))
    (if (and (> n* 0)
	     (not exists))
	(user-error "No work file for that date")
      (find-file filename)
      (when (not exists)
	(save-excursion
	    (insert (my-default-daily-work-file time)))))))

(define-key 'my-keymap (kbd "w") 'my-open-daily-work)

(defun my-open-daily-journal (n)
  (interactive "P")
  (let* ((n* (if (numberp n) n 0))
	 (time (my-time-days-ago n*))
	 (filename (my-journal-filename time))
	 (exists (file-exists-p filename)))
    (if (and (> n* 0)
	     (not exists))
	(user-error "No journal file for that date")
      (find-file filename))))

(define-key 'my-keymap (kbd "d") 'my-open-daily-journal)
