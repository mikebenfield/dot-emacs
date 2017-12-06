
(setq mouse-wheel-progressive-speed nil)
; (setq mouse-wheel-scroll-amount '(1 ((shift . 1))))


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package ivy
  :ensure
  :pin melpa)

(require 'evil)
(evil-mode 1)

(winner-mode t)

(setq visible-bell t)

; (use-package auctex
;  :ensure t
;  :pin gnu
;  :config
;  (setq TeX-auto-save t)
;  (setq TeX-parse-self t)
;  (setq TeX-PDF-mode t)
;  (setq-default TeX-master nil)
;  ;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
;  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;  (setq reftex-plug-into-AUCTeX t))

(use-package cuda-mode :ensure t :pin melpa)

(use-package origami
  :ensure t
  :pin melpa)

(use-package exec-path-from-shell
  :ensure t
  :pin melpa
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (setq-default eshell-path-env (getenv "PATH"))))

(use-package nasm-mode
  :ensure t
  :pin melpa)

(use-package d-mode
  :ensure t
  :pin melpa)

(use-package julia-mode
  :ensure t
  :pin melpa)

(use-package rust-mode
  :ensure t
  :pin melpa)

(use-package counsel
 :ensure t
 :pin melpa-stable
 :config
 (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(use-package projectile
  :ensure t
  :pin melpa
  :config
  (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :pin melpa
  :config
  (counsel-projectile-on))

(use-package evil
  :ensure t
  :pin melpa-stable)

;; ensime (scala)
(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package magit
  :ensure t
  :pin melpa-stable
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; nim
(use-package
  nim-mode
  :ensure t
  :pin melpa)

;; python
(use-package elpy
  :ensure t
  :pin melpa-stable
  :config
  (elpy-enable)
  (elpy-use-ipython))

(use-package cython-mode
  :ensure t
  :pin melpa)

;; occur mode should still use vi bindings
(add-hook 'occur-hook
	  '(lambda ()
	     (evil-mode 1)
	     (evil-normal-state)))

;; workaround for stupid Warning bug
(with-eval-after-load 'python
  (setq fill-column 79)
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
	  (python-shell-completion-native-output-timeout
	   python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "2439e27fb4695fa3f6b6336fa3f053e97f51ee251d583edc1a26e3b392eade55" "b747fb36e99bc7f497248eafd6e32b45613ee086da74d1d92a8da59d37b9a829" default)))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/GoogleDrive/DailyPlan/2017-02-18.org")))
 '(package-selected-packages
   (quote
    (org solarized-theme gandalf-theme eink-theme forest-blue-theme liso-theme cuda-mode origami exec-path-from-shell nasm-mode d-mode rust-mode julia-mode nim-mode cython-mode auctex counsel-projectile projectile counsel moe-theme molokai-theme use-package ## ensime elpy jedi rainbow-delimiters material-theme magit zenburn-theme organic-green-theme haskell-mode markdown-mode silkworm-theme evil-commentary evil)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default indicate-empty-lines t)

(define-key evil-normal-state-map " " 'evil-window-map)
(define-key evil-motion-state-map " " 'evil-window-map)
(define-key evil-normal-state-map (kbd "M-f") 'toggle-frame-fullscreen)
(define-key evil-normal-state-map (kbd "RET") nil)
(define-key evil-motion-state-map (kbd "RET") nil)

;; (define-key evil-normal-state-map (kbd "<tab>") 'origami-toggle-node)
;; (define-key evil-normal-state-map (kbd "S-<tab>")
;;   'origami-recursively-toggle-node)

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/saves" t)))

; gc, as in vim-commentary
(evil-commentary-mode)
;; (set-cursor-color "#006fbf")
(global-hl-line-mode)
(set-face-background 'hl-line "#F1FA2A")
(set-face-foreground 'highlight nil)
(blink-cursor-mode 0)

(setq-default fill-column 80)
(winner-mode t)

;;;; appearance

(setq org-odd-levels-only t)
(setq org-hide-leading-stars t)
(setq org-adapt-indentation nil)
(require 'gandalf-theme)
;; (require 'eink-theme)
(setq create-lockfiles nil) ; I don't want these stupid .# files
;; (require 'moe-theme)
;; (moe-light)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; (load-theme 'organic-green)
;; (load-theme 'silkworm)
;; (defun my/org-mode-hook ()
;;   "My `org-mode' hook"
;;   (set-face-attribute 'org-level-1 nil :foreground "black" :height 'unspecified)
;;   (set-face-attribute 'org-level-2 nil :foreground "blue")
;;   (set-face-attribute 'org-level-3 nil :foreground "red")
;;   (set-face-attribute 'org-level-4 nil :foreground "dark green")
;;   ;; (set-face-attribute 'org-level-4 nil :foreground "00FF00")
;;   ;; (set-face-attribute 'org-level-5 nil :foreground "purple")
;;   ;; (set-face-attribute 'org-level-6 nil :foreground "red")
;;   )
;; (face-attribute 'org-level-1 :height)
;; (face-attribute 'org-level-2 :height)
;; (eval-after-load "org" '(my/org-mode-hook))
;; (add-hook 'org-mode-hook 'my/org-mode-hook)

;; my functions

(add-to-list 'load-path "/Users/mike/.emacs.d/my/")

(format-time-string "%Y-%m-%d")

(defvar my-daily-plan-directory "~/GoogleDrive/DailyPlan/")

(defun my-open-daily-plan ()
  (interactive)
  (let ((filename (concat my-daily-plan-directory
			  (format-time-string "%Y-%m-%d")
			  ".org")))
    (find-file filename)))

(defun my-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun my-insert-time ()
  (interactive)
  (insert (format-time-string "%H:%M")))

(global-set-key [f1] 'my-open-daily-plan)
(global-set-key [f2] 'my-insert-date)
(global-set-key [f3] 'my-insert-time)

(global-set-key (kbd "C-c a") 'org-agenda)

(setq eshell-prompt-function
    (lambda ()
    (concat (eshell/pwd) "\n $ ")))

;; gdb
(setq gdb-many-windows t)

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

(require 'my-outline)
(require 'my-rust)
(require 'my-keys)
(require 'my-org)
