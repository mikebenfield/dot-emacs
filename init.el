(setq scroll-conservatively 0)
(setq scroll-step 0)
(setq mouse-wheel-progressive-speed nil)
;; (setq mouse-wheel-scroll-amount '(1 ((shift . 1))))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

; gc, as in vim-commentary
(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(use-package ivy
  :ensure t
  :pin melpa
  :config
  (ivy-mode t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package vimish-fold
  :ensure t
  :pin melpa)

(use-package cuda-mode :ensure t :pin melpa)

(use-package origami
  :ensure t
  :pin melpa)

(use-package avy
  :ensure t
  :pin melpa
  :config
  (setq avy-style 'pre)
  (setq avy-background t)
  (setq avy-highlight-first t))

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

;; (use-package d-mode
;;   :ensure t
;;   :pin melpa)

;; (use-package julia-mode
;;   :ensure t
;;   :pin melpa)

(use-package rust-mode
  :ensure t
  :pin melpa)

(use-package rainbow-delimiters
  :ensure
  :pin melpa
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; (use-package racer
;;   :ensure t
;;   :after rust-mode
;;   :diminish racer-mode
;;   :init
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package ripgrep
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
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :pin melpa
  :config
  (counsel-projectile-mode))

;; ensime (scala)
(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package magit
 :ensure t
 :pin melpa
 :config
 (global-set-key (kbd "C-x g") 'magit-status))

(use-package evil-magit
  :ensure t
  :pin melpa
  :config
  (require 'evil-magit))

;; nim
(use-package
  nim-mode
  :ensure t
  :pin melpa)

(use-package
  org-ref
  :ensure t
  :pin melpa-stable
  :config
  (setq org-ref-default-bibliography '("~/Dropbox/Documents/Papers/bib.bib"))
  (setq org-ref-pdf-directory '("~/Dropbox/Documents/Papers/"))
  (setq org-ref-insert-cite-key "C-c ."))

(use-package
  ivy-bibtex
  :ensure t
  :pin melpa-stable)

;; python
(use-package elpy
  :ensure t
  :pin melpa-stable
  :config
  (elpy-enable)
  (define-key elpy-mode-map (kbd "C-c C-f") 'elpy-format-code))

(use-package cython-mode
  :ensure t
  :pin melpa)

(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample))
  :defer t
  :ensure t)

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

(when (not (eq system-type 'darwin))
  ;; on Macs, for some reason disabling the menu bar just enables irritating
  ;; focus-losing behavior when changing desktops
  (menu-bar-mode -1))

(setq-default require-final-newline 'visit-save)

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

(global-hl-line-mode)
(set-face-background hl-line-face "#444466")
(blink-cursor-mode 0)

(setq-default fill-column 80)

;;;; appearance

(set-face-font 'default "-*-Inconsolata-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")

(setq sentence-end-double-space nil)

(setq org-odd-levels-only t)
(setq org-hide-leading-stars t)
(setq org-adapt-indentation nil)
(setq org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("DONE" . "orange")
        ("DELAYED" . "blue")
        ("CANCELED" . "blue")))

(add-hook 'org-mode-hook
          (lambda ()
            (evil-local-set-key 'normal "\C-t" 'org-todo)
            (evil-local-set-key 'insert "\C-t" 'org-todo)))

(setq create-lockfiles nil) ; I don't want these stupid .# files

;; my functions

(add-to-list 'load-path "~/.emacs.d/my/")

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

(setq eshell-prompt-function
    (lambda ()
    (concat (eshell/pwd) "\n $ ")))

(require 'my-outline)
(require 'my-rust)
(require 'my-keys)
(require 'my-org)

(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

(winner-mode t)

(setq visible-bell t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; On a Mac, I get this ridiculous extra tab bar even when not using tabs, and
;; even if I disable it, it still appears in the first frame created
(when (eq system-type 'darwin)
  (setq mac-frame-tabbing nil)
  (let (f (selected-frame))
    (make-frame)
    (delete-frame f)))

(c-set-offset 'innamespace 0)

;; C++
(load "/opt/local/libexec/llvm-8.0/share/clang/clang-format.el")
(add-hook
     'c++-mode-hook
      (lambda ()
        (local-set-key (kbd "C-c f") 'clang-format-buffer)
        (local-set-key (kbd "C-c C-f") 'clang-format-buffer)))

(setq-default indent-tabs-mode nil)
