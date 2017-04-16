

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(eval-when-compile
  (require 'use-package))

(require 'evil)
(evil-mode 1)

(winner-mode t)

;; (use-package auctex
;;  :ensure t
;;  :pin gnu
;;  :config
;;  (setq TeX-auto-save t)
;;  (setq TeX-parse-self t)
;;  (setq TeX-PDF-mode t)
;;  (setq-default TeX-master nil)
;;  ;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
;;  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;  (setq reftex-plug-into-AUCTeX t))

(use-package d-mode
  :ensure t
  :pin melpa)

(use-package julia-mode
  :ensure t
  :pin melpa)

(use-package rust-mode
  :ensure t
  :pin melpa)

(use-package ivy
  :ensure t
  :pin melpa-stable
  :config
  (ivy-mode t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

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
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "2439e27fb4695fa3f6b6336fa3f053e97f51ee251d583edc1a26e3b392eade55" "b747fb36e99bc7f497248eafd6e32b45613ee086da74d1d92a8da59d37b9a829" default)))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/GoogleDrive/DailyPlan/2017-02-18.org")))
 '(package-selected-packages
   (quote
    (d-mode rust-mode julia-mode nim-mode cython-mode auctex counsel-projectile projectile counsel moe-theme molokai-theme use-package ## ensime elpy jedi rainbow-delimiters material-theme magit zenburn-theme organic-green-theme haskell-mode markdown-mode silkworm-theme evil-commentary evil)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "nil" :slant normal :weight normal :height 120 :width normal)))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default indicate-empty-lines t)

(define-key evil-normal-state-map " " 'evil-window-map)
(define-key evil-motion-state-map " " 'evil-window-map)
(define-key evil-normal-state-map (kbd "M-f") 'toggle-frame-fullscreen)

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

; gc, as in vim-commentary
(evil-commentary-mode)
(set-cursor-color "#00bfff")
(global-hl-line-mode)
(blink-cursor-mode 0)

(setq-default fill-column 80)
(winner-mode t)

;;;; appearance


(setq org-odd-levels-only t)
(setq org-hide-leading-stars t)
(setq org-adapt-indentation nil)
(require 'moe-theme)
(moe-light)
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
