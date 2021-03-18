;;; init.el --- My Emacs configuration.
;;; Commentary:
;;; Author: Enrique Aleman
;;; Created on: 17 April 2021
;;; Copyright (c) 2021 Enrique Aleman

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

;; Set up package

(require 'package)

;; add archives
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Install 'use-package' if it is not installed.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Use better defaults
(setq-default
 ;; Don't use the compiled code if its the older package.
 load-prefer-newer t

 ;; Do not show the startup message.
 inhibit-startup-message t

 ;; Do not put 'customize' config in init.el; give it another file.
 custom-file "~/.emacs.d/custom-file.el"

  ;; Do not create lockfiles.
 create-lockfiles nil

 ;; Don't use hard tabs
 indent-tabs-mode nil

  ;; Emacs can automatically create backup files. This tells Emacs to put all backups in
 ;; ~/.emacs.d/backups. More info:
 ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

 ;; Do not autosave.
 auto-save-default nil

  ;; Allow commands to be run on minibuffers.
 enable-recursive-minibuffers t

 ;; Do not ring bell
 ring-bell-function 'ignore)

;; Load `custom-file` manually as we have modified the default path.
(load-file custom-file)

;; Delete whitespace just when a file is saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;set font size default to larger than usual
(set-face-attribute 'default nil :height 150)
;;initial window size
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 160))

;; make all words one
(global-superword-mode 1)

;; Make the command key behave as 'meta'
(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'meta))

;; (when (eq system-type 'darwin)
;;   (setq mac-command-modifier 'meta)
;;   (setq mac-right-command-modifier 'hyper))

;;string rectangle to append first letter instead of replacing it
(global-set-key (kbd "C-x r t") 'string-insert-rectangle)

;;ibuffer much better than buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; global key bindings
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-,") #'prev-window)
(defun prev-window ()
  (interactive)
  (other-window -1))

;; ───────────────────── Additional packages and their configurations ─────────────────────
(require 'use-package)

;; Add `:doc' support for use-package so that we can use it like what a doc-strings is for
;; functions.
(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)
  (defun use-package-handler/:doc (name-symbol _keyword _docstring rest state)
    "An identity handler for :doc.
     Currently, the value for this keyword is being ignored.
     This is done just to pass the compilation when :doc is included

     Argument NAME-SYMBOL is the first argument to `use-package' in a declaration.
     Argument KEYWORD here is simply :doc.
     Argument DOCSTRING is the value supplied for :doc keyword.
     Argument REST is the list of rest of the  keywords.
     Argument STATE is maintained by `use-package' as it processes symbols."

    ;; just process the next keywords
    (use-package-process-keywords name-symbol rest state)))

;; ─────────────────────────────────── Generic packages ───────────────────────────────────
(use-package recentf
  :doc "Recent buffers in a new Emacs session"
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t)
  :delight)

(use-package helm
  :doc "interactive fuzzy matching on everything"
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-<tab>" . completion-at-point))
  )

(use-package ace-jump-mode
  :doc "Jump around the visible buffer using 'Head Chars'"
  :ensure t
  :bind (("C-j" . ace-jump-mode)
         ("C-c j" . 'ace-jump-mode-pop-mark))
  :delight)

(use-package ace-jump-helm-line
  :doc "ace jump on the helm line"
  :ensure t
  :bind ("C-'" . ace-jump-helm-line)
  )

(use-package helm-swoop
  :doc "like occur but with helm"
  :ensure t
  :bind ("M-i" . helm-swoop)
  )

(use-package magit
  :doc "Git integration for Emacs"
  :ensure t
  :bind ("C-x g" . magit-status)
  )
