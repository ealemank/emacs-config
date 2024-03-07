;;; init.el --- My Emacs configuration.
;;; Commentary:
;;; Author: Enrique Aleman
;;; Created on: 17 April 2021
;;; Copyright (c) 2021 Enrique Aleman

;; This file is not part of GNU Emacs.

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
 custom-file "~/.emacs.d/custom_utils.el"

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
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;;set font size default to larger than usual
(set-face-attribute 'default nil :height 150)
;;initial window size
;;(add-to-list 'default-frame-alist '(height . 48))
;;(add-to-list 'default-frame-alist '(width . 100))

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

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq xref-prompt-for-identifier nil)

;;set bash for shell.  Mac has a custom bash install from brew
;;(if (file-exists-p "/opt/homebrew/bin/bash")
;;    (setq-default explicit-shell-file-name "/opt/homebrew/bin/bash"))

;;(setq-default shell-file-name "/opt/homebrew/bin/bash/bin/bash")
;;Loading default shells.  I always use them
(shell "temp_shell")
(shell "build_shell")

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
         ("C-x r b" . helm-filtered-bookmarks)
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

(use-package helm-descbinds
  :doc "describe key bindings"
  :ensure t
  :bind ("C-h b" . helm-descbinds)
  )

(use-package magit
  :doc "Git integration for Emacs"
  :ensure t
  :bind ("C-x g" . magit-status)
  )

(use-package projectile
  :doc "Project navigation"
  :ensure t
  :config
  ;; Use it everywhere
  (projectile-mode t)
  (setq projectile-mode-line "Projectile")
  :bind-keymap
  ("C-c p" . projectile-command-map)
  )

(use-package helm-projectile
  :doc "enable helm in projectile"
  :ensure t
  :config (helm-projectile-on)
  )

(use-package multiple-cursors
  :doc "Multiple cursors for Emacs."
  :ensure t
  :bind (("C-c m m" . mc/edit-lines)
         ("C-c m s" . mc/mark-next-like-this)
         ("C-c m a" . mc/mark-all-like-this))
  )

(use-package expand-region
  :doc "Expand text region for marking"
  :ensure t
  :bind ("C-c e" . er/expand-region)
  )

(use-package google-this
  :doc "call google on a text"
  :ensure t
  :bind ("C-M-g" . google-this)
  )

(use-package yaml-mode
  :doc "Major mode for editing YAML files"
  :ensure t
  )


(use-package org-superstar
  :ensure t
  :config (org-superstar-configure-like-org-bullets)
  )

(use-package org
  :doc "org mode settings"
  :ensure t
  :config
  (add-hook 'org-mode-hook
            (lambda()
              (setq org-hide-emphasis-markers t)
              (org-superstar-mode 1)
              (local-unset-key (kbd "C-j")))
            )
  )

(use-package imenu-list
  :doc "list functions in files"
  :ensure t
  )

(use-package groovy-mode
  :ensure t)

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t)
  )

(use-package company
  :doc "need for elpy for some reason"
  :ensure t
  )


(use-package helm-company
  :bind (:map company-mode-map
              ("C-'" . helm-company)
              :map company-active-map
              ("C-'" . helm-company))
  :ensure t)

(use-package pyenv-mode
  :doc "python virtual environent"
  :ensure t
  )

;; (use-package pyenv-mode-auto
;;   :doc "automatically switch pyenv mode"
;;   :ensure t
;;   )

(use-package flycheck
  :ensure t)

(use-package lsp-ui
  :doc "lsp user interface"
  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol nil)
  :ensure t)


(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook #'lsp))


;; I have mixed results with lsp-pyright and pyenv
;; lsp-python-ms seems to work right off the bat but its slower
;; need to look into direnv solution for pyenv
;; or pyvenv seems to be something
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))  ; or lsp-deferred


(use-package hydra
  :ensure t
  :config (hydra-add-font-lock))

(use-package origami
  :ensure t
  :bind ("C-c h o" . hydra-origami/body)
  :config
  (defhydra hydra-origami (:color red
                                  :hint nil)
    "
_t_: toggle    _r_: redo    _p_: prev        _c_: close all
_u_: undo      _n_: next    _o_: open all    _q_: quit
"
    ("t" origami-recursively-toggle-node)
    ("u" origami-undo)
    ("r" origami-redo)
    ("p" origami-previous-fold)
    ("n" origami-next-fold)
    ("o" origami-open-all-nodes)
    ("c" origami-close-all-nodes)
    ("q" nil "Quit" :color blue))

  (global-origami-mode))

;; lsp-origami provides support for origami.el using language server protocol’s
;; textDocument/foldingRange functionality.
;; https://github.com/emacs-lsp/lsp-origami/
;; (use-package lsp-origami
;;   :ensure t
;;   :hook ((lsp-after-open . lsp-origami-mode)))

(use-package bash-completion
  :ensure t
  :config (bash-completion-setup))

;; (use-package docker-tramp
;;   :doc "access docker files via tramp"
;;   :ensure t
;;   )

(use-package dockerfile-mode
  :doc "major mode for editing dockerfiles"
  :ensure t
  )

(use-package docker-compose-mode
  :doc "major mode for editing docker compose files"
  :ensure t
  )

(use-package restclient
  :doc "making rest apis from emacs"
  :ensure t
  )

(use-package restclient-helm
  :doc "helm interface to restclient"
  :ensure t
  )

(use-package vlf
  :doc "very large files"
  :ensure t
  )

(use-package logview
  :doc "viewing logs locally"
  :ensure t
  )
