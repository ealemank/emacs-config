(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;;load custom lisp functions
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "custom_utils.el")

;;set font size default to larger than usual
(set-face-attribute 'default nil :height 150)
;;initial window size
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 160))

;;get rid of welcome screen
(setq inhibit-startup-screen t)

;;shell settings
;;changing the tab for pcomplete
;;this is due to compatibility with helm mode
;;(add-hook 'shell-mode-hook 'pcomplete-shell-setup)
;;(add-hook 'shell-mode-hook
;;  (lambda ()
;;    (define-key shell-mode-map "\t" 'pcomplete)))

;;stop asking if you really want to kill buffer
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
;;set bash for shell.  Mac has a custom bash install from brew
(if (file-exists-p "/usr/local/bin/bash")
    (setq explicit-shell-file-name "/usr/local/bin/bash"))
;;Loading default shells.  I always use them
(shell "build_shell")
(shell "temp_shell")

;;Changing the term escape character
(add-hook 'term-mode-hook
  (lambda ()
    ;; C-x is the prefix command, rather than C-c
    (term-set-escape-char ?\C-x)
    (define-key term-raw-map "\M-x" 'helm-M-x)
    (define-key term-raw-map "\C-x\C-y" 'term-paste)
    (define-key term-mode-map "\C-x\C-k" 'term-char-mode)
    (define-key term-mode-map "\C-a" 'term-send-raw)))
    
;;vterm
(setq vterm-term-environment-variable "eterm-color")

;;backups to one place
(setq backup-directory-alist '(("." . "~/.emacs.d/emacs-backup")))

;;treats non-space characters as words
(global-superword-mode 1)

;;string rectangle to append first letter instead of replacing it
(global-set-key (kbd "C-x r t") 'string-insert-rectangle)

;;helm settings
;;(setq helm-command-prefix-key "C-c h")
;;Current helm command prefix-key is "C-x c"
;;helm-mode seems to disrupt the shell buffers
;;so currently disabled it.
;;it can be enabled anytime to typing M-x helm-mode
;;(helm-mode 1) 
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
;;(global-set-key (kbd "C-h f") 'helm-describe-function)
;;(global-set-key (kbd "C-h k") 'helm-describe-key)
(global-set-key (kbd "C-<tab>") 'completion-at-point)

;;ibuffer much better than buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;ace-jump-helm-line
(eval-after-load "helm"
  '(define-key helm-map (kbd "C-'") 'ace-jump-helm-line))

;;Theme
(load-theme 'spacemacs-dark t)

;;pyenv mode unbind C-c C-s so that elpy can get it
(eval-after-load "pyenv-mode"
  '(define-key pyenv-mode-map (kbd "C-c C-s") nil))

;;flycheck config
(setq flycheck-python-pycompile-executable "~/.pyenv/versions/emacs/bin/python")
(setq flycheck-python-flake8-executable "~/.pyenv/versions/emacs/bin/python")
(setq flycheck-python-mypy-executable "~/.pyenv/versions/emacs/bin/mypy")
(setq flycheck-python-pylint-executable "~/.pyenv/versions/emacs/bin/pylint")
;;elpy config
(setq elpy-rpc-virtualenv-path "~/.pyenv/versions/emacs")
;;(setq elpy-rpc-virtualenv-path 'current)
(setq elpy-rpc-python-command "~/.pyenv/versions/emacs/bin/python")
(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;Ace-Jump-Mode settings
(define-key global-map (kbd "C-j") 'ace-jump-mode)
(define-key global-map (kbd "C-c j") 'ace-jump-mode-pop-mark)

;;projectile settings
(projectile-mode)
(helm-projectile-on)
;;this line was added because tramp was kind of slow
;;on ssh connections
(setq projectile-mode-line "Projectile")
(setq projectile-completion-system (quote helm))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;tramp settings
(setq password-cache-expiry nil)

;;robot-mode
(load "robot-mode.el")
(add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))
(add-to-list 'auto-mode-alist '("\\.jenkins\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;Helm-Swoop
(global-set-key (kbd "M-i") 'helm-swoop)

;;Multiple-cursors
(global-set-key (kbd "C-c m m") 'mc/edit-lines)
(global-set-key (kbd "C-c m s") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)

;;Expand-region
(global-set-key (kbd "C-c e") 'er/expand-region)

;;google-this settings
(global-set-key (kbd "C-x g") 'google-this-mode-submap)

;;Ace Window
;;(global-set-key (kbd "C-x o") 'ace-window)
;;(global-set-key (kbd "M-p") 'ace-window)

;;change default window switches
;;from: https://emacs.stackexchange.com/a/3471
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-,") #'prev-window)
(defun prev-window ()
  (interactive)
  (other-window -1))

;;Save settings for later use
(desktop-save-mode 1)
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'kill-ring)
;;(setq desktop-files-not-to-save "^$")

;;dired settings.
;; Remember to navigate files with a instead of enter key
(put 'dired-find-alternate-file 'disabled nil)

;;Org-mode settings
(add-hook 'org-mode-hook
          (lambda()
            (local-unset-key (kbd "C-j"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (forge flycheck pyenv-mode markdown-mode helm-descbinds dockerfile-mode docker-tramp eterm-256color vterm pcomplete-extension ox-gfm multishell groovy-mode ace-window imenu-list google-this expand-region multiple-cursors helm-swoop ace-jump-helm-line magit-gerrit magit spacemacs-theme helm-projectile elpy ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
