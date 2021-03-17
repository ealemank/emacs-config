(setq boe_lab_list '(("example_server" . "example_server_shell")
		     ))

(defun ssh_term (host name)
  "terminal with ssh connection.
the name of the buffer will have the
connection name"
  (interactive "shost:\nsname:")
  (if (get-buffer name)
      (message "terminal already exists")
    (progn
      (if (file-exists-p "/usr/local/bin/bash")
	  (term "/usr/local/bin/bash")
	(term "/bin/bash"))
      (rename-buffer name)
      (setq proc (get-buffer-process (current-buffer)))
      (setq sshcmd (concat "ssh " host))
      (term-send-string proc sshcmd)
      (term-send-string proc "\n"))))

(defun ssh_vterm (host name)
  "vterm with ssh connection.
the name of the buffer will have the
connection name"
  (interactive "shost:\nsname:")
  (if (get-buffer name)
      (message "terminal already exists")
    (progn
      (vterm name)
      (vterm-send-string (concat "ssh " host))
      (vterm-send-return)
      (vterm_set_prompt host))))

(defun vterm_set_prompt (&optional host)
  (interactive)
  (setq currhost "$(hostname)")
  (when host
    (setq currhost host))
  ;;(vterm-send-string "export PS1=$PS1\'\\[\\e]51;A$(whoami)@$(hostname):$(pwd)\\e\\\\\\]'")
  (vterm-send-string (concat "export PS1=$PS1\'\\[\\e]51;A$(whoami)@" currhost ":$(pwd)\\e\\\\\\]'"))
  (vterm-send-return))


(defun open_ssh_term_all()
  (interactive)
  (dolist (target boe_lab_list)
    (ssh_term (car target) (cdr target))))

(defun open_ssh_vterm_all()
  (interactive)
  (dolist (target boe_lab_list)
    (ssh_vterm (car target) (cdr target))))

(defun open_tramp(target)
  (interactive "starget:")
  (find-file (concat "/-:" target ":")))

(defun open_tramp_all()
  (interactive)
  (dolist (target boe_lab_list)
    (open_tramp (car target))))
