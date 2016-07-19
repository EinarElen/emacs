(defun lp-login (login-name)
  "Testing"
  (interactive "sLastpass account: ")
  (shell-command (concat "lpass login " login-name)))

(defun lp-ls
    (&optional args &optional output-buffer &optional error-buffer)
  "Derp"
  (interactive "s(Optional) Groupname:
s(Optional) Output buffer: ")
  (if (string= output-buffer "")
      (shell-command (concat "lpass ls " args))
    (shell-command (concat "lpass ls " args) output-buffer error-buffer)))

(defun lp-show (name &optional output-buffer &optional error-buffer)
  "darp"
  (interactive "sName: ")
  (if (string= output-buffer "") (shell-command (concat "lpass show" name))(shell-command (concat "lpass show " name) output-buffer error-buffer)))

(defun lp-insert-show (name &optional)
  "dlarp"
  (interactive "sName: ") (lp-show name t))
(defun lp-insert-ls (&optional args)
  "Derp"
  (interactive "s(Optional) Groupname:") (lp-ls args t))

(defun lp-get-password (name &optional output-buffer &optional error-buffer)
  (interactive "sName: ")
  (lp-show (concat name "| grep password | grep -v sudo | cut -d\" \" -f2 ") output-buffer error-buffer))

(defun lp-insert-password (name)
  (interactive "sName: ")
  (lp-get-password name t)
  )

(provide 'lastpass)
