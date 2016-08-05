(require 'irony)
(require 'company-irony)
(defun my-irony-mode-hook()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(require 'company-irony-c-headers)

(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))



(provide 'setup-irony)
