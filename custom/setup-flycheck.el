(require 'flycheck)
(require 'company)
(setq flycheck-idle-change-delay 0.00001)
(global-flycheck-mode)

(when (featurep 'flycheck-rtags) (add-hook 'c-mode-common-hook 'another-flycheck-rtags-setup) (defun another-flycheck-rtags-setup()
                                                                                                (interactive)
                                                                                                (flycheck-select-checker 'rtags)
                                                                                                (setq-local flycheck-highlighting-mode nil)
                                                                                                (setq-local flycheck-check-syntax-automatically nil)
                                                                                                (rtags-enable-standard-keybindings)
                                                                                                ))

(when (featurep 'irony) (require 'flycheck-irony)
      (eval-after-load 'flycheck
        '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
      )

(provide 'setup-flycheck)
