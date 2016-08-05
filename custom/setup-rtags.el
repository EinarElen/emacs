
(setq rtags-use-helm t)
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(global-company-mode t)

(add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)

(provide 'setup-rtags)
