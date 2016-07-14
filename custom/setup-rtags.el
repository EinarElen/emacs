
(require 'flycheck)
(require 'flycheck-rtags)
(setq rtags-use-helm t)
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(global-company-mode t)



(defun another-flycheck-rtags-setup()
  (interactive)
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil)
  (rtags-enable-standard-keybindings)
  )
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

(require 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(require 'cmake-ide)
(cmake-ide-setup)

(add-hook 'c-mode-common-hook 'another-flycheck-rtags-setup)
(add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
(setq company-idle-delay 0.0001)
(setq company-tooltip-idle-delay 0.0001)
(setq flycheck-idle-change-delay 0.0001)
;(define-key c-mode-map [(tab)] 'company-complete)
;(define-key c++-mode-map [(tab)] 'company-complete)
(define-key c-mode-map (kbd "TAB") 'company-complete)
(define-key c++-mode-map (kbd "TAB") 'company-complete)

(provide 'setup-rtags)
