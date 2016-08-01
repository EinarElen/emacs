(require 'ycmd)
(require 'company-ycmd)
(require 'flycheck-ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)
;(add-hook 'c-mode-hook 'ycmd-mode-hook)
(set-variable 'ycmd-server-command '("python" "/home/einarelen/src/ycmd/ycmd/"))
(company-ycmd-setup)
(flycheck-ycmd-setup)
(provide 'setup-ycmd)
