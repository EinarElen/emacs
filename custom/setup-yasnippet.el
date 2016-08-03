
;; Package: yasnippet
;; GROUP: Editing -> Yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(set 'yas-verbosity 1)
(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
(provide 'setup-yasnippet)
