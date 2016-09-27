
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec" default)))
 '(font-use-system-font t)
 '(org-agenda-files
   (quote
    ("~/ownCloud/org/cal/main.org" "~/.emacs.d/org/refile.org" "~/.emacs.d/configuration.org")))
 '(org-babel-load-languages (quote ((java . t) (python . t) (emacs-lisp . t) (C . t))))
 '(org-twbs-extension "shtml")
 '(package-selected-packages
   (quote
    (calfw auctex-latexmk nlinum pdf-tools gnuplot ob-C org-gcal htmlize ox-twbs web-mode hideshowvis hyperbole zoom-frm twittering-mode org-plus-contrib helm-dash helm-descbinds org-beautify-theme zygospore yalinum ws-butler volatile-highlights undo-tree sr-speedbar solarized-theme smartparens powerline nyan-mode multi-term monokai-theme malinka magit latex-preview-pane iedit helm-swoop helm-projectile helm-gtags helm-c-yasnippet ggtags function-args flycheck-ycmd flycheck-pos-tip flycheck-irony exec-path-from-shell elscreen duplicate-thing dtrt-indent diminish company-ycmd company-irony-c-headers company-irony company-c-headers company-auctex comment-dwim-2 color-identifiers-mode cmake-ide clean-aindent-mode clang-format avy-zap anzu ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
