
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
 '(org-agenda-files
   (quote
    ("~/.emacs.d/org/refile.org" "~/.emacs.d/configuration.org")))
 '(package-selected-packages
   (quote
    (org-plus-contrib helm-dash helm-descbinds org-beautify-theme zygospore yalinum ws-butler volatile-highlights undo-tree sr-speedbar solarized-theme smartparens powerline nyan-mode multi-term monokai-theme malinka magit latex-preview-pane iedit helm-swoop helm-projectile helm-gtags helm-c-yasnippet ggtags function-args flycheck-ycmd flycheck-pos-tip flycheck-irony exec-path-from-shell elscreen duplicate-thing dtrt-indent diminish company-ycmd company-irony-c-headers company-irony company-c-headers company-auctex comment-dwim-2 color-identifiers-mode cmake-ide clean-aindent-mode clang-format avy-zap anzu ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
