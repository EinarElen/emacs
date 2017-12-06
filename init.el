;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(defvar debuginit-p nil)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
                                        ;(eval-when-compile (require 'use-package))
(setq use-package-debug nil)
                                        ;(setq use-package-verbose 'debug)
(setq use-package-verbose nil)
(setq use-package-always-ensure t)
(if (not debuginit-p)
    (progn (if (file-newer-than-file-p "~/.emacs.d/configuration.org"
                                 "~/.emacs.d/configuration.el")
         (progn
           (require 'ob)
           (org-babel-tangle-file "~/.emacs.d/configuration.org")
           (byte-compile-file "~/.emacs.d/configuration.el")))
     (let ((time (current-time)))
       (load-file "~/.emacs.d/configuration.el")
       ;; (with-current-buffer (get-buffer "*scratch*")
       ;;   (insert (format "Setup took %f seconds!\n" (float-time (time-since time))))
       ;;   (let ((buf (or (get-buffer "*warnings*")
       ;;                  (get-buffer "*use-package*"))))
       ;;     (if (and debug-on-error buf)
       ;;         (progn
       ;;           (insert (format "Debug information from use-package:\n"))
       ;;           (insert-buffer (get-buffer "*use-package*"))))))
       (message "%f" (float-time (time-since time)))
       )
     (when (string= (getenv "DESKTOP_SESSION") "gnome")
       ;(load-file "~/.emacs.d/gnome-server.el")
       )
     )
  (load-file "~/.emacs.d/debug-helper.el"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d5b121d69e48e0f2a84c8e4580f0ba230423391a78fcb4001ccb35d02494d79e" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "28130127bbf3072c1bbc7652fca7245f186bb417b3b385a5e4da57b895ffe9d8" "bb749a38c5cb7d13b60fa7fc40db7eced3d00aa93654d150b9627cabd2d9b361" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec" default)))
 '(font-use-system-font t)
 '(org-agenda-files
   (quote
    ("~/ownCloud/org/cal/main.org" "~/.emacs.d/configuration.org")))
 '(org-twbs-extension "shtml")
 '(package-selected-packages
   (quote
    (slime-company elisp-slime-nav-mode slime ob org-agenda tex-buf company-clang TeX-save-query helm-files helm-command helm-elisp helm-ag helm-apropos helm-google whitespace-mode helm-rtags flycheck-rtags ob-async transpose-frame ace-window hydra org-gcal calfw-gcal mu4e-org epa-file offlineimap mu4e-contrib helm-purpose window-purpose swiper-helm nameless em-smart cask-mode buttercup flycheck company-rtags pp-c-l highlight-cl eldoc-extension elisp-slime-nav redshank paredit-everywhere paredit-menu auto-complete-clang paredit meson-mode xah-replace-pairs multiple-cursors expand-region info+ all-the-icons winum eyebrowse persp-projectile persp-mode perspective spaceline-all-the-icons spacemacs-theme helm-themes blackboard-theme counsel cpputils-cmake org-beautify-theme ox-twbs cmake-font-lock cmake-project cmake-mode dumb-jump exwm ein glsl-mode wolfram fold-dwim org rtags pdf-tools srefactor macrostep ox-latex calfw-org latex latexx clipmon use-package calfw auctex-latexmk nlinum gnuplot ob-C htmlize web-mode hideshowvis hyperbole zoom-frm twittering-mode helm-dash helm-descbinds zygospore yalinum ws-butler volatile-highlights undo-tree sr-speedbar solarized-theme smartparens powerline nyan-mode multi-term monokai-theme malinka magit latex-preview-pane iedit helm-swoop helm-projectile helm-gtags helm-c-yasnippet ggtags function-args flycheck-ycmd flycheck-pos-tip flycheck-irony exec-path-from-shell elscreen duplicate-thing dtrt-indent diminish company-ycmd company-irony-c-headers company-irony company-c-headers company-auctex comment-dwim-2 color-identifiers-mode cmake-ide clean-aindent-mode clang-format avy-zap anzu ace-jump-mode)))
 '(safe-local-variable-values
   (quote
    ((nameless-current-name . "meson-ide")
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "cask exec buttercup -L ." projectile-test-cmd-map))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
