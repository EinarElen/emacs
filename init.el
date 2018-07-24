;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(defvar debuginit-p nil)
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))
(require 'package)
;; (unless package--initialized (package-initialize))
(package-initialize)



(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(setq use-package-debug nil)
(setq use-package-verbose nil)
(setq use-package-always-ensure t)
(defvar no-packages-installed?
  (not (file-exists-p (concat user-emacs-directory
                              "elpa")))
  "Does the .emacs.d/elpa directory exist?")
(eval-when-compile
  (unless (and (not no-packages-installed?)
               (package-installed-p 'use-package)
               (package-installed-p 'diminish)
               (package-installed-p 'bind-key))
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish)
    (package-install 'bind-key))
  (require 'use-package)
  (use-package bind-key)
  (use-package diminish)
  (unless (package-installed-p 'helm)
    (use-package helm)))

(defvar first-install-needs-all-the-icons?
  (or no-packages-installed?
      (not (package-installed-p 'all-the-icons)))
  "Does `all-the-icons-install-fonts' need to be called?")
(defvar first-install-needs-pdf-tools?
  (or no-packages-installed?
      (not (package-installed-p 'pdf-tools))
      (not
       (and (boundp 'pdf-tools-directory)
	    (file-exists-p (concat pdf-tools-directory "epdfinfo")))
       ))
  "Has `pdf-tools-install' been successfully called?")



(unless (package-installed-p 'pdf-tools)
  (use-package pdf-tools))
(unless (package-installed-p 'all-the-icons)
  (use-package all-the-icons))


(if (not debuginit-p)
    (progn (if  (file-newer-than-file-p "~/.emacs.d/configuration.org"
                                        "~/.emacs.d/configuration.el")
               (progn
                 (require 'ob)
                 (org-babel-tangle-file "~/.emacs.d/configuration.org")
                 (byte-compile-file "~/.emacs.d/configuration.el")))
           (let ((time (current-time)))
             (load-file "~/.emacs.d/configuration.el")
             ;;(require 'elib-configuration "~/.emacs.d/configuration")
             ;; (with-current-buffer (get-buffer "*scratch*")
             ;;   (insert (format "Setup took %f seconds!\n" (float-time (time-since time))))
             ;;   (let ((buf (or (get-buffer "*warnings*")
             ;;                  (get-buffer "*use-package*"))))
             ;;     (if (and debug-on-error buf)
             ;;         (progn
             ;;           (insert (format "Debug information from use-package:\n"))
             ;;           (insert-buffer (get-buffer "*use-package*"))))))
             (message "%f" (float-time (time-since time))))
           (when (string= (getenv "DESKTOP_SESSION") "gnome")
                                        ;(load-file "~/.emacs.d/gnome-server.el")
             ))
  (load-file "~/.emacs.d/debug-helper.el"))
(when first-install-needs-pdf-tools?
  (pdf-tools-install t))
(when first-install-needs-all-the-icons?
  (all-the-icons-install-fonts t))
(find-file config-file-file-name)
(delete-other-windows)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "/home/einarelen/.emacs.d/.abbrev_defs")
 '(abbrev-mode t t)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d5b121d69e48e0f2a84c8e4580f0ba230423391a78fcb4001ccb35d02494d79e" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "28130127bbf3072c1bbc7652fca7245f186bb417b3b385a5e4da57b895ffe9d8" "bb749a38c5cb7d13b60fa7fc40db7eced3d00aa93654d150b9627cabd2d9b361" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec" default))
 '(flyspell-abbrev-p t)
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-mode 1 t)
 '(font-use-system-font t)
 '(org-twbs-extension "html")
 '(package-selected-packages
   '(org-caldav restart-emacs calfw-ical ox-reveal lispy default-text-scale leuven helm-grep bind-key helm-flyspell flyspell-helm abbrev-mode abbrev oauth2 ouath2 helm-mu notmuch wl wanderlust bbdb mu4e-multi mu4e-jump-to-list mu4e-alert mu4e-conversation mu4e-maildirs-extension eglot el-mock google-translate aggressive-indent rainbow-delimiters rainbow-delimeters which-key langtool graphviz-dot-mode async-await atom-dark-theme auctex-lua writegood-mode writeroom-mode org-evil evil treemacs-projectile treemacs slime-company elisp-slime-nav-mode slime ob org-agenda tex-buf company-clang TeX-save-query helm-files helm-command helm-elisp helm-ag helm-apropos helm-google whitespace-mode helm-rtags flycheck-rtags ob-async transpose-frame ace-window hydra org-gcal calfw-gcal mu4e-org epa-file offlineimap mu4e-contrib helm-purpose window-purpose swiper-helm nameless em-smart cask-mode buttercup flycheck company-rtags pp-c-l highlight-cl eldoc-extension elisp-slime-nav redshank paredit-everywhere paredit-menu auto-complete-clang paredit meson-mode xah-replace-pairs multiple-cursors expand-region info+ all-the-icons winum eyebrowse persp-projectile persp-mode perspective spaceline-all-the-icons spacemacs-theme helm-themes blackboard-theme counsel cpputils-cmake org-beautify-theme ox-twbs cmake-font-lock cmake-project cmake-mode dumb-jump exwm ein glsl-mode wolfram fold-dwim rtags pdf-tools srefactor macrostep ox-latex calfw-org latex latexx clipmon use-package calfw auctex-latexmk nlinum gnuplot ob-C htmlize web-mode hideshowvis hyperbole zoom-frm twittering-mode helm-dash helm-descbinds zygospore yalinum ws-butler volatile-highlights undo-tree sr-speedbar solarized-theme smartparens powerline nyan-mode multi-term monokai-theme malinka magit latex-preview-pane iedit helm-swoop helm-projectile helm-gtags ggtags function-args flycheck-ycmd flycheck-pos-tip flycheck-irony exec-path-from-shell elscreen duplicate-thing dtrt-indent diminish company-ycmd company-irony-c-headers company-irony company-c-headers company-auctex comment-dwim-2 color-identifiers-mode cmake-ide clean-aindent-mode clang-format avy-zap anzu ace-jump-mode))
 '(safe-local-variable-values
   '((nameless-current-name . "meson-ide")
     (eval progn
           (require 'projectile)
           (puthash
            (projectile-project-root)
            "cask exec buttercup -L ." projectile-test-cmd-map))))
 '(send-mail-function 'mailclient-send-it))

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
