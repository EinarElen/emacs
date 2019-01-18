;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(defvar elib-tangle?
  (file-newer-than-file-p "~/.emacs.d/configuration.org"
                          "~/.emacs.d/configuration.el"))
(defvar elib-compile?
  nil)




(setq package-user-dir "~/.emacs.d/machine-local-files/elpa/")
(package-initialize)
(defvar debuginit-p nil)
(setq gc-cons-threshold 64000000)
(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name (concat
                          "~/.emacs.d/"
                          "machine-local-files"))))
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
(setq auto-save-list-file-prefix (concat user-emacs-directory "auto-save-list/.saves"))
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))
(require 'package)
;; (unless package--initialized (package-initialize))
;; (package-initialize)



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
  (and (not (daemonp))
       (or no-packages-installed?
           (not (package-installed-p 'all-the-icons)))
       )
  "Does `all-the-icons-install-fonts' need to be called?")



(defvar first-install-needs-pdf-tools?
  (and (not (daemonp))
       (or no-packages-installed?
           (not (package-installed-p 'pdf-tools))
           (not
            (and (boundp 'pdf-tools-directory)
	         (file-exists-p (concat pdf-tools-directory "epdfinfo")))
            )))
  "Has `pdf-tools-install' been successfully called?")



(unless (package-installed-p 'pdf-tools)
  (use-package pdf-tools))
(unless (package-installed-p 'all-the-icons)
  (use-package all-the-icons))


(if (not debuginit-p)
    (progn (when elib-tangle?
             (require 'ob)
             (org-babel-tangle-file "~/.emacs.d/configuration.org")
             (when elib-compile? (byte-compile-file "~/.emacs.d/configuration.el")))
           (let ((time (current-time)))
             (load-file "~/.emacs.d/configuration.el")
             (message "%f" (float-time (time-since time))))
           (when (string= (getenv "DESKTOP_SESSION") "gnome")
                                        ;(load-file "~/.emacs.d/scripts/gnome-server.el")
             ))
  (load-file "~/.emacs.d/scripts/debug-helper.el"))
(when first-install-needs-pdf-tools?
  (pdf-tools-install t))
(when first-install-needs-all-the-icons?
  (all-the-icons-install-fonts t))


;; (find-file config-file-file-name)
;; (delete-other-windows)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "/home/einarelen/.emacs.d/.abbrev_defs")
 '(abbrev-mode t t)
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d5b121d69e48e0f2a84c8e4580f0ba230423391a78fcb4001ccb35d02494d79e" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "28130127bbf3072c1bbc7652fca7245f186bb417b3b385a5e4da57b895ffe9d8" "bb749a38c5cb7d13b60fa7fc40db7eced3d00aa93654d150b9627cabd2d9b361" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec" default))
 '(flyspell-abbrev-p t)
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-mode 1 t)
 '(font-use-system-font t)
 '(org-agenda-files
   '("/home/einarelen/current_events.org" "/home/einarelen/.emacs.d/configuration.org" "/home/einarelen/nextcloud/org/main-gtd.org" "/home/einarelen/nextcloud/org/inbox@elfriede.org" "/home/einarelen/nextcloud/org/shopping.org" "/home/einarelen/nextcloud/org/tickler.org"))
 '(org-twbs-extension "html")
 '(package-selected-packages
   '(doom-themes org-plus-contrib spacemacs dmenu helm-exwm exwm mu4e-maildirs-extension mu4e-conversation mu4e-jump-to-list mu4e-alert helm-mu evil-tutor neotree treemacs esup macrostep expand-region multiple-cursors xah-replace-pairs nameless lorem-ipsum ein python-mode htmlize ox-reveal demo-it google-translate org-mime toc-org oauth2 org-caldav calfw-ical calfw-org calfw ob-ipython ox-twbs org-bullets cider lispy cmake-ide meson-mode cmake-mode clang-format web-mode flycheck company-auctex company-irony company-c-headers company latex-preview-pane auctex multi-term yasnippet-snippets eglot helm-projectile helm-swoop helm-dash helm-themes helm-descbinds helm-ag helm-c-yasnippet helm-google helm-gtags magit projectile dumb-jump avy-zap iedit anzu comment-dwim-2 smartparens undo-tree ws-butler dtrt-indent volatile-highlights which-key aggressive-indent rainbow-delimiters spacemacs-theme spaceline color-identifiers-mode restart-emacs default-text-scale transpose-frame ace-window hydra zygospore clipmon all-the-icons pdf-tools helm bind-key diminish use-package))
 '(pdf-view-midnight-colors '("#655370" . "#fbf8ef"))
 '(safe-local-variable-values
   '((eval defadvice org-babel-tangle
           (after change-script-modes activate)
           (let
               ((file-lst
                 (cddr
                  (directory-files "~/.emacs.d/scripts/" t))))
             (print file-lst)
             (dolist
                 (x file-lst)
               (chmod x 511))))
     (nameless-current-name . "meson-ide")
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
