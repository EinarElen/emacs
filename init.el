;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (add-to-list 'command-switch-alist `("--exwm" . #'init-emacs-exwm))
(defvar custom-org (expand-file-name
		    (concat user-emacs-directory "custom.org")))
(defvar custom-el (expand-file-name
		    (concat user-emacs-directory "custom.el")))
(defvar config-org (expand-file-name
		    (concat user-emacs-directory "configuration.org")))
(defvar config-el (expand-file-name
		    (concat user-emacs-directory "configuration.el")))
(defvar exwm-org (expand-file-name
		    (concat user-emacs-directory "init-exwm.org")))
(defvar exwm-el (expand-file-name
		   (concat user-emacs-directory "init-exwm.el")))


(defvar elib-tangle?
  (or (file-newer-than-file-p config-org
                              config-el)
      (file-newer-than-file-p custom-org
                              custom-el)
      (file-newer-than-file-p exwm-org
                              exwm-el)
      ))
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

                                (setq gc-cons-threshold 800000)
                                ))
 (require 'package)
 ;; (unless package--initialized (package-initialize))
 ;; (package-initialize)



 (setq package-enable-at-startup nil)
 (add-to-list 'package-archives
              '("melpa" . "https://melpa.org/packages/") t)
 (add-to-list 'package-archives
              '("org" . "https://orgmode.org/elpa/") t)
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
   (package-install 'org-plus-contrib)
   (use-package bind-key)
   (use-package diminish)
   (unless (package-installed-p 'helm)
     (use-package helm)))





(unless (daemonp)
   (save-window-excursion (unless (package-installed-p 'irony)
                            (use-package irony)
                            (call-interactively #'irony-install-server)))
   (save-window-excursion
     (unless (package-installed-p 'pdf-tools)
       (use-package pdf-tools)
       (pdf-tools-install t)))
   (print "\n")
   (save-window-excursion
     (unless (package-installed-p 'all-the-icons)
       (use-package all-the-icons)
       (all-the-icons-install-fonts t))))

 (if (not debuginit-p)
     (progn (when elib-tangle?
	      (package-install 'org-plus-contrib)
	      (require 'org)
              (require 'ob)
	      (org-babel-tangle-file custom-org)
              (org-babel-tangle-file config-org)
              (org-babel-tangle-file exwm-org)
              (when elib-compile? (byte-compile-file config-el))
	      (use-package restart-emacs)
	      (restart-emacs))
            (let ((time (current-time)))
              (load-file custom-el)
              (when (member "--exwm" command-line-args)
                (load-file exwm-el))
              (load-file elib-user-org-calendar-secrets-file)
              (load-file config-el)
              (message "%f" (float-time (time-since time))))
            (when (string= (getenv "DESKTOP_SESSION") "gnome")
                                        ;(load-file "~/.emacs.d/scripts/gnome-server.el")
              ))
   (load-file "~/.emacs.d/scripts/debug-helper.el"))



;; (find-file config-file-file-name)
;; (delete-other-windows)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(helm-external-programs-associations '(("pdf" . "evince")))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")
     ("???" . "#dc752f")))
 '(org-agenda-files
   '("~/ASTM14/PBL/PBLA.org" "/home/einarelen/Nextcloud/org/cal/gmail.org" "/home/einarelen/Nextcloud/org/phone.org" "/home/einarelen/Nextcloud/org/inbox@elfriede.org" "/home/einarelen/Nextcloud/org/gtd.org"))
 '(org-twbs-extension "html")
 '(package-selected-packages
   '(tab-bar-mode ob-async jupyter zmq simple-httpd swiper w3m with-editor yasnippet websocket emacs-websocket openwith pdf-annot ov all-the-icons-dired helm-org-rifle helm-rifle org-brain desktop-environment guix emacs-guix paredit general cquery el-mock helm-eshell helm-esh eshell-helm esh-autosuggest tco ox-beamer org-ref-bibtex org-ref ox-md ox-markdown demangle-mode ob-gnuplot elisp--witness--lisp org-mks cmake-font-lock org-evil evil evil-commands org-gcal lastpass noflet helm-xref lsp emacs-ccls ccls company-lsp lsp-ui lsp-mode company-irony-c-headers doom-themes org-plus-contrib spacemacs dmenu helm-exwm exwm mu4e-maildirs-extension mu4e-conversation mu4e-jump-to-list mu4e-alert helm-mu evil-tutor neotree treemacs esup macrostep expand-region multiple-cursors xah-replace-pairs nameless lorem-ipsum ein python-mode htmlize ox-reveal demo-it google-translate org-mime toc-org oauth2 org-caldav calfw-ical calfw-org calfw ox-twbs org-bullets cider lispy cmake-ide meson-mode cmake-mode clang-format web-mode flycheck company-auctex company-irony company-c-headers company latex-preview-pane auctex multi-term yasnippet-snippets eglot helm-projectile helm-swoop helm-dash helm-themes helm-descbinds helm-ag helm-c-yasnippet helm-google helm-gtags magit projectile dumb-jump avy-zap iedit anzu comment-dwim-2 smartparens undo-tree ws-butler dtrt-indent volatile-highlights which-key aggressive-indent rainbow-delimiters spacemacs-theme spaceline color-identifiers-mode restart-emacs default-text-scale transpose-frame ace-window hydra zygospore clipmon all-the-icons pdf-tools helm bind-key diminish use-package))
 '(pdf-view-midnight-colors '("#655370" . "#fbf8ef"))
 '(safe-local-variable-values
   '((org-use-property-inheritance . t)
     (eval defadvice org-babel-tangle
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
