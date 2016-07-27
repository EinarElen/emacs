
(set 'frame-title-format "Emacs")
(set 'visible-bell t)
(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(set-default 'cursor-type 'hbar)
(set 'column-number-mode t)
(set 'line-number-mode t)
                                        ;(set 'show-paren-mode t)

(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)
(set-face-attribute 'default nil :height 140)
(defconst demo-packages
  '(magit
    ycmd
    exec-path-from-shell
    powerline
    company-ycmd
    flycheck-ycmd
    yalinum
    flycheck-irony
    solarized-theme
    company-c-headers
    malinka
    flycheck
    flycheck-pos-tip
    avy
    avy-zap
    anzu
    company
    duplicate-thing
    ggtags
    helm
    helm-gtags
    clang-format
    helm-projectile
    helm-swoop
    function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    color-identifiers-mode
    iedit
    smartparens
    projectile
    irony
    company-irony
    company-irony-c-headers
    volatile-highlights
    undo-tree
    diminish
    auctex
    company-auctex
    helm-c-yasnippet
    monokai-theme
    zygospore
    helm-swoop
    ))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)
(require 'powerline)
(powerline-vim-theme)
(require 'diminish)

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")
(when (not (string= system-type "windows-nt"))
           (require 'magit)
           (global-set-key (kbd "C-x g") 'magit-status))
(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 4
 version-control t)


(add-to-list 'load-path "~/.emacs.d/custom")
(global-yalinum-mode t)
(require 'setup-helm)
;;(require 'setup-helm-gtags)
;; (require 'setup-ggtags)
;;(require 'setup-cedet)
(require 'setup-editing)
(require 'cc-mode)

(windmove-default-keybindings)

;; function-args
(require 'function-args)
(fa-config-default)


;;(require 'malinka-projects)
;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq
 c-default-style "stroustrup" ;; set style to "linux"
 )

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: yasnippet
(require 'yasnippet)
(add-to-list 'yas/root-directory "/home/einarelen/.emacs.d/tuhdosnippets")
(add-hook 'c-mode-common-hook 'yas-minor-mode-hook)

;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Package: projejctile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)


;; (require 'setup-rtags)
(require 'clang-format)
(define-key c++-mode-map (kbd "C-c f") 'clang-format-region)
(define-key c++-mode-map (kbd "C-c C-f") 'clang-format-buffer)
(define-key c-mode-map (kbd "C-c f") 'clang-format-region)
(define-key c-mode-map (kbd "C-c C-f") 'clang-format-buffer)
(add-hook 'after-init-hook 'global-color-identifiers-mode)
(require 'lastpass)
(diminish 'anzu-mode)
(diminish 'projectile-mode)
(diminish 'undo-tree-mode)
(diminish 'color-identifiers-mode)
(diminish 'ws-butler-mode)
(diminish 'smartparens-mode)
(diminish 'volatile-highlights-mode)
(diminish 'auto-revert-mode)
(diminish 'ycmd-mode)
(diminish 'company-mode)
(diminish 'helm-mode)
(diminish 'malinka-mode)
(diminish 'abbrev-mode)
(diminish 'hs-minor-mode)
(diminish 'function-args-mode)

(require 'setup-company)
(require 'setup-org)
(require 'setup-ycmd)
(require 'setup-irony)
(require 'setup-flycheck)
(require 'avy)
(require 'avy-zap)
(setq avy-all-windows nil)
                                        ;(setq avy-all-windows 'all-frames)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-;") 'avy-goto-word-1)
(global-set-key (kbd "M-;") 'avy-goto-line)

(add-hook 'text-mode-hook 'auto-fill-mode)
(require 'choose-random-theme)
(choose-random-theme)
(diminish 'color-identifiers-mode)
(diminish 'yas-minor-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (avy-zap avy monokai-theme helm-c-yasnippet yasnippet company-auctex auctex diminish powerline zygospore yalinum ws-butler volatile-highlights undo-tree solarized-theme smartparens malinka magit iedit helm-swoop helm-projectile helm-gtags ggtags function-args flycheck-ycmd flycheck-pos-tip flycheck-irony duplicate-thing dtrt-indent company-ycmd company-irony-c-headers company-irony company-c-headers comment-dwim-2 color-identifiers-mode cmake-ide clean-aindent-mode clang-format anzu ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
