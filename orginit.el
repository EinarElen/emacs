(require 'package)
(require 'cl)
(setq gc-cons-threshold 64000000)
(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name (concat
                          "~/.emacs.d/"
                          "machine-local-files"))))
;(setq package-enable-at-startup nil)

(setq package-user-dir "~/.emacs.d/machine-local-files/elpa/")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(require 'use-package)




(setq use-package-debug t)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq org-export-async-debug t)
(use-package org)
(use-package ox)
(use-package ox-twbs)
(use-package ox-latex)
(use-package ox-html)
(use-package ox-odt)
(use-package ox-reveal)
