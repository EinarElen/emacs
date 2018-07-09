#!/usr/bin/emacs --script
(defvar debuginit-p nil)
(require 'package)
(unless package--initialized (package-initialize))
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
                                        ;(eval-when-compile (require 'use-package))
(setq use-package-debug nil)
                                        ;(setq use-package-verbose 'debug)
(setq use-package-verbose nil)
(setq use-package-always-ensure t)
                                        ;(byte-compile-file "orginit.el")
(byte-compile-file "configuration.el")

(check-declare-file "configuration.el")
