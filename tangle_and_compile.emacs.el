#!/usr/bin/emacs --script
(when (file-newer-than-file-p "~/.emacs.d/configuration.org"
                              "~/.emacs.d/configuration.el")
  )
(load-file "./tangle.emacs.el")
(load-file "./compile.emacs.el")
