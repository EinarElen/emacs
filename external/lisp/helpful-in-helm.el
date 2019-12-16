;;; helpful-in-helm ---- Add helpful action to helm-sources  -*- lexical-binding: t; -*-

;;; Usage:

;;; Activate with:
;; (use-package helpful
;;;  :init
;;;  (use-package helm
;;;    :init
;;;    (add-hook 'after-init-hook #'helpful-in-helm-activate)))

;;; ... then use M-x helm-apropos as normal. Press tab to see
;;; "Helpful" persistant action and press ENTER to select it.

;;; Commentary:

;;; Implementation: I don't understand how to add a persistent action
;;; to helm-elisp stuff, so I went with a advice-add style
;;; modification.

;;; Code:

(require 'helm-elisp)


(defvar helpful-in-helm--persistent-action-text
  "Helpful: View in helpful.")

(defvar helpful-in-help--helm-apropos-sources
  '(helm-def-source--emacs-commands
    helm-def-source--emacs-functions
    helm-def-source--eieio-classes
    helm-def-source--eieio-generic
    helm-def-source--emacs-variables
    helm-def-source--emacs-faces)
  "List of helm source generator functions that return a helm-source alist.
The command `helm-apropos' uses these sources. We will modify the
\"action\" key in the alist to include a helm persistent action
that will call `helpful'.

You should modify this if you modify `helm-apropos-function-list'.
FIXME: Should be linked instead of copied.")

;; command interface

;;;###autoload
(defun helpful-in-helm-activate ()
  "Activate the functionality.
See the docs in`helpful-in-help--helm-apropos-sources'.
FIXME: should be a minor mode?"
  (interactive)
  ;; first deactivate the advice, if any, then activate
  (helpful-in-helm--do-init :deactivate)
  (helpful-in-helm--do-init :activate))

(defun helpful-in-helm-deactivate ()
  "Deactivate the functionality.
See the docs in`helpful-in-help--helm-apropos-sources'."
  (interactive)
  (helpful-in-helm--do-init :deactivate))

;; lib (internals)

(defun helpful-in-helm--do-init (action)
  "Activate/deactivate based on ACTION."
  (mapcar
     (lambda (gensrcfun)
       (cond ((eql action :deactivate)
              (advice-remove gensrcfun #'helpful-in-helm--add-action-helpful))
             (t
              (advice-add gensrcfun :filter-return #'helpful-in-helm--add-action-helpful))))
     helpful-in-help--helm-apropos-sources))


(defun helpful-in-helm--add-action-helpful (orgfun-retval)
  "Add a persistent action to a helm source that will call helpful.

This advice function will :filter-return the original function's
return val ORGFUN-RETVAL."
  (prog1 orgfun-retval
    (let* ((src orgfun-retval)
           (insert-idx 0)) ;; insert at front
      (helm-add-action-to-source helpful-in-helm--persistent-action-text
                                 #'helpful-in-help--lookup-str-as-symbol
                                 src
                                 insert-idx))))

(defun helpful-in-help--lookup-str-as-symbol (str-sym)
  "Internal function to call `helpful-symbol'<f> on STR-SYM."
  (helpful-symbol (intern str-sym)))


(provide 'helpful-in-helm)
;;helpful-in-helm.el ends here
