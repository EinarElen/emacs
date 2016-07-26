
;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
(define-key c-mode-map (kbd "TAB") 'company-complete)
(define-key c++-mode-map (kbd "TAB") 'company-complete)
;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)
(global-company-mode t)



(when (featurep 'yasnippet)
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (global-set-key (kbd "C-c y") 'company-yasnippet)
  )
(semantic-mode -1)
(setq company-idle-delay 0.0001)
(setq company-tooltip-idle-delay 0.0001)
(when (string= system-name "arch-desktop") (add-to-list 'company-c-headers-path-system "/usr/include/c++/6.1.1/"))
(when (string= system-name "virtualbox") (add-to-list 'company-c-headers-path-system "/usr/lib64/gcc/x86_64-pc-linux-gnu/4.9.3/include/g++-v4/")
      (
       add-to-list 'company-c-headers-path-system "/usr/lib64/gcc/x86_64-pc-linux-gnu/4.9.3/include/")
      )
(provide 'setup-company)
