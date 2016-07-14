(require 'malinka)

(malinka-define-project
 :name "achelor"
 :root-directory "/home/einarelen/achelor/code"
 :build-directory "/home/einarelen/achelor/code"
 :configure-cmd "bear --append make"
 :compile-db-cmd "bear --append make"
 :test-cmd "make clean && make"
 :compile-cmd "make -k"
 :run-cmd "./main --projectfiles=true --silent"
 )

(malinka-define-project
 :name "BACHELOR"
 :root-directory "/home/einarelen/BACHELOR/code"
 :build-directory "/home/einarelen/BACHELOR/code"
 :configure-cmd "make clean && bear --append make"
 :compile-db-cmd "make clean && bear --append make"
 :test-cmd "make clean && make"
 :compile-cmd "make -k"
 :run-cmd "./main --projectfiles=true --silent"
 )

(add-hook 'c-mode-common-hook 'malinka-mode)
(setq malinka-enable-idle-project-check t)
(setq malinka-idle-project-check-seconds 0.001)
(setq malinka-print-debug? nil)
(setq malinka-print-info? nil)
(setq malinka-print-warning? nil)
(setq malinka-print-xdebug? nil)


(provide 'malinka-projects)
