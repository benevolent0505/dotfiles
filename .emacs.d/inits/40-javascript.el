;; For JavaScript
(el-get-bundle! js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'js2-mode-hook
             '(lambda ()
                (setq js2-basic-offset 2)
                (setq indent-tabs-mode nil)))
(add-hook 'js-mode-hook 'js2-minor-mode)

(el-get-bundle! tern)
(add-hook 'js2-mode-hook '(lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))
;; eslint
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
    ))
(setq js2-include-browser-externs nil)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-highlight-external-variables nil)
(setq js2-include-jslint-globals nil)

;; rjsx-mode
(el-get-bundle rjsx-mode)
(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
