;; ------------------------------------------------------------------------
;; @ flycheck
;; modern on-the-fly syntax checking extension.
;; https://github.com/flycheck/flycheck
(el-get-bundle flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
