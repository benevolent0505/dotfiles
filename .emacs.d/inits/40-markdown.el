;; For Markdown
(el-get-bundle! markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; markdown-mdlのlintを取り敢えずoff
(add-hook 'markdown-mode-hook
          '(lambda ()
             (setq flycheck-disabled-checkers '(markdown-mdl))
             (flycheck-mode 1)))

