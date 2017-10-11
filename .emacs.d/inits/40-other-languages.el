(el-get-bundle! yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


;; sass & scss
(el-get-bundle! scss-mode
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))
(el-get-bundle! sass-mode
  (add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode)))
