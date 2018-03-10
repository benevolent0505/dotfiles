(el-get-bundle ansible
  :type github
  :pkgname "k1LoW/emacs-ansible"
  :depends (s f)
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

(el-get-bundle jinja2-mode
  (add-to-list 'auto-mode-alist '(".*\\.j2\\'" . jinja2-mode)))
