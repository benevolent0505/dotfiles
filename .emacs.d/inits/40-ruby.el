
;; For Ruby
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(setq ruby-insert-encoding-magic-comment nil)

(el-get-bundle ruby-electric
  (require 'ruby-electric)
  (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
  (setq ruby-electric-expand-delimiters-list nil))
(el-get-bundle ruby-block
  (require 'ruby-block)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t))
(el-get-bundle inf-ruby
  (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))
(el-get-bundle robe-mode
  :type github
  :description "Code navigation, documentation lookup and completion for Ruby"
  :pkgname "dgutov/robe"
  :website "https://github.com/dgutov/robe"
  :depends (inf-ruby)
  :post-init (add-hook 'ruby-mode-hook 'robe-mode))
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'ac-robe-setup)
