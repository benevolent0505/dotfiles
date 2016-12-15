(el-get-bundle! go-mode-autoloads in go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

(el-get-bundle! go-autocomplete)
(el-get-bundle! go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "green"
                    :weight 'bold)

(add-to-list 'load-path "~/develop/go/src/github.com/golang/lint/misc/emacs")
(require 'golint)
