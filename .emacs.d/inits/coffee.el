(el-get-bundle! coffee-mode
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))

  (custom-set-variables
    '(coffee-tab-width 2)
    '(coffee-args-compile '("-c" "--no-header" "--bare")))

  (add-hook 'coffee-mode
    '(lambda ()
       (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
       (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent))))
