;; ------------------------------------------------------------------------
;; @auto-complete
;; Emacs auto-complete package http://auto-complete.org
;; https://github.com/auto-complete/auto-complete
(el-get-bundle auto-complete
  (require 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'fundamental-mode)
  (add-to-list 'ac-modes 'org-mode)
  (ac-set-trigger-key "TAB")
  (setq ac-use-menu-map t)
  (setq ac-use-fuzzy t)
  (setq ac-use-comphist t)
  (setq ac-auto-show-menu 0.02))

(el-get-bundle popup)
(el-get-bundle fuzzy)
