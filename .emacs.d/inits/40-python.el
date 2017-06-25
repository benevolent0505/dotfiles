;; For Python Settings

;; Old python-mode repository is controlled by bzr. so if you show "command bzr not found" message, you should update el-get.
(el-get-bundle! python-mode)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))

(el-get-bundle! elpy
  (elpy-enable)
  (add-hook 'elpy-mode-hook
    '(lambda ()
       (auto-complete-mode -1)
       (define-key company-active-map (kbd "C-n") 'company-select-next)
       (define-key company-active-map (kbd "C-p") 'company-select-previous)
       (define-key company-active-map (kbd "<tab>") 'company-complete))))
