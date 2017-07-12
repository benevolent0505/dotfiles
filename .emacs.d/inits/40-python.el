;; For Python Settings

;; Old python-mode repository is controlled by bzr. so if you show "command bzr not found" message, you should update el-get.
(el-get-bundle! python-mode)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))

(el-get-bundle! elpy
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")
  (add-hook 'elpy-mode-hook
    '(lambda ()
       ;; auto-complete停止
       (auto-complete-mode -1)

       ;; companyのキーバインド設定
       (define-key company-active-map (kbd "C-n") 'company-select-next)
       (define-key company-active-map (kbd "C-p") 'company-select-previous)
       (define-key company-active-map (kbd "<tab>") 'company-complete)

       ;; 保存時にautopep8実行
       (add-hook 'before-save-hook 'py-autopep8-before-save))))
