(el-get-bundle helm
  (global-set-key (kbd "M-x") 'helm-M-x)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  ;; For find-file etc.
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; For helm-find-files etc.
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list))
;; helm-ls-git
(el-get-bundle! helm-ls-git)
