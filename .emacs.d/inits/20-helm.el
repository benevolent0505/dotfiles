(el-get-bundle helm)

(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode +1)
(helm-autoresize-mode 1)
(setq-default helm-truncate-lines t)

;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)

;; helm-ls-git
(el-get-bundle! helm-ls-git)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)

;; helm-git-grep
(el-get-bundle! helm-git-grep)
(global-set-key (kbd "C-c g") 'helm-git-grep)

;; helm-ghq
(el-get-bundle helm-ghq)
(add-to-list 'exec-path "~/develop/bin")
(add-to-list 'helm-for-files-preferred-list 'helm-source-ghq)
(global-set-key (kbd "C-x C-g") 'helm-ghq)
