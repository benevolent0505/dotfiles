;; For Python Settings
;; Refference from http://blog.shibayu36.org/entry/2017/04/02/193000

;; Old python-mode repository is controlled by bzr. so if you show "command bzr not found" message, you should update el-get.
(el-get-bundle! python-mode)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))

;; after this, you should run "M-x jedi:install-server"
(el-get-bundle! jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; 一応残す
;; (el-get-bundle! virtualenvwrapper)
;; (el-get-bundle! auto-virtualenvwrapper)
;; (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
