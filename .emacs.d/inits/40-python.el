;; For Python Settings

;; Old python-mode repository is controlled by bzr. so if you show "command bzr not found" message, you should update el-get.
(el-get-bundle! python-mode)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))

(el-get-bundle! elpy
  (elpy-enable))
