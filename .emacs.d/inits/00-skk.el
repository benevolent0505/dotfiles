;; AquaSKK setting
(if (eq system-type 'darwin)
    (setq skk-server-portnum 1178))
(if (eq system-type 'darwin)
    (setq skk-server-host "localhost"))

;; ddskk
(setq-default skk-user-directory "~/.ddskk")
(el-get-bundle ddskk)
(global-set-key (kbd "C-j") 'skk-mode)

(setq-default dired-bind-jump nil)
(add-hook 'isearch-mode-hook
          (function (lambda ()
                      (and (boundp 'skk-mode) skk-mode
                           (skk-isearch-mode-setup)))))
(add-hook 'isearch-mode-end-hook
          (function (lambda ()
                      (and (boundp 'skk-mode) skk-mode (skk-isearch-mode-cleanup))
                      (and (boundp 'skk-mode-invoked) skk-mode-invoked
                           (skk-set-cursor-properly)))))
