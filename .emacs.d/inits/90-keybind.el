;; C-mにnewline-and-indentを割り当てる.初期値はnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)

;; "C-t" でウィンドウを切り替える.初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; "C-h"をdeleteに
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; window change
(global-set-key (kbd "C-x n") 'other-window)
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))
