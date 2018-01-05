;; C-mにnewline-and-indentを割り当てる.初期値はnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)

;; "C-h"をdeleteに
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
