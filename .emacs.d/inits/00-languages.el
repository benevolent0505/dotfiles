;; language setting
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

;; Windowsで英数と日本語にMeiryoを指定
;; Macで英数と日本語にRictyを指定
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-face-attribute 'default nil
                             :family "Meiryo" ;;英数
                             :height 100)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo"))) ;; 日本語
        ((eq ws 'ns)
         (set-face-attribute 'default nil
                             :family "Ricty" ;;英数
                             :height 140)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty")))
        ((eq ws 'x)
         (set-face-attribute 'default nil
                             :family "Ricty"
                             :height 120)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty")))))
