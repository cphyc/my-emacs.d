;;; css --- CSS mode
;;; Commentary:

;;; Code:
(use-package css-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
    (add-to-list 'auto-mode-alist '("\\.sass$" . css-mode))))

(provide 'lang-css)
;;; lang-css ends here
