;;; theme --- Light and slean theme
;;; Commentary:
;;; Contains my theme configs

;;; Code:

;;; Powerline
(use-package powerline
  :config
  (powerline-center-theme))

;; (use-package material-theme)

;; (use-package apropospriate-theme
;;   :ensure t
;;   :config
;;   (load-theme 'apropospriate-dark t))

(use-package sublimity
  :config
  (sublimity-mode 1))

(provide 'base-theme)
;;; base-theme ends here
