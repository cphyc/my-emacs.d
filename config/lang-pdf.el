;;; lang-pdf --- Provides support for pdf
;;; Commentary:

;;; Code:
(use-package pdf-tools
  :ensure t
  :mode "\\.pdf\\'"
  :config
  (pdf-tools-install))

(provide 'lang-pdf)
;;; lang-pdf ends here
