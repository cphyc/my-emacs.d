;;; lang-latex --- Provides support for LaTeX
;;; Commentary:
; In the following, LaTeX-mode refers to auctex's latex mode whereas
; latex-mode is Emacs'.

;;; Code:
(use-package company-auctex
  :ensure t)
(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))


;; Activate math mode by default
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; Flymake for syntax errors
(defun flymake-get-tex-args (file-name)
  (list "xelatex"
	(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
(add-hook 'LaTeX-mode-hook 'flymake-mode)
  (defun flymake-get-tex-args (file-name)
    (list "chktex" (list "-q" "-v0" file-name)))

;; Flyspell activation for latex
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;; Reftex
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)

;; Bind f7 to next error in latex mode
(add-hook 'LaTeX-mode-hook
	  (lambda()
	    (local-set-key [f7] 'TeX-next-error)))

;; Configure latex-extra mode
;; (use-package latex-extra-mode
;;   :ensure t
;;   :config
;;   (add-hook 'LaTeX-mode-hook #'latex-extra-mode))

;; CDLaTeX is a minor mode supporting fast insertion of
;; environmenttemplates and math stuff in LaTeX.
(use-package cdlatex
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))

;; Support for latexmk
(use-package auctex-latexmk
  :ensure t
  :init
  (auctex-latexmk-setup)
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (setq LaTeX-math-abbrev-prefix "`")
  (setq LaTeX-section-label (quote
			     (("part" . "part:")
			      ("chapter" . "chap:")
			      ("section" . "sec:")
			      ("subsection" . "ssec:")
			      ("subsubsection" . "sssec:"))))
  (setq TeX-auto-save t)
  (setq TeX-complete-expert-commands t)
  (setq TeX-electric-math (quote ("$" . "$")))
  (setq TeX-electric-sub-and-superscript nil)
  (setq TeX-engine (quote default))
  (setq TeX-file-extensions
	(quote
	 ("tex" "sty" "cls" "ltx" "texi" "txi" "texinfo" "dtx" "Rnw")))
  (setq TeX-insert-braces t)
  (setq TeX-parse-self t)
  (setq TeX-source-correlate-mode t))

;; Bibtex support
(use-package company-bibtex
  :ensure t)
(use-package helm-bibtex
  :ensure t)
(use-package bibslurp
  :ensure t)

;; Customize some variables

(setq preview-auto-cache-preamble t)
(setq preview-preserve-counters t)
(setq preview-scale-function 1.5)

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

(provide 'lang-latex)
;;; lang-latex ends here
