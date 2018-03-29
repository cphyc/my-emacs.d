;;; fortran --- Fortran mode
;;; Commentary:
; Setups flycheck, auto-fill and fortpy mode

;;; Code:
(add-hook 'f90-mode-hook 'company-mode)

(add-to-list 'flycheck-checkers 'fortran-linter)
(flycheck-add-next-checker 'fortran-linter
			   'fortran-gfortran
			   'append)

;; Set paths
(setq flycheck-fortran-gfortran-executable "mpif90")
(setq flycheck-fortran-linter-executable "/home/ccc/.anaconda/bin/fortran-linter")

(setq flycheck-gfortran-language-standard nil)
(setq f90-auto-keyword-case (quote downcase-word))
(setq fortran-do-indent 2)
(setq fortran-if-indent 2)
(setq fortran-structure-indent 2)
(setq fortran-line-lenth 128)

(use-package fortpy
  :config
  (add-hook 'f90-mode-hook 'fortpy-mode))

(require 'f90-namelist-mode)
(add-to-list 'magic-mode-alist
             '(f90-buffer-is-input-file . f90-namelist-mode))

;; (require 'namelist-mode)
;; (setq namelist-indent-offset 4)
;; (add-to-list 'auto-mode-alist '("\\.nml\\'" . namelist-mode))


(provide 'lang-fortran)
;;; lang-fortran ends here
