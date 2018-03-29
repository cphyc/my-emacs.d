;;; rust --- Rust mode
;;; Commentary:
; Setups flycheck, auto-fill and fortpy mode

;;; Code:
(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

;;; lang-rust ends here
