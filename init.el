;;; Init --- My Config
;;; Commentary:
;; Corentin Cadiou
;; corentin.cadiou@cphyc.me

;;; Code:
(package-initialize)

(setq inhibit-splash-screen t)

(setq indent-tabs-mode nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 2)
 '(ac-ispell-requires 4)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(autopair-global-mode t)
 '(conda-anaconda-home "~/.anaconda")
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" default)))
 '(package-selected-packages
   (quote
    (use-package indent-guide cmake-mode jedi-direx jedi conda ein company-math volatile-highlights golden-ratio helm-tramp sublime-themes git-timemachine django-mode fish-mode smooth-scrolling elpy workgroups2 whitespace-cleanup-mode weechat visual-regexp-steroids tabbar ssh request rainbow-mode project-explorer ox-impress-js multiple-cursors multi minimap markdown-preview-mode markdown-mode+ load-dir jade-mode iedit ido-yes-or-no icicles hippie-exp-ext helm-dictionary gnuplot-mode fold-this flycheck-ocaml firefox-controller etags-table etags-select emms ctags-update ctags csv-mode crontab-mode cl-generic anything angular-mode anaconda-mode ack ace-window)))
 '(python-shell-interpreter "ipython")
 '(python-shell-interpreter-args " --simple-prompt")
 '(pyvenv-workon "astro")
 '(safe-local-variable-values
   (quote
    ((projectile-project-compilation-dir . trunk/ramses/bin))))
 '(send-mail-function (quote smtpmail-send-it)))

;; Load dirs
(let ((default-directory "~/.emacs.d/load.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; General config
(add-to-list 'load-path (concat user-emacs-directory "config"))
(require 'base)
(require 'base-functions)
(require 'base-theme)
(require 'base-extensions)
(require 'base-global-keys)

;; Language specific config
(require 'flycheck-fortran-linter)
(require 'lang-c)
(require 'lang-css)
(require 'lang-fortran)
(require 'lang-js)
(require 'lang-latex)
(require 'lang-ocaml)
(require 'lang-pdf)
(require 'lang-python)
(require 'lang-web)

;; Other language
(require 'lang-misc)


;; Edit cron files
(add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))

;; cpp
(add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))

;; rc files
(add-to-list 'auto-mode-alist '(".*rc\\'" . sh-mode))

;; PKGbuild
(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . sh-mode))

;; Misc
(setq smerge-command-prefix "\C-cv")

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
