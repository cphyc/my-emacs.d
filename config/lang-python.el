;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

;;; Code:

(use-package cython-mode
  :ensure t)

(use-package elpy
  :ensure t
  :defer 2
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (elpy-enable)
    ;; jedi is great
    (setq elpy-rpc-backend "jedi")
    (setq elpy-rpc-python-command "~/.virtualenvs/astro/bin/python3")
    (setq elpy-test-runner (quote elpy-test-nose-runner))
    )
  :bind (:map elpy-mode-map
	      ("M-." . elpy-goto-definition)
	      ("M-," . pop-tag-mark)))

(use-package pip-requirements
  :ensure t
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package company-jedi
  :ensure t)

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs/"))


;; (use-package pyenv-mode
;;   :init
;;   (add-to-list 'exec-path "~/.pyenv/shims")
;;   (setenv "WORKON_HOME" "~/.pyenv/versions/")
;;   :config
;;   (pyenv-mode)
;;   :bind
;;   ("C-x p e" . pyenv-activate-current-project))

;; (defun pyenv-init()
;;   (setq global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global")))
;;   (message (concat "Setting pyenv version to " global-pyenv))
;;   (pyenv-mode-set global-pyenv)
;;   (defvar pyenv-current-version nil global-pyenv))

;; (defun pyenv-activate-current-project ()
;;   "Automatically activates pyenv version if .python-version file exists."
;;   (interactive)
;;   (f-traverse-upwards
;;    (lambda (path)
;;      (message path)
;;      (let ((pyenv-version-path (f-expand ".python-version" path)))
;;        (if (f-exists? pyenv-version-path)
;;           (progn
;;             (setq pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8)))
;;             (pyenv-mode-set pyenv-current-version)
;;             (pyvenv-workon pyenv-current-version)
;;             (message (concat "Setting virtualenv to " pyenv-current-version))))))))

;; (add-hook 'after-init-hook 'pyenv-init)

;; (add-hook 'projectile-after-switch-project-hook 'pyenv-activate-current-project)

;; ;;; Elpy
;; (elpy-enable)

;; ;;; Use jedi
;; ;; (add-hook 'python-mode-hook 'jedi:setup)
;; ;; (setq jedi:setup-keys t)
;; ;; (setq jedi:complete-on-dot t)


;; ;; Python lookup
;; (setq pylookup-dir "~/.emacs.d/pylookup")
;; (add-to-list 'load-path pylookup-dir)

;; ;; ;; Activate conda
;; ;; (require 'conda)
;; ;; ;; if you want interactive shell support, include:
;; ;; (conda-env-initialize-interactive-shells)
;; ;; ;; if you want eshell support, include:
;; ;; (conda-env-initialize-eshell)
;; ;; ;; if you want auto-activation (see below for details), include:
;; ;; (conda-env-autoactivate-mode t)


;; ;; load pylookup when compile time
;; (eval-when-compile (require 'pylookup))

;; ;; set executable file and db file
;; (setq pylookup-program "~/.emacs.d/pylookup/pylookup.py")
;; (setq pylookup-db-file "~/.emacs.d/pylookup/pylookup.db")

;; ;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

;; ;; to speedup, just load it on demand
;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)

;; (autoload 'pylookup-update "pylookup"
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)

;; (add-hook
;;  'python-mode-hook
;;  '(lambda ()
;;     (local-set-key (kbd "C-c h") 'pylookup-lookup)))

(provide 'lang-python)
;;; lang-python ends here
