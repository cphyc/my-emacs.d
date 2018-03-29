(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(defconst private-dir  (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir)
  "Hostname-based elisp temp directories")

;; Core settings
;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Emacs customizations
(setq confirm-kill-emacs                  'y-or-n-p
      confirm-nonexistent-file-or-buffer  t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point                 t
      require-final-newline               t
      visible-bell                        nil
      ring-bell-function                  'ignore
      custom-file                         "~/.emacs.d/.custom.el"
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

      ;; Disable non selected window highlight
      cursor-in-non-selected-windows     nil
      highlight-nonselected-windows      nil
      ;; PATH
      exec-path                          (append exec-path '("/usr/local/bin/"))
      indent-tabs-mode                   nil
      inhibit-startup-message            t
      fringes-outside-margins            t
      select-enable-clipboard            t
      use-package-always-ensure          t)

;; Bookmarks
(setq
 ;; persistent bookmarks
 bookmark-save-flag                      t
 bookmark-default-file              (concat temp-dir "/bookmarks"))

;; Backups enabled, use nil to disable
(setq
 history-length                     1000
 backup-inhibited                   nil
 make-backup-files                  t
 auto-save-default                  t
 auto-save-list-file-name           (concat temp-dir "/autosave")
 make-backup-files                  t
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(concat temp-dir "/backup/")))
 auto-save-file-name-transforms    `((".*" ,(concat temp-dir "/auto-save-list/") t)))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

;; Disable toolbar & menubar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (  fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(show-paren-mode 1)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default show-trailing-whitespace t)

;;; Tramp configuration
;; (add-to-list 'tramp-remote-path
;; 	     ("/softs/intel/compilers_and_libraries_2016.3.210/linux/bin/intel64"
;; 	      "/softs/openmpi/1.8.8-ifort-15.0-torque-CentOS6/bin"
;; 	      "/home/cadiou/anaconda3/bin"
;; 	      "/softs/git/2.10.1/bin/git"
;; 	      "/home/cadiou/bin"))
(setq tramp-default-method "ssh")


;;; Auto recompile els files
(defun auto-byte-recompile ()
  "If the current buffer is in `emacs-lisp-mode' and there already
exists an `.elc' file corresponding to the current buffer file, then
recompile the file on save."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'auto-byte-recompile)

;;; Make shell scripts executable automatically on save
(add-hook 'after-save-hook
'executable-make-buffer-file-executable-if-script-p)

;;; Save history
(savehist-mode)

;;; Server mode
(server-force-delete)
(server-start)

;;; No auto-fill mode
(auto-fill-mode nil)

;;; Start in org mode
(require 'org-install)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;;; Follow output while compiling
(setq compilation-scroll-output t)

;;; Compilation buffer is smaller by default
(defun my-buffer-face-mode-smaller ()
  "Set a fixed width (monospace) font in current buffer."
  (interactive)
  (setq buffer-face-mode-face '(:height 100))
  (buffer-face-mode))

;; Set default font faces for Info and ERC modes
(add-hook 'compilation-mode-hook 'my-buffer-face-mode-smaller)

(provide 'base)
;;; base ends here
