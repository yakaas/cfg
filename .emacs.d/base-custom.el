;;; base-custom.el --- define generci custom emacs configuration

;;; Commentary:
;; - General screen configuration
;; - Add melpa to package-archives

;;; Code:

;; UTF8 Unix by default where possible.
(prefer-coding-system 'utf-8-unix)
(setq buffer-file-coding-system 'utf-8-unix)

;; disable useless graphical extras
(menu-bar-mode -1)
(tool-bar-mode -1)
;(scroll-bar-mode -1)

;; hide startup messages
(setq inhibit-startup-screen t) ; no splash screen

;; nothing in *scratch* when started
(setq initial-scratch-message nil)

;; show matching parentheses
(show-paren-mode 1)

;; whitespace
(setq show-trailing-whitespace t)

;; delete trailing whitespaces before saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; disable beeping and visual bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; dialog boxes are annoying
(setq use-dialog-box nil)

;; bigger message log than default of 100
(setq message-log-max 5000)

;; editing adjustments
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)
(setq-default js-indent-level 2)

;; Prefer newer byte code
(setq load-prefer-newer t)

;; Configuration for ido mode.
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Configure use-package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq scroll-step            1
      scroll-conservatively  10000)

;; Lets make sure auto-saves and backups doesn't clutter current directory
(defvar backup-dir (expand-file-name "~/.emacs.d/emacs_backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory autosave-dir)

;; Start the server first, but only if emacs is not currently running.
(load "server")
(unless (server-running-p) (server-start))

;; Show trailing whitespace. Trailing whitespace is the devil.
(require 'whitespace)
(setq-default show-trailing-whitespace t)

(defun no-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(add-hook 'minibuffer-setup-hook
          'no-trailing-whitespace)
(add-hook 'eww-mode-hook
          'no-trailing-whitespace)
(add-hook 'ielm-mode-hook
          'no-trailing-whitespace)
(add-hook 'gdb-mode-hook
          'no-trailing-whitespace)
(add-hook 'help-mode-hook
          'no-trailing-whitespace)

;; Only y/n questions, not yes/no
(defalias 'yes-or-no-p 'y-or-n-p )

(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key [remap move-beginning-of-line]
                'beginning-of-line-or-indentation)


(dolist (p '(monokai-theme))
  (unless (package-installed-p p)
    (package-install p)))

;;
;; Dried
;;
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")

;; Automatically revert buffers, silently
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Less verbosity
;(require 'dired-details)
;(setq dired-details-hidden-string "")
;(dired-details-install)

(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "^\\.$\\|^\\.\\.$"))

(require 'linum)
(set-face-attribute 'linum nil
                    :background (face-attribute 'default :background)
                    :foreground (face-attribute 'font-lock-comment-face :foreground))
(defface linum-current-line-face
  `((t :background "gray30" :foreground "gold"))
  "Face for the currently active Line number")
(defvar my-linum-current-line-number 0)
(defun get-linum-format-string ()
  (setq-local my-linum-format-string
              (let ((w (length (number-to-string
                                (count-lines (point-min) (point-max))))))
                (concat " %" (number-to-string w) "d "))))
(add-hook 'linum-before-numbering-hook 'get-linum-format-string)
(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face
              (if (eq line-number my-linum-current-line-number)
                  'linum-current-line-face
                'linum)))
(setq linum-format 'my-linum-format)
(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

;;
;; Terminal
;;
(defvar my-term-shell "/usr/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defun term-toggle-mode ()
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(defun my-term-hook ()
  (goto-address-mode)
  (local-set-key "\C-c\C-j" 'term-toggle-mode) ;; toggle line/char mode
  (local-set-key "\C-c\C-k" 'term-toggle-mode)
  (setq global-hl-line-mode nil)
  (setq term-buffer-maximum-size 10000)
  (setq-local ml-interactive? t) ;; for mode line
  (setq-local show-dir-in-mode-line? t) ;; also mode linec'
  (setq show-trailing-whitespace nil)
  ;; disable company in favor of shell completion
  (company-mode -1))
(add-hook 'term-mode-hook 'my-term-hook)

(defalias 'sh 'ansi-term)

(set-frame-font "Inconsolata 15" nil t)

(provide 'base-custom)
;;; base-custom.el ends here
