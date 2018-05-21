;;; init.el --- Emacs configuration

;;; Commentary:
;; - Load base custom configuration.
;; - Install a bunch of packages.

;;; Code:

(package-initialize)

;; These are base customizations that are emacs centric.
(load (concat user-emacs-directory "base-custom.el"))

;; Record customizations from `Customize` at this path instead of init.el.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

; Basic libraries needed for the rest of the configuration.
(require 'cl)

(let ((default-directory user-emacs-directory))
  ;; personal script path
  (normal-top-level-add-to-load-path '("extension"))
  ;; external script path
  (normal-top-level-add-to-load-path
   ;; Select all directories in site-lisp.
   ;;
   ;; normal-top-level-add-subdirs-to-load-path is insufficient
   ;; because we don't actually want it to recurse into the
   ;; subdirectories.
   (remove-if-not (lambda (filename)
                    ;; First attribute is T if file is a directory.
                    (first (file-attributes filename)))
                  (file-expand-wildcards "site-lisp/*"))))

(defun ensure-package-installed (package)
  (unless (package-installed-p package)
    (package-install package)))

(defvar my-required-packages
  '(auto-complete
    color-theme
    csharp-mode
    ;framemove
    fiplr
    paredit
    flycheck
    erlang
    linum
    markdown-mode
    magit
    multiple-cursors
    neotree
    haskell-mode
    iedit
    edts
    dockerfile-mode
    terraform-mode
    yaml-mode
    ;; Rust support
    rust-mode
    quickrun
    web-mode
    rjsx-mode
    racer
    yasnippet
    )
  "A list of packages to ensure are installed at launch.")

(setq fiplr-ignored-globs '((directories (".git" ".svn"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))

(global-set-key "\C-xf" 'fiplr-find-file)

;; Check for any packages that are not installed, and auto install.
(let ((uninstalled-packages (remove-if 'package-installed-p my-required-packages)))
  ;; refresh the package cache if we're going to install.
  (when uninstalled-packages
    (package-refresh-contents))
  ;; install each of the packages that are uninstalled.
  (dolist (p uninstalled-packages)
    (ensure-package-installed p)))

;; Determine he environment.
(let ((system-extension (concat user-emacs-directory (symbol-name system-type) ".el")))
  (when (file-exists-p system-extension)
    (load system-extension)))

;; Navigate through windows and frames using Shift-<Arrow>
;(require 'framemove)
;(windmove-default-keybindings)
;(setq framemove-hook-into-windmove t)


;;; magit configuration
(autoload 'magit-status "magit" nil t)
(autoload 'git-blame-mode "git-blame" "Git blame mode" t)

(define-prefix-command 'git-tools)
(global-set-key "\C-cg" 'git-tools)
(global-set-key "\C-cgs" 'magit-status)
(global-set-key "\C-cgk" 'gitk)
(global-set-key "\C-cgc" 'git-insert-credit)
(global-set-key "\C-cgb" 'git-blame-mode)
(global-set-key "\C-cgl" 'git-show-current-commit)

(setq-default git-commit-setup-hook
              '(git-commit-save-message
                git-commit-setup-changelog-support
                git-commit-turn-on-auto-fill
                git-commit-turn-on-flyspell
                git-commit-propertize-diff
                with-editor-usage-message))

;; Spelling is awesome!
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; paredit configuration

;; enable paredit by default for the various lisp modes
(loop for hook in '(emacs-lisp-mode-hook
		    lisp-mode-hook
		    inferior-lisp-mode-hook
		    scheme-mode-hook)
      do (add-hook hook (lambda ()
			  (paredit-mode t)
			  ;; better indentation mode
			  (local-set-key "\r" 'paredit-newline))))

;; markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;; org mode
(progn
  ;; must be set before loading 'org
  (setq org-replace-disputed-keys t)
  ;; ensure we have at least the default 'org-directory
  (require 'org)
  ;; attempt to load config, ignoring if it doesn't exist
  (load (concat (file-name-as-directory org-directory) "config.el")
        'noerror))

;;; Extension packages to configure various language environments.

;(require 'extension-eproject)
;(require 'extension-ruby)
;(require 'extension-erlang)
;(require 'extension-rust)
(require 'extension-go)
;(require 'extension-haskell)
;(require 'extension-cpp)
;(require 'extension-csharp)

(require 'quickrun)

; start auto-complete with emacs
(require 'auto-complete)
; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)


(defun xah-new-empty-buffer ()
  "Create a new empty buffer. New buffer will be named “untitled” or “untitled<2>”, etc.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2016-12-27"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))


(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; (add-to-list 'load-path "/some/path/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


(global-set-key (kbd "<f7>") 'xah-new-empty-buffer)
(global-set-key (kbd "<f10>") 'quickrun)
(global-set-key (kbd "C-z") 'save-buffer)
(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(global-set-key (kbd "<f5>") 'revert-buffer)

(global-set-key [(meta shift up)] 'move-line-up)
(global-set-key [(meta shift down)] 'move-line-down)

; Fix iedit bug in Mac
(define-key global-map (kbd "C-c ;") 'iedit-mode)

; Multiple cursors
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Rust
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

;; Docker
(use-package dockerfile-mode
 :ensure t
 :mode "Dockerfile.\\'")

; YAML Files
(use-package yaml-mode
 :ensure t
 :mode "\\.ya?ml\\'")

; Terraform
(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'")

; JSX
(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")

; JSX
(use-package web-mode
  :ensure t
  :mode "\\.html\\'")

; init.el ends here
