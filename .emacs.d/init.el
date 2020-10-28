;;; init.el --- Emacs configuration

;;; Commentary:
;; - Load base custom configuration.
;; - Install a bunch of packages.

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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
    ack
    ag
    )
  "A list of packages to ensure are installed at launch.")

(setq fiplr-ignored-globs '((directories (".git" ".svn" ".gen" "vendor" ".tmp" ".go"))
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
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

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

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (remove-if-not '(lambda (x) (or (buffer-file-name x) (eq 'dired-mode (buffer-local-value 'major-mode x)))) (buffer-list)))))

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

;;; copy without highlighting https://www.emacswiki.org/emacs/CopyWithoutSelection
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
      "Copy thing between beg & end into kill ring."
      (save-excursion
        (let ((beg (get-point begin-of-thing 1))
              (end (get-point end-of-thing arg)))
          (copy-region-as-kill beg end))))

(defun paste-to-mark (&optional arg)
      "Paste things to mark, or to the prompt in shell-mode."
      (unless (eq arg 1)
        (if (string= "shell-mode" major-mode)
            (comint-next-prompt 25535)
          (goto-char (mark)))
        (yank)))

;; copy word
(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

(global-set-key (kbd "C-c w")         (quote copy-word))

;; copy line
(defun copy-line (&optional arg)
      "Save current line into Kill-Ring without mark the line "
       (interactive "P")
       (copy-thing 'beginning-of-line 'end-of-line arg)
       (paste-to-mark arg)
       )

(global-set-key (kbd "C-c l")         (quote copy-line))

;; copy paragraph
(defun copy-paragraph (&optional arg)
      "Copy paragraphes at point"
       (interactive "P")
       (copy-thing 'backward-paragraph 'forward-paragraph arg)
       (paste-to-mark arg)
       )

(global-set-key (kbd "C-c p")         (quote copy-paragraph))

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

; show cloumn numbers
(setq column-number-mode t)

; searching

; rgrep
(global-set-key (kbd "C-c s") 'rgrep)

; ack
;(global-set-key (kbd "C-c a") 'ack)
;(add-hook 'ack-minibuffer-setup-hook 'ack-skel-vc-grep t)
;(add-hook 'ack-minibuffer-setup-hook 'ack-yank-symbol-at-point t)

; ag
(global-set-key (kbd "C-c a") 'ag)
(setq ag-reuse-buffers 't)
(setq ag-highlight-search t)
(setq ag-arguments '("--ignore" "mocks/" "--ignore" "vendor/"))

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

(require 'thrift-mode)
(add-to-list 'auto-mode-alist '("\\.thrift$" . thrift-mode))

; init.el ends here
