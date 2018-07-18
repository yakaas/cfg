;(when (memq window-system '(mac ns))
;  (exec-path-from-shell-copy-env "GOPATH"))

(dolist (p '(go-mode go-autocomplete go-eldoc))
  (unless (package-installed-p p)
    (package-install p)))

(defun my-go-mode-hook ()
  ;; Show signature of function at point in status bar
  (go-eldoc-setup)
  ;; Use 'goimports' as our formatter
  (setq gofmt-command "goimports")
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v -cover && go vet"))

  ;; Enable Oracle for code analysis
  ;(let ((guru-el (substitute-in-file-name "$HOME/.emacs.d/elpa/go-mode-20180327.830/go-mode.el src/golang.org/x/tools/cmd/gur/go-guru.el")))
  ;  (when (file-exists-p guru-el)
  ;    (load-file guru-el)))

(add-to-list 'load-path "~/tmp/elisp/use-package")
(require 'use-package)

(use-package go-mode
  :load-path "~/tmp/elisp/go-mode")

(use-package go-guru)


  ;; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-c C-n") 'flycheck-next-error)
  (local-set-key (kbd "C-c C-p") 'flycheck-previous-error))

(require 'go-autocomplete)

(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'flycheck-mode)

(provide 'extension-go)
