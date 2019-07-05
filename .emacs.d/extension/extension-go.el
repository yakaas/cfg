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
           "go build -v -gcflags=\"-m=1 -l\" && go vet && go test -v -cover -coverprofile=/tmp/c"))

  (use-package go-guru)

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers



  ;; https://github.com/melpa/melpa/blob/master/recipes/go-guru
  ;;  :repo "dominikh/go-mode.el"
  ;;  :fetcher github
  ;;  :files ("go-guru.el")

  ;; Enable Oracle for code analysis
  ;;(add-to-list 'load-path "/place/where/you/put/it/")
  ;;  (autoload 'go-mode "go-mode" nil t)
  ;;  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

                                        ;(let ((guru-el (substitute-in-file-name "$HOME/.emacs.d/elpa/go-mode-20180327.830/go-mode.el src/golang.org/x/tools/cmd/gur/go-guru.el")))
                                        ;  (when (file-exists-p guru-el)
                                        ;    (load-file guru-el)))

  (add-to-list 'load-path "~/.emacs.d/manual-clones/use-package")
  (require 'use-package)

  (use-package go-mode
    :load-path "~/.emacs.d/manual-clones/go-mode.el")

  ;; If the go-guru.el file is in the load path, this will load it.
  ;;(require 'go-guru)

  (require 'go-autocomplete)
  ;; Misc go stuff
  ;;(auto-complete-mode 1))                         ; Enable auto-complete mode

  ;; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-c C-n") 'flycheck-next-error)
  (local-set-key (kbd "C-c C-p") 'flycheck-previous-error))

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

(add-hook 'go-mode-hook 'flycheck-mode)

;; Ensure the go specific autocomplete is active in go-mode.
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(provide 'extension-go)
