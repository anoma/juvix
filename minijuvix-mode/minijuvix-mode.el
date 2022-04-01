(require 'minijuvix-highlight)

(defgroup minijuvix nil
  "Major mode for MiniJuvix files."
  :group 'languages)

(defvar minijuvix-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "MiniJuvix")))
    (define-key map "\C-c\C-l" 'minijuvix-load)
    map)
  "Keymap for MiniJuvix mode.")


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m?juvix\\'" . minijuvix-mode))

(define-derived-mode minijuvix-mode prog-mode "MiniJuvix"

  (font-lock-mode 0)
  )

(defun minijuvix-load ()
  "Load current buffer."
  (interactive)
  (eval (read (shell-command-to-string (concat "minijuvix highlight " (buffer-file-name)))))
  )

(provide 'minijuvix-mode)
