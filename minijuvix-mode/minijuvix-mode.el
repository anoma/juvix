(require 'minijuvix-highlight)

(defgroup minijuvix nil
  "Major mode for MiniJuvix files."
  :group 'languages)

(defvar minijuvix-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "MiniJuvix")))
    (define-key map "\C-c\C-l" 'minijuvix-load)
    (define-key map (kbd "M-.") 'minijuvix-goto-definition)
    map)
  "Keymap for MiniJuvix mode.")


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m?juvix\\'" . minijuvix-mode))

(define-derived-mode minijuvix-mode prog-mode "MiniJuvix"

  (font-lock-mode 0)
  (setq-local comment-start "--")


  (add-hook
   'minijuvix-mode-hook
   (lambda ()

     (eval-after-load 'evil-maps
       '(evil-leader/set-key-for-mode 'minijuvix-mode
          "l" 'minijuvix-load
          "g" 'minijuvix-goto-definition
          ))
     ))
  )

(defun minijuvix-clear-annotations ()
  "Remove all annotations from the current buffer."
  (interactive)
  (remove-list-of-text-properties (point-min) (point-max)
                                  '(face))
  )

(defun minijuvix-load ()
  "Load current buffer."
  (interactive)
  (save-buffer)
  (minijuvix-clear-annotations)
  (eval (read (shell-command-to-string (concat "minijuvix highlight " (buffer-file-name)))))
  )

(defun minijuvix-goto-definition ()
  "Goes to the definition of the symbol at point."
  (interactive)
  (message "goto")
  (goto-char (cdr (get-text-property (point) 'minijuvix-goto)))
  )

(provide 'minijuvix-mode)
