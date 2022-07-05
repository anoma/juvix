(require 'minijuvix-highlight)
(require 'minijuvix-input)
(require 'flycheck-minijuvix)

(defgroup minijuvix nil
  "Major mode for MiniJuvix files."
  :group 'languages)

(defvar minijuvix-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "MiniJuvix")))
    (define-key map (kbd "C-c C-l") 'minijuvix-load)
    (define-key map (kbd "M-.") 'minijuvix-goto-definition)
    map)
  "Keymap for MiniJuvix mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m?juvix\\'" . minijuvix-mode))

(defcustom minijuvix-auto-input-method t
  "Automatically set the input method in minijuvix files."
  :type 'boolean
  :group 'minijuvix)

(define-derived-mode minijuvix-mode prog-mode "MiniJuvix"

  (font-lock-mode 0)
  (when minijuvix-auto-input-method
    (set-input-method "minijuvix")
    )
  (setq-local comment-start "--")

  (add-hook
   'minijuvix-mode-hook
   (lambda ()
     (with-eval-after-load 'evil
       (evil-define-key 'normal minijuvix-mode-map (kbd "SPC m l") 'minijuvix-load)
       (evil-define-key 'normal minijuvix-mode-map (kbd "SPC m g") 'minijuvix-goto-definition)
       (evil-define-key 'normal minijuvix-mode-map (kbd "g d") 'minijuvix-goto-definition)
       (evil-normalize-keymaps))))
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
  (save-buffer)
  )

(defun minijuvix-goto-definition ()
  "Goes to the definition of the symbol at point."
  (interactive)
  (let ((goto-info (get-text-property (point) 'minijuvix-goto)))
    (if goto-info
      (goto-char (cdr goto-info))
      (message "No goto information found at cursor"))))

(provide 'minijuvix-mode)
