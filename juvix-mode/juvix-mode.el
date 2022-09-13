(require 'juvix-customize)
(require 'juvix-highlight)
(require 'juvix-input)
(require 'flycheck-juvix)

(defgroup juvix nil
  "Major mode for Juvix files."
  :group 'languages)

(defvar juvix-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Juvix")))
    (define-key map (kbd "C-c C-l") 'juvix-load)
    (define-key map (kbd "M-.") 'juvix-goto-definition)
    map)
  "Keymap for Juvix mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.juvix\\'" . juvix-mode))

(defun juvix-version ()
  (let ((version (car (split-string (shell-command-to-string "juvix --version")
                                    "\n"))))
    (if (string-prefix-p "Juvix" version)
        version
      "Juvix")))

(define-derived-mode juvix-mode prog-mode (juvix-version)

  (font-lock-mode 0)
  (when juvix-auto-input-method
    (set-input-method "juvix"))
  (setq-local comment-start "--")

  (add-hook
   'juvix-mode-hook
   (lambda ()
     (with-eval-after-load 'evil
       (evil-define-key 'normal juvix-mode-map (kbd "SPC m l") 'juvix-load)
       (evil-define-key 'normal juvix-mode-map (kbd "SPC m g") 'juvix-goto-definition)
       (evil-define-key 'normal juvix-mode-map (kbd "g d") 'juvix-goto-definition)
       (evil-normalize-keymaps)))))

(defun juvix-clear-annotations ()
  "Remove all annotations from the current buffer."
  (interactive)
  (remove-list-of-text-properties (point-min) (point-max) '(face)))

(defun juvix-load ()
  "Load current buffer."
  (interactive)
  (save-buffer)
  (juvix-clear-annotations)
  (eval (read (shell-command-to-string
               (concat "juvix " (if juvix-disable-embedded-stdlib "--no-stdlib " "") "dev highlight "
                       (buffer-file-name)))))
  (save-buffer))

(defun juvix-goto-definition ()
  "Go to the definition of the symbol at point."
  (interactive)
  (let ((goto-info (get-text-property (point) 'juvix-goto)))
    (if goto-info
        (progn
          (find-file (car goto-info))
          (juvix-load)
          (goto-char (cdr goto-info)))
      (message "No goto information found at cursor"))))

(provide 'juvix-mode)
