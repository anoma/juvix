(require 'comint)

(defgroup juvix-repl nil
  "Interaction mode for Juvix"
  :group 'juvix)

(defvar juvix-repl-program "juvix"
  "The Juvix program.")

(defvar juvix-repl-program-args '("repl")
  "The argument to pass to Juvix to launch the REPL.")

(defvar juvix-repl-mode-map
  (nconc (make-sparse-keymap) comint-mode-map))

(defvar juvix-repl-buffer-name "*juvix-repl*"
  "The name of the buffer to use for the `juvix-repl' comint instance.")

(defun run-juvix-repl ()
  "Run an inferior instance of `juvix repl' inside Emacs."
  (interactive)
  (let* ((juvix-program juvix-repl-program)
         (juvix-program-args juvix-repl-program-args)
         (buffer (get-buffer-create juvix-repl-buffer-name))
         (proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer)))
    (unless proc-alive
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "juvix-repl" buffer
               juvix-program nil juvix-program-args)
        (juvix-repl-mode)))
    (when buffer
      (pop-to-buffer buffer))))

(defun juvix-repl--initialize ()
  "Helper function to initalize juvix-repl."
  (setq comint-process-echoes nil))

(define-derived-mode juvix-repl-mode comint-mode "Juvix REPL" "Major mode for juvix-repl")

(add-hook 'juvix-mode-hook 'juvix-repl--initialize)

(defun juvix-repl-load-file (filename)
  "Load FILENAME into the juvix-repl if it is running."
  (let* ((buffer (get-buffer juvix-repl-buffer-name))
         (proc-alive (comint-check-proc buffer)))
    (when proc-alive
      (comint-simple-send juvix-repl-buffer-name (concat ":load " filename)))))

(defun juvix-repl-restart ()
  "Restart the juvix REPL."
  (interactive)
  (let* ((buffer (get-buffer juvix-repl-buffer-name))
         (proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer)))
    (if proc-alive
        (progn
          (set-process-sentinel process (lambda (p e) (run-juvix-repl)))
          (kill-process process))
      (error "No juvix repl process is runnning. Use run-juvix-repl"))))

(provide 'juvix-repl)
