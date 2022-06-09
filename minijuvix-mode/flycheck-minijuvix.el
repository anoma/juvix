(require 'flycheck)

(defgroup flycheck-minijuvix nil
  "MiniJuvix support for Flycheck."
  :prefix "flycheck-minijuvix-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/heliaxdev/MiniJuvix"))

(flycheck-define-checker minijuvix
  "A MiniJuvix syntax checker."
  :command ("minijuvix" "microjuvix" "typecheck" "--only-errors" "--no-colors"
                source-original)
  :error-patterns
    (
     (error line-start (file-name) ":" line ":" column ": error:" (message (one-or-more (not "ת"))))
     (error line-start (file-name) ":" line ":" column "-" end-column ": error:" (message (one-or-more (not "ת"))))
     (error line-start (file-name) ":" line "-" end-line ":" column ": error:" (message (one-or-more (not "ת"))))
     (error line-start (file-name) ":" line "-" end-line ":" column "-" end-column ": error:" (message (one-or-more (not "ת"))))
     )
  :modes minijuvix-mode
  )
(add-to-list 'flycheck-checkers 'minijuvix)

(provide 'flycheck-minijuvix)
