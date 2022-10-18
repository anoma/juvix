(require 'flycheck)
(require 'juvix-customize)

(defgroup flycheck-juvix nil
  "Juvix support for Flycheck."
  :prefix "flycheck-juvix-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/anoma/juvix"))

(flycheck-define-checker juvix
  "A Juvix syntax checker."
  :command ("juvix" "--only-errors" "--no-colors" "--stdin"
            (option-flag "--no-stdlib" juvix-disable-embedded-stdlib)
            (option "--stdlib-path" juvix-stdlib-path)
            "typecheck"
            source-original)
  :standard-input t
  :error-patterns
    (
     (error line-start (file-name) ":" line ":" column ": error:" (message (one-or-more (not "ת"))))
     (error line-start (file-name) ":" line ":" column "-" end-column ": error:" (message (one-or-more (not "ת"))))
     (error line-start (file-name) ":" line "-" end-line ":" column ": error:" (message (one-or-more (not "ת"))))
     (error line-start (file-name) ":" line "-" end-line ":" column "-" end-column ": error:" (message (one-or-more (not "ת"))))
     )
  :modes juvix-mode
  )
(add-to-list 'flycheck-checkers 'juvix)

(provide 'flycheck-juvix)
