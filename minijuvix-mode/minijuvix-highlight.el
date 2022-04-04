(require 'font-lock)

(defgroup minijuvix-highlight nil
  "Syntax highlighting for MiniJuvix."
  :group 'minijuvix)

(defgroup agda2-highlight-faces nil
  "Faces used to highlight MiniJuvix code."
  :group 'minijuvix-highlight)

(defface minijuvix-highlight-keyword-face
  '((((background light))
     (:foreground "#399ee6"))
    (((background dark))
     (:foreground "#81a1c1")))
  "The face used for keywords."
  :group 'minijuvix-highlight-faces)

(defface minijuvix-highlight-function-face
  '((((background light))
     (:foreground "#f2ae49"))
    (((background dark))
     (:foreground "#ebcb8b")))
  "The face used for functions."
  :group 'minijuvix-highlight-faces)

(defface minijuvix-highlight-inductive-face
  '((((background light))
     (:foreground "#86b300"))
    (((background dark))
     (:foreground "#a3be8c")))
  "The face used for inductive types."
  :group 'minijuvix-highlight-faces)

(defface minijuvix-highlight-constructor-face
  '((((background light))
     (:foreground "#a37acc"))
    (((background dark))
     (:foreground "#b48ead")))
  "The face used for constructors."
  :group 'minijuvix-highlight-faces)

(defface minijuvix-highlight-axiom-face
  '((((background light))
     (:foreground "#f07171"))
    (((background dark))
     (:foreground "#bf616a")))
  "The face used for axioms."
  :group 'minijuvix-highlight-faces)

(defface minijuvix-highlight-string-face
  '((((background light))
     (:foreground "#f07171"))
    (((background dark))
     (:foreground "#bf616a")))
  "The face used for string literals."
  :group 'minijuvix-highlight-faces)

(defface minijuvix-highlight-number-face
  '((((background light))
     (:foreground "#000000"))
    (((background dark))
     (:foreground "#d8dee9")))
  "The face used for numbers."
  :group 'minijuvix-highlight-faces)


(provide 'minijuvix-highlight)
