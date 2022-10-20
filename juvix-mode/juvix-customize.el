
(defcustom juvix-auto-input-method t
  "Automatically set the input method in juvix files."
  :type 'boolean
  :group 'juvix)

(defcustom juvix-disable-embedded-stdlib nil
  "Disable the embedded standard library."
  :type 'boolean
  :group 'juvix)

(defcustom juvix-stdlib-path nil
  "Specify the path to the standard library."
  :type 'directory
  :group 'juvix)

(provide 'juvix-customize)
