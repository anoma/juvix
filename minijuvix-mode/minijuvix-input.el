;;; -*- lexical-binding: t; -*-
;;; minijuvix-input.el --- The minijuvix input method

;; Copyright (c) 2005-2021 remains with the authors.
;; Agda 2 was originally written by Ulf Norell,
;; partially based on code from Agda 1 by Catarina Coquand and Makoto Takeyama,
;; and from Agdalight by Ulf Norell and Andreas Abel.

;; Agda 2 is currently actively developed mainly by Andreas Abel,
;; Guillaume Allais, Liang-Ting Chen, Jesper Cockx, Matthew Daggitt, Nils
;; Anders Danielsson, Ulf Norell, Andrés Sicard-Ramírez, and Andrea
;; Vezzosi.

;; Further, Agda 2 has received contributions by, amongst others, Stevan
;; Andjelkovic, Marcin Benke, Jean-Philippe Bernardy, Guillaume Brunerie,
;; James Chapman, Dominique Devriese, Péter Diviánszky, Robert Estelle,
;; Olle Fredriksson, Adam Gundry, Daniel Gustafsson, Philipp Hausmann,
;; Alan Jeffrey, Wolfram Kahl, Wen Kokke, John Leo, Fredrik Lindblad,
;; Víctor López Juan, Ting-Gan Lua, Francesco Mazzoli, Stefan Monnier,
;; Guilhem Moulin, Fredrik Nordvall Forsberg, Nicolas Pouillard, Jonathan
;; Prieto, Christian Sattler, Makoto Takeyama, Noam Zeilberger, and Tesla
;; Ice Zhang.  The full list of contributors is available at
;; https://github.com/agda/agda/graphs/contributors or from the git
;; repository via ``git shortlog -sne``.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; SPDX-License-Identifier: MIT License
;;; Commentary:

;; A highly customisable input method which can inherit from other
;; Quail input methods. By default the input method is geared towards
;; the input of mathematical and other symbols in MiniJuvix programs.
;;
;; Use M-x customize-group minijuvix-input to customise this input method.
;; Note that the functions defined under "Functions used to tweak
;; translation pairs" below can be used to tweak both the key
;; translations inherited from other input methods as well as the
;; ones added specifically for this one.
;;
;; Use minijuvix-input-show-translations to see all the characters which
;; can be typed using this input method (except for those
;; corresponding to ASCII characters).

;;; Code:

(require 'quail)

;; This is present in agda-input but we think it is not required in
;; newer emacsen.
;; (require 'cl-lib)

;; Quail is quite stateful, so be careful when editing this code.  Note
;; that with-temp-buffer is used below whenever buffer-local state is
;; modified.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun minijuvix-input-concat-map (f xs)
  "Concat (map F XS)."
  (apply 'append (mapcar f xs)))

(defun minijuvix-input-to-string-list (s)
  "Convert a string S to a list of one-character strings, after
removing all space and newline characters."
  (minijuvix-input-concat-map
   (lambda (c) (if (member c (string-to-list " \n"))
              nil
            (list (string c))))
   (string-to-list s)))

(defun minijuvix-input-character-range (from to)
  "A string consisting of the characters from FROM to TO."
  (let (seq)
    (dotimes (i (1+ (- to from)))
      (setq seq (cons (+ from i) seq)))
    (concat (nreverse seq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions used to tweak translation pairs

(defun minijuvix-input-compose (f g)
  "\x -> concatMap F (G x)"
    (lambda (x) (minijuvix-input-concat-map f (funcall g x))))

(defun minijuvix-input-or (f g)
  "\x -> F x ++ G x"
    (lambda (x) (append (funcall f x) (funcall g x))))

(defun minijuvix-input-nonempty ()
  "Only keep pairs with a non-empty first component."
  (lambda (x) (if (> (length (car x)) 0) (list x))))

(defun minijuvix-input-prepend (prefix)
  "Prepend PREFIX to all key sequences."
    (lambda (x) `((,(concat prefix (car x)) . ,(cdr x)))))

(defun minijuvix-input-prefix (prefix)
  "Only keep pairs whose key sequence starts with PREFIX."
    (lambda (x)
      (if (equal (substring (car x) 0 (length prefix)) prefix)
          (list x))))

(defun minijuvix-input-suffix (suffix)
  "Only keep pairs whose key sequence ends with SUFFIX."
    (lambda (x)
      (if (equal (substring (car x)
                            (- (length (car x)) (length suffix)))
                 suffix)
          (list x))))

(defun minijuvix-input-drop (ss)
  "Drop pairs matching one of the given key sequences.
SS should be a list of strings."
    (lambda (x) (unless (member (car x) ss) (list x))))

(defun minijuvix-input-drop-beginning (n)
  "Drop N characters from the beginning of each key sequence."
    (lambda (x) `((,(substring (car x) n) . ,(cdr x)))))

(defun minijuvix-input-drop-end (n)
  "Drop N characters from the end of each key sequence."
    (lambda (x)
      `((,(substring (car x) 0 (- (length (car x)) n)) .
         ,(cdr x)))))

(defun minijuvix-input-drop-prefix (prefix)
  "Only keep pairs whose key sequence starts with PREFIX.
This prefix is dropped."
  (minijuvix-input-compose
   (minijuvix-input-drop-beginning (length prefix))
   (minijuvix-input-prefix prefix)))

(defun minijuvix-input-drop-suffix (suffix)
  "Only keep pairs whose key sequence ends with SUFFIX.
This suffix is dropped."
    (minijuvix-input-compose
     (minijuvix-input-drop-end (length suffix))
     (minijuvix-input-suffix suffix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

;; The :set keyword is set to 'minijuvix-input-incorporate-changed-setting
;; so that the input method gets updated immediately when users
;; customize it. However, the setup functions cannot be run before all
;; variables have been defined. Hence the :initialize keyword is set to
;; 'custom-initialize-default to ensure that the setup is not performed
;; until minijuvix-input-setup is called at the end of this file.

(defgroup minijuvix-input nil
  "The minijuvix input method.
After tweaking these settings you may want to inspect the resulting
translations using `minijuvix-input-show-translations'."
  :group 'minijuvix2
  :group 'leim)

(defcustom minijuvix-input-tweak-all
  '(minijuvix-input-compose
    (minijuvix-input-prepend "\\")
    (minijuvix-input-nonempty))
  "An expression yielding a function which can be used to tweak
all translations before they are included in the input method.
The resulting function (if non-nil) is applied to every
\(KEY-SEQUENCE . TRANSLATION) pair and should return a list of such
pairs. (Note that the translations can be anything accepted by
`quail-defrule'.)

If you change this setting manually (without using the
customization buffer) you need to call `minijuvix-input-setup' in
order for the change to take effect."
  :group 'minijuvix-input
  :set 'minijuvix-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type 'sexp)

(defcustom minijuvix-input-inherit
  `(("TeX" . (minijuvix-input-compose
              (minijuvix-input-drop '("geq" "leq" "bullet" "qed" "par"))
              (minijuvix-input-or
               (minijuvix-input-drop-prefix "\\")
               (minijuvix-input-or
                (minijuvix-input-compose
                 (minijuvix-input-drop '("^l" "^o" "^r" "^v"))
                 (minijuvix-input-prefix "^"))
                (minijuvix-input-prefix "_")))))
    )
  "A list of Quail input methods whose translations should be
inherited by the minijuvix input method (with the exception of
translations corresponding to ASCII characters).

The list consists of pairs (qp . tweak), where qp is the name of
a Quail package, and tweak is an expression of the same kind as
`minijuvix-input-tweak-all' which is used to tweak the translation
pairs of the input method.

The inherited translation pairs are added last, after
`minijuvix-input-user-translations' and `minijuvix-input-translations'.

If you change this setting manually (without using the
customization buffer) you need to call `minijuvix-input-setup' in
order for the change to take effect."
  :group 'minijuvix-input
  :set 'minijuvix-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Quail package")
                       (sexp :tag "Tweaking function"))))

(defcustom minijuvix-input-translations
  (let ((max-lisp-eval-depth 2800)) `(

  ;; Equality and similar symbols.

  ("eq"  . ,(minijuvix-input-to-string-list "=∼∽≈≋∻∾∿≀≃⋍≂≅ ≌≊≡≣≐≑≒≓≔≕≖≗≘≙≚≛≜≝≞≟≍≎≏≬⋕"))
  ("eqn" . ,(minijuvix-input-to-string-list "≠≁ ≉     ≄  ≇≆  ≢                 ≭    "))

                    ("=n"  . ("≠"))
  ("~"    . ("∼"))  ("~n"  . ("≁"))
  ("~~"   . ("≈"))  ("~~n" . ("≉"))
  ("~~~"  . ("≋"))
  (":~"   . ("∻"))
  ("~-"   . ("≃"))  ("~-n" . ("≄"))
  ("-~"   . ("≂"))
  ("~="   . ("≅"))  ("~=n" . ("≇"))
  ("~~-"  . ("≊"))
  ("=="   . ("≡"))  ("==n" . ("≢"))
  ("==="  . ("≣"))
  (".="   . ("≐"))  (".=." . ("≑"))
  (":="   . ("≔"))  ("=:"  . ("≕"))
  ("=o"   . ("≗"))
  ("(="   . ("≘"))
  ("and=" . ("≙"))  ("or=" . ("≚"))
  ("*="   . ("≛"))
  ("t="   . ("≜"))
  ("def=" . ("≝"))
  ("m="   . ("≞"))
  ("?="   . ("≟"))

  ;; Inequality and similar symbols.

  ("leq"  . ,(minijuvix-input-to-string-list "<≪⋘≤≦≲ ≶≺≼≾⊂⊆ ⋐⊏⊑ ⊰⊲⊴⋖⋚⋜⋞"))
  ("leqn" . ,(minijuvix-input-to-string-list "≮  ≰≨≴⋦≸⊀ ⋨⊄⊈⊊  ⋢⋤ ⋪⋬   ⋠"))
  ("geq"  . ,(minijuvix-input-to-string-list ">≫⋙≥≧≳ ≷≻≽≿⊃⊇ ⋑⊐⊒ ⊱⊳⊵⋗⋛⋝⋟"))
  ("geqn" . ,(minijuvix-input-to-string-list "≯  ≱≩≵⋧≹⊁ ⋩⊅⊉⊋  ⋣⋥ ⋫⋭   ⋡"))

  ("<="   . ("≤"))  (">="   . ("≥"))
  ("<=n"  . ("≰"))  (">=n"  . ("≱"))
  ("len"  . ("≰"))  ("gen"  . ("≱"))
  ("<n"   . ("≮"))  (">n"   . ("≯"))
  ("<~"   . ("≲"))  (">~"   . ("≳"))
  ("<~n"  . ("⋦"))  (">~n"  . ("⋧"))
  ("<~nn" . ("≴"))  (">~nn" . ("≵"))

  ("sub"   . ("⊂"))  ("sup"   . ("⊃"))
  ("subn"  . ("⊄"))  ("supn"  . ("⊅"))
  ("sub="  . ("⊆"))  ("sup="  . ("⊇"))
  ("sub=n" . ("⊈"))  ("sup=n" . ("⊉"))

  ("squb"   . ("⊏"))  ("squp"   . ("⊐"))
  ("squb="  . ("⊑"))  ("squp="  . ("⊒"))
  ("squb=n" . ("⋢"))  ("squp=n" . ("⋣"))

  ;; Set membership etc.

  ("member" . ,(minijuvix-input-to-string-list "∈∉∊∋∌∍⋲⋳⋴⋵⋶⋷⋸⋹⋺⋻⋼⋽⋾⋿"))

  ("inn" . ("∉"))
  ("nin" . ("∌"))

  ;; Intersections, unions etc.

  ("intersection" . ,(minijuvix-input-to-string-list "∩⋂∧⋀⋏⨇⊓⨅⋒∏ ⊼      ⨉"))
  ("union"        . ,(minijuvix-input-to-string-list "∪⋃∨⋁⋎⨈⊔⨆⋓∐⨿⊽⊻⊍⨃⊎⨄⊌∑⅀"))

  ("and" . ("∧"))  ("or"  . ("∨"))
  ("And" . ("⋀"))  ("Or"  . ("⋁"))
  ("i"   . ("∩"))  ("un"  . ("∪"))  ("u+" . ("⊎"))  ("u." . ("⊍"))
  ("I"   . ("⋂"))  ("Un"  . ("⋃"))  ("U+" . ("⨄"))  ("U." . ("⨃"))
  ("glb" . ("⊓"))  ("lub" . ("⊔"))
  ("Glb" . ("⨅"))  ("Lub" . ("⨆"))

  ;; Entailment etc.

  ("entails" . ,(minijuvix-input-to-string-list "⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯"))

  ("|-"   . ("⊢"))  ("|-n"  . ("⊬"))
  ("-|"   . ("⊣"))
  ("|="   . ("⊨"))  ("|=n"  . ("⊭"))
  ("||-"  . ("⊩"))  ("||-n" . ("⊮"))
  ("||="  . ("⊫"))  ("||=n" . ("⊯"))
  ("|||-" . ("⊪"))

  ;; Divisibility, parallelity.

  ("|"  . ("∣"))  ("|n"  . ("∤"))
  ("||" . ("∥"))  ("||n" . ("∦"))

  ;; Some symbols from logic and set theory.

  ("all" . ("∀"))
  ("ex"  . ("∃"))
  ("exn" . ("∄"))
  ("0"   . ("∅"))
  ("C"   . ("∁"))

  ;; Corners, ceilings and floors.

  ("c"  . ,(minijuvix-input-to-string-list "⌜⌝⌞⌟⌈⌉⌊⌋"))
  ("cu" . ,(minijuvix-input-to-string-list "⌜⌝  ⌈⌉  "))
  ("cl" . ,(minijuvix-input-to-string-list "  ⌞⌟  ⌊⌋"))

  ("cul" . ("⌜"))  ("cuL" . ("⌈"))
  ("cur" . ("⌝"))  ("cuR" . ("⌉"))
  ("cll" . ("⌞"))  ("clL" . ("⌊"))
  ("clr" . ("⌟"))  ("clR" . ("⌋"))

  ;; Various operators/symbols.

  ("qed"       . ("∎"))
  ("x"         . ("×"))
  ("o"         . ("∘"))
  ("comp"      . ("∘"))
  ("."         . ("∙"))
  ("*"         . ("⋆"))
  (".+"        . ("∔"))
  (".-"        . ("∸"))
  (":"         . ,(minijuvix-input-to-string-list "∶⦂ː꞉˸፥፦：﹕︓"))
  (","         . ,(minijuvix-input-to-string-list "ʻ،⸲⸴⹁⹉、︐︑﹐﹑，､"))
  (";"         . ,(minijuvix-input-to-string-list "؛⁏፤꛶；︔﹔⍮⸵;"))
  ("::"        . ("∷"))
  ("::-"       . ("∺"))
  ("-:"        . ("∹"))
  ("+ "        . ("⊹"))
  ("surd3"     . ("∛"))
  ("surd4"     . ("∜"))
  ("increment" . ("∆"))
  ("inf"       . ("∞"))
  ("&"         . ("⅋"))
  ("z;"        . ("⨟"))
  ("z:"        . ("⦂"))

  ;; Circled operators.

  ("o+"  . ("⊕"))
  ("o--" . ("⊖"))
  ("ox"  . ("⊗"))
  ("o/"  . ("⊘"))
  ("o."  . ("⊙"))
  ("oo"  . ("⊚"))
  ("o*"  . ("⊛"))
  ("o="  . ("⊜"))
  ("o-"  . ("⊝"))

  ("O+"  . ("⨁"))
  ("Ox"  . ("⨂"))
  ("O."  . ("⨀"))
  ("O*"  . ("⍟"))

  ;; Boxed operators.

  ("b+" . ("⊞"))
  ("b-" . ("⊟"))
  ("bx" . ("⊠"))
  ("b." . ("⊡"))

  ;; Various symbols.

  ("integral" . ,(minijuvix-input-to-string-list "∫∬∭∮∯∰∱∲∳"))
  ("angle"    . ,(minijuvix-input-to-string-list "∟∡∢⊾⊿"))
  ("join"     . ,(minijuvix-input-to-string-list "⋈⋉⋊⋋⋌⨝⟕⟖⟗"))

  ;; Arrows.

  ("l"  . ,(minijuvix-input-to-string-list "←⇐⇚⇇⇆↤⇦↞↼↽⇠⇺↜⇽⟵⟸↚⇍⇷ ↹     ↢↩↫⇋⇜⇤⟻⟽⤆↶↺⟲                                     "))
  ("r"  . ,(minijuvix-input-to-string-list "→⇒⇛⇉⇄↦⇨↠⇀⇁⇢⇻↝⇾⟶⟹↛⇏⇸⇶ ↴    ↣↪↬⇌⇝⇥⟼⟾⤇↷↻⟳⇰⇴⟴⟿ ➵➸➙➔➛➜➝➞➟➠➡➢➣➤➧➨➩➪➫➬➭➮➯➱➲➳➺➻➼➽➾⊸"))
  ("u"  . ,(minijuvix-input-to-string-list "↑⇑⟰⇈⇅↥⇧↟↿↾⇡⇞          ↰↱➦ ⇪⇫⇬⇭⇮⇯                                           "))
  ("d"  . ,(minijuvix-input-to-string-list "↓⇓⟱⇊⇵↧⇩↡⇃⇂⇣⇟         ↵↲↳➥ ↯                                                "))
  ("ud" . ,(minijuvix-input-to-string-list "↕⇕   ↨⇳                                                                    "))
  ("lr" . ,(minijuvix-input-to-string-list "↔⇔         ⇼↭⇿⟷⟺↮⇎⇹                                                        "))
  ("ul" . ,(minijuvix-input-to-string-list "↖⇖                        ⇱↸                                               "))
  ("ur" . ,(minijuvix-input-to-string-list "↗⇗                                         ➶➹➚                             "))
  ("dr" . ,(minijuvix-input-to-string-list "↘⇘                        ⇲                ➴➷➘                             "))
  ("dl" . ,(minijuvix-input-to-string-list "↙⇙                                                                         "))

  ("l-"  . ("←"))  ("<-"  . ("←"))  ("l="  . ("⇐"))  ("<="  . ("⇐"))
  ("r-"  . ("→"))  ("->"  . ("→"))  ("r="  . ("⇒"))  ("=>"  . ("⇒"))
  ("u-"  . ("↑"))                   ("u="  . ("⇑"))
  ("d-"  . ("↓"))                   ("d="  . ("⇓"))
  ("ud-" . ("↕"))                   ("ud=" . ("⇕"))
  ("lr-" . ("↔"))  ("<->" . ("↔"))  ("lr=" . ("⇔"))  ("<=>" . ("⇔"))
  ("ul-" . ("↖"))                   ("ul=" . ("⇖"))
  ("ur-" . ("↗"))                   ("ur=" . ("⇗"))
  ("dr-" . ("↘"))                   ("dr=" . ("⇘"))
  ("dl-" . ("↙"))                   ("dl=" . ("⇙"))

  ("l==" . ("⇚"))  ("l-2" . ("⇇"))                   ("l-r-" . ("⇆"))
  ("r==" . ("⇛"))  ("r-2" . ("⇉"))  ("r-3" . ("⇶"))  ("r-l-" . ("⇄"))
  ("u==" . ("⟰"))  ("u-2" . ("⇈"))                   ("u-d-" . ("⇅"))
  ("d==" . ("⟱"))  ("d-2" . ("⇊"))                   ("d-u-" . ("⇵"))

  ("l--"  . ("⟵"))  ("<--"  . ("⟵"))  ("l~"  . ("↜" "⇜"))
  ("r--"  . ("⟶"))  ("-->"  . ("⟶"))  ("r~"  . ("↝" "⇝" "⟿"))
  ("lr--" . ("⟷"))  ("<-->" . ("⟷"))  ("lr~" . ("↭"))

  ("l-n"  . ("↚"))  ("<-n"  . ("↚"))  ("l=n"  . ("⇍"))
  ("r-n"  . ("↛"))  ("->n"  . ("↛"))  ("r=n"  . ("⇏"))  ("=>n"  . ("⇏"))
  ("lr-n" . ("↮"))  ("<->n" . ("↮"))  ("lr=n" . ("⇎"))  ("<=>n" . ("⇎"))

  ("l-|"  . ("↤"))  ("ll-" . ("↞"))
  ("r-|"  . ("↦"))  ("rr-" . ("↠"))
  ("u-|"  . ("↥"))  ("uu-" . ("↟"))
  ("d-|"  . ("↧"))  ("dd-" . ("↡"))
  ("ud-|" . ("↨"))

  ("l->" . ("↢"))
  ("r->" . ("↣"))

  ("r-o" . ("⊸"))  ("-o"  . ("⊸"))

  ("dz" . ("↯"))

  ;; Ellipsis.

  ("..." . ,(minijuvix-input-to-string-list "⋯⋮⋰⋱"))

  ;; Box-drawing characters.

  ("---" . ,(minijuvix-input-to-string-list "─│┌┐└┘├┤┬┼┴╴╵╶╷╭╮╯╰╱╲╳"))
  ("--=" . ,(minijuvix-input-to-string-list "═║╔╗╚╝╠╣╦╬╩     ╒╕╘╛╞╡╤╪╧ ╓╖╙╜╟╢╥╫╨"))
  ("--_" . ,(minijuvix-input-to-string-list "━┃┏┓┗┛┣┫┳╋┻╸╹╺╻
                                        ┍┯┑┕┷┙┝┿┥┎┰┒┖┸┚┠╂┨┞╀┦┟╁┧┢╈┪┡╇┩
                                        ┮┭┶┵┾┽┲┱┺┹╊╉╆╅╄╃ ╿╽╼╾"))
  ("--." . ,(minijuvix-input-to-string-list "╌╎┄┆┈┊
                                        ╍╏┅┇┉┋"))

  ;; Triangles.

  ;; Big/small, black/white.

  ("t" . ,(minijuvix-input-to-string-list "◂◃◄◅▸▹►▻▴▵▾▿◢◿◣◺◤◸◥◹"))
  ("T" . ,(minijuvix-input-to-string-list "◀◁▶▷▲△▼▽◬◭◮"))

  ("tb" . ,(minijuvix-input-to-string-list "◂▸▴▾◄►◢◣◤◥"))
  ("tw" . ,(minijuvix-input-to-string-list "◃▹▵▿◅▻◿◺◸◹"))

  ("Tb" . ,(minijuvix-input-to-string-list "◀▶▲▼"))
  ("Tw" . ,(minijuvix-input-to-string-list "◁▷△▽"))

  ;; Squares.

  ("sq"  . ,(minijuvix-input-to-string-list "■□◼◻◾◽▣▢▤▥▦▧▨▩◧◨◩◪◫◰◱◲◳"))
  ("sqb" . ,(minijuvix-input-to-string-list "■◼◾"))
  ("sqw" . ,(minijuvix-input-to-string-list "□◻◽"))
  ("sq." . ("▣"))
  ("sqo" . ("▢"))

  ;; Rectangles.

  ("re"  . ,(minijuvix-input-to-string-list "▬▭▮▯"))
  ("reb" . ,(minijuvix-input-to-string-list "▬▮"))
  ("rew" . ,(minijuvix-input-to-string-list "▭▯"))

  ;; Parallelograms.

  ("pa"  . ,(minijuvix-input-to-string-list "▰▱"))
  ("pab" . ("▰"))
  ("paw" . ("▱"))

  ;; Diamonds.

  ("di"  . ,(minijuvix-input-to-string-list "◆◇◈"))
  ("dib" . ("◆"))
  ("diw" . ("◇"))
  ("di." . ("◈"))

  ;; Circles.

  ("ci"   . ,(minijuvix-input-to-string-list "●○◎◌◯◍◐◑◒◓◔◕◖◗◠◡◴◵◶◷⚆⚇⚈⚉"))
  ("cib"  . ("●"))
  ("ciw"  . ("○"))
  ("ci."  . ("◎"))
  ("ci.." . ("◌"))
  ("ciO"  . ("◯"))

  ;; Stars.

  ("st"   . ,(minijuvix-input-to-string-list "⋆✦✧✶✴✹ ★☆✪✫✯✰✵✷✸"))
  ("st4"  . ,(minijuvix-input-to-string-list "✦✧"))
  ("st6"  . ("✶"))
  ("st8"  . ("✴"))
  ("st12" . ("✹"))

  ;; Blackboard bold letters.

  ("bA"   . ("𝔸"))
  ("bB"   . ("𝔹"))
  ("bC"   . ("ℂ"))
  ("bD"   . ("𝔻"))
  ("bE"   . ("𝔼"))
  ("bF"   . ("𝔽"))
  ("bG"   . ("𝔾"))
  ("bH"   . ("ℍ"))
  ("bI"   . ("𝕀"))
  ("bJ"   . ("𝕁"))
  ("bK"   . ("𝕂"))
  ("bL"   . ("𝕃"))
  ("bM"   . ("𝕄"))
  ("bN"   . ("ℕ"))
  ("bO"   . ("𝕆"))
  ("bP"   . ("ℙ"))
  ("bQ"   . ("ℚ"))
  ("bR"   . ("ℝ"))
  ("bS"   . ("𝕊"))
  ("bT"   . ("𝕋"))
  ("bU"   . ("𝕌"))
  ("bV"   . ("𝕍"))
  ("bW"   . ("𝕎"))
  ("bX"   . ("𝕏"))
  ("bY"   . ("𝕐"))
  ("bZ"   . ("ℤ"))
  ("bGG"  . ("ℾ"))
  ("bGP"  . ("ℿ"))
  ("bGS"  . ("⅀"))
  ("ba"   . ("𝕒"))
  ("bb"   . ("𝕓"))
  ("bc"   . ("𝕔"))
  ("bd"   . ("𝕕"))
  ("be"   . ("𝕖"))
  ("bf"   . ("𝕗"))
  ("bg"   . ("𝕘"))
  ("bh"   . ("𝕙"))
  ("bi"   . ("𝕚"))
  ("bj"   . ("𝕛"))
  ("bk"   . ("𝕜"))
  ("bl"   . ("𝕝"))
  ("bm"   . ("𝕞"))
  ("bn"   . ("𝕟"))
  ("bo"   . ("𝕠"))
  ("bp"   . ("𝕡"))
  ("bq"   . ("𝕢"))
  ("br"   . ("𝕣"))
  ("bs"   . ("𝕤"))
  ("bt"   . ("𝕥"))
  ("bu"   . ("𝕦"))
  ("bv"   . ("𝕧"))
  ("bw"   . ("𝕨"))
  ("bx"   . ("𝕩"))
  ("by"   . ("𝕪"))
  ("bz"   . ("𝕫"))
  ("bGg"  . ("ℽ"))
  ("bGp"  . ("ℼ"))

  ;; Blackboard bold numbers.

  ("b0"   . ("𝟘"))
  ("b1"   . ("𝟙"))
  ("b2"   . ("𝟚"))
  ("b3"   . ("𝟛"))
  ("b4"   . ("𝟜"))
  ("b5"   . ("𝟝"))
  ("b6"   . ("𝟞"))
  ("b7"   . ("𝟟"))
  ("b8"   . ("𝟠"))
  ("b9"   . ("𝟡"))

  ;; Mathematical bold digits.

  ("B0"   . ("𝟎"))
  ("B1"   . ("𝟏"))
  ("B2"   . ("𝟐"))
  ("B3"   . ("𝟑"))
  ("B4"   . ("𝟒"))
  ("B5"   . ("𝟓"))
  ("B6"   . ("𝟔"))
  ("B7"   . ("𝟕"))
  ("B8"   . ("𝟖"))
  ("B9"   . ("𝟗"))

  ;; Parentheses.

  ("(" . ,(minijuvix-input-to-string-list "([{⁅⁽₍〈⎴⟅⟦⟨⟪⦃〈《「『【〔〖〚︵︷︹︻︽︿﹁﹃﹙﹛﹝（［｛｢❪❬❰❲❴⟮⦅⦗⧼⸨❮⦇⦉"))
  (")" . ,(minijuvix-input-to-string-list ")]}⁆⁾₎〉⎵⟆⟧⟩⟫⦄〉》」』】〕〗〛︶︸︺︼︾﹀﹂﹄﹚﹜﹞）］｝｣❫❭❱❳❵⟯⦆⦘⧽⸩❯⦈⦊"))

  ("[[" . ("⟦"))
  ("]]" . ("⟧"))
  ("<"  . ("⟨"))
  (">"  . ("⟩"))
  ("<<" . ("⟪"))
  (">>" . ("⟫"))
  ("{{" . ("⦃"))
  ("}}" . ("⦄"))

  ("(b" . ("⟅"))
  (")b" . ("⟆"))

  ("lbag" . ("⟅"))
  ("rbag" . ("⟆"))

  ("<|" . ("⦉"))  ;; Angle bar brackets
  ("|>" . ("⦊"))

  ("(|" . ("⦇"))  ;; Idiom brackets
  ("|)" . ("⦈"))

  ("((" . ("⦅"))  ;; Banana brackets
  ("))" . ("⦆"))

  ;; Primes.

  ("'" . ,(minijuvix-input-to-string-list "′″‴⁗"))
  ("`" . ,(minijuvix-input-to-string-list "‵‶‷"))

  ;; Fractions.

  ("frac" . ,(minijuvix-input-to-string-list "¼½¾⅓⅔⅕⅖⅗⅘⅙⅚⅛⅜⅝⅞⅟"))

  ;; Bullets.

  ("bu"  . ,(minijuvix-input-to-string-list "•◦‣⁌⁍"))
  ("bub" . ("•"))
  ("buw" . ("◦"))
  ("but" . ("‣"))

  ;; Musical symbols.

  ("note" . ,(minijuvix-input-to-string-list "♩♪♫♬"))
  ("b"    . ("♭"))
  ("#"    . ("♯"))

  ;; Other punctuation and symbols.

  ("\\"         . ("\\"))
  ("en"         . ("–"))
  ("em"         . ("—"))
  ("!!"         . ("‼"))
  ("??"         . ("⁇"))
  ("?!"         . ("‽" "⁈"))
  ("!?"         . ("⁉"))
  ("die"        . ,(minijuvix-input-to-string-list "⚀⚁⚂⚃⚄⚅"))
  ("asterisk"   . ,(minijuvix-input-to-string-list "⁎⁑⁂✢✣✤✥✱✲✳✺✻✼✽❃❉❊❋"))
  ("8<"         . ("✂" "✄"))
  ("tie"        . ("⁀"))
  ("undertie"   . ("‿"))
  ("apl"        . ,(minijuvix-input-to-string-list "⌶⌷⌸⌹⌺⌻⌼⌽⌾⌿⍀⍁⍂⍃⍄⍅⍆⍇⍈
                                               ⍉⍊⍋⍌⍍⍎⍏⍐⍑⍒⍓⍔⍕⍖⍗⍘⍙⍚⍛
                                               ⍜⍝⍞⍟⍠⍡⍢⍣⍤⍥⍦⍧⍨⍩⍪⍫⍬⍭⍮
                                               ⍯⍰⍱⍲⍳⍴⍵⍶⍷⍸⍹⍺⎕"))

  ;; Some combining characters.
  ;;
  ;; The following combining characters also have (other)
  ;; translations:
  ;; ̀ ́ ̂ ̃ ̄ ̆ ̇ ̈ ̋ ̌ ̣ ̧ ̱

  ("^--" . ,(minijuvix-input-to-string-list"̅̿"))
  ("_--" . ,(minijuvix-input-to-string-list"̲̳"))
  ("^~"  . ,(minijuvix-input-to-string-list"̃͌"))
  ("_~"  .  (                         "̰"))
  ("^."  . ,(minijuvix-input-to-string-list"̇̈⃛⃜"))
  ("_."  . ,(minijuvix-input-to-string-list"̣̤"))
  ("^l"  . ,(minijuvix-input-to-string-list"⃖⃐⃔"))
  ("^l-" .  (                         "⃖"))
  ("^r"  . ,(minijuvix-input-to-string-list"⃗⃑⃕"))
  ("^r-" .  (                         "⃗"))
  ("^lr" .  (                         "⃡"))
  ("_lr" .  (                         "͍"))
  ("^^"  . ,(minijuvix-input-to-string-list"̂̑͆"))
  ("_^"  . ,(minijuvix-input-to-string-list"̭̯̪"))
  ("^v"  . ,(minijuvix-input-to-string-list"̌̆"))
  ("_v"  . ,(minijuvix-input-to-string-list"̬̮̺"))

  ;; Shorter forms of many greek letters plus ƛ.

  ("Ga"  . ("α"))  ("GA"  . ("Α"))
  ("Gb"  . ("β"))  ("GB"  . ("Β"))
  ("Gg"  . ("γ"))  ("GG"  . ("Γ"))
  ("Gd"  . ("δ"))  ("GD"  . ("Δ"))
  ("Ge"  . ("ε"))  ("GE"  . ("Ε"))
  ("Gz"  . ("ζ"))  ("GZ"  . ("Ζ"))
  ("Gh"  . ("η"))  ("GH"  . ("Η"))
  ("Gth" . ("θ"))  ("GTH" . ("Θ"))
  ("Gi"  . ("ι"))  ("GI"  . ("Ι"))
  ("Gk"  . ("κ"))  ("GK"  . ("Κ"))
  ("Gl"  . ("λ"))  ("GL"  . ("Λ"))  ("Gl-" . ("ƛ"))
  ("Gm"  . ("μ"))  ("GM"  . ("Μ"))
  ("Gn"  . ("ν"))  ("GN"  . ("Ν"))
  ("Gx"  . ("ξ"))  ("GX"  . ("Ξ"))
  ;; \omicron \Omicron
  ;; \pi \Pi
  ("Gr"  . ("ρ"))  ("GR"  . ("Ρ"))
  ("Gs"  . ("σ"))  ("GS"  . ("Σ"))
  ("Gt"  . ("τ"))  ("GT"  . ("Τ"))
  ("Gu"  . ("υ"))  ("GU"  . ("Υ"))
  ("Gf"  . ("φ"))  ("GF"  . ("Φ"))
  ("Gc"  . ("χ"))  ("GC"  . ("Χ"))
  ("Gp"  . ("ψ"))  ("GP"  . ("Ψ"))
  ("Go"  . ("ω"))  ("GO"  . ("Ω"))

  ;; Mathematical characters

  ("MiA" . ("𝐴"))
  ("MiB" . ("𝐵"))
  ("MiC" . ("𝐶"))
  ("MiD" . ("𝐷"))
  ("MiE" . ("𝐸"))
  ("MiF" . ("𝐹"))
  ("MiG" . ("𝐺"))
  ("MiH" . ("𝐻"))
  ("MiI" . ("𝐼"))
  ("MiJ" . ("𝐽"))
  ("MiK" . ("𝐾"))
  ("MiL" . ("𝐿"))
  ("MiM" . ("𝑀"))
  ("MiN" . ("𝑁"))
  ("MiO" . ("𝑂"))
  ("MiP" . ("𝑃"))
  ("MiQ" . ("𝑄"))
  ("MiR" . ("𝑅"))
  ("MiS" . ("𝑆"))
  ("MiT" . ("𝑇"))
  ("MiU" . ("𝑈"))
  ("MiV" . ("𝑉"))
  ("MiW" . ("𝑊"))
  ("MiX" . ("𝑋"))
  ("MiY" . ("𝑌"))
  ("MiZ" . ("𝑍"))
  ("Mia" . ("𝑎"))
  ("Mib" . ("𝑏"))
  ("Mic" . ("𝑐"))
  ("Mid" . ("𝑑"))
  ("Mie" . ("𝑒"))
  ("Mif" . ("𝑓"))
  ("Mig" . ("𝑔"))
  ("Mih" . ("ℎ"))
  ("Mii" . ("𝑖"))
  ("Mij" . ("𝑗"))
  ("Mik" . ("𝑘"))
  ("Mil" . ("𝑙"))
  ("Mim" . ("𝑚"))
  ("Min" . ("𝑛"))
  ("Mio" . ("𝑜"))
  ("Mip" . ("𝑝"))
  ("Miq" . ("𝑞"))
  ("Mir" . ("𝑟"))
  ("Mis" . ("𝑠"))
  ("Mit" . ("𝑡"))
  ("Miu" . ("𝑢"))
  ("Miv" . ("𝑣"))
  ("Miw" . ("𝑤"))
  ("Mix" . ("𝑥"))
  ("Miy" . ("𝑦"))
  ("Miz" . ("𝑧"))
  ("MIA" . ("𝑨"))
  ("MIB" . ("𝑩"))
  ("MIC" . ("𝑪"))
  ("MID" . ("𝑫"))
  ("MIE" . ("𝑬"))
  ("MIF" . ("𝑭"))
  ("MIG" . ("𝑮"))
  ("MIH" . ("𝑯"))
  ("MII" . ("𝑰"))
  ("MIJ" . ("𝑱"))
  ("MIK" . ("𝑲"))
  ("MIL" . ("𝑳"))
  ("MIM" . ("𝑴"))
  ("MIN" . ("𝑵"))
  ("MIO" . ("𝑶"))
  ("MIP" . ("𝑷"))
  ("MIQ" . ("𝑸"))
  ("MIR" . ("𝑹"))
  ("MIS" . ("𝑺"))
  ("MIT" . ("𝑻"))
  ("MIU" . ("𝑼"))
  ("MIV" . ("𝑽"))
  ("MIW" . ("𝑾"))
  ("MIX" . ("𝑿"))
  ("MIY" . ("𝒀"))
  ("MIZ" . ("𝒁"))
  ("MIa" . ("𝒂"))
  ("MIb" . ("𝒃"))
  ("MIc" . ("𝒄"))
  ("MId" . ("𝒅"))
  ("MIe" . ("𝒆"))
  ("MIf" . ("𝒇"))
  ("MIg" . ("𝒈"))
  ("MIh" . ("𝒉"))
  ("MIi" . ("𝒊"))
  ("MIj" . ("𝒋"))
  ("MIk" . ("𝒌"))
  ("MIl" . ("𝒍"))
  ("MIm" . ("𝒎"))
  ("MIn" . ("𝒏"))
  ("MIo" . ("𝒐"))
  ("MIp" . ("𝒑"))
  ("MIq" . ("𝒒"))
  ("MIr" . ("𝒓"))
  ("MIs" . ("𝒔"))
  ("MIt" . ("𝒕"))
  ("MIu" . ("𝒖"))
  ("MIv" . ("𝒗"))
  ("MIw" . ("𝒘"))
  ("MIx" . ("𝒙"))
  ("MIy" . ("𝒚"))
  ("MIz" . ("𝒛"))
  ("McA" . ("𝒜"))
  ("McB" . ("ℬ"))
  ("McC" . ("𝒞"))
  ("McD" . ("𝒟"))
  ("McE" . ("ℰ"))
  ("McF" . ("ℱ"))
  ("McG" . ("𝒢"))
  ("McH" . ("ℋ"))
  ("McI" . ("ℐ"))
  ("McJ" . ("𝒥"))
  ("McK" . ("𝒦"))
  ("McL" . ("ℒ"))
  ("McM" . ("ℳ"))
  ("McN" . ("𝒩"))
  ("McO" . ("𝒪"))
  ("McP" . ("𝒫"))
  ("McQ" . ("𝒬"))
  ("McR" . ("ℛ"))
  ("McS" . ("𝒮"))
  ("McT" . ("𝒯"))
  ("McU" . ("𝒰"))
  ("McV" . ("𝒱"))
  ("McW" . ("𝒲"))
  ("McX" . ("𝒳"))
  ("McY" . ("𝒴"))
  ("McZ" . ("𝒵"))
  ("Mca" . ("𝒶"))
  ("Mcb" . ("𝒷"))
  ("Mcc" . ("𝒸"))
  ("Mcd" . ("𝒹"))
  ("Mce" . ("ℯ"))
  ("Mcf" . ("𝒻"))
  ("Mcg" . ("ℊ"))
  ("Mch" . ("𝒽"))
  ("Mci" . ("𝒾"))
  ("Mcj" . ("𝒿"))
  ("Mck" . ("𝓀"))
  ("Mcl" . ("𝓁"))
  ("Mcm" . ("𝓂"))
  ("Mcn" . ("𝓃"))
  ("Mco" . ("ℴ"))
  ("Mcp" . ("𝓅"))
  ("Mcq" . ("𝓆"))
  ("Mcr" . ("𝓇"))
  ("Mcs" . ("𝓈"))
  ("Mct" . ("𝓉"))
  ("Mcu" . ("𝓊"))
  ("Mcv" . ("𝓋"))
  ("Mcw" . ("𝓌"))
  ("Mcx" . ("𝓍"))
  ("Mcy" . ("𝓎"))
  ("Mcz" . ("𝓏"))
  ("MCA" . ("𝓐"))
  ("MCB" . ("𝓑"))
  ("MCC" . ("𝓒"))
  ("MCD" . ("𝓓"))
  ("MCE" . ("𝓔"))
  ("MCF" . ("𝓕"))
  ("MCG" . ("𝓖"))
  ("MCH" . ("𝓗"))
  ("MCI" . ("𝓘"))
  ("MCJ" . ("𝓙"))
  ("MCK" . ("𝓚"))
  ("MCL" . ("𝓛"))
  ("MCM" . ("𝓜"))
  ("MCN" . ("𝓝"))
  ("MCO" . ("𝓞"))
  ("MCP" . ("𝓟"))
  ("MCQ" . ("𝓠"))
  ("MCR" . ("𝓡"))
  ("MCS" . ("𝓢"))
  ("MCT" . ("𝓣"))
  ("MCU" . ("𝓤"))
  ("MCV" . ("𝓥"))
  ("MCW" . ("𝓦"))
  ("MCX" . ("𝓧"))
  ("MCY" . ("𝓨"))
  ("MCZ" . ("𝓩"))
  ("MCa" . ("𝓪"))
  ("MCb" . ("𝓫"))
  ("MCc" . ("𝓬"))
  ("MCd" . ("𝓭"))
  ("MCe" . ("𝓮"))
  ("MCf" . ("𝓯"))
  ("MCg" . ("𝓰"))
  ("MCh" . ("𝓱"))
  ("MCi" . ("𝓲"))
  ("MCj" . ("𝓳"))
  ("MCk" . ("𝓴"))
  ("MCl" . ("𝓵"))
  ("MCm" . ("𝓶"))
  ("MCn" . ("𝓷"))
  ("MCo" . ("𝓸"))
  ("MCp" . ("𝓹"))
  ("MCq" . ("𝓺"))
  ("MCr" . ("𝓻"))
  ("MCs" . ("𝓼"))
  ("MCt" . ("𝓽"))
  ("MCu" . ("𝓾"))
  ("MCv" . ("𝓿"))
  ("MCw" . ("𝔀"))
  ("MCx" . ("𝔁"))
  ("MCy" . ("𝔂"))
  ("MCz" . ("𝔃"))
  ("MfA" . ("𝔄"))
  ("MfB" . ("𝔅"))
  ("MfC" . ("ℭ"))
  ("MfD" . ("𝔇"))
  ("MfE" . ("𝔈"))
  ("MfF" . ("𝔉"))
  ("MfG" . ("𝔊"))
  ("MfH" . ("ℌ"))
  ("MfI" . ("ℑ"))
  ("MfJ" . ("𝔍"))
  ("MfK" . ("𝔎"))
  ("MfL" . ("𝔏"))
  ("MfM" . ("𝔐"))
  ("MfN" . ("𝔑"))
  ("MfO" . ("𝔒"))
  ("MfP" . ("𝔓"))
  ("MfQ" . ("𝔔"))
  ("MfR" . ("ℜ"))
  ("MfS" . ("𝔖"))
  ("MfT" . ("𝔗"))
  ("MfU" . ("𝔘"))
  ("MfV" . ("𝔙"))
  ("MfW" . ("𝔚"))
  ("MfX" . ("𝔛"))
  ("MfY" . ("𝔜"))
  ("MfZ" . ("ℨ"))
  ("Mfa" . ("𝔞"))
  ("Mfb" . ("𝔟"))
  ("Mfc" . ("𝔠"))
  ("Mfd" . ("𝔡"))
  ("Mfe" . ("𝔢"))
  ("Mff" . ("𝔣"))
  ("Mfg" . ("𝔤"))
  ("Mfh" . ("𝔥"))
  ("Mfi" . ("𝔦"))
  ("Mfj" . ("𝔧"))
  ("Mfk" . ("𝔨"))
  ("Mfl" . ("𝔩"))
  ("Mfm" . ("𝔪"))
  ("Mfn" . ("𝔫"))
  ("Mfo" . ("𝔬"))
  ("Mfp" . ("𝔭"))
  ("Mfq" . ("𝔮"))
  ("Mfr" . ("𝔯"))
  ("Mfs" . ("𝔰"))
  ("Mft" . ("𝔱"))
  ("Mfu" . ("𝔲"))
  ("Mfv" . ("𝔳"))
  ("Mfw" . ("𝔴"))
  ("Mfx" . ("𝔵"))
  ("Mfy" . ("𝔶"))
  ("Mfz" . ("𝔷"))

  ;; (Sub / Super) scripts
  ;;
  ;; Unicode 12.1 omits several latin characters from sub/superscript.
  ;; https://www.quora.com/Why-is-there-no-character-for-superscript-q-in-Unicode
  ;;
  ;; Perhaps they will be added in future versions, however there are no
  ;; proposals for it currently in the pipeline:
  ;; https://www.unicode.org/alloc/Pipeline.html

  ("_a" . ("ₐ"))
  ;; ("_b" . ("b"))
  ;; ("_c" . ("c"))
  ;; ("_d" . ("d"))
  ("_e" . ("ₑ"))
  ;; ("_f" . ("f"))
  ;; ("_g" . ("g"))
  ("_h" . ("ₕ"))
  ("_i" . ("ᵢ"))
  ("_j" . ("ⱼ"))
  ("_k" . ("ₖ"))
  ("_l" . ("ₗ"))
  ("_m" . ("ₘ"))
  ("_n" . ("ₙ"))
  ("_o" . ("ₒ"))
  ("_p" . ("ₚ"))
  ;; ("_q" . ("q"))
  ("_r" . ("ᵣ"))
  ("_s" . ("ₛ"))
  ("_t" . ("ₜ"))
  ("_u" . ("ᵤ"))
  ("_v" . ("ᵥ"))
  ;; ("_w" . ("w"))
  ("_x" . ("ₓ"))
  ;; ("_y" . ("y"))
  ;; ("_z" . ("z"))

  ("_Gb" . ("ᵦ"))
  ("_Gg" . ("ᵧ"))
  ("_Gr" . ("ᵨ"))
  ("_Gf" . ("ᵩ"))
  ("_Gc" . ("ᵪ"))

  ("^a" . ("ᵃ"))
  ("^b" . ("ᵇ"))
  ("^c" . ("ᶜ"))
  ("^d" . ("ᵈ"))
  ("^e" . ("ᵉ"))
  ("^f" . ("ᶠ"))
  ("^g" . ("ᵍ"))
  ("^h" . ("ʰ"))
  ("^i" . ("ⁱ"))
  ("^j" . ("ʲ"))
  ("^k" . ("ᵏ"))
  ("^l" . ("ˡ"))
  ("^m" . ("ᵐ"))
  ("^n" . ("ⁿ"))
  ("^o" . ("ᵒ"))
  ("^p" . ("ᵖ"))
  ;; ("^q" . ("q"))
  ("^r" . ("ʳ"))
  ("^s" . ("ˢ"))
  ("^t" . ("ᵗ"))
  ("^u" . ("ᵘ"))
  ("^v" . ("ᵛ"))
  ("^w" . ("ʷ"))
  ("^x" . ("ˣ"))
  ("^y" . ("ʸ"))
  ("^z" . ("ᶻ"))

  ("^A" . ("ᴬ"))
  ("^B" . ("ᴮ"))
  ;; ("^C" . ("C"))
  ("^D" . ("ᴰ"))
  ("^E" . ("ᴱ"))
  ;; ("^F" . ("F"))
  ("^G" . ("ᴳ"))
  ("^H" . ("ᴴ"))
  ("^I" . ("ᴵ"))
  ("^J" . ("ᴶ"))
  ("^K" . ("ᴷ"))
  ("^L" . ("ᴸ"))
  ("^M" . ("ᴹ"))
  ("^N" . ("ᴺ"))
  ("^O" . ("ᴼ"))
  ("^P" . ("ᴾ"))
  ;; ("^Q" . ("Q"))
  ("^R" . ("ᴿ"))
  ;; ("^S" . ("S"))
  ("^T" . ("ᵀ"))
  ("^U" . ("ᵁ"))
  ("^V" . ("ⱽ"))
  ("^W" . ("ᵂ"))
  ;; ("^X" . ("X"))
  ;; ("^Y" . ("Y"))
  ;; ("^Z" . ("Z"))

  ("^Gb" . ("ᵝ"))
  ("^Gg" . ("ᵞ"))
  ("^Gd" . ("ᵟ"))
  ("^Ge" . ("ᵋ"))
  ("^Gth" . ("ᶿ"))
  ("^Gf" . ("ᵠ"))
  ("^Gc" . ("ᵡ"))

  ;; Some ISO8859-1 characters.

  (" "         . (" "))
  ("!"         . ("¡"))
  ("cent"      . ("¢"))
  ("brokenbar" . ("¦"))
  ("degree"    . ("°"))
  ("?"         . ("¿"))
  ("^a_"       . ("ª"))
  ("^o_"       . ("º"))

  ;; Circled, parenthesised etc. numbers and letters.

  ( "(0)" . ,(minijuvix-input-to-string-list " ⓪🄀⓿🄋🄌"))
  ( "(1)" . ,(minijuvix-input-to-string-list "⑴①⒈❶➀➊"))
  ( "(2)" . ,(minijuvix-input-to-string-list "⑵②⒉❷➁➋"))
  ( "(3)" . ,(minijuvix-input-to-string-list "⑶③⒊❸➂➌"))
  ( "(4)" . ,(minijuvix-input-to-string-list "⑷④⒋❹➃➍"))
  ( "(5)" . ,(minijuvix-input-to-string-list "⑸⑤⒌❺➄➎"))
  ( "(6)" . ,(minijuvix-input-to-string-list "⑹⑥⒍❻➅➏"))
  ( "(7)" . ,(minijuvix-input-to-string-list "⑺⑦⒎❼➆➐"))
  ( "(8)" . ,(minijuvix-input-to-string-list "⑻⑧⒏❽➇➑"))
  ( "(9)" . ,(minijuvix-input-to-string-list "⑼⑨⒐❾➈➒"))
  ("(10)" . ,(minijuvix-input-to-string-list "⑽⑩⒑❿➉➓"))
  ("(11)" . ,(minijuvix-input-to-string-list "⑾⑪⒒⓫"))
  ("(12)" . ,(minijuvix-input-to-string-list "⑿⑫⒓⓬"))
  ("(13)" . ,(minijuvix-input-to-string-list "⒀⑬⒔⓭"))
  ("(14)" . ,(minijuvix-input-to-string-list "⒁⑭⒕⓮"))
  ("(15)" . ,(minijuvix-input-to-string-list "⒂⑮⒖⓯"))
  ("(16)" . ,(minijuvix-input-to-string-list "⒃⑯⒗⓰"))
  ("(17)" . ,(minijuvix-input-to-string-list "⒄⑰⒘⓱"))
  ("(18)" . ,(minijuvix-input-to-string-list "⒅⑱⒙⓲"))
  ("(19)" . ,(minijuvix-input-to-string-list "⒆⑲⒚⓳"))
  ("(20)" . ,(minijuvix-input-to-string-list "⒇⑳⒛⓴"))

  ("(a)"  . ,(minijuvix-input-to-string-list "⒜Ⓐⓐ🅐🄰🅰"))
  ("(b)"  . ,(minijuvix-input-to-string-list "⒝Ⓑⓑ🅑🄱🅱"))
  ("(c)"  . ,(minijuvix-input-to-string-list "⒞Ⓒⓒ🅒🄲🅲"))
  ("(d)"  . ,(minijuvix-input-to-string-list "⒟Ⓓⓓ🅓🄳🅳"))
  ("(e)"  . ,(minijuvix-input-to-string-list "⒠Ⓔⓔ🅔🄴🅴"))
  ("(f)"  . ,(minijuvix-input-to-string-list "⒡Ⓕⓕ🅕🄵🅵"))
  ("(g)"  . ,(minijuvix-input-to-string-list "⒢Ⓖⓖ🅖🄶🅶"))
  ("(h)"  . ,(minijuvix-input-to-string-list "⒣Ⓗⓗ🅗🄷🅷"))
  ("(i)"  . ,(minijuvix-input-to-string-list "⒤Ⓘⓘ🅘🄸🅸"))
  ("(j)"  . ,(minijuvix-input-to-string-list "⒥Ⓙⓙ🅙🄹🅹"))
  ("(k)"  . ,(minijuvix-input-to-string-list "⒦Ⓚⓚ🅚🄺🅺"))
  ("(l)"  . ,(minijuvix-input-to-string-list "⒧Ⓛⓛ🅛🄻🅻"))
  ("(m)"  . ,(minijuvix-input-to-string-list "⒨Ⓜⓜ🅜🄼🅼"))
  ("(n)"  . ,(minijuvix-input-to-string-list "⒩Ⓝⓝ🅝🄽🅽"))
  ("(o)"  . ,(minijuvix-input-to-string-list "⒪Ⓞⓞ🅞🄾🅾"))
  ("(p)"  . ,(minijuvix-input-to-string-list "⒫Ⓟⓟ🅟🄿🅿"))
  ("(q)"  . ,(minijuvix-input-to-string-list "⒬Ⓠⓠ🅠🅀🆀"))
  ("(r)"  . ,(minijuvix-input-to-string-list "⒭Ⓡⓡ🅡🅁🆁"))
  ("(s)"  . ,(minijuvix-input-to-string-list "⒮Ⓢⓢ🅢🅂🆂"))
  ("(t)"  . ,(minijuvix-input-to-string-list "⒯Ⓣⓣ🅣🅃🆃"))
  ("(u)"  . ,(minijuvix-input-to-string-list "⒰Ⓤⓤ🅤🅄🆄"))
  ("(v)"  . ,(minijuvix-input-to-string-list "⒱Ⓥⓥ🅥🅅🆅"))
  ("(w)"  . ,(minijuvix-input-to-string-list "⒲Ⓦⓦ🅦🅆🆆"))
  ("(x)"  . ,(minijuvix-input-to-string-list "⒳Ⓧⓧ🅧🅇🆇"))
  ("(y)"  . ,(minijuvix-input-to-string-list "⒴Ⓨⓨ🅨🅈🆈"))
  ("(z)"  . ,(minijuvix-input-to-string-list "⒵Ⓩⓩ🅩🅉🆉"))

  ))
  "A list of translations specific to the minijuvix input method.
Each element is a pair (KEY-SEQUENCE-STRING . LIST-OF-TRANSLATION-STRINGS).
All the translation strings are possible translations
of the given key sequence; if there is more than one you can choose
between them using the arrow keys.

Note that if you customize this setting you will not
automatically benefit (or suffer) from modifications to its
default value when the library is updated.  If you just want to
add some bindings it is probably a better idea to customize
`minijuvix-input-user-translations'.

These translation pairs are included after those in
`minijuvix-input-user-translations', but before the ones inherited
from other input methods (see `minijuvix-input-inherit').

If you change this setting manually (without using the
customization buffer) you need to call `minijuvix-input-setup' in
order for the change to take effect."
  :group 'minijuvix-input
  :set 'minijuvix-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Key sequence")
                       (repeat :tag "Translations" string))))

(defcustom minijuvix-input-user-translations nil
  "Like `minijuvix-input-translations', but more suitable for user
customizations since by default it is empty.

These translation pairs are included first, before those in
`minijuvix-input-translations' and the ones inherited from other input
methods."
  :group 'minijuvix-input
  :set 'minijuvix-input-incorporate-changed-setting
  :initialize 'custom-initialize-default
  :type '(repeat (cons (string :tag "Key sequence")
                       (repeat :tag "Translations" string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inspecting and modifying translation maps

(defun minijuvix-input-get-translations (qp)
  "Return a list containing all translations from the Quail
package QP (except for those corresponding to ASCII).
Each pair in the list has the form (KEY-SEQUENCE . TRANSLATION)."
  (with-temp-buffer
    (activate-input-method qp) ; To make sure that the package is loaded.
    (unless (quail-package qp)
      (error "%s is not a Quail package." qp))
    (let ((decode-map (list 'decode-map)))
      (quail-build-decode-map (list (quail-map)) "" decode-map 0)
      (cdr decode-map))))

(defun minijuvix-input-show-translations (qp)
  "Display all translations used by the Quail package QP (a string).
\(Except for those corresponding to ASCII)."
  (interactive (list (read-input-method-name
                      "Quail input method (default %s): " "minijuvix")))
  (let ((buf (concat "*" qp " input method translations*")))
    (with-output-to-temp-buffer buf
      (with-current-buffer buf
        (quail-insert-decode-map
         (cons 'decode-map (minijuvix-input-get-translations qp)))))))

(defun minijuvix-input-add-translations (trans)
  "Add the given translations TRANS to the minijuvix input method.
TRANS is a list of pairs (KEY-SEQUENCE . TRANSLATION). The
translations are appended to the current translations."
  (with-temp-buffer
    (dolist (tr (minijuvix-input-concat-map (eval minijuvix-input-tweak-all) trans))
      (quail-defrule (car tr) (cdr tr) "minijuvix" t))))

(defun minijuvix-input-inherit-package (qp &optional fun)
  "Let the minijuvix input method inherit the translations from the
Quail package QP (except for those corresponding to ASCII).

The optional function FUN can be used to modify the translations.
It is given a pair (KEY-SEQUENCE . TRANSLATION) and should return
a list of such pairs."
  (let ((trans (minijuvix-input-get-translations qp)))
    (minijuvix-input-add-translations
     (if fun (minijuvix-input-concat-map fun trans)
       trans))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up the input method

(defun minijuvix-input-setup ()
  "Set up the minijuvix input method based on the customisable
variables and underlying input methods."

  ;; Create (or reset) the input method.
  (with-temp-buffer
    (quail-define-package "minijuvix" "UTF-8" "∏" t ; guidance
     "minijuvix input method.
The purpose of this input method is to edit MiniJuvix programs, but
since it is highly customisable it can be made useful for other
tasks as well."
     nil nil nil nil nil nil t ; maximum-shortest
     ))

  (minijuvix-input-add-translations
   (mapcar (lambda (tr) (cons (car tr) (vconcat (cdr tr))))
           (append minijuvix-input-user-translations
                   minijuvix-input-translations)))
  (dolist (def minijuvix-input-inherit)
    (minijuvix-input-inherit-package (car def)
                                (eval (cdr def)))))

(defun minijuvix-input-incorporate-changed-setting (sym val)
  "Update the minijuvix input method based on the customisable
variables and underlying input methods.
Suitable for use in the :set field of `defcustom'."
  (set-default sym val)
  (minijuvix-input-setup))

;; Set up the input method.

(minijuvix-input-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Administrative details

(provide 'minijuvix-input)
;;; minijuvix-input.el ends here
