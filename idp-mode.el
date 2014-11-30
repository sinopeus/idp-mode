;; IDP mode for Emacs
;; Author: Xavier Goás Aguililla
;; License: GNU GPL v3

;; (defvar idp-events
;;   '("reservedword1"
;;     "reservedword2"))

(defvar idp-keywords
  '("namespace" 
    "include"
    "vocabulary"
    "LTCvocabulary"
    "structure"
    "factlist"
    "aspbelief"		
    "theory"		
    "formula"		
    "query"		
    "term"		
    "options"	
    "procedure"	
    "LFD"
    "GFD"
    "card"
    "sum"
    "prod"
    "prod"		
    "max"		
    "min"		
    "type"		
    "isa"		
    "contains"	
    "partial"	
    "generate"	
    "extern"	
    "using"	
    "namespace"
    "options"
    )
  )


(defvar idp-keywords-regexp (regexp-opt idp-keywords 'words))

(defvar idp-logical-operators
  '("=>" 
    "<-"	
    "&"	
    "|"	
    "~"	
    "!"	
    "?"	
    "="	
    )
  )

(defvar idp-logical-operators-regexp (regexp-opt idp-logical-operators 'words))

(defvar idp-arithmetic-operators
  '("=>"
    "<=>"
    "<="
    "<-"	
    "&"	
    "|"	
    "~"	
    "!"	
    "?"	
    "="	
    "~="	
    )
  )

(defvar idp-logical-operators-regexp (regexp-opt idp-logical-operators 'words))

(defvar idp-builtin-types
  '("nat" 
    "int"
    )
  )

(defvar idp-builtin-types-regexp (regexp-opt idp-builtin-types 'words))

(defvar idp-lua-types
  '("sort"
    "predicate_symbol"
    "function_symbol"
    "symbol"
    "vocabulary"
    "compound"
    "tuple"
    "predicate_table"
    "predicate_interpretation"
    "function_interpretation"
    "structure"
    "theory"
    "options"
    "namespace"
    "overloaded"
    )
  )

(defvar idp-lua-types-regexp (regexp-opt idp-lua-types 'words))

;; I'd probably put in a default that you want, as opposed to nil
(defvar idp-tab-width 4 "Width of a tab for IDP mode")

;; Two small edits.
;; First is to put an extra set of parens () around the list
;; which is the format that font-lock-defaults wants
;; Second, you used ' (quote) at the outermost level where you wanted ` (backquote)
;; you were very close
(defvar idp-font-lock-defaults
  `(
     ("\"\\.\\*\\?" . font-lock-string-face) ;; strings
     ;; (":\\|{\\|}" . font-lock-keyword-face) ;\;\ delimiters **/
     ("~" . font-lock-negation-char-face)
     (,idp-keywords-regexp . font-lock-keyword-face)
     (,idp-logical-operators-regexp . font-lock-builtin-face)
     (,idp-builtin-types-regexp . font-lock-type-face)
     ;; ( ,(regexp-opt idp-events 'words) . font-lock-constant-face)
     ))

;; command to comment/uncomment text
(defun idp-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let (
        (comment-start "/*") (comment-end "*/")
        )
    (comment-dwim arg)))

;; syntax table
(defvar idp-syntax-table nil "Syntax table for IDP-Mode.")
(setq idp-syntax-table
      (let ((synTable (make-syntax-table)))

	;; C++ style comment
	(modify-syntax-entry ?\/ ". 124b" synTable)
	(modify-syntax-entry ?* ". 23" synTable)

	(modify-syntax-entry ?\n "> b" synTable)
	
        synTable))

(define-derived-mode idp-mode prog-mode "IDP-Mode"
  "IDP mode is a major mode for editing IDP programs"
  :syntax-table idp-syntax-table
  ;; you again used quote when you had '((idp-hilite))
  ;; I just updated the variable to have the proper nesting (as noted above)
  ;; and use the value directly here
  (setq font-lock-defaults '((idp-font-lock-defaults)))
  
  ;; when there's an override, use it
  ;; otherwise it gets the default value
  (when idp-tab-width
    (setq tab-width idp-tab-width))

  ;; modify the keymap
  (define-key idp-mode-map [remap comment-dwim] 'idp-comment-dwim)
  )


(add-to-list 'auto-mode-alist '("\\.idp\\'" . idp-mode))

(provide 'idp-mode)
