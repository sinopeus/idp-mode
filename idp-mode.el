;; IDP mode for Emacs
;; Author: Xavier Goás Aguililla
;; License: GNU GPL v3

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

;; Default tab width
(defvar idp-tab-width 4 "Width of a tab for IDP mode")

(defvar idp-font-lock-defaults
  `(
     ("\"\\.\\*\\?" . font-lock-string-face) ;; strings
     ("~" . font-lock-negation-char-face)
     (,idp-keywords-regexp . font-lock-keyword-face)
     (,idp-logical-operators-regexp . font-lock-builtin-face)
     (,idp-builtin-types-regexp . font-lock-type-face)
     ))

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
  (setq font-lock-defaults '((idp-font-lock-defaults)))
  
  (when idp-tab-width
    (setq tab-width idp-tab-width))

  (setq comment-start "/*")
  (setq comment-end "*/")
  )


(add-to-list 'auto-mode-alist '("\\.idp\\'" . idp-mode))

(provide 'idp-mode)
