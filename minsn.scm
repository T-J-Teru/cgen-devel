; Macro instruction definitions.
; Copyright (C) 2000, 2009 Red Hat, Inc.
; This file is part of CGEN.
; See file COPYING.CGEN for details.

; Expansion:
; If the macro expands to a string, arguments in the input string
; are refered to with %N.  Multiple insns are separated with '\n'.
; String expansion is a special case of the normal form which is a Scheme
; expression that controls the expansion.  The Scheme expression will be able
; to refer to the current assembly state to decide how to perform the
; expansion.  Special expression `emit' is used to call the assembler emitter
; for a particular insn.  Special expression `expand' is used to return a
; string to be reparsed (which is special cased).

; Parse a list of macro-instruction expansion descriptions.
; This is the main routine for building an minsn-expansion object from a
; description in the .cpu file.
; All arguments are in raw (non-evaluated) form.

; ??? At present we only support macros that are aliases of one real insn.

; Object to describe a macro-insn.

(define <macro-insn>
  (class-make '<macro-insn>
	      '(<source-ident>)
	      '(
		; syntax of the macro
		syntax
		; list of expansion expressions
		expansions
		)
	      nil)
)

(method-make-make! <macro-insn>
		   '(location name comment attrs syntax expansions))

; Accessor fns

(define minsn-syntax (elm-make-getter <macro-insn> 'syntax))
(define minsn-expansions (elm-make-getter <macro-insn> 'expansions))

; Return a list of the machs that support MINSN.

(define (minsn-machs minsn)
  nil
)

; Return macro-instruction mnemonic.
; This is computed from the syntax string.

(define minsn-mnemonic insn-mnemonic)

; Return enum cgen_minsn_types value for MINSN.

(define (minsn-enum minsn)
  (string-upcase (string-append "@ARCH@_MINSN_" (gen-sym minsn)))
)

; Parse a macro-insn expansion description.
; ??? At present we only support unconditional simple expansion.

(define (/minsn-parse-expansion context expn)
  (if (not (form? expn))
      (parse-error context "invalid macro expansion" expn))
  (if (not (eq? 'emit (car expn)))
      (parse-error context "invalid macro expansion, must be `(emit ...)'" expn))
  expn
)

; Parse a macro-instruction description.
; This is the main routine for building a macro-insn object from a
; description in the .cpu file.
; All arguments are in raw (non-evaluated) form.
; The result is the parsed object or #f if object isn't for selected mach(s).

(define (/minsn-parse context name comment attrs syntax expansions)
  (logit 2 "Processing macro-insn " name " ...\n")
  (logit 4 "        name " name "\n")
  (logit 4 "     comment " comment "\n")
  (logit 4 "       attrs " attrs "\n")
  (logit 4 "      syntax " syntax "\n")
  (logit 4 "  expansions " expansions "\n")

  (if (not (list? expansions))
      (parse-error context "invalid macro expansion list" expansions))

  ;; Pick out name first to augment the error context.
  (let* ((name (parse-name context name))
	 (context (context-append-name context name))
	 (atlist-obj (atlist-parse context attrs "cgen_minsn")))

    (if (keep-atlist? atlist-obj #f)

	(let ((result (make <macro-insn>
			(context-location context)
			name
			(parse-comment context comment)
			atlist-obj
			(parse-syntax context syntax)
			(map (lambda (e) (/minsn-parse-expansion context e))
			     expansions))))
	  result)

	(begin
	  (logit 2 "Ignoring " name ".\n")
	  #f)))
)

; Read a macro-insn description
; This is the main routine for analyzing macro-insns in the .cpu file.
; CONTEXT is a <context> object for error messages.
; ARG-LIST is an associative list of field name and field value.
; /minsn-parse is invoked to create the `macro-insn' object.

(define (/minsn-read context . arg-list)
  (let (
	(name nil)
	(comment "")
	(attrs nil)
	(syntax "")
	(expansions nil)
	)

    ; Loop over each element in ARG-LIST, recording what's found.
    (let loop ((arg-list arg-list))
      (if (null? arg-list)
	  nil
	  (let ((arg (car arg-list))
		(elm-name (caar arg-list)))
	    (case elm-name
	      ((name) (set! name (cadr arg)))
	      ((comment) (set! comment (cadr arg)))
	      ((attrs) (set! attrs (cdr arg)))
	      ((syntax) (set! syntax (cadr arg)))
	      ((expansions) (set! expansions (cdr arg)))
	      (else (parse-error context "invalid macro-insn arg" arg)))
	    (loop (cdr arg-list)))))

    ; Now that we've identified the elements, build the object.
    (/minsn-parse context name comment attrs syntax expansions))
)

; Define a macro-insn object, name/value pair list version.

(define define-minsn
  (lambda arg-list
    (if (eq? APPLICATION 'SIMULATOR)
	#f ; don't waste time if simulator
	(let ((m (apply /minsn-read (cons (make-current-context "define-minsn")
					  arg-list))))
	  (if m
	      (current-minsn-add! m))
	  m)))
)

; Define a macro-insn object, all arguments specified.
; This only supports one expansion.
; Use define-minsn for the general case (??? which is of course not implemented
; yet :-).

(define (define-full-minsn name comment attrs syntax expansion)
  (if (eq? APPLICATION 'SIMULATOR)
      #f ; don't waste time if simulator
      (let ((m (/minsn-parse (make-current-context "define-full-minsn")
			     name comment
			     (cons 'ALIAS attrs)
			     syntax (list expansion))))
	(if m
	    (current-minsn-add! m))
	m))
)


; Helper for minsn-compute-iflds, handle entries in the ifield list
; that look like (thing value).
; The "thing" could be an ifield, or it might be an operand.
; We are passed the <ifield> IFLD object in either case, and if thing
; is an operand we are also passed the <operand> OP object. If thing
; is an ifield, not an operand, then OP is #f.
; If the value is a number then we just return the number, simple.
; If the value is a symbol then if the thing is an operand use the type
; of the operand to look up a value for a symbol, for example, the 
; symbol coould be a register name if operand is of type register.
; WIP: We only support looking up for register types right now.

(define (/parse-minsn-operand-value context ifld op val)

  (logit 2 "Looking up value " val " for ifield " (obj:name ifld)
         (if op
             (string-append " with operand " (obj:name op)))
         "\n")

  ;; First, the easy case, handle:
  ;;   (ifield number) and (operand number)
  ;; in both cases, just return the number.
  (if (number? val) val
      ;; If we don't have an operand then we must have and ifield, we
      ;; don't allow symbols for the value in this case.
      (if (not op)
          (parse-error context 
                       "unable to understand value " 
                       val 
                       " without an operand")
          (begin 
          (logit 2 "Looking up value " val " in operand " (obj:name op) "\n")
          ;; We know that we have (operand symbol), lets lookup symbol
          ;; in the type of operand to get the value.
          (let ((op-type (op:type op)))
            ;; Right now we only handle lookups for operands of type
            ;; register, this is the only case I've wanted to far.
            ;; WIP - do other types make sense here ???
            (if (not (register? op-type))
                (parse-error context 
                             "unable to extract values from " 
                             op-type)
                ;; Look up the value for this register symbol.
                (let ((reg-val (assq val (kw-values (hw-indices op-type)))))
                  (if (not reg-val)
                      (parse-error context 
                                   "failed to find value for" 
                                   val)
                      (cadr reg-val)))))))))

; Compute the ifield list for an alias macro-insn.
; This involves making a copy of REAL-INSN's ifield list and assigning
; known quantities to operands that have fixed values in the macro-insn.

(define (/minsn-compute-iflds context minsn-iflds real-insn)
    (logit 2 "Computing ifields for minsn...\n")
  (let* ((iflds (list-copy (insn-iflds real-insn)))
         ;; List of "free variables", i.e. operands.
	 (ifld-ops (find ifld-operand? iflds))
         ;; List of base ifields for those ifields that are using
         ;; operands, the base ifields are multi-ifields split into
         ;; their component ifields.
         (base-ifld-ops
          (apply 
           append
           (map 
            (lambda (f) 
              (assert (ifield? f))
              (logit 4 "  Breaking ifield " (obj:name f) " into base ifields\n")
              (logit 4 "   Class: " (object-class-name f) "\n")
              (let ((base-ifields (ifld-real-ifields f)))
                (logit 4 "  Base ifields: " (map obj:name base-ifields) "\n")
                base-ifields))
            ifld-ops)))
	 ; Names of fields in `ifld-ops'.  As elements of minsn-iflds are
	 ; parsed the associated element in ifld-names is deleted.  At the
	 ; end ifld-names must be empty.  delq! can't delete the first
	 ; element in a list, so we insert a fencepost.
	 (base-ifld-names (cons #f (map obj:name base-ifld-ops)))
         (m-iflds (find ifld-constant? iflds)))
    (logit 3 "   base-ifld-names: " base-ifld-names "\n")
    (logit 3 "   all iflds: " (map obj:name iflds) "\n")
    (logit 3 "   constant iflds: " (map obj:name (find ifld-constant? iflds)) "\n")
    (for-each 
     (lambda (f)
       (if (not (ifield? f))
           (logit 2 "Found non-ifield in ifield list: " f "\n"))
       (let* ((op-name (if (pair? f) (car f) f))
              (op-obj (current-op-lookup op-name))
              ;; If `op-name' is an operand, use its ifield.
              ;; Otherwise `op-name' must be an ifield name.
              ;; In either case, get the ifield object.
              (f-obj 
               (if op-obj
                   (hw-index:value (op:index op-obj))
                   (let ((ifld (current-ifld-lookup op-name)))
                     (if ifld
                         ifld
                         (parse-error context "unknown ifield" op-name)))))
              ;; Create a list of the component ifield names defined
              ;; by this operand/ifield.
              (f-names 
               (map obj:name (ifld-real-ifields f-obj))))
         (logit 3 "Using minsn specifier: " f "\n")
         (logit 3 "Looking for name: " op-name "\n")
         (logit 3 " which gives ifield: " (obj:name f-obj) "\n")
         (logit 3 "   of class: " (object-class-name f-obj) "\n")
         (logit 3 " which is made of ifields: " f-names "\n")

         (logit 3 "Remaining ifields to match: " base-ifld-names "\n")

         ;; Check for the case where an ifield has been used on it's own.
         (if (and (not (pair? f))
                  (not op-obj))
             (parse-error context "invalid ifield without value" op-name))

         ;; Check that each member of f-names is in the
         ;; base-ifld-names list, removing each as we confirm that
         ;; it's there.
         (for-each 
          (lambda (f-name)
            (logit 4 "    Looking for ifield: " f-name "\n")
            (if (member f-name base-ifld-names)
                (begin
                  (logit 4 "    Found it!\n")
                  (delq! f-name base-ifld-names))
                (parse-error context "attempt to define unexpected ifield" f-name))) 
          f-names)

         (logit 3 "   Preparing to create new value for ifield: " 
                (obj:name f-obj) "\n")
         (logit 3 "Using: " f "\n")
         ;; Now create a new ifield instance and set the value to
         ;; either the constant or operand as specified, add this to
         ;; the m-iflds list.
         (let ((new-val 
                (if (pair? f)
                    (begin
                      (logit 4 "   Extracting value from pair: " f "\n")
                      (/parse-minsn-operand-value context f-obj op-obj (cadr f)))
                    (begin
                      (assert (operand? op-obj))
                      op-obj) )))
           (logit 3 "Creating new ifield value for: " (obj:name f-obj) "\n")
           (append! m-iflds (list (ifld-new-value f-obj new-val)))
           (logit 3 "minsn ifield list is now: " (map obj:name m-iflds) "\n\n\n")
           )))
     minsn-iflds)
    (logit 3 "All minsn ifields have now been processed.\n")
    (logit 3 "Final ifield list: " (map obj:name m-iflds) "\n")

    ; APB: === START ===
    ; This forces all operands to be named in the macro...
    ; not convinced this is a good thing.
    (if (not (equal? base-ifld-names '(#f)))
	(parse-error context "incomplete operand list, missing: "
                     (cdr base-ifld-names)))
    ; APB: === END ===

    (logit 2 "minsn ifields: " (map obj:name m-iflds) "\n")
    m-iflds))

;;   (let* ((iflds (list-copy (insn-iflds real-insn)))
;; 	 ; List of "free variables", i.e. operands.
;; 	 (ifld-ops (find ifld-operand? iflds))
;; 	 ; Names of fields in `ifld-ops'.  As elements of minsn-iflds are
;; 	 ; parsed the associated element in ifld-names is deleted.  At the
;; 	 ; end ifld-names must be empty.  delq! can't delete the first
;; 	 ; element in a list, so we insert a fencepost.
;; 	 (ifld-names (cons #f (map obj:name ifld-ops)))
;; 	 (isa-name-list (obj-isa-list real-insn)))
;;     ;(logit 3 "Computing ifld list, operand field names: " ifld-names "\n")
;;     ; For each macro-insn ifield expression, look it up in the real insn's
;;     ; ifield list.  If an operand without a prespecified value, leave
;;     ; unchanged.  If an operand or ifield with a value, assign the value to
;;     ; the ifield entry.
;;     (for-each (lambda (f)
;; 		(let* ((op-name (if (pair? f) (car f) f))
;; 		       (op-obj (current-op-lookup op-name isa-name-list))
;; 		       ; If `op-name' is an operand, use its ifield.
;; 		       ; Otherwise `op-name' must be an ifield name.
;; 		       (f-name (if op-obj
;; 				   (obj:name (hw-index:value (op:index op-obj)))
;; 				   op-name))
;; 		       (ifld-pair (object-memq f-name iflds)))
;; 		  ;(logit 3 "Processing ifield " f-name " ...\n")
;; 		  (if (not ifld-pair)
;; 		      (parse-error context "unknown operand" f))
;; 		  ; Ensure `f' is an operand.
;; 		  (if (not (memq f-name ifld-names))
;; 		      (parse-error context "not an operand" f))
;; 		  (if (pair? f)
;; 		      (set-car! ifld-pair (ifld-new-value (car ifld-pair) (cadr f))))
;; 		  (delq! f-name ifld-names)))
;; 	      minsn-iflds)
;;     (if (not (equal? ifld-names '(#f)))
;; 	(parse-error context "incomplete operand list, missing: " (cdr ifld-names)))
;;     iflds)
;; )

; Helper function used when creating an ALIAS macro instruction. Check that 
; all the attributes of the instruction being aliased are present on the 
; macro-instruction.

(define (/check-alias-attrs context alias-of minsn)
  (let ((minsn-atlist (obj-atlist minsn)))
    (logit 3 "Attributes of instruction: " (obj:name alias-of) "\n")
    (for-each 
     (lambda (a)
       (if (not (or (eq? (car a) 'NO-DIS) (eq? (car a) 'MNEMONIC-CASE)))
	   (begin 
	     (logit 3 "    attribute: " (car a) "\n")
	     (logit 3 "   insn value: " (cdr a) "\n")
	     (let ((attr-value 
		    (atlist-attr-value minsn-atlist (car a) minsn)))
	       (if (equal? attr-value '())
		   (begin
		     (logit 3 "minsn atlist: " (obj-atlist minsn) "\n")
		     (logit 3 " minsn alist: " (atlist-attrs (obj-atlist minsn)) "\n")
		     (logit 3 "WARNING: missing attribute " (car a) " in minsn " (obj:name minsn))))
	       (logit 3 "  minsn value: " attr-value "\n")
	       (if (not (equal? attr-value (cdr a)))
		   (logit 3 "WARNING: attribute '" (car a)
                          "' value different '"
                          (cdr a) "' to '"
                          attr-value "' in minsn "
                          (obj:name minsn) "\n"))))))
     (atlist-attrs (obj-atlist alias-of))))
  *UNSPECIFIED*
)

; Create an aliased real insn from an alias macro-insn.

(define (minsn-make-alias context minsn)
  (if (or (not (has-attr? minsn 'ALIAS))
	  ; Must emit exactly one real insn.
	  (not (eq? 'emit (caar (minsn-expansions minsn)))))
      (parse-error context "macro '" (obj:name minsn) "' not an alias macro-insn" minsn))

  (let* ((expn (car (minsn-expansions minsn)))
	 (alias-of (current-insn-lookup (cadr expn) (obj-isa-list minsn))))

    (if (not alias-of)
	(parse-error context (string-append "unknown real insn '" (cadr expn)  "' in expansion of '" (obj:name minsn) "'") minsn))

    (logit 3 "Creating minsn: " (obj:name minsn) "\n")

    ;; Check the attributes on the alias instruction are similar to
    ;; those on the instruction being aliased. This only prints
    ;; warnings if debugging is turned on as there's no easy way to
    ;; say which differences are ok, and which are not.
    (/check-alias-attrs context alias-of minsn)

    (let ((i (make <insn>
		   (context-location context)
		   (obj:name minsn)
		   (obj:comment minsn)
		   (obj-atlist minsn)
		   (minsn-syntax minsn)
		   (/minsn-compute-iflds (context-append context
							 (string-append ": " (obj:str-name minsn)))
					 (cddr expn) alias-of)
		   #f ; ifield-assertion
		   #f ; semantics
		   #f ; timing
		   )))
      ; FIXME: use same format entry as real insn,
      ; build mask and test value at run time.
      (insn-set-ifmt! i (ifmt-build i -1 #f (insn-iflds i))) ; (car (ifmt-analyze i #f))))
      ;(insn-set-ifmt! i (insn-ifmt alias-of))
      i))
)

; Called before a .cpu file is read in.

(define (minsn-init!)
  (reader-add-command! 'define-minsn
		       "\
Define a macro instruction, name/value pair list version.
"
		       nil 'arg-list define-minsn)
  (reader-add-command! 'define-full-minsn
		       "\
Define a macro instruction, all arguments specified.
"
		       nil '(name comment attrs syntax expansion)
		       define-full-minsn)

  *UNSPECIFIED*
)

; Called after the .cpu file has been read in.

(define (minsn-finish!)
  *UNSPECIFIED*
)
