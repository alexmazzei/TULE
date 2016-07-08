(in-package "USER")

;(defvar *HOME-DIR* "/Users/mazzei/lavori/Projects/ATLAS/softExt/tup/2012-JAN-SYSTEM/SYSTEM/")

(defvar *LISP-CHAR-SET-ID* 'UTF-8)
; *** this encodes the character encoding scheme used by the Lisp machine

; (defvar *CHAR-SET-ID* 'ISO-8859-1) ; Iso Latin 1: ISO/IEC 8859-1
  (defvar *CHAR-SET-ID* 'UTF-8)
; *** this encodes the character encoding scheme of the input data

(defvar *SYSTEM-CONTEXT* 'tule)
         ; other possible values are hops and hops-dial

; *** the next few functions are needed for "build-file-name", which is in
;     turn needed for loading all files, among which "utilities"

;; (defun uconcat (&rest x)
;;   (cond (x (format nil "~{~a~}" x)) ))

;; (defun concat (&rest x) (identity (intern (string (apply #'uconcat x)))))

;; (defun build-file-name (filen) (string (concat *HOME-DIR* filen)))

;; (defun build-subdir-file-name (filen sub) (string (concat *HOME-DIR* sub filen)))

;********* General utilities ***************************************
;(load (build-file-name "ALLLANG/PROC-ALL/utilities"))
(load "/Users/mazzei/lavori/Projects/ATLAS/softExt/tup/2012-JAN-SYSTEM/SYSTEM/ALLLANG/PROC-ALL/utilities")

(defun build-file-name (filen) (string (concat *HOME-DIR* filen)))

(defvar loadresult)              ; for testing the correct loading of knowledge bases
(defvar *LANGUAGE*)
(defvar *TEMP*)

     ; *** the three next variables hold the name of the lisp properties associated
     ;     with the dictionary entries
(defvar *MAIN-DICT*)
(defvar *PROPER-DICT*)
(defvar *SIGLE-DICT*)
(defvar *PRINT-LEV*)
(defvar *TREE-FORMAT*)  ; may be "tut" or "avm"; set in main or in chunk-parser


;********* Variables and Functions for the character encoding ******
(defvar *SYMBOL-CODE-TABLE* nil)
(defvar *BASE-CODE-TABLE* nil)
(defvar *CODE-TO-TULE-TABLE* nil)
(defvar *CURRLISP-TO-TULE-TABLE* nil)
(defvar *LISP-TO-TULE-TABLE* nil)
(defvar *NON-DICTIONARY-CHARS* nil)

(load (build-file-name "ALLLANG/PROC-ALL/MORPHO-PROC-ALL/char-funct"))
        ; *** char-funct includes basic functions, as "uppercase", so it must
        ;     be loaded before others

; *** the next loads the infos about the specific character set in use.
; *** The code is chosen on the basis of the *CHAR-SET-ID* defined above
; *** currently, the only available charset are ISO-8859-1 and UTF-8
;     N.B. set-symbol-codes, set-char-set-values and set-upcase-lowcase are
;          defined in char-funct (MORPHO-PROC-ALL)
(set-symbol-codes)      ; loads the *SYMBOL-CODE-TABLE*, the *CODE-TO-TULE-TABLE*
                        ; and the *LISP-TO-TULE-TABLE*; sets the char-value and lisp-char
                        ; properties of the tule char names
(set-char-set-values)
(set-upcase-lowcase)

;########################### GLOBAL VARIABLES #########################

;******************* Debugging ************************
(defvar *TRACE-UNL* nil)

;******************* Morphology ***********************
(defvar *SUFF-TABLE* nil)
(defvar *SUFF-NET* nil)
(defvar *ENCLITINFO* nil)
(defvar *ANALYS-CONTEXT* 'text)    ; in ALLLANG/PROC-ALL/main:ana-and-tag
                                   ; used for sentence separation: if the
                                   ; value is "text", then in "34.", the
                                   ; period is taken as a sentence separator
                                   ; Currently, "text" is the only value used
                                   ; The other value I was thinking about is
                                   ; "formula"

;******************* Lexicon **************************
(defvar *LOCUTIONS* nil)           ; Multiwords
(defvar *COMPL-PROPER* nil)        ; Proper-names multiwords (e.g. United States)
(defvar *MARKERS* nil)             ; metalevel text markers (ignored)
(defvar *CATEG-INVAR* nil)         ; loaded from KB/DICTIONARY/default-types
                                   ; These are the syntactic categories for
                                   ; which there is no morphological variation
                                   ; (see ALLLANG/PROC-ALL/MORPHO-PROC-ALL/analizzatore)

;************** File Output ****************************
(defvar *DEFAULT-TYPES* nil)       ; loaded from ALLLANG/KB-ALL/DICTION-KB-ALL/default-types
                                   ; In printing the output on file, the
                                   ; standard types (not appearing in the
                                   ; dictionary (as QUALIF for ADJ)) are
                                   ; included, on the basis of this variable
                                   ; (see ALLLANG/PROC-ALL/TAGGER-PROC-ALL/postagger)

;******************* Parsing ***************************
; *** all the next are language-specific ***************
(defvar *NOUN-GOVERN* nil)         ; loaded from ITALIAN/KB-ITA/GRAMM-KB-ITA/noungovern.dat
                                   ; This and the next two specify if there
                                   ; is any preposition usually governed by
                                   ; the noun (adj, adv, respectively)
(defvar *NOUN-NOUN* nil)           ; loaded from KB/GRAMM/noun-noun.dat
(defvar *ADJ-GOVERN* nil)          ; loaded from ITALIAN/KB-ITA/GRAMM-KB-ITA/adjgovern.dat
(defvar *ADV-GOVERN* nil)          ; loaded from ITALIAN/KB-ITA/GRAMM-KB-ITA/advgovern.dat
(defvar *PREF-IMPERSONAL* nil)     ; used only for Italian 
                            ; loaded from ITALIAN/KB-ITA/SUBCAT-KB-ITA/v-prefnotrefl-ita.dat
(defvar *PREFER-PP-ATTACH* nil)    ; loaded form ITALIAN/KB-ITA/GRAMM-KB-ITA/prefppattach-ita.dat
(defvar *PREFER-CONJ-ATTACH* nil)  ; loaded form ITALIAN/KB-ITA/GRAMM-KB-ITA/prefconjattach-ita.dat
(defvar *VERB-PARTICLE* nil)       ; used only for English (e.g. "carry out")
(defvar *SUBJ-CONTROL-VERBS* nil)  ; loaded from ITALIAN/KB-ITA/SUBCAT-KB-ITA/verbal-control-ita.dat
(defvar *OBJ-CONTROL-VERBS* nil)   ; loaded from ITALIAN/KB-ITA/SUBCAT-KB-ITA/verbal-control-ita.dat
(defvar *INDOBJ-CONTROL-VERBS* nil) ; loaded from ITALIAN/KB-ITA/SUBCAT-KB-ITA/verbal-control-ita.dat
(defvar *GRAMMTYPES* nil)          ; loaded from ITALIAN/KB-ITA/DICTION-KB-ITA/grammtypes-ita.dat
(defvar *LEXICAL-PREFERENCES* nil) ; loaded from ITALIAN/KB-ITA/DICTION-KB-ITA/lexical-pref.dat

; *** the next two loaded from ALLLANG/KB-ALL/SUBCAT-KB-ALL/verbclass-marked-cases.dat
(defvar *ABSTRACT-SUBCAT-CLASSES* nil) 
(defvar *VERBCLASS-MARKED-CASES* nil)  
(defvar *VERB-CF-TRANSFORM* nil)   ; verbal transformations; loaded from ALLLANG/KB-ALL/SUBCAT-KB-ALL/transf-def.dat

;******************* Semantics ***************************
(defvar *ONTO-TIME-START* nil)     ; loaded in ALLLANG/PROC-ALL/SEMANT-PROC-ALL/onto-reasoning
(defvar *ONTOLOGY-CACHE* nil)      ; loaded in ALLLANG/PROC-ALL/main
                                   ;   from ALLLANG/KB-ALL/SEMANT-KB-ALL/onto-cache.dat
(defvar *WORD-MEANING* nil)        ; loaded from ITALIAN/KB-ITA/SEMANT-KB-ITA/word-mean.dat
(defvar *CONCEPT-WORD* nil)        ; loaded from ITALIAN/KB-ITA/SEMANT-KB-ITA/conc-word.dat
(defvar *SUBTYPES-SPEC* nil)       ; loaded from ITALIAN/KB-ITA/SEMANT-KB-ITA/subtype-spec-ita.dat
(defvar *DEICTIC-REFERENCE* nil)   ; loaded from ALLLANG/KB-ALL/SEMANT-KB-ALL/deictic-ref.dat
(defvar *ANAPHORIC-CONTEXT* nil)    ; Dynamically set by the semantic interpreter
(defvar *DEFAULT-CONCEPT-INFOS* nil)  ; loaded from ALLLANG/KB-ALL/SEMANT-KB-ALL/query-target.dat

;******************* Some file names ********************
(defvar *ONTOLOGY-FILE-BASE* (build-file-name "ALLLANG/KB-ALL/SEMANT-KB-ALL/ontology-base.dat"))
(defvar *PARSERULES-FILE* (build-file-name "ALLLANG/KB-ALL/GRAMM-KB-ALL/parserules.dat"))
(defvar *VERB-HIER-FILE* (build-file-name "ALLLANG/KB-ALL/SUBCAT-KB-ALL/newhier.out"))
(defvar *LAB-HIER-FILE* (build-file-name "ALLLANG/KB-ALL/SUBCAT-KB-ALL/lab-hierarchy.dat"))

;########################### LOADING DATA #############################

(load (build-file-name "ALLLANG/PROC-ALL/SUBCAT-PROC-ALL/subc-hier"))
(load (build-file-name "ALLLANG/PROC-ALL/PARSER-PROC-ALL/semtypes-funct"))

; ****** main **********************
(load (build-file-name "ALLLANG/PROC-ALL/loadfunctions"))
                                             ; *** loaded in advance since it
                                             ;     includes the functions for loading
                                             ;     the dictionaries and the ontology

; ##################### LANGUAGE SPECIFIC DATA #########################

; ****** morphological analysis ****
(with-open-file
     (inptab (build-file-name "ITALIAN/KB-ITA/MORPHO-KB-ITA/suff-tab-ita.dat")
                :direction :input :if-does-not-exist :error)
         (setq *SUFF-TABLE* (read inptab)))
(load (build-file-name "ITALIAN/KB-ITA/MORPHO-KB-ITA/network-ita.dat"))
(load (build-file-name "ITALIAN/KB-ITA/MORPHO-KB-ITA/enclitinfo-ita.dat"))
(load (build-file-name "ITALIAN/KB-ITA/MORPHO-KB-ITA/numbautom-ita"))

; *** lexicon ***************************
        ; *** the next must be made first, since it "puts" property vals
        ;     while the others "add" them
(with-open-file
      (ff (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/dictionary-ita.dat")
          :direction :input :if-does-not-exist :error)
   (with-open-file
         (fsigla (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/sigle-ita.dat")
                 :direction :input :if-does-not-exist :error)
      (with-open-file
            (fsiggiur (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/sigleg-ita.dat")
                      :direction :input :if-does-not-exist :error)
         (with-open-file
               (fforeign (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/dictionary-foreign.dat")
                         :direction :input :if-does-not-exist :error)
            (load_all ff fsigla fsiggiur nil fforeign)))))

(load (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/invariabili-ita.dat"))
(load (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/prep_art-ita.dat"))
(load (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/grammtypes-ita.dat"))
(load (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/lexical-pref-ita.dat"))
(load (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/locutions-ita.dat")) 	;standard locutions
(load (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/locutionsg-ita.dat")) 	;legal locutions
(load (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/text-markers-ita.dat"))

(f-loadnames 'mproperdef (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/proper-names-ita.dat"))
(f-loadinvar 'mproperdef (build-file-name "ITALIAN/KB-ITA/DICTION-KB-ITA/proper-comp-ita.dat"))


; ##################### LANGUAGE INDEPENDENT PROCEDURES ################

; ****** connection between submodules
(load (build-file-name "ALLLANG/PROC-ALL/main"))
(load (build-file-name "ALLLANG/PROC-ALL/top-level-fun"))

; ****** tokenizing ****************
(load (build-file-name "ALLLANG/KB-ALL/MORPHO-KB-ALL/token-autom"))
(load (build-file-name "ALLLANG/PROC-ALL/MORPHO-PROC-ALL/tokenizer"))

; ****** morphological analysis ****
(load (build-file-name "ALLLANG/PROC-ALL/MORPHO-PROC-ALL/endings"))
(load (build-file-name "ALLLANG/PROC-ALL/MORPHO-PROC-ALL/analizzatore"))
(load (build-file-name "ALLLANG/PROC-ALL/MORPHO-PROC-ALL/numbers"))
(load (build-file-name "ALLLANG/PROC-ALL/MORPHO-PROC-ALL/parsenumbers"))
(load (build-file-name "ALLLANG/PROC-ALL/MORPHO-PROC-ALL/tb-functions"))

; ****** tagging *******************
(load (build-file-name "ALLLANG/PROC-ALL/TAGGER-PROC-ALL/postagger"))
(load (build-file-name "ALLLANG/KB-ALL/TAGGER-KB-ALL/lexdisambr"))
(load (build-file-name "ALLLANG/KB-ALL/DICTION-KB-ALL/default-types.dat"))


; ******* single-word proper names ***********************************
(f-loadnames 'mproperdef (build-file-name "ALLLANG/KB-ALL/DICTION-KB-ALL/proper-names-all.dat"))
                                 ; general for all languages

; ******* multi-word proper names ***********************************
(f-loadinvar 'mproperdef (build-file-name "ALLLANG/KB-ALL/DICTION-KB-ALL/proper-comp-all.dat"))
                                 ; general for all languages

; ******* word meanings ***********************************
(load (build-file-name "ALLLANG/KB-ALL/SEMANT-KB-ALL/word-mean-proper-all.dat"))



