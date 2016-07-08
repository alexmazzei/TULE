(in-package "USER")

;##################################################################
;	IN THIS FILE MAIN FUNCTIONS FOR THE OPERATIONS
;	AVAILABLE TO THE USER:
;	1. Morphological analysis of a sequence of words given via the keyboard
;	2. Morphological analysis + POS tagging of a text stored in a file
;##################################################################
;
;  go-main (the main for tule, legal, hops, and hops-dialogue: the choice is made
;    |      on the basis of the global variable *SYSTEM-CONTEXT*, loaded in
;    |      initial load files)
;    |--> [*SYSTEM-CONTEXT* = hops-dial]
;    |    dialogue-manager [user asks for dialogue] in dialogue-manager.lisp
;    |    start-hops [user asks for single sentence]
;    |               (read-line, q or p) (avm) (variable print-level)  in main.lisp
;    |--> [*SYSTEM-CONTEXT* = hops]
;    |    start-hops (no input or optional string) (avm)       in main.lisp
;    |     |--> hops-ana-text+tag                in top-level-functions.lisp [this file]
;    |     |    [result in flat AVM format]
;    |     |     |--> tokenize              in tokenizer.lisp
;    |     |     |      (input: a port; output: a list of tokens)
;    |     |     |--> analyser              in analizzatore.lisp
;    |     |     |     |   (input: a list of tokens; output: two lists: the first one
;    |     |     |     |    is the result of the morpho-lexical analysis; the second
;    |     |     |     |    one is the - possibly modified - list of input tokens)
;    |     |     |     |--> test-incompl (tut) in top-level-functions.lisp [this file]
;    |     |     |--> postagger             in postagger.lisp
;    |    adjust-attributes (input: the attributes got by strt-hops) (avm)       in main.lisp
;    |--> [*SYSTEM-CONTEXT* = tule, legal]
;    |    tulemain (no input)               in main.lisp
;          |--> ana-and-tag-and-parse (tut) in top-level-functions.lisp [this file]
;             |         [this is used for parsing files (answer 1 to the initial question)]
;             |         (no input; reads from file; writes on .dis, .tb, .tball, .sim, .tb 
;             |          and .prs; the name of the input is taken from the keyboard
;             |          or from the file "tagcorpus.dat")
;             |--> file-ana-text+tag (tut)  in top-level-functions.lisp [this file]
;                |      (input: the name of a file; output on .dis, .tb, .tball, .sim)
;                |--> tokenize
;                |      (input: a port; output: a list of tokens)
;                |--> analyser
;                   |   (input: a list of tokens; output: two lists: the first one
;                   |    is the result of the morpho-lexical analysis; the second
;                   |    one is the - possibly modified - list of input tokens)
;                   |--> test-incompl (tut) in top-level-functions.lisp [this file]
;                |--> postagger
;             |--> parse-single-f (tut)
;             |         (input: .tb, .prs, and .csf file names; reads from .tb; writes on .prs
;             |          and on .csf; optional input: extended labels)
;          |--> ana-and-tag-and-parse-and-seminterp (avm) in top-level-functions.lisp [this file]
        ; *** parseres is an avm with tree information;
        ;     mergeres is needed, since the result of parse-sentences is
        ;     given in form of two separate list: the word list ant the link list
;             |         [this is used for parsing files (answer 1 to the initial question)]
;             |         (input: single sentence from keyboard; output on screen)
;             |--> hops-ana-text+tag (avm) in top-level-functions.lisp [this file]
;             |  |--> tokenize
;             |  |      (input: a port; output: a list of tokens)
;             |  |--> analyser
;             |     |   (input: a list of tokens; output: two lists: the first one
;             |     |    is the result of the morpho-lexical analysis; the second
;             |     |    one is the - possibly modified - list of input tokens)
;             |     |--> test-incompl (avm) in top-level-functions.lisp [this file]
;             |  |--> postagger
;             |--> parse-sentences 
;             |         (input: a list representing the POStagger result in "flat AVM format")
;             |--> mergeresult
;             |         (input: a pair of lists; the first in flat AVM format; the second
;             |          (representing the links) in internal link format (i.e. as pairs
;             |           <pointer, label>)
;             |--> reshuffle-tree (avm)
;             |         (input: a list representing the parser result in "full flat AVM format"
;             |          i.e. where each line includes also the link infos)
;             |--> semantic-interpretation (avm)
;             |         (input: an AVM tree)
;          |--> ana-text (avm) in top-level-functions.lisp [this file]
;             |                (just for morpho+lex for single words)
;             |--> tokenize
;             |      (input: a port; output: a list of tokens)
;             |--> analyser
;                   |   (input: a list of tokens; output: two lists: the first one
;                   |    is the result of the morpho-lexical analysis; the second
;                   |    one is the - possibly modified - list of input tokens))
;                   |--> test-incompl (avm) in top-level-functions.lisp [this file]
;          |--> ana-and-tag (tut) in main.lisp
;             |--> file-ana-text+tag (tut) in top-level-functions.lisp [this file]
;                |--> tokenize
;                |      (input: a port; output: a list of tokens)
;                |--> analyser
;                   |   (input: a list of tokens; output: two lists: the first one
;                   |    is the result of the morpho-lexical analysis; the second
;                   |    one is the - possibly modified - list of input tokens)
;                   |--> test-incompl (tut) in top-level-functions.lisp [this file]
;                |--> postagger
;          |--> ana-and-tag-and-parse-and-eval (tut) in main.lisp
;             |         (input from file and from .man; output on .tb, .prs, .cmp,
;             |          .err, .tag, and .dis)
;             |--> file-ana-text+tag (tut) in top-level-functions.lisp [this file]
;                |--> tokenize
;                |      (input: a port; output: a list of tokens)
;                |--> analyser
;                   |   (input: a list of tokens; output: two lists: the first one
;                   |    is the result of the morpho-lexical analysis; the second
;                   |    one is the - possibly modified - list of input tokens)
;                   |--> test-incompl (tut) in top-level-functions.lisp [this file]
;                |--> postagger
;             |--> parse-single-f (tut)
;             |--> compparse-single-f (tut)
;          |--> parse-and-eval (tut) in main.lisp
;             |--> parse-single-f (tut)
;             |--> compparse-single-f (tut)
; --- this is a stressed a: à
      
; *************************************************************************
; ******** FUNCTIONS FOR SERVERS ******************************************
; *************************************************************************
; *** The organization is based on the input and output of sentences (at 
;     different level of analysis) as strings of set of sentences.
; *** There are two main "public" functions for the analysis, the first one
;     for syntax, the second one for semantics. They are based on some
;     sub-functions, that are also usable from outside, since they exchange
;     data as strings. The overall schema is as follows:
;
;     tule-morpho-and-parse      (parsing from raw input text)
;       |------> tule-tokenizer  (splits the input texts in words and sentences)
;       |------> tule-analyser   (morphological analysis and access to dictionary)
;       |------> tule-postagger  (part of speech tagging)
;       |------> tule-parser     (parsing into avm format)
;
;     tule-semantic-interpreter  (from avm trees to fol formulae)
;       |------> tule-annotate   (adds to the avm trees pointers to ontology)
;       |------> tule-build-ontorepr (builds graphs based on the ontology)
;       |------> tule-build-fol  (builds the final fol formulae)
;
; *** Note that nothing is read or written on files, though the two main functions
;     also act as dispatchers, since they accept also other input formats. This
;     does not hold for the sub-functions

; *** The next is just for debugging purposes *********************
(defun mymain (inpstring)
  (let (temp)
    (setq temp (tule-morpho-and-parse inpstring))
    (format t "After parsing:~% ~a~%" temp)
    (break "")
    (tule-semantic-interpreter temp)))

; *************************************************************************
; *** the next is the main function for carrying out morphological analysis,
;     pos tagging and parsing.
; *** The input can be:
;     - empty (nil), in which case a dialogue is started with the user
;       to get the name of a file or a set of files that have to be processed
;       (this is done inside ana-and-tag-and-parse)
;       or a string that includes one or more sentences.
; *** The output is either written on files (if the input is from file) or a
;     string including the avm representation of the parse trees of the input 
;     sentence. The string includes a list each of whose elements is an avm tree.
(defun tule-morpho-and-parse (&optional inpdata)
 (declare (special *TREE-FORMAT*))
 (let (tokenizer-result analyser-result postagger-result parser-result)
   (setq *TREE-FORMAT* 'tut)
   (cond ((null inpdata)
            (ana-and-tag-and-parse))
         ((stringp inpdata)
            (setq tokenizer-result (tule-tokenizer inpdata))
    ;(format t "after tokenizer ********~% ~a~% ~s" tokenizer-result tokenizer-result)
    ;(break "")
            (setq analyser-result (tule-analyser tokenizer-result))
    ;(format t "after analyser ********~% ~a~% ~s" analyser-result analyser-result)
    ;(break "")
            (setq postagger-result (tule-postagger analyser-result))
    ;(format t "after postagger ********~% ~a~% ~s" postagger-result postagger-result)
    ;(break "")
            (setq *TREE-FORMAT* 'avm)
            (setq parser-result (tule-parser postagger-result))
    ;(format t "after parser ********~% ~a~% ~s" parser-result parser-result)
    ;(break "")
            parser-result)
         (t (exception 'generic-error
              "PROC/top-level-fun: the input to tule-ana-and-parse is neither empty nor a string")))))

; *************************************************************************
(defun tule-tokenizer (string)
  (let ((sent-buff nil) rem-line continue (token-result nil) (in-parenth nil))
  (declare (special rem-line))
; *** the function "tokenize", applied in the do* below, reads the lines of the input file,
;     and returns the 'tokens' composing a single sentence. This means that the tokenizer 
;     should in principle detect sentence ends, and that it must be ready to tokenize the 
;     next sentence in the next step of the loop. The buffer rem-line contains the part of 
;     the line that refers to the next sentence.
; *** The main problem is that it is sometimes difficult to separate the sentence just on 
;     the basis of the single chars (which is what "tokenize" should do). So, "tokenize" 
;     takes the safer assumption that, in case of doubt, there is a sentence separation. 
;     If this is wrong, it is the function check-sent-continuation (below) that decides 
;     that the current sentence is not complete
     (setq rem-line nil)
     (do* ((one-phrase (tokenize string) (tokenize ""))
           (stop nil (first one-phrase)))
         ((and (eq stop 's-eof) 		; *** nothing more in file or string
               (null (second one-phrase))	; *** nothing in the output of tokenize
               (null sent-buff))		; *** nothing left in the sentence buffer
            (format nil "~s" token-result))
         (multiple-value-setq (continue sent-buff in-parenth)
                 (check-sent-continuation sent-buff (second one-phrase) 
                              (eq 's-eof (first one-phrase)) in-parenth))
         (cond ((not continue)
                  (setq token-result (append1 token-result sent-buff))
                  (setq sent-buff nil))))))

; *******************************************************************
(defun tule-analyser (scan-string)
 (let (tokenized-sentences analyser-result sent-lex-items sent-tokens)
   (cond ((stringp scan-string)
            (setq tokenized-sentences (read-from-string scan-string))
            (dolist (nxtsent tokenized-sentences (format nil "~s" analyser-result))
                  (multiple-value-setq (sent-lex-items sent-tokens) (analyser nxtsent))
                  (setq analyser-result
                      (append1 analyser-result (outpprint sent-lex-items sent-tokens)))
     ; (format t "Analyser-result ~% ~a~% ~s~%" analyser-result analyser-result)
     ; (format t "sent-lex-items ~% ~a~% ~s~%" sent-lex-items sent-lex-items)
     ; (format t "sent-tokens ~% ~a~% ~s~%" sent-tokens sent-tokens)
     ; (break "")
                  ))
     ; *** the result of outpprint is the same as sent-lex-items plus sent-tokens, but
     ;     the two parallel lists are converted in a list of pairs
         (t (exception 'generic-error
               "top-level-fun: non-string input to tule-analyser")))))

; *************************************************************************
(defun tule-postagger (analysed-string)
 (let (analysed-sentences postagger-result)
   (cond ((stringp analysed-string)
            (setq analysed-sentences (read-from-string analysed-string))
            (dolist (nxtsent analysed-sentences (format nil "~s" postagger-result))
                   (setq postagger-result
                       (append1 postagger-result (postagger nxtsent)))))
         (t (exception 'generic-error
               "top-level-fun: non-string input to tule-postagger")))))

; *************************************************************************
(defun tule-parser (tagged-string)
 (let (tagged-sentences parser-result temp-parse temp-parse2)
   (cond ((stringp tagged-string)
            (setq tagged-sentences (read-from-string tagged-string))
            (setq temp-parse (start-parse-sentences tagged-sentences))
    ;(format t "after parser proper ********~% ~a~% ~s" temp-parse temp-parse)
    ;(break "")
    ; *** temp-parse is a flat-avm, but with two separate lists for data and links
            (setq temp-parse2 (mergeresult temp-parse))
    ;(format t "after mergeresult ********~% ~a~% ~s" temp-parse2 temp-parse2)
    ;(break "")
    ; *** temp-parse2 is the standard flat-avm
            (setq parser-result (reshuffle-tree temp-parse2))
    ; *** the result is in standard avm format
            (format nil "~s" parser-result))
         (t (exception 'generic-error
               "top-level-fun: non-string input to tule-parser")))))

; ****************************************************************************
; *** MAIN FUNCTION FOR SEMANTIC INTEPRETATION STARTING FROM AVM FORMAT ******
; ****************************************************************************
; *** the input may be:
;     1. is a string including the s-expression of a set of trees in avm format. 
;     2. A set of avm trees (an s-expression, not a string)
;     3. the symbol 'f. In this case, it  is assumed that the optional "fname"
;        contains the name of the file containing the avm syntactic trees
;        (the extension is not relevant, since .avm is forced)
(defun tule-semantic-interpreter (synt-trees &optional fname)
 (let (annot-avm-trees onto-repr fol-repr)
  (cond ((stringp synt-trees)
      ; *** input ad output from string
           (setq annot-avm-trees (tule-annotate synt-trees))
           (setq onto-repr (tule-build-ontorepr annot-avm-trees))
           (setq fol-repr (tule-build-fol onto-repr))
           fol-repr)
        ((eq synt-trees 'f)
      ; *** input ad output on files (file-sem-interp in SEMANT-PROC-ALL/seminterp)
           (file-sem-interp (change-extens fname ".avm")
                           (change-extens fname ".svm")		; avm short
                           (change-extens fname ".sem")
                           (change-extens fname ".ssh")         ; semantic short
                           (change-extens fname ".fol")))
      ; *** direct interpretation of an s-expression (singsent-sem-interp in 
      ;     SEMANT-PROC-ALL/seminterp)
        (t (mapcar #'(lambda (x) (singsent-sem-interp x)) synt-trees)))))

; ****************************************************************************
(defun tule-annotate (avm-trees)
 (let (parse-trees annotation-result)
   (cond ((stringp avm-trees)
            (setq parse-trees (read-from-string avm-trees))
            (dolist (nxtsent parse-trees (format nil "~s" annotation-result))
                  (setq annotation-result
                       (append1 annotation-result (annotate nxtsent)))))
         (t (exception 'generic-error
               "top-level-fun: non-string input to tule-annotate")))))

; ****************************************************************************
(defun tule-build-ontorepr (annotated-trees)
 (let (annot-parse-trees full-semrepr interp-result)
   (cond ((stringp annotated-trees)
            (setq annot-parse-trees (read-from-string annotated-trees))
            (dolist (nxtsent annot-parse-trees (format nil "~s" interp-result))
     ; *** build-sem-query in SEMANT-PROC-ALL/build-ontorepr
                  (setq full-semrepr (build-sem-query nxtsent))
     ; *** full-sem-repr includes all SUBCLASS links (subsumption), that are
     ;     removed by "simplify-onto-repr" (in SEMANT-PROC-ALL/seminterp)
                  (setq interp-result
                       (append1 interp-result (simplify-onto-repr full-semrepr)))))
         (t (exception 'generic-error
               "top-level-fun: non-string input to tule-build-ontorepr")))))

; ****************************************************************************
(defun tule-build-fol (ontoreprs)
 (let (ontological-graphs fol-result)
   (cond ((stringp ontoreprs)
            (setq ontological-graphs (read-from-string ontoreprs))
            (dolist (nxtsent ontological-graphs (format nil "~s" fol-result))
                  (setq fol-result
                       (append1 fol-result (sem-to-fol-translation nxtsent)))))
         (t (exception 'generic-error
               "top-level-fun: non-string input to tule-build-fol")))))

; ****************************************************************************
; *** this takes the content of the buffer of previous "sentence" and the analysis
;     of the current "sentence" and checks if the current sentence ends with a
;     sentence terminator or not. The buffer prev-sent is non-null if the
;     preceding fragment did not end with a sentence terminator
; *** It returns three values: the first one is t in case the current sentence actually
;     is not complete, false otherwise. The second value is the buffer of the
;     full sentence; it can include incomplete previous fragments (input prev-sent)
;     and always includes the new part (curr-sent). The third values is a boolean that
;     specifies if, at the end of curr-sent, we are inside a parenthesis
; *** end-of-file? is t in case curr-sent is the last one (in the file or in the string)
;     in-parenth is t in case curr-sent comes after (in the previous text) an open
;        parenthesis, nil otherwise
(defun check-sent-continuation (prev-sent curr-sent end-of-file? in-parenth)
   (declare (special *ANALYS-CONTEXT* *SYSTEM-CONTEXT*))
   (let (sigla-continue siglinterp inside-par)
; *** The next anticipates the analysis of siglas, and assumes that any element
;     tokenizer-ambiguous between a sigla and something else is in fact a sigla.
;     This is important in case of siglas closed by a period (ex. Ch.), where
;     the period has to be taken as part of the sigla and not as a sentence terminator
	 (multiple-value-setq 
              (sigla-continue siglinterp)
              (poss-sigla (ult curr-sent) (second (reverse curr-sent))))
; *** inside-par says if the last parenthesis in curr-sent is closed ('closed),
;     open ('open), or there is no parenthesis (nil)
; *** in-parenth refers to the situation concerning also the previous sentences:
;     it is t if we are inside a parenthesis, nil otherwise
; *** The treatment of parentheses is difficult, since a parenthesis may include
;     more than one sentence. In such a case, the current 'single' sentence
;     includes all material inside the parenthesis (i.e. more sentences)
	 (setq inside-par (inside-a-parenth (reverse curr-sent)))
	 (cond ((eq inside-par 'open)
		  (setq in-parenth t))
	       ((eq inside-par 'closed)
		  (setq in-parenth nil)))
	 (cond ((and siglinterp sigla-continue)
; ******* treatment of the case where the last element is in fact a sigla
; *** first, curr-sent is reversed; then the first element is eliminated (the former 
;     last element, which has a sigla interpretation), and it is substituted by the 
;     sigla interpretation (disregarding the other possibilities). Finally, the result
;     is reversed to get the original order
                 (values t
		     (append prev-sent
		          (reverse (cons siglinterp (rest (reverse curr-sent)))))
                     in-parenth))
; *** the second condition for not advancing refers to initial 'dotted' numbers
;     (ex. chapter numbers in books), which are kept together with the title of the chapter.
	       ((and (null prev-sent)
		     (= 1 (length curr-sent))
		     (scan-number-ambig? (first curr-sent)))
		 (values t (choose-number-interp (first curr-sent)) in-parenth))
; *** the third condition for not advancing refers to initial identifiers of
;     European directives. They have the form "32000L0035"
	       ((and (null prev-sent)
		     (= 1 (length curr-sent))
		     (scan-eucode-ambig? (first curr-sent)))
		 (values t (choose-eucode-interp (first curr-sent)) in-parenth))
; *** this fourth condition for not advancing refers to being inside a parenthesis
;     Although this can be questionable, I do not accept breaking a sentence inside 
;     a parenthesis (this could cause problems in case of a typing error, so that I set 
;     the escape condition that the resulting sentence cannot be longer than 300 words).
	       ((or (and in-parenth
			 (not end-of-file?)
			 (neq 'closed inside-par)
		         (not (> (+ (length prev-sent) (length curr-sent)) 300)))
		    (and (not end-of-file?)
			 (eq 'open inside-par)
		         (not (> (+ (length prev-sent) (length curr-sent)) 300))))
		  (setq in-parenth t)
		  (values t (append prev-sent curr-sent) in-parenth))
; *** The fifth condition for not advancing is related to legal texts. In case the last
;     item is a colon, and the penultimate item is an entity of the semantic class "word"
	       ((and (eq *SYSTEM-CONTEXT* 'legal)
                     (is-last-colon (ult curr-sent)))
		  (values t (append prev-sent curr-sent) in-parenth))
	       (siglinterp
; ******* treatment of the case where the last element is in fact a sigla
;         This the case of roman numbers (chapter V.), where the sentence
;         must not be continued, but the result of poss-sigla must be used
                  (values t
		      (append prev-sent (reverse (cons siglinterp (rest (reverse curr-sent)))))
                      nil))
; *** in all other cases, the sentence is considered as terminated
; *** the next cond selects the integer+punct interpretation in case of ambiguities such 
;     as '312.'. This is the same choice as in the previous branch of the upper 'cond'. 
;     However, here the choice is conditioned to being in the analysis of a standard text 
;     (and not, say, a formula). Actually, 'text' is the only value currently used for the 
;     global *ANALYS-CONTEXT*. Moreover, in this case, the sentence is considered as terminated.
               ((and (eq *ANALYS-CONTEXT* 'text)
		    (scan-number-ambig? (ult curr-sent)))
		  (setq curr-sent
                     (append (butlast curr-sent) (choose-number-interp (ult curr-sent))))
	 	  (values t (append prev-sent curr-sent) in-parenth))
; *** the sentence is complete: standard case; return nil as first value
	       (t (values nil (append prev-sent curr-sent) nil)))))

; *************************************************************************
; ***** MORPHOLEXICAL ANALYSIS + TAGGING + PARSING ************************
; *************************************************************************
; *** this function is used in case the input has to be taken from files
(defun ana-and-tag-and-parse ()
    (declare (special *TREE-FORMAT*))
 (let (answ xinput)
    (setq *TREE-FORMAT* 'tut)
    (format t " Do you want to carry out the work on a whole corpus? (y/n)~%")
    (setq answ (checkanswer '(y n)))
    (cond ((eq answ 'n)
            (setq xinput (ask-file-name))
            (format t " ~%  @@@@@@@@@ MORPHOLEXICAL ANALYSIS AND TAGGING @@@@@@~%")
            (file-ana-text+tag xinput)
             ; *** file-ana-text+tag in PROC/top-level-fun *****
            (format t " ~%  @@@@@@@@@@@@@@@@@@@@ PARSING @@@@@@@@@@@@@@@@@@@@@@~%")
            (parse-single-f
                        (change-extens xinput ".tb")
                        (change-extens xinput ".prs")
                        (change-extens xinput ".csf")))
          (t (with-open-file (iport (build-file-name "../DATI/tagcorpus.dat")
                                :direction :input :if-does-not-exist :error)
            (dolist (filen (read iport))
                    (setq filen (build-file-name filen))
                    (format t " ~%  $$$$$$$$$$$$$ Next file: ~a $$$$$$$ ~%" filen)
                    (file-ana-text+tag filen)
                      ; *** file-ana-text+tag in PROC/top-level-fun *****
                    (parse-single-f
                         (change-extens filen ".tb")
                         (change-extens filen ".prs")
                         (change-extens filen ".csf"))))))))

; *************************************************************************
; ***** MORPHOLEXICAL ANALYSIS + TAGGING + PARSING + PARSING EVALUATION ***
; *************************************************************************
(defun ana-and-tag-and-parse-and-eval ()
    (declare (special *TREE-FORMAT*))
 (let (answ xinput)
    (setq *TREE-FORMAT* 'tut)
    (format t " Do you want to carry out the work on a whole corpus? (y/n)~%")
    (setq answ (checkanswer '(y n)))
    (cond ((eq answ 'n)
            (setq xinput (ask-file-name))
            (format t " ~%  @@@@@@@@@ MORPHOLEXICAL ANALYSIS AND TAGGING @@@@@@~%")
            (file-ana-text+tag xinput)
                      ; *** file-ana-text+tag in PROC/top-level-fun *****
            (format t " ~%  @@@@@@@@@@@@@@@@@@@@ PARSING @@@@@@@@@@@@@@@@@@@@@@~%")
            (parse-single-f
                        (change-extens xinput ".tb")
                        (change-extens xinput ".prs")
                        (change-extens xinput ".csf"))
            (format t " ~% ***** COMPARISON ******************** ~% ~%")
            (compparse-single-f
                        (change-extens xinput ".prs")
                        (change-extens xinput ".man")
                        (change-extens xinput ".cmp")
                        (change-extens xinput ".err")
                        (change-extens xinput ".tag")
                        (change-extens xinput ".dis")))
          (t (with-open-file (iport (build-file-name "../DATI/tagcorpus.dat")
                                :direction :input :if-does-not-exist :error)
            (dolist (filen (read iport))
                    (setq filen (build-file-name filen))
                    (format t " ~%  $$$$$$$$$$$$$ Next file: ~a $$$$$$$ ~%" filen)
                    (file-ana-text+tag filen)
                      ; *** file-ana-text+tag in PROC/top-level-fun *****
                    (parse-single-f
                        (change-extens filen ".tb")
                        (change-extens filen ".prs")
                        (change-extens filen ".csf"))
                    (compparse-single-f
                        (change-extens filen ".prs")
                        (change-extens filen ".man")
                        (change-extens filen ".cmp")
                        (change-extens filen ".err")
                        (change-extens filen ".tag")
                        (change-extens filen ".dis"))))))))

; *************************************************************************
; *** this function takes the input from the .man file, so that it carries out
;     the parsing on the manually tagged (hopefully correct) input
; *** with respect to the next one, it produces the output with extended labels,
;     e.g. ADVB-RMOD-LOC instead of RMOD
(defun parse-extend-and-eval ()
    (declare (special *TREE-FORMAT*))
 (let (answ xinput)
    (setq *TREE-FORMAT* 'tut)
    (format t " Do you want to carry out the work on a whole corpus? (y/n)~%")
    (setq answ (checkanswer '(y n)))
    (cond ((eq answ 'n)
            (setq xinput (ask-file-name))
            (format t " ~%  @@@@@@@@@@@@@@@@@@@@ PARSING @@@@@@@@@@@@@@@@@@@@@@~%")
            (parse-single-f
                        (change-extens xinput ".man")
                        (change-extens xinput ".spr")	; semantic parse
                        (change-extens xinput ".csf")
                        t)				
            (compparse-single-f
                        (change-extens xinput ".spr")
                        (change-extens xinput ".man")
                        (change-extens xinput ".cmp")
                        (change-extens xinput ".tag")
                        (change-extens xinput ".dis")
                        (change-extens xinput ".err")))
          (t (with-open-file (iport (build-file-name "../DATI/tagcorpus.dat")
                                :direction :input :if-does-not-exist :error)
            (dolist (filen (read iport))
                    (setq filen (build-file-name filen))
                    (format t " ~%  $$$$$$$$$$$$$ Next file: ~a $$$$$$$ ~%" filen)
                    (parse-single-f
                        (change-extens filen ".man")
                        (change-extens filen ".spr")	; semantic parse
                        (change-extens filen ".csf")
                        t)			
                    (compparse-single-f
                        (change-extens filen ".spr")
                        (change-extens filen ".man")
                        (change-extens filen ".cmp")
                        (change-extens filen ".err")
                        (change-extens filen ".tag")
                        (change-extens filen ".dis"))))))))

; *************************************************************************
; *** this function takes the input from the .man file, so that it carries out
;     the parsing on the manually tagged (hopefully correct) input
(defun parse-from-tbfile ()
    (declare (special *TREE-FORMAT*))
 (let (answ xinput)
    (setq *TREE-FORMAT* 'tut)
    (format t " Do you want to carry out the work on a whole corpus? (y/n)~%")
    (setq answ (checkanswer '(y n)))
    (cond ((eq answ 'n)
            (setq xinput (ask-file-name))
            (format t " ~%  @@@@@@@@@@@@@@@@@@@@ PARSING @@@@@@@@@@@@@@@@@@@@@@~%")
            (parse-single-f
                        (change-extens xinput ".tb")
                        (change-extens xinput ".prs")
                        (change-extens xinput ".csf")))
          (t (with-open-file (iport (build-file-name "../DATI/tagcorpus.dat")
                                :direction :input :if-does-not-exist :error)
            (dolist (filen (read iport))
                    (setq filen (build-file-name filen))
                    (format t " ~%  $$$$$$$$$$$$$ Next file: ~a $$$$$$$ ~%" filen)
                    (parse-single-f
                        (change-extens filen ".tb")
                        (change-extens filen ".prs")
                        (change-extens filen ".csf"))))))))

; *************************************************************************
; *** this function takes the input from the .man file, so that it carries out
;     the parsing on the manually tagged (hopefully correct) input
(defun parse-and-eval ()
    (declare (special *TREE-FORMAT*))
 (let (answ xinput)
    (setq *TREE-FORMAT* 'tut)
    (format t " Do you want to carry out the work on a whole corpus? (y/n)~%")
    (setq answ (checkanswer '(y n)))
    (cond ((eq answ 'n)
            (setq xinput (ask-file-name))
            (format t " ~%  @@@@@@@@@@@@@@@@@@@@ PARSING @@@@@@@@@@@@@@@@@@@@@@~%")
            (parse-single-f
                        (change-extens xinput ".man")
                        (change-extens xinput ".prs")
                        (change-extens xinput ".csf"))
            (compparse-single-f
                        (change-extens xinput ".prs")
                        (change-extens xinput ".man")
                        (change-extens xinput ".cmp")
                        (change-extens xinput ".err")
                        (change-extens xinput ".tag")
                        (change-extens xinput ".dis")
                        t))
          (t (with-open-file (iport (build-file-name "../DATI/tagcorpus.dat")
                                :direction :input :if-does-not-exist :error)
            (dolist (filen (read iport))
                    (setq filen (build-file-name filen))
                    (format t " ~%  $$$$$$$$$$$$$ Next file: ~a $$$$$$$ ~%" filen)
                    (parse-single-f
                        (change-extens filen ".man")
                        (change-extens filen ".prs")
                        (change-extens filen ".csf"))
                    (compparse-single-f
                        (change-extens filen ".prs")
                        (change-extens filen ".man")
                        (change-extens filen ".cmp")
                        (change-extens filen ".err")
                        (change-extens filen ".tag")
                        (change-extens filen ".dis")
                        t)))))))

; *************************************************************************
; *** This function carries out all operations, starting from the morpholexical
;     analysis until the semantic interpretation expressed in logical form
; *** It has two modes of operation (encoded as 'i and 'f in the "mode" parameter):
;     a. Interactive, where the user gives a single sentence and gets its interpretation
;        a.1 The sentence is given via keyboard and the result is seen only on
;            the screen
;        a.2 The user gives the name of a file and the system produces all
;            intermediate analysis files and stores the  resulting interpretation
;            in another file. As usual, if the input file is "fname", we get:
;            fname.tb (morpholexical + Postagged)
;            fname.tball (morpholexical + Postagged, together with all morpholexical)
;            fname.sim (lemmata associated to the words in the input)
;            fname.dis (tagging rules used by the pos-tagger)
;            fname.prs (parsed file in TUT format)
;            fname.csf (all verbs with their dependents)
;            fname.sem (the semantic interpretation)
;     b. On file, which is different from a.2 above, because a whole text is analysed.
;        In this case, no interaction is required with the user, since the file name
;        was asked outside (see start-atlas in the PROC-ALL/main file) and nothing is 
;        seen on the screen
;     In case mode = f (case b above), the optional "input" actually is mandatory.
(defun ana-and-tag-and-parse-and-seminterp (mode &optional fname)
 (declare (special *ONTOLOGY-CACHE* *ONTO-CACHE-FILE* *TREE-FORMAT* *PRINT-LEV* 
                   *HOME-DIR* *SYSTEM-CONTEXT* *TURN-TO-TALK*))
 (let (sentence tagres parseres new-tree semrepr tempparse inpsource fpathname
       avmoutp semoutp trace?)
  ;   (format t "ana-and-tag-and-parse-and-seminterp; *TURN-TO-TALK*: ~a~%" *TURN-TO-TALK*)
  (with-open-file (iport *ONTO-CACHE-FILE*
                         :direction :input :if-does-not-exist :error)
           (setq *ONTOLOGY-CACHE* (read iport)))
  (cond ((eq mode 'i)
          (setq *TREE-FORMAT* 'avm)
          (format t "Is the input manual (m) or from a file (f)?~%")
          (setq inpsource (check_risp '(m f) 0))
          (format t "Do you want to trace the execution? (y or n)~%")
          (setq trace? (check_risp '(y n) 0))
          (cond ((eq trace? 'y) (setq trace? t))
                (t (setq trace? nil)))
          (format t "Print level? ~%0: semantic query;~%1: also the annotated syntactic tree in AVM format~%2: also the syntactic tree in treebank format~%")
          (setq *PRINT-LEV* (check_risp '(0 1 2) 0))
          (cond ((eq inpsource 'f)
                   (format t "Name of the input file (without double quotes)?~%")
                   (format t "  (The base directory is *HOME-DIR*/DATI/ATLAS/)~%")
                   (setq fpathname (build-subdir-file-name (read-line) "../DATI/ATLAS/"))
                   (with-open-file (iport fpathname
                                 :direction :input :if-does-not-exist :error)
                       (setq sentence (read-line iport))))
            (t (format t "Sentence? ")
               (setq sentence (read-line))))
          (setq tagres 
                (catch 'morpho-error (hops-ana-text+tag sentence)))
             ; *** tagres is a tree in treebank format without tree information
          (cond ((stringp (first tagres))
                  (cond ((eq *SYSTEM-CONTEXT* 'tocai)
                           (list 'morpho sentence))
                        (t nil)))
                (t (setq tempparse (catch 'parse-error (start-parse-sentences tagres)))
                   (cond ((stringp (first tempparse))
                            (cond ((eq *SYSTEM-CONTEXT* 'tocai)
                                    (list 'parser sentence))
                                  (t nil)))
             ; *** tempparse is the result in the form:
             ;     ((wordsent1 wordsent2 ... wordsentn) (linksent1 linksent2 ... linksentn))
             ;     each wordsenti is the list of items of the i-th sentence, and the
             ;     linksenti are the corresponding links to parents.
             ;     The form of wordsenti (a sentence) is:
             ;     (word1 word2 ... wordn), and each word is represented as:
             ;     ((posit .) (form .) (syn ((lemma .) (cat .) ...)) (sem (...)))
                         (t (setq parseres (mergeresult tempparse))
             ; *** parseres is an avm with tree information;
             ;     mergeres is needed, since the result of parse-sentences is
             ;     given in form of two separate list: the word list and the link list
                            (cond ((= *PRINT-LEV* 2)
                                    (format t "PARSERES: ~s ~%" parseres)
                                    (break "")))
             ; *** the next produces the actual AVM format
                            (setq new-tree (reshuffle-tree parseres))
                    ; *** the two branches below return the same result, but the first one has,
                    ;     as a side effect, the printing of the semantically annotated AVM tree
                    ;     in the .avm file
  ;    (format t "ana-and-tag-and-parse-and-seminterp 2; *TURN-TO-TALK*: ~a~%" *TURN-TO-TALK*)
                            (cond ((eq inpsource 'f)
                                      (setq avmoutp (change-extens fpathname ".avm"))
                                      (setq semrepr (semantic-interpretation new-tree avmoutp)))
                                  (t (setq semrepr (semantic-interpretation new-tree))))
                            (cond ((stringp (first semrepr))
                                    (cond ((eq *SYSTEM-CONTEXT* 'tocai)
                                             (list 'seminterp sentence))
                                          (t nil)))
                                  (t (format t "SEMANTIC QUERY: ~s ~%" semrepr)
                                     (cond ((eq inpsource 'f)
                                              (setq semoutp (change-extens fpathname ".sem"))
                                              (with-open-file (outsemport semoutp
                                                                 :direction :output
                                                                 :if-exists :overwrite
                                                                 :if-does-not-exist :create)
                                                 (format outsemport "~a~%" semrepr))))
                                     (break "")
                                     (with-open-file (iport *ONTO-CACHE-FILE*
                                                            :direction :output :if-exists :overwrite)
                                             (format iport "~a~%" *ONTOLOGY-CACHE*))
       ; *** returns to main a 4-tuple:
       ;     - ok (if eveything was ok)
       ;     - the resulting ontological representation
       ;     - the flag indicating if a trace has been required
       ;     - in case the input was (for atlas) from a file, the file name; this is for enabling
       ;       the main to print on filename.atl the frame representation
                                        (list 'ok semrepr trace?
                                              (cond ((and (member *SYSTEM-CONTEXT* '(atlas tocai))
                                                          (eq inpsource 'f))
                                                       fpathname)
                                                    (t nil))))))))))
     ; *** the next branch for the analysis of a set of sentences (text). They must be contained
     ;     in a single file. 
        ((eq mode 'f)
           (setq *TREE-FORMAT* 'tut)
           (setq *PRINT-LEV* -1)
           (format t " ~%  @@@@@@@@@ MORPHOLEXICAL ANALYSIS AND TAGGING @@@@@@~%")
           (file-ana-text+tag fname)
                    ; *** file-ana-text+tag in PROC/top-level-fun *****
           (format t " ~%  @@@@@@@@@@@@@@@@@@@@ PARSING @@@@@@@@@@@@@@@@@@@@@@~%")
           (setq tempparse
               (parse-single-f
                      (change-extens fname ".tb")
                      (change-extens fname ".prs")
                      (change-extens fname ".csf")))
             ; *** now we have in "fname.prs" the parsed data in TUT format
             ;     the next produces the "fname.fav" version in flat AVM format
           (from-tut-to-flatavm
                      (change-extens fname ".prs")
                      (change-extens fname ".fav"))
             ; *** the next produces the actual AVM format
       ;    (break "After conversion in flat avm")
           (setq *TREE-FORMAT* 'avm)
     ; *** fav: flat avm
     ;     avm: true avm
     ;     svm: simplified avm (as avm, but readable by humans)
           (file-reshuffle-tree 
                      (change-extens fname ".fav")
                      (change-extens fname ".avm"))
       ;    (break "After conversion in true avm")
           (setq semrepr (semantic-interpretation 'f fname))))))

; *******************************************************************
;     Morphological analysis + POS tagging of a text stored in a file
; *******************************************************************
; *** this function is used in "ana-and-tag", "ana-and-tag-and-comp",
;     "ana-and-tag-and-parse", "ana-and-tag-and-parse-and-eval", assuming
;     to have as input a file. All these functions are in ALLLANG/PROC-ALL/main
; *** It reads a text (via tokenize) and carries out the morphological/lexical
;     analysis (accessing the dictionary). The result consists in four files:
;     assuming the input file has name XXX.YYY, they are:
;     - XXX.dis (tag rules used to choose the POS of the words)
;     - XXX.tb (POS tagged file)
;     - XXX.tball (file including both the word of the chosen POS and also
;       all other lexical interpretations)
;     - XXX.sim (sequence of lemmas corresponding to the input words, according
;       to the chosen POS)
(defun file-ana-text+tag (filename)
  (let (sent-tokens sent-lex-items element morpholex-result
	flag xoutpt xoutpt2 xoutpt3 xoutpt4 scan-element next-phrase postag-result
	(linecount 0) rem-line siglinterp in-parenth inside-par file-label
	(glob-time 0) (tok-time 0) (analy-time 0) (wri-time 0) (tot-time 0)
	(tag-time 0) sent-numb act-phrase sigla-continue)
  (declare (special element flag *TREE-FORMAT* *SYSTEM-CONTEXT*
	    scan-element linecount rem-line tag-time glob-time
	    wri-time *ANALYS-CONTEXT*))
; *** linecount used in tokenize
   (setq tot-time (get-internal-run-time))
   (setq xoutpt (change-extens filename ".dis"))
   (setq xoutpt2 (change-extens filename ".tb"))
   (setq xoutpt3 (change-extens filename ".tball"))
   (setq xoutpt4 (change-extens filename ".sim"))
   (with-open-file (portin filename
			   :direction :input
		     	   :if-does-not-exist :error)
     (with-open-file (disoutputport xoutpt
                                 :direction :output
                                 :if-exists :overwrite
                                 :if-does-not-exist :create)
     (with-open-file (tboutputport xoutpt2
                                 :direction :output
                                 :if-exists :overwrite
                                 :if-does-not-exist :create)
     (with-open-file (tbaoutputport xoutpt3
                                 :direction :output
                                 :if-exists :overwrite
                                 :if-does-not-exist :create)
     (with-open-file (simoutputport xoutpt4
                                 :direction :output
                                 :if-exists :overwrite
                                 :if-does-not-exist :create)
; *** the function "tokenize", applied in the do* below, reads the lines of the
;     input file, and returns the 'tokens' composing a single sentence. This
;     means that the tokenizer should in principle detect sentence ends, and that
;     it must be ready to tokenize the next sentence in the next step of the loop.
;     The buffer rem-line contains the part of the line that refers to the next
;     sentence.
; *** The main problem is that it is sometimes difficult to separate the sentence
;     just on the basis of the single chars (which is what "tokenize" should do).
;     So, "tokenize" takes the safer assumption that, in case of doubt, it assumes
;     there is a sentence separation. If this is wrong, it is this function
;     (file-ana-text+tag) that 'attaches' the next portion to the current one to
;     produce a single sentence for the parser
     (setq rem-line nil)
; *** read-header defined in ALLLANG/PROC-ALL/TAGGER-PROC-ALL/postagger
     (multiple-value-setq (file-label sent-numb) (read-header portin))
     (do* ((glob-time (get-internal-run-time) (get-internal-run-time))
           (one-phrase (tokenize portin) (tokenize portin))
           (stop nil (first one-phrase)))
         ((and (eq stop 's-eof) 
          (null (second one-phrase))
          (null next-phrase)))   ; if next-phrase is not null, then we have
                                 ; hypothesized a continuation. But the
                                 ; continuation is null. The previous part
                                 ; has to be analysed anyway
         (setq act-phrase (second one-phrase))
         (setq tok-time (+ tok-time (- (get-internal-run-time) glob-time)))
; ****************************
;     THE NEXT PART TO DECIDE IF TWO CANDIDATE SENTENCES HAVE TO BE TREATED AS A
;     SINGLE ONE
; ****************************
; *** one-phrase contains 's-eos plus all tokens found by the tokenizer until the
;     next possible sentence ending. Such tokens are appended to next-phrase
;     (initially empty)
; *** if the recognized end of sentence is not a possible sigla, then analyze the
;     contents of next-phrase. Otherwise do nothing, so that tokenize returns
;     another piece of sentence, which is appended to next-phrase.
; *** This anticipates the analysis of siglas, and assumes that any element
;     tokenize-ambiguous between a sigla and something else is in fact a sigla
;     (so that the sentence continues)
; *** the second condition for not advancing refers to initial 'dotted' numbers
;     (ex. chapter numbers in books), which are kept together with the title of
;     the chapter.
         ;(format t " Tokenizer output: ~a~%" act-phrase)
         ;(break "")
	 (multiple-value-setq 
              (sigla-continue siglinterp)
              (poss-sigla (ult act-phrase) (second (reverse act-phrase))))
; *** inside-par says if the last parenthesis in one-phrase is closed ('closed),
;     open ('open), or there is no parenthesis (nil)
; *** in-parenth refers to the situation concerning also the previous phrases:
;     it is t if we are inside a parenthesis, nil otherwise
; *** The treatment of parentheses is difficult, since a parenthesis may include
;     more than one sentence. In such a case, the current 'single' sentence
;     includes all material inside the parenthesis (i.e. more sentences)
	 (setq inside-par (inside-a-parenth (reverse act-phrase)))
	 (cond ((and (not (null siglinterp))
                     sigla-continue)
; ******* treatment of the case where the last element is in fact a sigla
; *** first, act-phrase is reversed (reverse); then the first element is
;     eliminated (the former last element, which has a sigla interpretation), so
;     that it is substituted (cons) with the sigla interpretation (disregarding
;     the other possibilities). Finally, the result is reversed to get the
;     original order
                 (setq next-phrase
		       (append next-phrase
		        (reverse 
		           (cons siglinterp ;(mapcar #'list siglinterp)
			         (rest (reverse act-phrase))))))
		 (cond ((eq inside-par 'open)
			  (setq in-parenth t))
		       ((eq inside-par 'closed)
			  (setq in-parenth nil))))
; *** the second condition for not advancing refers to initial 'dotted' numbers
;     (ex. chapter numbers in books), which are kept together with the title of
;     the chapter.
	       ((and (null next-phrase)
		     (= 1 (length act-phrase))
		     (scan-number-ambig? (first act-phrase)))
		 (setq next-phrase 
			(choose-numb+period-interp (first act-phrase))))
; *** the third condition for not advancing refers to initial identifiers of
;     European directives. They have the form "32000L0035"
	       ((and (null next-phrase)
		     (= 1 (length act-phrase))
		     (scan-eucode-ambig? (first act-phrase)))
		 (setq next-phrase 
			(choose-eucode-interp (first act-phrase))))
; *** this fourth condition for not advancing refers to being inside a parenthesis
;     Although this can be questionable, I do not accept breaking a sentence
;     inside a parenthesis (this could cause problems in case of a typing error,
;     so that I set the escape condition that the resulting sentence cannot be
;     longer than 300 words).
	       ((or (and in-parenth
			 (neq 's-eof (first one-phrase))
			 (neq 'closed inside-par)
		         (not (> (+ (length next-phrase) (length act-phrase))
                                 300)))
		    (and (neq 's-eof (first one-phrase))
			 (eq 'open inside-par)
		         (not (> (+ (length next-phrase) (length act-phrase))
                                 300))))
		  (setq in-parenth t)
		  (setq next-phrase (append next-phrase act-phrase)))
; *** The fifth condition for not advancing is related to legal texts.
;     In case the last item is a colon, and the penultimate item is an
;     entity of the semantic class "word"
	       ((and (eq *SYSTEM-CONTEXT* 'legal)
                     (is-last-colon (ult act-phrase)))
		  (setq next-phrase (append next-phrase act-phrase)))
	       (t (setq in-parenth nil)
	         (cond ((not (null siglinterp))
; ******* treatment of the case where the last element is in fact a sigla
;         This the case of roman numbers (chapter V.), where the sentence
;         must not be continued, but the result of poss-sigla must be used
                         (setq next-phrase
		               (append next-phrase
		                (reverse 
		                   (cons siglinterp ;(mapcar #'list siglinterp)
			                 (rest (reverse act-phrase)))))))
; *** in all other cases, the sentence is considered as terminated, so that the
;     analysis is started up
; *** the next cond selects the integer+punct interpretation in case of
;     ambiguities such as '312.'. This is the same choice as in the previous
;     branch of the upper 'cond'. However, here the choice is conditioned to
;     being in the analysis of a standard text (and not, say, a formula).
;     Actually, 'text' is the only value currently used for the global
;     *ANALYS-CONTEXT*. Moreover, in this case, the sentence is considered as
;     terminated.
                        ((and (eq *ANALYS-CONTEXT* 'text)
			      (scan-number-ambig? (ult act-phrase)))
	 	          (setq next-phrase 
                              (append next-phrase 
				   (append (butlast act-phrase)
					   (choose-number-interp (ult act-phrase))))))
	 	        (t 
      ; *** standard processing ********************************************
                 (setq next-phrase (append next-phrase act-phrase))
	 	 (setq glob-time (get-internal-run-time))
                 (multiple-value-setq (sent-lex-items sent-tokens) (analyser next-phrase))
	 	 (setq analy-time 
                       (+ analy-time (- (get-internal-run-time) glob-time)))
                 (setq morpholex-result (outpprint sent-lex-items sent-tokens))
     ;(format t "Output-1: ~s~% Output-2: ~s~% Output tot 1: ~a~% Output tot 2: ~s~% Input: ~s~%" 
     ;           sent-lex-items sent-tokens morpholex-result morpholex-result next-phrase)
     ;(break "")
     ; *** morpholex-result is the same as sent-lex-items plus sent-tokens, but
     ;     the two parallel lists are converted in a list of pairs
     ; *** in this case, the pos tagger is called on a file, and the result is put
     ;     in the various output files:
     ;     disoutputport:  .dis
     ;     tboutputport: .tb
     ;     tbaoutputport: .tball
     ;     simoutputport: .sim
     ; !!! Note that file-label is NOT the name of the file, but the internal label
     ;     specified in the file Header
	 	 (setq postag-result 
                          (postagger morpholex-result file-label sent-numb
			 	 disoutputport tboutputport tbaoutputport simoutputport))
     ; !!! postag-result is not used, but it is assigned as a debugging facility
	 	 (setq sent-numb (1+ sent-numb))
	 	 (setq next-phrase nil)
	 	 (setq sent-lex-items nil))))))
     (setq tot-time (- (get-internal-run-time) tot-time))
     (format t "Analyser+Lemmatizer statistics:~%  Tokenizer time: ~s~%  Analyzer time: ~s~%  Tagger time: ~s~%  Output time: ~s~%      TOTAL TIME: ~s~%~%"
		(print-time tok-time) (print-time analy-time)
		(print-time tag-time) (print-time wri-time)
		(print-time tot-time)))))))))
      ; *** print-time defined in PROC/utilities

; *******************************************************************
; *** this function is used in "ana-and-tag-and-parse-and-seminterp",
;     (in ALLLANG/PROC-ALL/main) assuming to have as input a string 
;     including a sentence. 
;(defun string-ana-text+tag (input-string)
;  (let (sent-tokens sent-lex-items element postag-result
;        flag graf scan-element one-phrase rem-line tagresult morpholex-result)
;  (declare (special element flag graf *TREE-FORMAT*
;            scan-element rem-line *ANALYS-CONTEXT*))
; *** "tokenize" takes the string and extracts from it the first sentence,
;     i.e. until a sentence separator (: ; . ! ?) is found. The material
;     after the separator is ignored. Actually, the function could be 
;     modified in order to deal with multi-sentence strings, in a way analogous
;     to the file-ana-text+tag function defined above.
;     (setq one-phrase (tokenize input-string))
; ****************************
;     THE NEXT PART TO DECIDE IF TWO CANDIDATE SENTENCES HAVE TO BE TREATED AS A
;     SINGLE ONE
; ****************************
; *** one-phrase is a triple containing:
;     1. 'eos
;     2. all tokens found until the sentence ending by the tokenizer.
;     3. The string remaining after the first sentence
;     The items in 2 above  (the "second" of one_phrase) are given as input 
;     (in a second-level list,
;     for homogenity with file-ana.text+tag) to "analyser".
;     (multiple-value-setq (sent-lex-items sent-tokens) 
;                 (analyser (rest one-phrase)))
;     (setq morpholex-result (outpprint sent-lex-items sent-tokens))
;     (setq postag-result (postagger morpholex-result))
;     (setq tagresult (cons postag-result tagresult))
;     (reverse tagresult)))

; ******************************************************************************
; *** it determines if there is an open-parenthesis in the output of the
;     tokenizer, without the corresponding closed parenthesis. It moves 
;     backward: if it finds a closed parenthesis, it returns 'closed; if it
;     finds an open parenthesis, it returns 'open; otherwise, it returns nil
(defun inside-a-parenth (scan)
  (cond ((null scan) nil)
; *** the first 'first' takes the last element of the scan output
; *** the second 'first' takes the first scan interpretation of this element
; *** the third 'first' takes the first scan 'component' of that interpretation
; *** the fourth 'first' takes the list of character codes composing the input
;     item
; *** the fifth 'first' takes the first character
        (t (let ((firstscanel (first (first (first (first (first scan)))))))
     (cond ((member firstscanel '(open-par open-square-par open-double-angle)) 
	      'open)
	   ((member firstscanel '(closed-par closed-square-par closed-double-angle)) 
	      'closed)
	   (t (inside-a-parenth (rest scan))))))))

; ******************************************************************************
; *** it determines if the last two elements are a word referring to a
;     fragment of phrase followed by a colon, as in "le parole:"
;     This is a big trick, since, no morphological analysis can be made
;     here on the previous word, so that there is just a check on the
;     chars forming it (except for the last, marking the sing/pl distinction)
(defun is-last-colon (scanel)
(cond ((null scanel) nil)
; *** the first 'first' takes the first scan interpretation of the last element
;     of the scan output
; *** the 'ult' takes the last scan 'component' of that interpretation
; *** the second 'first' takes the list of character codes composing the input
;     item
; *** the third 'first' takes the first character
((and (eq (first (first (ult (first scanel)))) 58)
      (or (equal '(115 101 103 117 101 110 116) 	; "seguent"
		  (butlast (first (first (first scanel)))))
	  (equal '(112 97 114 111 108)			; "parol"
		  (butlast (first (first (first scanel))))))) t)
(t (is-last-colon (rest scanel)))))  ; 58 is ":"

; *******************************************************************
; *** it determines whether the current element is ambiguous between
;     integer+period and real
; >>> tokelem is the tokenizer output
(defun scan-number-ambig? (tokelem)
    (and (= 2 (length tokelem))    ; *** there are exactly two interpretations
         (or (and (is-numb+period (first tokelem)) (is-numb (second tokelem)))
             (and (is-numb (first tokelem)) (is-numb+period (second tokelem))))))

; *******************************************************************
; *** this checks if the two possible interpretations are an eucode including
;     an extra period, and an eucode followed by a period
; >>> tokelem is the tokenizer output
(defun scan-eucode-ambig? (tokelem)
(and (= 2 (length tokelem))    ; *** there are exactly two interpretations
(or (and (is-eucode+period (first tokelem)) (is-eucode (second tokelem)))
    (and (is-eucode (first tokelem)) (is-eucode+period (second tokelem))))))

; *******************************************************************
; *** the input is a token interpretation, e.g.
;     (((down-l down-a period) sigla))
(defun is-eucode (tokinterp)
(and (eq 'sigla (second (first tokinterp)))
(has-eucode-format (first (first tokinterp)))))

; *******************************************************************
; *** the input is a token interpretation, e.g.
;     (((down-l down-a period) sigla))
(defun is-eucode+period (tokinterp)
(and (eq 'sigla (second (first tokinterp)))
(eq 'period (ult (first (first tokinterp))))
(has-eucode-format (butlast (first (first tokinterp))))))

; *******************************************************************
; *** the initial code for EU directives is a sequence of digits
;     (probably one of two digits for the month plus four digits
;     for the year), followed by "L", followed by other digits
;     (probably encoding the publication identifier)
;     ex. 32000L0035
(defun has-eucode-format (chars)
(let (fail (status 'start))
(do ((nxtchar (first chars) (first chars))
(chars (rest chars) (rest chars)))
((or (null nxtchar) fail)
  (not fail))
(cond ((member nxtchar (get-charset-value 'digit)) nil)	; "digit" in char-funct.lisp
     ((and (eq status 'start)
	   (eq nxtchar 'up-l))       ; L
	(setq status 'secondpart))
     (t (setq fail t))))))

; *******************************************************************
; *** chooses the number + period interpretation
;     We assume it is something as a chapter number
; *** the addition of list levels aims at converting a 'single tokenizer element'
;     into two subsequent tokenizer elements
;     Ex. ( (((ONE TWO) NUMBER) ((PERIOD) SEGNOINTER))
;	    (((ONE TWO PERIOD) NUMBER)) )
;           --->
;         ( ((((ONE TWO) NUMBER)))
;	    ((((PERIOD) SEGNOINTER))) )
(defun choose-numb+period-interp (tokelem)
   (cond ((is-numb+period (first tokelem))
            (list (list (list (first (first tokelem))))
	          (list (list (second (first tokelem))))))
         (t (list (list (list (first (second tokelem))))
	          (list (list (second (second tokelem))))))))

; *******************************************************************
; *** chooses the number interpretation
;     This is useful in "Add 312. and 7.32", where 312. can also be 312 + period
;     Ex. ( (((ONE TWO) NUMBER) ((PERIOD) SEGNOINTER))
;	    (((ONE TWO PERIOD) NUMBER)) )
;           --->
;         ( ((((ONE TWO) NUMBER)))
;	    ((((PERIOD) SEGNOINTER))) )
(defun choose-number-interp (tokelem)
   (cond ((is-numb (first tokelem))
            (list (list (first tokelem))))
         (t (list (list (second tokelem))))))

; *******************************************************************
; *** chooses the EuCode + period interpretation
;     We assume it is something as a chapter number
; *** the addition of list levels aims at converting a 'single tokenizer element'
;     into two subsequent tokenizer elements
;     Ex. ( (((ONE TWO) NUMBER) ((PERIOD) SEGNOINTER))
;	    (((ONE TWO PERIOD) NUMBER)) )
;           --->
;         ( ((((ONE TWO) NUMBER)))
;	    ((((PERIOD) SEGNOINTER))) )
(defun choose-eucode-interp (tokelem)
(cond ((is-eucode+period (first tokelem))
    (list (list (list (first (second tokelem))))
	  (list (list (second (second tokelem))))))
 (t (list (list (list (first (first tokelem))))
	  (list (list (second (first tokelem))))))))

; *******************************************************************
(defun is-numb+period (elem-interp)
   (and (= 2 (length elem-interp))
        (eq 'number (second (first elem-interp)))
        (eq 'period (first (first (second elem-interp))))))

; *******************************************************************
(defun is-numb (elem-interp)
(and (= 1 (length elem-interp))
(eq 'number (second (first elem-interp)))))

; *******************************************************************
; *** verifies if, among the token interpretations in tokelem, there is one
;     referring to a sigla, and in this case, if it exists in the dictionary
; *** in a relevant case, tokelem has the form
;     ( ( ((118 46) SIGLA) )
;       ( ((118) GW) ((46) SEGNOINTER) )
;     )
; *** it also succeeds if, for a structure of the form "X.". where X is a 
;     single character, X is not a sigla. In such a case, it is assumed that we
;     are in the situation of the initial of a name (as in G. Wilson), so that 
;     the two (possible) sentences must be kept together.
; *** in this case, tokelem has the form
;     ( ( ((118 46) SIGLA) )
;       ( ((118) GW) ((46) SEGNOINTER) )
;       ( ((118) SIGLA) ((46) SEGNOINTER) )
;     )
; *** In the first case, however, it returns the current Sigla interpretation,
;     but in the second it must return a NOMEP interpretation; it is built
;     up by concatenating the two characters
; *** A third possibility refers to the situation of: 
;      "Paragraph V. Title of paragraph"
;     This is the case of a Roman number, capitalized, followed by a period, and
;     preceded by a "section of text identifier" (i.e. Chapter, paragraph, ...)
;     In this case, the result should consist in two elements (the ordinal plus
;     the period) and not a single item (the initial together with the period
;     in case of names and other siglas). This situation arises the character
;     preceding the comma is I (roman one), C (Roman 100), L (roman 50), 
;     V (roman five), D (roman 500), M (roman 1000), X (roman 10), and the
;     previous word is one of the section names (which, of course, are language
;     dependent).
;     N.B. The sequence 
;      "Paragraph V. Title of paragraph"
;          Is considered in the corpus as a single sentence!!!
;     Of course, this anticipates a work that should be made by the POS tagger,
;     but I must do it here, in order to avoid a misalignment in the sentence
;     entries (two items for romans+period; a single item in the other cases)
; *** the function returns two values:
;     1. A boolean that is nil if the possible sigla has to be taken as two items
;        (i.e. not actually a sigla, as for paragraph roman numbers), but the
;        sentence must be continued with the next one. It is true is the item
;        has a real sigla interpretation (all other cases)
;     2. The "tokenizer" value obtained for the chosen analysis (in general a
;        sigla, in case it has such an interpretation, or two items if the
;        first item returned (see above)) is nil
;     If there is no relevant interpretation, the second value returned is nil
(defun poss-sigla (tokelem prevelem)
(declare (special *SIGLE-DICT*))
(cond ((null tokelem) nil)
; *** the first of tokelem is ( ((down-v period) SIGLA) )  [first interpretation]
; *** the first of the first of tokelem is ((down-v period) SIGLA)  [first component of
;     the first interpretation]
; *** the second condition to exclude cases as "II.", where II is a sigla, but
;     the entire interpretation is not a sigla
 (t (let* ((firstinterp (first tokelem))
	   (firstcomp (first firstinterp))
	   (firstcompchars (first firstcomp))
	   (firstcompbasechars 
		 (convert-tule-char-names-to-base-codes firstcompchars))
	   (firstcomptype (second firstcomp)))
; *** in this first case, the sigla interpretation is chosen and returned. This
;     is taken as a confirmation of the opportunity to not to interrupt the sentence
	 (cond ((eq 'sigla firstcomptype)
		 (cond ((and (has-roman-number-interp 
					firstinterp firstcompchars)
			     (is-section-identifier prevelem))
	      ; *** the form is V. and it is preceded by a section identifier
	      ;     So it is taken as a roman number followed by a period
			  (values
			      nil
			     (list (list
				 (list (list (first firstcompchars)) 'sigla)
				 (list (list 'period) 'segnointer)))))
		       ((has-true-sigla-interp firstinterp firstcompbasechars)
			 (cond ((has-short-nomep-interp firstinterp firstcompchars)
	      ; *** the form is N. so it has both a sigla interp and a nomep
	      ;     interp
				  (values t
				      (cons
					(list
					 (list
					    (list (first firstcompchars) 'period) 'nomep))
					(list firstinterp))))
	      ; *** the form is pag. so it has only a sigla interp
				(t (values t (list firstinterp)))))
		       ((has-short-nomep-interp firstinterp firstcompchars)
	      ; *** the form is A. so it has only a nomep interp
			  (values t
			      (list (list (list
					(list (first firstcompchars) 'period) 'nomep)))))
		       (t (poss-sigla (rest tokelem) prevelem))))
	       (t (poss-sigla (rest tokelem) prevelem)))))))

; *******************************************************************
(defun has-true-sigla-interp (firstinterp basechars)
(declare (special *SIGLE-DICT*))
(and
; *** this is the only component
(= 1 (length firstinterp))
; *** and it appears in the dictionary of the SIGLA's
(or (not (null (get (implode basechars) *SIGLE-DICT*)))
    (not (null (get (base-uppercase (implode basechars)) *SIGLE-DICT*))))))

; *******************************************************************
(defun has-roman-number-interp (firstinterp tulechars)
(and
; *** this is the only component
(= 1 (length firstinterp))
; *** it is 2 chars long
(= (length tulechars) 2)
; *** the second char is a period
(eq 'period (second tulechars))
(member (first tulechars) 
	'(up-i up-v up-x up-l up-c up-d up-m))))

; *******************************************************************
(defun has-short-nomep-interp (firstinterp tulechars)
(and
; *** this is the only component
(= 1 (length firstinterp))
; *** it is 2 chars long
(= (length tulechars) 2)
; *** the second char is a period
(eq 'period (second tulechars))
; *** the first char is an initial capital letter
(member (first tulechars) (get-charset-value 'caplet))))

; *******************************************************************
; *** this tries to establish if the a token element is a section identifier name
;     (as "chapter", "section", ...)
(defun is-section-identifier (prevelem)
(let (found basechars)
(do ((nxt-tok-interp (first prevelem) (first prevelem))
  (prevelem (rest prevelem) (rest prevelem)))
 ((or found (null nxt-tok-interp)) found)
 (setq basechars
       (convert-tule-char-names-to-base-codes (first (first nxt-tok-interp))))
 (cond ((and (eq (length nxt-tok-interp) 1)
	     (has-gramm-type (implode basechars) '&section))
	   (setq found t))))))

; *******************************************************************
;     Morphological analysis of a sequence of words given via the keyboard
; *******************************************************************
; *** It reads the words (via 'tokenize') and carries out the morphological/
;     lexical analysis (by accessing the dictionary). The output is a
;     sequence of "morphological entries", possibly ambiguous
(defun ana-text ()
  (declare (special *SYSTEM-CONTEXT*))
  (cond ((or (eq *SYSTEM-CONTEXT* 'tule) (eq *SYSTEM-CONTEXT* 'legal))
          (let (sent-tokens sent-lex-items element phrase morpholex-result
                flag scan-element (linecount 0) rem-line)
          (declare (special element flag scan-element linecount rem-line
                            *TREE-FORMAT*))
; *** linecount used in tokenize
    ; -------------------------INPUT FROM KEYBOARD ---------------------
              (setq *TREE-FORMAT* 'avm)
              (format t "~% Write the words to analyze. Finish with a ! ~%")
; *** the input from keyboard is tokenized (the final exclamative mark is
;     interpreted as end of sentence) and the result is sent to "analyser"
;     (which puts the final result in 'sent-lex-items')
;     a possibly modified (disambiguated) tokenizer output is stored in sent-tokens
              (setq phrase (second (tokenize *terminal-io*)))
	   ; *** the 'rest' excludes the final exclamative mark
              (multiple-value-setq (sent-lex-items sent-tokens) (analyser phrase))
              (setq sent-lex-items (butlast sent-lex-items))
              (setq morpholex-result (outpprint sent-lex-items sent-tokens))
              (format t "~% Result of the morphological analysis: ~% ~a ~%"
                         morpholex-result)))
        (t (exception 'generic-error
               "PROC/top-level-fun: ana-text used in the HOPS context"))))

; *******************************************************************
; *** . lex-items is a list of items (lemmata) resulting from the morpho-lexical analysis
;     . tokens is a list of input tokens (resulting from the tokenizer)
;     They are assumed to be parallel lists
; *** it simply re-organizes the data, imploding the input token (so that it
;     becomes readable) and building a single list of pairs  <inp-word lex-result>.
;     It also takes care of possible errors in the morpholexical analysis
(defun outpprint (lex-items tokens)
   (declare (special segni))
   (let (inpword outbuff)
      (do ((word-to-print (first lex-items) (first lex-items))
           (lex-items (rest lex-items) (rest lex-items))
           (nextscan (first tokens) (first tokens))
           (tokens (rest tokens) (rest tokens)))
         ((null word-to-print) (nreverse outbuff))
; *** unknown form ***************
; *** in such a case, the morpholexical analyser returns a string instead of a
;     list, but the string must be either "Does not exist" or "Does not exist;
;     maybe a proper name"
(cond ((not (listp word-to-print))
       (cond ((not (or (string-equal word-to-print "Does not exist")
		       (string-equal word-to-print
			       "Does not exist; maybe a proper name")))
	       nil))
       (setq inpword (first (first (first nextscan))))
; *** nextscan has the structure [[[[112 123 102] GW]]]
;     so, inpword is [112 123 102]
;     The new element is added to outbuff (which is the reversed sentence)
       (cond ((atom (first inpword))
	       (setq outbuff 
		 (cons (list (implode inpword) word-to-print) outbuff)))
	     (t 
; *** if inpword has not the form shown above, there are problems, but continue
	       (setq outbuff
		 (cons (list (implode (second inpword)) word-to-print)
		       outbuff)))))
; *** not an unknown form: standard output on outbuff
; *** some levels of parentheses are removed for enhancing readability
    (t (setq word-to-print (delete-parent (first word-to-print)))
       (setq inpword (first (first (first nextscan))))
       (setq outbuff (cons (list (implode inpword) word-to-print)
						    outbuff)))))))
	   
;*****************************************************************
; *** the input has 5 levels of parentheses. They are 5, and not 6, since we
;     are working on the single tokenizer element (and a level should have been
;     removed by "split-elements" in "analyser")
;     Moreover, the first of the remaining levels refers to compound tokenizer
;     element, which sould not exist any more
(defun delete-parent (wrd)
(cond ((not (null (rest wrd)))
   ;(throw 'morpho-error
   ;   (list "In PROC: top-level-fun; compound tokenizer element"))
   nil))
(setq wrd (first wrd))
; *** the second level refers to possible lexical ambiguities; they may exist,
;     but each of them requires the removal of a level of parentheses
(mapcar #'single-del-par wrd))

; *******************************************************************
(defun single-del-par (interp)
; *** the next level concerns compound word (ex. 'prendilo')
;     If the word is simple, a level of parentheses is removed, then the next
;     level is checked, and if it too is not needed it is removed. This
;     level should serve only for single-component ambiguities, as in
;     porci --> ((porre ...) ((ci1 ...) (ci2 ...)))
;     so, it cannot appear in simple words
(cond ((null (rest interp))
  (cond ((not (null (rest (first interp))))
	  (exception 'morpho-error
		     "PROC/top-level-fun; single-component ambiguity" interp))
	(t (first (first interp)))))
; *** compound word
(t (mapcar #'local-del-par interp))))

; *******************************************************************
; *** final level
(defun local-del-par (elem)
(cond ((null (rest elem)) (first elem))
(t elem)))

;******************************************************************
; *** it gets as input a lexical output (result of lexicon access) and
;     a lexical input (tokenizer output) and checks if there are
;     unrecognized parts. In such a case, introduces the suitable error
;     markers
; *** The inputs have the form:
;     el --> (el1 el2 ... eln),
;          where el are "components" of the lexical output, i.e. lexical
;          interpretations of a word. In fact, the name (el) notwithstanding,
;          the input has the form of a whole sentence
;     sc-el --> (sc1 sc2 ... scn),
;          where sc are "components" of the tokenizer output
; *** the result is a list of two elements:
;     (el' sc-el'), which are the possibly modified inputs
(defun test-incompl (el sc-el)
(declare (special *TREE-FORMAT* *SYSTEM-CONTEXT*))
(let* (rest-el
 (first-el-word (first el))
 (first-scan-word (first sc-el))
 (first-scan-interp (first first-scan-word))
 (first-comp-f-sc-int (first first-scan-interp))
 (chars-f-c-f-sc-int (first first-comp-f-sc-int))
 (tule-f-c-f-sc-int (convert-base-codes-to-tule-char-names chars-f-c-f-sc-int)))
; *** first-scan-interp is the set of lexical intrpretations associated
;     with the first tokenizer output
(cond ((null el) nil)
    ((equal first-el-word '((nil)))
; *** the word has not been found;
      (cond ((eq *TREE-FORMAT* 'avm)
; *** if the input is from the keyboard, then we are either checking a
;     the lexical access on a set of words, or we are interpreting a
;     sentence. In such a case ask the user to retype the input, unless
;     it is a possible proper name
; *** actually, in case of morpholexical analysis, this should not
;     be done!!!!!!
	      (setq rest-el (test-incompl (rest el) (rest sc-el)))
	      (cond ((and (or (member (first tule-f-c-f-sc-int)
				      (get-charset-value 'minlet))
			      (= (length tule-f-c-f-sc-int) 1))
			  (neq *SYSTEM-CONTEXT* 'buildnames))
			      (let ((newword
				     (ask-user-unknown-word
				       (implode chars-f-c-f-sc-int))))
				   (list (cons (first newword)
					       (first rest-el))
					 (cons (second newword) 
					       (second rest-el)))))
		    (t (list (cons "Does not exist; maybe a proper name"
				   (first rest-el))
			     (cons first-scan-word (second rest-el))))))
; *** if the input is from file, then the "unknown word" marker is
;     inserted in the output file (taking into account the possibility
;     of proper names)
	    ((eq *TREE-FORMAT* 'tut)
	      (setq rest-el (test-incompl (rest el) (rest sc-el)))
	      (cond ((or (member (first tule-f-c-f-sc-int)
				      (get-charset-value 'minlet))
			  (= (length tule-f-c-f-sc-int) 1))
		      (list (cons "Does not exist" (first rest-el))
			    (cons (first sc-el) (second rest-el))))
		    (t (list (cons "Does not exist; maybe a proper name"
				(first rest-el))
			    (cons (first sc-el) (second rest-el))))))))
; *** the word is ok, go ahead
    (t (setq rest-el (test-incompl (rest el) (rest sc-el)))
       (list (cons first-el-word (first rest-el))
	     (cons first-scan-word (second rest-el)))))))

;******************************************************************
; *** it asks the user to repeat an unknown word
(defun ask-user-unknown-word (word)
(declare (special *LANGUAGE* *SYSTEM-CONTEXT*))
(let (newword newtoken lexresult)
(cond ((eq *SYSTEM-CONTEXT* 'hops-dial)
  (cond ((eq *LANGUAGE* 'italian)
	  (format t 
	     "Mi dispiace, non conosco la parola ~a; puo' riscriverla? --> " word))
	((eq *LANGUAGE* 'english)
	  (format t 
	     "I'm sorry, I don't know the word ~a; can you retype it? --> " word))
	((eq *LANGUAGE* 'spanish)
	  (format t 
	     "(this should be translated in Spanish) ~%I'm sorry, I don't know the word ~a; can you retype it? --> " word))
	((eq *LANGUAGE* 'french)
	   (format t 
	     "(this should be translated in French) ~%I'm sorry, I don't know the word ~a; can you retype it? --> " word)))
   (setq newword (read-line))
   (cond ((null newword)
	    (break "Sorry, I cannot work on a null input"))
	 (t (setq newtoken (tokenize newword))
	    (multiple-value-setq (lexresult newtoken)
		    (analyser_elem (first (second newtoken))))
; *** the first of newtoken is s-eof, i.e. the marker of end of sentence
	    (cond ((not (equal lexresult '((nil))))
		     (list lexresult newtoken))
		  (t (break "top-level-fun: ask-user-unknown-word"))))))
((eq *SYSTEM-CONTEXT* 'flat-analysis)
   nil)
(t (exception 'morpho-error
	  "PROC/top-level-fun: unknown word" word)))))

;****************************************************************
; *** parseres has the form:
;     (words parselinks)
;     each parselink (i.e. the pointer to the parent) must be associated
;     to its word
(defun mergeresult (parseres)
   (let ((sentences (first parseres))
         (links (second parseres)) 
         result)
         (do ((words (first sentences) (first sentences))
              (sentences (rest sentences) (rest sentences))
              (wlinks (first links) (first links))
              (links (rest links) (rest links)))
             ((null words) (reverse result))
             (setq result (cons (mapcar #'word-merge-res words wlinks) result)))))

;****************************************************************
(defun word-merge-res (word link)
  (declare (special *CHAR-SET-ID*))
  (let (newword syntinfo newsyntinfo)
; *** word is an avm, while link is a pair <pointer, label>
    (cond ((eq *CHAR-SET-ID* 'utf-8)
            (setq newword word)
            ;(let ((lemma (convert-currlisp-atom-or-string-to-base 
            ;                  (get-synt-word word))))
            ;   (setq syntinfo (first (leggi word 'syn)))
            ;   (setq newsyntinfo (replace-lemma syntinfo lemma))
            ;   (setq newword 
            ;      (dropnil
            ;         (list
            ;            (assoc 'posit word)		; posit
            ;            (assoc 'form word)		; form
            ;            (list 'syn newsyntinfo)		; syntinfo
            ;            (assoc 'sem word)		; seminfo
            ;            (assoc 'coref word)))))	; coreference of traces
                )
          (t (setq newword word)))
    (append1 newword 
          `(tree ((parent ,(first link)) (label ,(second link)))))))

;****************************************************************
(defun replace-lemma (syntinfo newlemma)
  (cond ((null syntinfo) (exception 'generic-error "No lemma in word-merge-res"))
        ((eq (first (first syntinfo)) 'lemma)
           (cons (list 'lemma newlemma) (rest syntinfo)))
        (t (cons (first syntinfo) (replace-lemma (rest-syntinfo) newlemma)))))

;##################################################################
; *** Analysis of a sequence of words + POS tagging
;##################################################################

(defun hops-ana-text+tag (input-string)
(let (sent-tokens sent-lex-items element morpholex-result
      flag graf inpt scan-element next-phrase postag-result
      rem-line siglinterp in-parenth inside-par tagresult sent-contin
      end-sent sigla-continue rest-sent)
(declare (special element flag graf inpt scan-element rem-line *ANALYS-CONTEXT*))
; *** the "tokenize" function, used in the do* below, analyses the input
;     string, by means of the tokenizer automaton, and returns the tokenized
;     data until the first sentence separator.
; *** In case more that one sentence appear in the line, the tokenizer uses,
;     for going on, the contents of the global variable rem-line. So,
;     the input string is used only the first time, while all subsequent
;     sentences are found in rem-line.
(setq rem-line nil)
(do* ((one-phrase (tokenize input-string) (tokenize ""))
      (tok-data (second one-phrase) (second one-phrase))
      (stop nil (first one-phrase)))
     ((and (eq stop 's-eof) (null (second one-phrase)))
         (reverse tagresult))
; ****************************
;     THE NEXT PART TO DECIDE IF TWO CANDIDATE SENTENCES HAVE TO BE TREATED AS A
;     SINGLE ONE
; ****************************
; *** one-phrase contains 's-eos plus all tokens found until the next possible
;     sentence endings by the tokenizer, plus a possible remaining part of the
;     input string 
; *** if the recognized end of sentence is not a possible sigla, then analyze the
;     contents of next-phrase. Otherwise do nothing, so that tokenize returns
;     another piece of sentence, which is appended to next-phrase.
; *** This anticipates the analysis of siglas, and assumes that any element
;     tokenize-ambiguous between a sigla and something else is in fact a sigla
;     (so that the sentence continues)
; *** the second condition for not advancing refers to initial 'dotted' numbers
;     (ex. chapter numbers in books), which are kept together with the title of
;     the chapter.
     (multiple-value-setq 
          (sigla-continue siglinterp)
          (poss-sigla (ult tok-data) (second (reverse tok-data))))
; *** inside-par says if the last parenthesis in one-phrase is closed ('closed),
;     open ('open), or there is no parenthesis (nil)
; *** in-parenth refers to the situation concerning also the previous phrases:
;     it is t if we are inside a parenthesis, nil otherwise
     (setq inside-par (inside-a-parenth tok-data))
    ; (format t "hops-ana-text+tag. Input string ~a~%rem-line: ~a~%" input-string rem-line)
    ; (break "")
     (cond ((and (not (null siglinterp))
	         sigla-continue)
; *** first, only the rest of one-phrase is taken (the end-code, s-eos or s-eof,
;     is eliminated); then it is reversed (reverse); then the first element is
;     eliminated (the former last element, which has a sigla interpretation), so
;     that it is subsituted (cons) with the sigla interpretation (disregarding
;     the other possibilities). Finally, the result is reversed to get the
;     original order
	     (setq sent-contin t)
	     (setq next-phrase
	          (reverse 
		      (cons siglinterp ; (mapcar #'list siglinterp)
			    (rest (reverse tok-data)))))
	     (cond ((eq inside-par 'open)
		      (setq in-parenth t))
	           ((eq inside-par 'closed)
		      (setq in-parenth nil))))
; *** the second condition for not advancing refers to initial 'dotted' numbers
;     (ex. chapter numbers in books), which are kept together with the title of
;     the chapter.
           ((and (= 2 (length tok-data))
	         (scan-number-ambig? tok-data))
	     (setq sent-contin t)
	     (setq next-phrase (choose-numb+period-interp tok-data)))
; *** this third condition for not advancing refers to being inside a parenthesis
;     Although this does can be questionable, I do not accept breaking a sentence
;     inside a parenthesis (this could cause problems in case of a typing error,
;     so that I set the escape condition that the resulting sentence cannot be
;     longer than 300 words).
           ((or (and in-parenth
		     (neq 's-eof end-sent)
		     (neq 'closed inside-par)
		     (not (> (+ (length next-phrase) (length tok-data)) 300)))
		(and (neq 's-eof (first one-phrase))
		     (eq 'open inside-par)
		     (not (> (+ (length next-phrase) (length tok-data)) 300))))
             (setq sent-contin t)
	     (setq in-parenth t)
	     (setq next-phrase tok-data)))
; *** in all other cases, the sentence is considered as terminated, so that the
;     analysis is started up
; *** the next cond selects the integer+punct interpretation in case of
;     ambiguities such as '312.'. This is the same choice as in the previous
;     branch of the upper 'cond'. However, here the choice is conditioned to
;     being in the analysis of a standard text (and not, say, a formula).
;     Actually, 'text' is the only value currently used for the global
;     *ANALYS-CONTEXT*. Moreover, in this case, the sentence is considered as
;     terminated.
    (cond ((and sent-contin
                (not (null rest-sent)))
             (setq one-phrase (tokenize rest-sent))
	     (setq next-phrase (append next-phrase (second one-phrase))))
 	  (t (setq next-phrase tok-data)))
    ;(break "hops-ana-text+tag: end")
    (multiple-value-setq (sent-lex-items sent-tokens) (analyser next-phrase))
    (setq morpholex-result (outpprint sent-lex-items sent-tokens))
    (setq postag-result (postagger morpholex-result))
    (setq tagresult (cons postag-result tagresult)))))

;****************************************************************
(defun ask-file-name ()
  (format t "~% Name of the input file containing the raw text (without quotation marks)? ~%")
  (format t "     Home directory: *HOME-DIR*/DATI ~%")
  (build-subdir-file-name (read-line) "../DATI/"))


