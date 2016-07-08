
(in-package "USER")

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;       OVERALL STRATEGY
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;   1. Standard syntactic analysis (POS tagger + parser)
;      --- this step is carried out by various functions; the main function
;          basically depends on the way of interaction. Usually, it is
;          " "hops-ana-text+tag"
;          "parse-sentences", and "mergeresult". The three functions are
;          called in "start-hops" (file "main"); "hops-ana-text+tag" and
;          mergeresult are defined in the file "main"; "parse-sentences"
;          is defined in the file "chunk-parser".
;      --- output: the syntactic tree, having the following structure:
;          (line1 line2 ... lineN)
;          Where each line contains the info on a word (or syntactic
;          component of a word, e.g. an enclitic):
;          ((posit P) (form F) (syn SY) (sem SE) (coref C) (tree T))
;          Where, in turn:
;          ++ P is an integer or a pair of integers (subcomponent)
;          ++ F is the input form (keeping capitalization)
;          ++ SY is a structure of lexical-syntactic information, including,
;             among others, the lemma, the syntactic category, the syntactic
;             subtype, gender, number, tense, etc.
;          ++ SE is a list including semantic features. Now, it's usually
;             empty, except for some particles (e.g. some conjunctions and
;             adverbs) that get some semantic information from the lexicon
;          ++ C is the coreference index, currently appearing just for
;             traces. Its value may be EMPTY, or a pair ((line LI) (ctype CT))
;             where LI is the "posit" of the referent, while CT is the
;             type of reference (full, partial, or word)
;          ++ T is a pair: ((parent PA) (label L)), where PA is the line
;             number (see P) above identifying the line of the parent, and
;             L is the label of the arc connecting this line to the parent
;       !! All data can be extracted from this representation by using the
;       !! get-flatavm-XXX functions appearing in the "tb-functions" file.
;       !! for instance, get-flatavm-numb returns the value of the feature "posit"
;       !! of a line, while get-flatavm-categ returns the value of the feature
;       !! categ of the feature syn of a line. These functions are applied
;       !! to a single line
;       ??? Note that the names of the functions are not very readable (e.g.
;       ??? "numb" instead of "posit") in order to keep some backward reference
;       ??? to the get-newtb-XXX functions used in the full system for text
;       ??? analysis
;             
;   2. Reorganization of the parse tree, which is converted into a standard
;      tree format.
;      --- this step is carried out by the function "reshuffle-tree", called
;          and defined in the file "main"
;      --- output: the syntactic tree, having the following structure:
;          ((head HEADINFO) (dependents (subtree1 subtree2 ... subtreeN)))
;          Each subtree is, in its turn, a tree, but it could also be the
;          special form (#\#), which simply marks the position of the head
;          within the sequence of dependents
;          HEADINFO contains all relevant data about the head. They are:
;          ((form F) (position P) (syn SY) (coref C) (link L) (sem SE))
;          No new information is added in this step, so that all data
;          are the same as in the previous format. Note that the "position"
;          information is now less relevant, but it has been kept for
;          coreference purposes.          
;       !! All data can be extracted from this representation by using the
;       !! get-actavm-XXX functions appearing in this file.
;       !! For instance, get-actavm-head returns the value of the feature "head"
;       !! (see HEADINFO above), get-actavm-headsyn return the value of the
;       !! feature "syn" of the feature "head", and get-actavm-headcateg
;       !! returns the value of the feature "cat" of the feature "syn" of
;       !! the feature "head"

;   3. Actual semantic interpretation.
;          --- this step is carried out by the function "semantic-interpretation",
;              called by "start-hops" (file "main") and is split into three substeps:
;      3.1 Annotate the tree with semantic information. This aims at filling
;          the "sem" features of the tree. The information is obtained from
;          the lexicon (actually, the file KB/word-meaning.dat) for standard
;          words (e.g. noun and verbs) and from the context for deictic
;          pronouns (I, you) and for some traces coming from pro-drop.
;          --- this step is carried out by the function "add-actavm-headmeaning",
;              called by the function "annotate" (defined in this file); "annotate"
;              is called by "semantic-interpretation" and is defined in this file
;          --- output: the tree is the same as before, but the "sem" feature
;              should have a value for each node (except for coreference traces
;              which get their value in the next substep)
;      3.2 Solve coreferences. This applies to traces coming from sharing
;          (e.g. equi), and involves, for each trace, a search in the whole
;          tree for its referent. 
;          --- this step is carried out by the function "solve-coreferences", also
;              called by "annotate".
;          --- output: see above
;      3.3 If needed, insertion, at the top of the tree, of some extra nodes.
;          this is done for cases where the root is a noun (N), so that the
;          sentence should be interpreted as "I want to know some information
;          about N"
;          --- this step is carried out inside the function "annotate".
;          --- output: as above, but with some possible extra nodes
;      3.4 Construction of the actual "ontologic" query. This involves the
;          access to the ontology and to some sort of default info, in order
;          to express, in terms of a path in the ontology, what need be found.
;          --- this step is carried out by the function "build-sem-query", called
;              by "semantic-interpretation" and defined in the file "buildquery"
;          --- output: an ontologic query, having the form:
;              SELECT path1 FROM node WHERE pathtree
;       !! Apart from the final query, all data can be extracted from the various
;       !! annotated trees using the same functions (get-actavm-XXX) mentioned above.
;
; *** To summarize:
;     start-hops [0] ("main")
;      |--- hops-ana-text+tag [1] ("main")
;      |--- parse-sentences [1] ("chunk-parser")
;      |--- mergeresult [1] ("main")
;      |--- reshuffle-tree [2] ("main")
;      |--- semantic-intepretation [3] ("seminterp")
;            |--- annotate [3.3] ("seminterp")
;                  |--- add-actavm-headmeaning [3.1] ("seminterp")
;                  |--- solve-coreferences [3.2] ("seminterp")
;            |--- build-sem-query [3.4] ("buildquery")

; ***************************************************************************
(defun semantic-interpretation (synt-trees &optional fname)
  (break "entering semantic interpretation")
  (cond ((stringp synt-trees)
      ; *** the input is a string including the s-expression of one or more
      ;     trees in avm format. If this type of input is used, one extra
      ;     level of parenthesis is required, since the input is a set of trees
           (seminterp-from-string synt-trees))
        ((eq synt-trees 'f)
      ; *** input ad output on files
           (file-sem-interp (change-extens fname ".avm")
                           (change-extens fname ".svm")
                           (change-extens fname ".sem")
                           (change-extens fname ".ssh")		; semantic short
                           (change-extens fname ".fol")))
        (t (mapcar #'(lambda (x) (singsent-sem-interp x)) synt-trees))))

; ***************************************************************************
; *** it is assumed that the input file contains
;     - sentence heading 1
;     - full avm-tree 1
;     - sentence heading 2
;     - full avm-tree 2
;     avm-trees are lisp expressions
(defun file-sem-interp (inpavm simpleavm outsem outsemshort outfol)
  (declare (special *PRINT-LEV* *ANAPHORIC-CONTEXT* *SYSTEM-CONTEXT* *TURN-TO-TALK*))
  (let (nxttree ontorepr ontoshort fol-repr (*full-tree* nil))
    (declare (special *full-tree*))
    (with-open-file (avmport inpavm :direction :input
                                    :external-format :utf8
                                    :if-does-not-exist :error)
     (with-open-file (simpleavmport simpleavm :direction :output
                                    :if-exists :overwrite
                                    :if-does-not-exist :create
                                    :external-format :utf8)
       (with-open-file (semport outsem :direction :output
                                      :if-exists :overwrite
                                      :if-does-not-exist :create
                                      :external-format :utf8)
        (with-open-file (semshport outsemshort :direction :output
                                      :if-exists :overwrite
                                      :if-does-not-exist :create
                                      :external-format :utf8)
         (with-open-file (folport outfol :direction :output
                                       :if-exists :overwrite
                                       :if-does-not-exist :create
                                       :external-format :utf8)
    (do ((nxtheading (read-line avmport nil #\Escape) (read-line avmport nil #\Escape)))
        ((equal nxtheading #\Escape))
        (cond ((string= (string-trim '(#\Space #\Tab #\Return) nxtheading) "") 
                 (format semport "~a~%" nxtheading)
                 (format semshport "~a~%" nxtheading)
                 (format folport "~a~%" nxtheading))
              ((is-sentence-heading nxtheading nil)
                 (format semport "~a~%" nxtheading)
                 (format semshport "~a~%" nxtheading)
                 (format folport "~a~%" nxtheading)
                    (setq nxttree (read avmport))
                    (setq *full-tree* (annotate nxttree))
                 (print-actavm-readable *full-tree* simpleavmport)
                    (setq ontorepr (build-sem-query *full-tree*))
                 (format semport "~a~%" ontorepr)
                    (setq ontoshort (simplify-onto-repr ontorepr))
                 (format semshport "~a~%" ontoshort)
                    (setq fol-repr (sem-to-fol-translation ontoshort))
                 (fol-print-pretty fol-repr folport)
                 )
              (t (exception 'semantic-error "Wrong data in the .avm file")))))))))))


; ***************************************************************************
; *** this takes an input string that may contain one or more avm trees, 
;     and produces a list of semantic interpretations (not a string)
; >>>>>>>> Used as main communication channel in atlas
(defun seminterp-from-string (string-synt-trees)
  (declare (special *PRINT-LEV*))
  (setq *PRINT-LEV* 0)
  (let ((synt-trees (read-from-string string-synt-trees)) fol-forms)
     (do ((nxttree (first synt-trees) (first synt-trees))
          (synt-trees (rest synt-trees) (rest synt-trees)))
         ((null nxttree) fol-forms)
         (setq fol-forms (append1 fol-forms (singsent-sem-interp nxttree))))))

; ***************************************************************************
(defun singsent-sem-interp (synt-tree &optional file-outp)
  (declare (special *PRINT-LEV* *ANAPHORIC-CONTEXT* *SYSTEM-CONTEXT*))
; *** 
  (let ((*full-tree* nil) ontorepr ontoshort fol-repr)
    (declare (special *full-tree*))
; *************** ANNOTATION *********************************************
     (setq *full-tree* (annotate synt-tree))
     (cond ((and (memq *SYSTEM-CONTEXT* '(tule tocai atlas))
                 (>= *PRINT-LEV* 3))
              (print-actavm-readable *full-tree* t)))
     (cond ((not (null file-outp))
             (with-open-file (outavmport (build-file-name (change-extens file-outp ".svm"))
                                :direction :output :if-exists :overwrite 
                                :if-does-not-exist :create)
                    (print-actavm-readable *full-tree* outavmport))))
                             ; *** print-actavm-readable in PARSER-PROC-ALL/avm-transf
; ******** ONTOLOGICAL INTERPRETATION ************************************
     (setq ontorepr (build-sem-query *full-tree*))	
     (setq ontoshort (simplify-onto-repr ontorepr))
     (cond ((and (memq *SYSTEM-CONTEXT* '(tule tocai atlas))
                 (>= *PRINT-LEV* 2))
              (format t "~a~%" ontoshort)))
     (cond ((not (null file-outp))
             (with-open-file (outsemport (build-file-name (change-extens file-outp ".sem"))
                                :direction :output :if-exists :overwrite 
                                :if-does-not-exist :create)
                    (format outsemport "~a~%" ontorepr))
             (with-open-file (outsshport (build-file-name (change-extens file-outp ".ssh"))
                                :direction :output :if-exists :overwrite 
                                :if-does-not-exist :create)
                    (format outsshport "~a~%" ontoshort))))
; ******** TRANSLATION INTO FOL ******************************************
     (setq fol-repr (sem-to-fol-translation ontoshort))
     (cond ((and (memq *SYSTEM-CONTEXT* '(tule tocai atlas))
                 (>= *PRINT-LEV* 1))
              (fol-print-pretty fol-repr t)))
     (cond ((not (null file-outp))
             (with-open-file (outfolport (build-file-name (change-extens file-outp ".fol"))
                                :direction :output :if-exists :overwrite 
                                :if-does-not-exist :create)
                 (fol-print-pretty fol-repr outfolport))))
; ******** Maybe the anaphoric context can be useful in the future *******
     (setq *ANAPHORIC-CONTEXT* 
          (cons (list '§speaker *full-tree* ontorepr)
                 *ANAPHORIC-CONTEXT*))
     fol-repr))

; ***************************************************************************
(defun annotate (synt-tree)
 (declare (special *DIALOGUE-CONTEXT* *ALL-DIAL-CONTEXTS* *SYSTEM-CONTEXT*
                   *TURN-TO-TALK*))
 (let* ((temp-annot-tree (add-actavm-headmeaning synt-tree nil))
            ; *** the first parameter is used for recursion, the second will get the
            ;     tree one level up with respect to the current one
        (annotated-tree (solve-coreferences temp-annot-tree temp-annot-tree nil))
            ; *** the first parameter is used for recursion, the second keeps the full tree,
            ;     for searching the referents, the third will keep the parent for possible
            ;     semantic checks
        (headcateg (get-actavm-headcateg annotated-tree))
        (headmeaning (get-actavm-headlexmean annotated-tree))
        obj-sentobj)
    (cond ((memq headcateg '(ART PRON NUM))
             (add-givinfo annotated-tree *TURN-TO-TALK*))
          ((eq 'NOUN headcateg)
            (cond ((eq '££information headmeaning)
                      (add-want-obj annotated-tree))
                  ((eq *SYSTEM-CONTEXT* 'tocai)
                     (add-getinfo annotated-tree))
                  (t (add-givinfo annotated-tree *TURN-TO-TALK*))))
          ((eq 'VERB headcateg)
            (let ((person (find-verb-person-or-number annotated-tree 'person))
                  (number (find-verb-person-or-number annotated-tree 'number)))
                (cond ((eq 1 person)
                        (cond ((eq *TURN-TO-TALK* '§speaker)
   ; *** if the verb is in first person, I assume the sentence expresses
   ;     a thing the user wants to know.
   ; *** "Vorrei sapere quali concerti ...."
   ;     "Voglio vedere un film ..."
   ;     "Vorrei un'informazione"
   ;     "Vorrei delle informazioni su ..."
   ;     "Vorrei il programma di ..."
   ;     "Quali spettacoli posso trovare ..."
   ;     "Posso sapere ..."
                                (cond ((eq '££want headmeaning)
                 ; *** "Vorrei sapere quali concerti ...."
                 ;     "Voglio vedere un film ..."
                 ;     "Vorrei un'informazione"
                 ;     "Vorrei delle informazioni su ..."
                                        (let ((sentobj (get-sentential-object annotated-tree))
                                               adjoin-label-up adjoin-label-down adjoin-path 
                                               attach-path)
                                            (cond ((null sentobj)
                         ; *** "Voglio un'informazione ..."
                         ;     in this case, the tree is left unchanges
                                                      annotated-tree)
                                                  ((eq '££know (get-actavm-headlexmean sentobj))
                         ; *** "Vorrei sapere quali concerti ...." 
                         ;     the tree is left unchanged
                                                     annotated-tree)
                                                  ((eq '££obtain (get-actavm-ext-headlexmean sentobj))
                         ; *** I would like to get ...
                                                    (setq obj-sentobj
                                                        (find-actavm-dep 'VERB-OBJ sentobj))
					            (cond ((null obj-sentobj)
                                                             (exception 'semantic-error
                                                               "PROC/seminterp: no obj for get in I would like to get"))
                                                          ((eq '££information
                                                              (get-actavm-ext-headlexmean obj-sentobj))
                                        ; *** I would like to get information ...
                                        ;     The tree is left unchanged
                                                             nil)
                                                          (t (exception 'semantic-error
                                                                 "PROC/seminterp: the obj for get in I would like to get is not 'information'"))))
                                                  (t (cond ((eq 'mod 
                                                               (get-actavm-headtype annotated-tree))
                                 ; *** the verb associated with ££want is a modal
                                 ;     e.g. Italian "vorrei andare ..."
                                                             (setq adjoin-label-up 
                                                                   'verb+modal-indcompl)
                                                             (setq adjoin-label-down 'verb-obj)
                                                             (setq adjoin-path 
                                                                   '(verb+modal-indcompl))
                                                             (setq attach-path 
                                                                   '(verb+modal-indcompl 
                                                                     verb-obj)))
                                 ; *** the verb associated with ££want is not a modal
                                 ;     e.g. English "I want to go ..."
                                                           (t (setq adjoin-label-up 'prep-arg)
                                                             (setq adjoin-label-down 'verb-obj)
                                                             (setq adjoin-path 
                                                                   '(verb-obj prep-arg))
                                                             (setq attach-path 
                                                                   '(verb-obj prep-arg 
                                                                     verb-obj))))
                                                     (setq annotated-tree
                         ; *** "Voglio vedere un film ..."
                         ;     in this case, I assume it may be interpreted as 
                         ;     I want [to know where] see a movie ...
                                                          (adjoin-subtree annotated-tree
                                                             `(,adjoin-path
                                                               (££know
                                                                  ,adjoin-label-up
                                                                   ((#\#)
                                                                    (§speaker verb-subj
                                                                      ((#\#)))
                                                                    (*)))
                                                               ,adjoin-label-down)))
                                                     (attach-subtree annotated-tree
                                                           `(,attach-path
                                                              (££event
                                                               advb+interr-rmod ((#\#)))
                                                            ))))))
                                      ((eq '££can headmeaning)
               ; *** the main verb is "can"
               ;     "Quali spettacoli posso trovare ..."
               ; *** this is expressed by inserting in the tree lexically empty
               ;     elements the can be interpreted as "I want to know"
                                         (let ((sentobj (get-sentential-object annotated-tree)))
                                             (cond ((eq '££get-info
                                                        (get-actavm-ext-headlexmean sentobj))
                     ; *** "Can I speak to somebody about ..."
                                                      annotated-tree)
                                                   (t (add-want-know annotated-tree)))))
                                      ((eq '££know headmeaning)
               ; *** the main verb is "cercare"
               ;     "Sto cercando ..."
               ; *** this is expressed by inserting in the tree lexically empty
               ;     elements the can be interpreted as "I want"
                                          (add-want annotated-tree))
                                      ((eq '££to-be headmeaning)
               ; *** the main verb is "to-be":
               ;     "I am interested in XXX"
               ; *** this is expressed as "I'm getting info about XXX"
                                        (let ((predcompl
                                                (find-actavm-dep 'VERB-PREDCOMPL+SUBJ annotated-tree))
                                              predmean)
                                            (cond ((null predcompl)
               ; *** there is no predicative complement. Try with a location (I'm in Rome Street)
               ;     The search is made by checking if there is an RMOD whose head has as
               ;     meaning "--in-relation" or "--on-relation"
                                                     (setq predcompl
                                                          (find-actavm-dep 'RMOD annotated-tree))
                                                     (cond ((null predcompl) annotated-tree)
                                                           (t (setq predmean
                                                                 (get-actavm-headlexmean predcompl))
                                                              (cond ((member predmean
                                                                        '(--in-relation --on-relation))
                                                                       (add-givinfo annotated-tree *TURN-TO-TALK*))
                                                                    (t annotated-tree)))))
                                                  ((eq '£interested
               ; *** the actual argument of "to be" actually is "interested"
                                                           (get-actavm-headlexmean predcompl))
                                                    (let ((preposit
                                                            (find-actavm-dep 'ADJC-ARG predcompl)))
                                                        (cond ((not (null preposit))
                    ; *** "interested" really has an adjectival argument (interested in)
                                                                 (add-givinfo preposit *TURN-TO-TALK*))
                                                              (t (exception 'semantic-error
                                                                    "PROC/seminterp: 'interested' without specification")))))
               ; *** the actual argument of "to be" is not "interested"
                                                  (t annotated-tree))))
               ; *** the top meaning is none of ££want, ££can, ££know, ££to-be
               ; *** "Ho visto ...": annotated-tree is left unchanged
                                      (t annotated-tree)))
   ; ---------- The sentence is in the first person ----------------
   ;            Here closes the case where the *TURN-TO-TALK* is the user
   ;            From now on the *TURN-TO-TALK* is of the system (§myself)
   ;            This also applies to ATLAS, where it is the system that plays the role of
   ;            the meteo speaker
   ;            - headmeaning: the lexmean of the root verb
                              ((eq *TURN-TO-TALK* '§myself)
                                (cond ((eq number 'sing)
              ; *** I will show you, I greet you, ...
              ; ???????????? this case must be examined ???????????????????????????????
                                         annotated-tree)
                                      ((eq number 'pl)
                                        (cond ((eq headmeaning '££to-have)
                                                 (add-givinfo annotated-tree *TURN-TO-TALK*))))))
                              (t (break "seminterp: Who is speaking?"))))
              ; *** We see, we have
                      ((eq 2 person)
                        (cond ((and (eq 'imper (get-actavm-headmood annotated-tree))
                                    (one-is-subclass-of headmeaning '££system-operation))
                                 (add-want annotated-tree))
   ; *** if the verb is imperative, I assume the form "dimmi" or "puoi dirmi"
                              ((eq '££can headmeaning)   ; could you tell me
                                 annotated-tree)
                              ((is-sem-interrogative annotated-tree)   ; sem --> seminterp
                                 (add-want-know annotated-tree))
                              (t annotated-tree)))
                      ((eq 3 person)
                        (cond ((eq '££can headmeaning)
   ; *** Here, I assume a form of polite question (Puo' dirmi ...?)
                                 annotated-tree)
                              ((eq '££want headmeaning)
   ; *** Here, for Italian "mi servono", annotated as "I want"
                                 annotated-tree)
                              ((is-sem-interrogative annotated-tree)   ; sem --> seminterp
   ; *** It is a direct question if the sentence has a question mark or includes some
   ;     question element
   ;     "Dove è la biglietteria di Settembre musica?"
   ;     "Quali spettacoli ci sono domani sera a Torino?"
                                 (add-want-know annotated-tree))
   ; *** The next for "My address is ..."
                              (t (add-givinfo annotated-tree *TURN-TO-TALK*))))
                      ((and (eq 'infinite (get-actavm-headmood annotated-tree))
                            (one-is-subclass-of headmeaning '££system-operation))
                         (setq annotated-tree (add-want annotated-tree)))
                      (t (exception 'semantic-error
                                  "PROC/seminterp: Unknown 'person' value for a verb")))))
          ((memq headcateg '(PREP ADV PHRAS ADJ INTERJ))
              (add-givinfo annotated-tree *TURN-TO-TALK*))
          (t (exception 'semantic-error
                     "PROC/seminterp: the root of the tree is of a wrong category"
                      headcateg)))))

; *****************************************************************************
; *** this gets the verbal object subtree, in case the main verb is a modal
;     or has a verbal object
; *** It works only after the initial annotation, since it uses the meaning
;     of the top verb, in order to know if it is an English tense marker
(defun get-sentential-object (tree)
   (setq tree (skip-question-tense-marker tree))
   (cond ((or (eq 'MOD (get-actavm-headtype tree))
              (null (get-actavm-headsyn tree)))
    ; *** the second disjunct, because this is used also on the semantic
    ;     tree to get the sentential object of the dummy "££want" node, 
    ;     which is syntactically empty
           (find-actavm-dep 'VERB+MODAL-INDCOMPL tree))
         (t (let* ((verb-obj (find-actavm-dep 'VERB-OBJ tree))
                   (verb-obj-cat (get-actavm-headcateg verb-obj)))
               (cond ((eq 'PREP verb-obj-cat)
                       (let ((prep-arg (find-actavm-dep 'PREP-ARG verb-obj)))
                           (cond ((or (eq 'VERB (get-actavm-headcateg prep-arg))
                                      (null (get-actavm-headsyn prep-arg)))
          ; *** in the second case, it is a trace adjoined in the annotation phase
                                    prep-arg)
                                 (t nil))))
                     ((eq 'CONJ verb-obj-cat)
                       (let ((conj-arg (find-actavm-dep 'CONJ-ARG verb-obj)))
                           (cond ((eq 'VERB (get-actavm-headcateg conj-arg))
                                    conj-arg)
                                 (t nil))))
                     ((eq 'VERB verb-obj-cat)
                        verb-obj)
                     (t nil))))))

; *****************************************************************************
; *** checks if a line is the head of a noun complex
;     Returns the semantic head of the complex (e.g. the noun) or nil
;     Raises an exception if the determiner requires a depending noun complex
;     (as in case of articles). This means that this function can ony be used
;     after the chunk rules (that link the determiner to its dependents)
(defun is-a-actavm-noun-complex (tree)
  (let ((categ (get-actavm-headcateg tree))
        (type (get-actavm-headtype tree))
        label downtree)
     (cond ((eq categ 'NUM)
             (find-actavm-dep 'DET+QUANTIF-ARG tree))
           ((eq categ 'ART)
             (setq label
                   (cond ((eq type 'DEF) 'DET+DEF-ARG)
                         ((eq type 'INDEF) 'DET+INDEF-ARG)))
             (setq downtree (find-actavm-dep label tree))
             (cond ((null downtree)
                     (exception 'parse-error "PROC/chunk-parser: determiner not governing 1"))
                   (t downtree)))
           ((and (eq categ 'ADJ) (memq type '(DEITT INDEF DEMONS POSS INTERR)))
             (setq label (case type
                               (DEITT 'DET+INDEF-ARG)
                               (INDEF 'DET+QUANTIF-ARG)
                               (DEMONS 'DET+DEF-ARG)
                               (POSS 'DET+DEF-ARG)
                               (INTERR 'DET+INTERR-ARG)))
             (setq downtree (find-actavm-dep label tree))
             (cond ((null downtree)
                     (exception 'parse-error "PROC/chunk-parser: determiner not governing 2"))
                   (t downtree)))
           (t nil))))

; *****************************************************************************
; *** this gets the object subtree, in standard cases 
;     However, it also handles verbal locutions (as "aver bisogno"), by taking
;     the argument of the locution
; *** This covers only the case when the actual object is the argument of a 
;     preposition (as in "Aver bisogno di OBJECT")
; *** Used in buildquery
(defun get-standard-object (tree)
   (let ((obj (find-actavm-dep 'VERB-OBJ tree)) locut locut-arg)
      (cond ((not (null obj)) obj)
            (t (setq locut (find-actavm-dep 'VERB-OBJ*LOCUT tree))
               (cond ((null locut) nil)
                     (t (setq locut-arg (find-actavm-dep 'PREP-RMOD locut))
                        (find-actavm-dep 'PREP-ARG locut-arg)))))))

; *****************************************************************************
; *** this gets the argument of a preposition
;     It is its 'prep-arg, unless the preposition has a continuation ("riguardo a")
;     in which case, one must go down two levels
(defun get-preposition-arg (tree)
   (let ((prep-arg (find-actavm-dep 'PREP-ARG tree)) down-prep)
      (cond ((not (null prep-arg)) prep-arg)
            (t (setq down-prep (find-actavm-dep 'CONTIN+PREP tree))
               (cond ((null down-prep) nil)
                     (t (find-actavm-dep 'PREP-ARG down-prep)))))))

; *****************************************************************************
(defun make-lexmean (loc-name)
    `((lexmean ,loc-name)))

; *****************************************************************************
; *** functions for extending the tree
;     The first ones are based on the "adjoin-subtree" function defined below
(defun add-givinfo (tree agent)
   (adjoin-subtree tree
               `(nil
                 (££give-info top-verb
                   ((#\#)
                    (,agent verb-subj
                        ((#\#)))
                    (--about-relation verb-indcompl-theme
                        ((#\#)
                         (*)))))
                 prep-arg)))

(defun add-getinfo (tree)
   (adjoin-subtree tree
          `(nil
            (££get-info top-verb
               ((#\#)
                (§speaker verb-subj
                   ((#\#)))
                    (*)))
          verb-indcompl-theme)))

(defun add-want-know (tree)
   (adjoin-subtree tree
          `(nil
            (££want top-verb
              ((#\#)
               (§speaker verb-subj
                  ((#\#)))
               (££know verb+modal-indcompl
                  ((#\#)
                  (££speaker verb-subj
                     ((#\#)))
                      (*)))))
            verb-obj)))

(defun add-want (tree)
   (adjoin-subtree tree
          `(nil
            (££want top-verb
              ((#\#)
               (§speaker verb-subj
                  ((#\#)))
                   (*)))
            verb+modal-indcompl)))

(defun add-want-obj (tree)
   (adjoin-subtree tree
          `(nil
            (££want top-verb
              ((#\#)
               (§speaker verb-subj
                  ((#\#)))
                   (*)))
            verb-obj)))

; *****************************************************************************
; *** this function inserts new (lexically empty) elements
;     in an existing tree.
; *** operations specify what to do on the tree
;     They are specified as follows:
;     - insertion path: how to reach the node which has to be detached and
;       reattached below the new elements that have to be inserted. It is a
;       sequence of arc labels. It is nil, in case the new elements appear as root
;     - elements to insert
;       They are described in a compact form, since most material is always empty:
;       <lexmean linkup-label position dependents>
;       where dependents is a list of analogous structures or the head position #\#
;       e.g. (££know top-verb (0 10) ((#\#) (§speaker verb-subj (0 11)) (*)))
;      The * marks the position where the insertion has to be made
;     - new link of the detached subtree to the lowest inserted element
(defun adjoin-subtree (tree operations)
  (let ((path (first operations))
        (tree-to-adjoin (second operations))
        (link (third operations)))
    (cond ((null path)		; *** insertion point found
            (first (expand-adjoin-tree (list tree-to-adjoin) tree link 0 9)))
        ; *** go down on the dependents
        ;     The last argument of the call (9) signals that we have not yet
        ;     found any trace within the dependent list
          (t (list 
                (list 'head (get-actavm-head tree))
                (list 'dependents
                      (replace-dependent 
                              (get-actavm-dependents tree)
                              path tree-to-adjoin link
                              (get-actavm-headnumb tree)
                              9)))))))

; *****************************************************************************
; *** this takes a list of dependents and inspects them in order to match
;     the first link on "path" with the arc linking the dependent to its parent
;   INPUT:
;     - deps: the dependents among which there must be one that includes the
;       tree to detach
;     - path: the remaining path (downward sequence of link labels)  to follow 
;       to find the tree to detach
;     - tree-to-adjoin: the compact description of the tree to adjoin
;     - newlinklab: the label of the link which connects the detached tree to
;       the foot of the detached subtree
;     - parentpos: the input string position of the node to which the adjoined
;       subtree must be attached
(defun replace-dependent (deps path tree-to-adjoin newlinklab parentpos lasttrace)
  (cond ((null deps)
          (exception 'semantic-error
                     "PROC/seminterp: Dependent not found in replace-dependent"))
        ((or (eq (first (first deps)) '#\#)
                 (neq (first path) (get-actavm-headlink (first deps))))
     ; *** the next dependent is not on the insertion path: leave it unchanged
           (cons 
              (first deps)
              (replace-dependent (rest deps) path tree-to-adjoin newlinklab
                       parentpos
                       (cond ((is-a-actavm-trace? (first deps))
                                (second (get-actavm-headnumb (first deps))))
                             (t 9)))))
        ((null (rest path))		; *** insertion point found
     ; *** we are on the right path, and the path is terminated:
     ;     we have found the subtree to detach
           (append	; *** append, since the next works on lists of trees
              (expand-adjoin-tree
                    (list tree-to-adjoin) 
                    (first deps) newlinklab parentpos lasttrace)
              (rest deps)))
        (t (cons
     ; *** we are on the right path, but the path is not terminated:
     ; *** go down on the first dependent
     ;     The last argument of the recursive call signals that we have not yet
     ;     found any trace within the dependent list
              (list 
                (list 'head (get-actavm-head (first deps)))
                (list 'dependents
                     (replace-dependent 
                            (get-actavm-dependents (first deps))
                            (rest path) tree-to-adjoin newlinklab parentpos 9)))
       ; *** and leave other dependents unchanged
              (rest deps)))))

; *****************************************************************************
; *** expands a compact tree representation including a foot
; *** INPUT:
;   - compact-tree-def: a list of trees to expand
;   - detached-tree: the tree that has been detached and that must be reattached
;     to the foot of compact-tree-def
;   - newlinklab: the label of the link of the detached tree to the foot
;   - pos: the input string position of the parent
;   - lasttraceind: the initial trace sequence number
(defun expand-adjoin-tree 
           (compact-tree-def detached-tree newlinklab pos lasttraceind)
   (declare (special lasttraceind)) 
   (cond ((numberp pos)
            (int-expand-adjoin-tree compact-tree-def detached-tree newlinklab pos))
         (t (exception 'semantic-error
                 "PROC/seminterp: Adjoining under a trace or subcomponent" pos))))

; *****************************************************************************
; *** tree-to-adjoin is a list of compact tree descriptions, in the form described
;    above. Initially, it contains a single element, i.e. the whole subtree, but
;    in further steps, it contains the list of dependents.
; *** so, we have a horizontal recursion (on the list of trees) and a vertical
;     recursion, going downward in the subtrees
; *** tree is the part of the original tree that was detached and has to be
;     reattached below the adjoined tree
(defun int-expand-adjoin-tree (tree-to-adjoin detached-tree newlinklab pos)
  (declare (special lasttraceind)) 
  (cond ((null tree-to-adjoin) nil) 
     ; *** end of horizontal recursion on dependents list
        (t (let ((firstdep (first tree-to-adjoin)))
             (cond ((equal firstdep '(#\#))
                     (cons firstdep 
                          (int-expand-adjoin-tree (rest tree-to-adjoin)
                                                  detached-tree newlinklab pos)))
                   ((equal firstdep '(*))
     ; *** end of vertical recursion: here, we must place the old subtree (tree)
                     (cons `((head ,(subst-head-val
                                      (get-actavm-head detached-tree)
                                      'link newlinklab))
                             (dependents ,(get-actavm-dependents detached-tree)))
                           (int-expand-adjoin-tree 
                              (rest tree-to-adjoin) detached-tree newlinklab pos)))
                   (t (setq lasttraceind (1+ lasttraceind))
     ; *** actual expansion of the compact form of the tree
                      (cons
                        `((head
                           ((form #\^)
                            (syn nil)
                            (sem ((lexmean ,(first firstdep))))
                            (link ,(second firstdep))
                            (position ,(list pos lasttraceind))))
                          (dependents
                            ,(int-expand-adjoin-tree 
                                   (third firstdep) detached-tree newlinklab pos)))
                       (int-expand-adjoin-tree 
                             (rest tree-to-adjoin) detached-tree newlinklab pos))))))))

; *****************************************************************************
; *** this function inserts new elements below some node of an existing tree.
; *** operations specify what to do on the tree
;     They are specified as follows:
;     - insertion path: how to reach the node below which the
;       new elements have to be inserted. Note that this is different from the
;       adjoin-subtree: in that case the node found at the end of the path must
;       be detached and substituted by the new tree. Here, the node found
;       is unaffected, but it gets a new dependent substructure must
;       be detached and substituted by the new tree. Here, the node found
;       is unaffected, but it gets a new dependent substructure
;       In particular, here the path cannot be nil!!!
;     - elements to insert (the same as above, but without the * marker)
;       They are described in a compact form, since most material is always empty:
;       <lexmean linkup-label position dependents>
;       where dependents is a list of analogous structures or the head position #\#
;       e.g. (§speaker verb-subj (0 11))
;        (attach-subtree annotated-tree
;                   `((verb+modal-indcompl verb+modal-indcompl)
;                     (££location rmod (1 12) ((#\#)))))
(defun attach-subtree (tree operations)
  (let ((path (first operations))
        (tree-to-attach (second operations)))
    (cond ((null path)	
            (exception 'semantic-error
                       "PROC/seminterp: Null path in attach-subtree"))
        ; *** go down on the dependents
          (t (list 
                (list 'head (get-actavm-head tree))
                (list 'dependents
                      (attach-dependent 
                              (get-actavm-dependents tree)
                              path tree-to-attach
                              (get-actavm-headnumb tree))))))))

; *****************************************************************************
; *** attaches a tree inside one of the trees in "deps"
; *** INPUT:
;      deps: list of dependents of a node along the path
;      path: the remaining path to be traveled to arrive to the insertion point
;      tree-to-attach: a compact tree representation of the material to attach
;      pos: the position of the parent in the input string; this is needed to
;           produce the correct trace position
(defun attach-dependent (deps path tree-to-attach pos)
  (cond ((null deps)
          (exception 'semantic-error 
                     "PROC/seminterp: Dependent not found in attach-dependent"))
        (t (let ((firstdep (first deps)))
             (cond ((null path)
     ; *** we have found the node under which the new material has to be put
                      (cond ((eq (first firstdep) '#\#)
                               (cons firstdep
                                  (skip-traces-and-expand
                                       (rest deps) tree-to-attach pos 9)))
                            (t (cons firstdep
                                     (attach-dependent 
                                         (rest deps) nil tree-to-attach pos)))))
     ; *** the next dependent is not on the path: leave it unchanged
                   ((or (eq (first firstdep) '#\#)
                            (neq (first path) (get-actavm-headlink firstdep)))
                     (cons 
                        firstdep
                        (attach-dependent (rest deps) path tree-to-attach pos)))
                   (t (cons
     ; *** the next dependent is on the path: go down on it
                         (list 
                            (list 'head (get-actavm-head firstdep))
                            (list 'dependents
                                  (attach-dependent 
                                         (get-actavm-dependents firstdep)
                                         (rest path)
                                         tree-to-attach 
                                         (get-actavm-headnumb firstdep))))
       ; *** and leave other dependents unchanged
                         (rest deps))))))))

; *****************************************************************************
; *** this skips all existing traces before inserting the new material
;     INPUT:
;       - deps: the list of dependents (the first of which could be traces)
;       - tree-to-attach: the tree to insert after the traces
;       - parentpos: the input string position of the parent
;       - traceind: the last trace index found (initialized to 9 and incremented
;         by 1 before inserting the new tree)
(defun skip-traces-and-expand (deps tree-to-attach parentpos traceind)
  (cond ((null deps)
  ; *** the list of dependents finishes with a trace, or with the head: add the
  ;     new tree at the end
          (expand-tree (list tree-to-attach) parentpos traceind))
        ((is-a-actavm-trace? (first deps))
  ; *** if the next dependent is a head, go ahead
          (cons (first deps)
                (skip-traces-and-expand 
                      (rest deps) tree-to-attach parentpos 
                      (second (get-actavm-headnumb (first deps))))))
  ; *** otherwise, insert the tree here
        (t (cons (expand-tree tree-to-attach parentpos traceind)
                 deps))))

; *****************************************************************************
; *** expands a compact tree representation
; *** INPUT:
;     compact-tree-def: a list of trees to expand
;     pos: the input string position of the parent
;     seq: the trace sequence number, starting from 10
(defun expand-tree (compact-tree-def pos lasttraceind)
   (declare (special lasttraceind)) 
   (cond ((numberp pos)
            (first (int-expand-tree (list compact-tree-def) pos)))
         (t (exception 'semantic-error
                       "PROC/seminterp: Attachment under a trace or subcomponent" pos))))

; *****************************************************************************
; *** this actually expands the compact representation in a subtree
;     the new trace indices are automatically determined on the basis of
;     the last traces found
(defun int-expand-tree  (compact-tree-def pos)
  (declare (special lasttraceind))
  (cond ((null compact-tree-def) nil)
        (t (let ((firstdep (first compact-tree-def)))
              (cond ((equal firstdep '(#\#))
   ; *** this is just the head position: insert it unchanged and go on with the
   ;     other trees
                       (cons firstdep
                             (int-expand-tree (rest compact-tree-def) pos)))
                    (t (setq lasttraceind (1+ lasttraceind))
   ; *** expand the head and go on with the other trees
                       (cons
                           `((head
                               ((form #\^)
                                (syn nil)
                                (sem ((lexmean ,(first firstdep))))
                                (link ,(second firstdep))
                                (position ,(list pos lasttraceind))))
                                (dependents
                                  ,(int-expand-tree (third firstdep) pos)))
                             (int-expand-tree (rest compact-tree-def) pos))))))))

; ***************************************************************************
; *** tree-up is the tree one-level up, with respect to the one being annotated
;     it is used to find the number of the possible governing verb for traces
; *** note that the parent is not enough, because of the presence of auxiliaries
;     (sono andato vs. sei andato)
(defun add-actavm-headmeaning (tree tree-up)
   (cond ((equal tree '(#\#)) tree)
         (t (let* ((newhead (add-head-wordmeaning 
                             tree
                             (get-actavm-headlemma tree)
                             (get-actavm-headlink tree)
                             tree-up))
                   (newtree (append1 newhead
                                (list 'dependents (get-actavm-dependents tree)))))
    ; *** this is made in two steps, in order to have available the 
    ;     interpretation of the root in the recursive call
                 (append newhead
                    `((dependents 
                        ,(mapcar #'(lambda (x) (add-actavm-headmeaning x newtree))
                                 (get-actavm-dependents tree)))))))))

; ***************************************************************************
; *** returns true if the first and the last dependent are quotes
(defun enclosed-in-quotes (tree)
  (let ((deps (get-actavm-dependents tree)))
      (and (not (equal (first deps) '(#\#)))
           (member (get-actavm-headlemma (first deps)) '(#\" #\'))   ; "
           (not (equal (ult deps) '(#\#)))
           (member (get-actavm-headlemma (ult deps)) '(#\" #\')))))   ; "

; ***************************************************************************
; *** adds to a node the semantic information got either from the semantic
;     dictionary, or, for some pronouns and traces, from the anaphoric context
(defun add-head-wordmeaning (tree lemma link tree-up)
  (declare (special *LANGUAGE* *ANAPHORIC-CONTEXT*))
  (let* ((head (get-actavm-head tree))
         (role (get-actavm-headlink tree))
         (categ (get-actavm-headcateg tree))
         (type (get-actavm-headtype tree))
         (hperson (get-actavm-headperson tree))
         (hnumber (get-actavm-headnumber tree))
         (up-categ (get-actavm-headcateg tree-up))
         (up-person (cond ((eq up-categ 'verb) 
                              (find-verb-person-or-number tree-up 'person))
                          (t nil)))
         newsem coref-ident anaph newnumb inst-class vtype
         (wmean (get-word-meaning lemma categ type tree)))
 ;(format t "entering add-head-wordmeaning; wmean: ~a~%" wmean)
 ;(break "")
  (cond ((enclosed-in-quotes tree)
   ; *** a tree surrounded (having as first and last dependent) by quotes
   ;     must refer to an individual insance: either it is recognized as such, or
   ;     it is simply interpreted as the event having the sentence covered
   ;     by the tree as its name (ident)
          (let ((inst-class (get-instance-class (first wmean))))
                  (cond ((null inst-class)
                           (setq newsem `((lexmean '££event)
                                          (ident ,(read-actavm-sent tree 0 t)))))
                        (t (setq newsem `((lexmean ,inst-class)
                                          (ident ,(first wmean))))))))
      ;  ((and (eq categ 'prep)
      ;        (lab-subsumes 'ARG link))
      ;    (setq newsem '((lexmean --dummy-prep))))
        ((eq categ 'PRON)
          (cond ((and (eq link 'VERB-SUBJ)
                      (eq (get-actavm-headtype tree-up) 'MOD)
                      (not (memq hperson '(1 2)))
                      (not (and (eq hperson 'allval)
                                (memq up-person '(1 2)))))
   ; *** the subjects of modal "Does IT go" are not interpreted, since
   ;     the actual referent depends on the governed verb
                   (setq newsem '((lexmean nil))))
                ((eq (first wmean) '--anaph-ref)
   ; *** anaphoric references are resolved looking in *ANAPHORIC-CONTEXT*
   ;     which is loaded in buildparam (works only for parameters, i.e.
   ;     dialogue)
   ; *** note that first and second person pronoun are not --anaph-ref
   ; *** anaph has the form (seminfo synt-number)
                  (setq newsem (make-lexmean '§indef-ref)))
                ((eq hperson 1)
                  (cond ((eq hnumber 'SING)
                           (setq newsem (make-lexmean '§speaker)))
                        (t (setq newsem (make-lexmean '§speaker+others)))))
                ((eq hperson 2)
                  (setq newsem (make-lexmean '§myself)))
                ((eq hperson 'allval)
           ; *** pronouns without a person should be traces
                  (cond ((eq (get-actavm-headform tree) #\t)
         ; *** check that it actually is a trace
                          (setq coref-ident (get-actavm-headcoref tree))
                          (cond ((eq coref-ident 'empty)
          ; *** empty coreferences refer to deictics or generics. What need to be
          ;     be done here is to check if the parent of the trace is a verb in
          ;     the first or second person
                                  (cond ((null tree-up)
              ; *** this should happen just in case the trace is the root of the
              ;     tree (i.e. never)
                                          (setq newsem '((lexmean nil))))
                                        ((eq 'verb (get-actavm-headcateg tree-up))
                                          (cond ((eq link 'verb-subj)
                                                  (cond ((eq 1 up-person)
                                                          (cond ((eq 'PL hnumber)
                                                                  (setq newsem 
                                                                    (make-lexmean '§speaker+others)))
                                                                (t (setq newsem 
                                                                     (make-lexmean '§speaker)))))
                                                        ((eq 2 up-person)
                                                          (setq newsem 
                                                             (make-lexmean '§myself)))
                                                        ((and (eq 3 up-person)
                                                              (eq 'pl hnumber))
                                                          (setq newsem 
                                                             (make-lexmean '§generic-ag)))
                                                        ((eq 'infinite 
                                                            (get-actavm-headmood tree-up))
     		; *** This is for expressions as "salir (spanish)"
                                                          (setq newsem 
                                                             (make-lexmean '§speaker)))
                                                        (t (setq anaph 
                                                               (get-last-anaph-ref 
                                                                  tree-up role))
                                                           (setq newsem (first anaph))
              ; *** anaph has the form (seminfo synt-number)
                                                           (setq newnumb 
                                                                   (second anaph)))))
                                                ((eq link 'verb-subj/verb-indcompl-agent)
                                                   (cond ((eq 'generic-t 
                                                                  (get-actavm-headlemma tree))
                                                            (setq newsem 
                                                               (make-lexmean '§generic-ag)))
                                                         (t (setq newsem '((lexmean nil))))))
                     ; *** the link up is neither 'verb-subj nor 'verb-subj/verb-indcompl-agent
                                                (t (setq newsem '((lexmean nil))))))
              ; *** the parent is not a verb
                                        (t (setq newsem '((lexmean nil))))))
              ; *** the coreference is not empty: it must be solved afterwards
                                (t (setq newsem '((lexmean nil))))))
                        (t (exception 'semantic-error
                               "PROC/seminterp: A pronoun with person allval, but not a trace"))))
                ((and (eq lemma 'GENERIC-T) (null hperson))
         ; *** this is a standard generic pronoun associated with a trace: set as meaning 
         ;     The referent is undefined (e.g. the deep subject of "Sono previste piogge")
                  (setq newsem (make-lexmean '§indef-ref)))
         ; *** this is a pronoun not anaphoric, and without person: standard
                (t (setq newsem `((lexmean ,(first wmean)))))))
        ((eq categ 'NUM)
   ; *** the meaning of numbers is the value of the number
           (let ((month (find-actavm-dep 'NOUN-RMOD-MONTH tree)))
                      ; *** if month is null, try to check if the structure is
                      ;     num of month
               (cond ((null month)
                        (let ((prep-dep (find-actavm-dep 'PREP-RMOD tree)))
                            (cond ((not (null prep-dep))
                                    (setq month (find-actavm-dep 'PREP-ARG prep-dep))
                                    (cond ((and (not (null month))
                                                (not (memq '££month-descr
                			  	           (get-instance-class
                                                              (first
                                                               (get-word-meaning 
                                                                  (get-actavm-headlemma month)
                                                                  (get-actavm-headcateg month)
                                                                  (get-actavm-headtype month)
                                                                  tree))))))
                                              (setq month nil))))))))
               (cond ((and (null month) (eq *LANGUAGE* 'english))
                        (setq month (eq link 'NUM-RMOD-DAY))))
               (cond ((null month)
                        (setq newsem `((lexmean ,(get-actavm-headvalue tree)))))
      ; *** if the number has a month dependent, then interpret it as a day
      ;     description
                     (t (setq newsem (make-lexmean '££day-numb-descr))))))
        ((and (eq categ 'ADJ)
              (eq (get-actavm-headtype tree) 'ORDIN)
              (eq *LANGUAGE* 'english)
              (not (null (find-actavm-dep 'NOUN-RMOD-MONTH tree))))
      ; *** if the ordinal has a month dependent, then interpret it as a day description
           (setq newsem (make-lexmean '££day-numb-descr)))
        ((and (eq categ 'NOUN)
              (eq 'verb-obj*locut (get-actavm-headlink tree)))
           (setq newsem '((lexmean verbal-locut))))	; *** it is a component of a locution
        ((memq categ '(NOUN VERB ADV))
   ;     1- (biglietto ££ticket)
   ;     2- (sentire (££to-feel (thematic-grid () ..))) ; verb with a thematic grid
   ;     3- (what (adj --q-art) (pron ££simple-event))  ; category-controlled ambiguity
   ;     4- (che ((conj subord) --empty-conj))          ; category+type-controlled ambiguity
   ;     5- (ragazzo ((noun (gender m)) ££boy))         ; category+other-feature-controlled ambiguity
   ;     6- (dirigere ((££conduct ££direct-movie)))     ; Uncontrolled ambiguity
   ;     7- (number ££number phone ££phone-number)      ; Dep-modified meaning
          ;(break "add-head-wordmeaning: NOUN or VERB")
          (setq vtype (get-actavm-headtype tree))
          (cond ((not (and (eq categ 'VERB) (eq vtype 'AUX)))
       ; *** the standard form of wmean is (meaning) or (meaning1 meaning2 ...)
       ;     but in case of verbs and of some nouns, another possibility is to have an associated 
       ;     thematic grid in this case, the form is:
       ;     ((meaning1 (THEMATIC-GRID (synlab11 themrole11) (synlab12 themrole12) ...))
       ;      (meaning2 (THEMATIC-GRID (synlab21 themrole21) (synlab22 themrole22) ...)))
                   (cond ((and (listp (first wmean))
                               (listp (second (first wmean)))
                               (memq (first (second (first wmean))) '(thematic-grid thematic-role)))
                            (setq newsem `((lexmean ,(first (first wmean)))
                                           (,(first (second (first wmean)))
                                            ,(rest (second (first wmean)))))))
                         ((and (listp (first wmean)) (listp (first (first wmean))))
       ; *** in this case the first of the first is a list, but not concerning a thematic grid
       ;     this is a case of ambiguity (e.g. (((££conduct ££direct-movie)))) for "dirigere"
                            (setq newsem (make-lexmean (first wmean))))
                      ;      (setq wmean (get-word-meaning lemma categ vtype tree))
                         ((null (rest wmean))
       ; *** in this case there is a single non-ambiguous item, so it is assumed to be an atom
       ;     check to see if it is an instance or a class
                            (cond ((atom (first wmean))
                                     (setq inst-class (get-instance-class (first wmean)))
                                     (cond ((null inst-class)
                                              (setq newsem `((lexmean ,(first wmean)))))
                                           (t (setq newsem `((lexmean ,inst-class)
                                                             (ident ,(first wmean)))))))
                                  (t (break "Add-head-wordmeaning: non ambiguous item, but not atom"))))
       ; *** there is a category-based ambiguity
                         (t (setq wmean (first (leggi wmean categ)))
                            (setq newsem (make-lexmean (first wmean))))))
                (t (setq newsem '((lexmean nil))))))	; *** it is an auxiliary
        ((eq categ 'CONJ)
    ;(break "CONJ")
           (setq newsem `((lexmean ,(first wmean)))))
   ; *** neither a pronoun nor a number nor a verb nor a noun nor a conj nor an adv
        (t (cond ((null (rest wmean))
   ; *** no ambiguity
                    (cond ((atom (first wmean))
       ; *** in this case there is a single non-ambiguous item, so it is assumed to be an atom
       ;     check to see if it is an instance or a class
                            (let ((inst-class (get-instance-class (first wmean))))
                                (cond ((null inst-class)
                                (setq newsem `((lexmean ,(first wmean)))))
                                      (t (setq newsem `((lexmean ,inst-class)
                                                        (ident ,(first wmean))))))))
                          (t (break "Add-head-wordmeaning: non ambiguous item, but not atom"))))
       ; *** there is a category-based ambiguity
                 (t (setq wmean (first (leggi wmean categ)))
                    (setq newsem `((lexmean ,wmean)))))))
; *** we have determined the new value for SEM, substitute it
     (cond ((null (get-flatavm-feat-val head 'coref))
             (list 
               (list 'head
                    (list (assoc 'form head)
                          (assoc 'position head)
                          (cond ((null newnumb) (assoc 'syn head))
                                (t (list 'syn
                                      (subst-syn-val 
                                          (second (assoc 'syn head))
                                          'number newnumb))))
                          (assoc 'link head)
                          (list 'sem (append
                                        (first (leggi head 'sem)) newsem))))))
           (t (list 
               (list 'head
                    (list (assoc 'form head)
                          (assoc 'position head)
                          (cond ((null newnumb) (assoc 'syn head))
                                (t (list 'syn
                                      (subst-syn-val 
                                          (second (assoc 'syn head))
                                          'number newnumb))))
                          (assoc 'coref head)
                          (assoc 'link head)
                          (list 'sem (append
                                        (first (leggi head 'sem)) newsem)))))))))

; ***************************************************************
(defun get-word-meaning (word categ type tree &optional other-feat)
  (declare (special *WORD-MEANING*))
  (let ((meaning (all-accent-leggi *WORD-MEANING* word)) deps found newword fmean)
   ; *** meaning can have the form:
   ;     1- (biglietto ££ticket)
   ;     2- (sentire (££to-feel (thematic-grid () ..))) ; verb with a thematic grid
   ;     3- (quindi (££to-cause (thematic-role ..)))    ; adverb assoc. with a thematic role
   ;     4- (what (adj --q-art) (pron ££simple-event))  ; category-controlled ambiguity
   ;     5- (che ((conj subord) --empty-conj))          ; category+type-controlled ambiguity
   ;     6- (ragazzo ((noun (gender m)) ££boy))         ; category+other-feature-controlled ambiguity
   ;     7- (dirigere ((££conduct ££direct-movie)))     ; Uncontrolled ambiguity
   ;     8- (number ££number phone ££phone-number)      ; Dep-modified meaning
     (setq fmean (first meaning))
     (cond ((null meaning)
   ; *** it may happen that the node refers to a verbal locution; see if there
   ;     is a *LOCUT modifier
             (cond ((eq categ 'VERB)
                      (setq deps (remove-head-marker (get-actavm-dependents tree)))
                      (do ((nxtdep (first deps) (first deps))
                           (deps (rest deps) (rest deps)))
                          ((or (null deps) found)
                             (cond (found
                                      (setq newword
                                         (implode
                                            (append (explode word) '(#\_)
                                                    (explode (get-actavm-headlemma found)))))
                                      (setq meaning (all-accent-leggi *WORD-MEANING* newword)))
                                   (t nil)))
                          (cond ((member 'LOCUT
                                     (expl+cats (explode (get-actavm-headlink nxtdep)) #\*))
                                   (setq found nxtdep)))))))
           ((atom fmean)
             (cond ((null (second meaning)) meaning)                          ; ------ case 1
                   (t (let ((dep (find-actavm-descendant
                                        '((lemma))
                                        (list (second meaning))
                                        (get-actavm-dependents tree))))
                          (cond ((null dep)
                                   (list (first meaning)))
                                (t (list (third meaning))))))))               ; ------ case 8
           ((atom (first fmean))
              (cond ((listp (second fmean))
   ; *** meaning = ((££to-feel (thematic-grid ...))) 
   ;     fmean = (££to-feel (thematic-grid ...))
   ;     (second fmean) = (thematic-grid ...)
                       (cond ((eq 'thematic-grid (first (second fmean)))      ; ------ case 2
                                (cond ((null tree) meaning)
                  ; *** this happens when the function is used during parsing to check the
                  ;     consistency of an assignment of case labels
                                      (t (list (choose-best-grid meaning tree)))))
      ; *** more than one thematic grid can be associated with a verb. In such a case,
      ;     meaning has the form ((vconc1 (thematic-grid ...)) (vconc2 (thematic-grid ...)) ...)
                             ((eq 'thematic-role (first (second fmean)))      ; ------ case 3
                                meaning)
                             (t (exception 'semantic-error 
                                    "Unknown structure in seminterp:get-word-meaning"))))
   ; *** meaning = ((adj --q-art) (pron ££simple-event))                      ; ------ case 4
   ;     fmean = (adj --q-art)
                    (t (first (leggi meaning categ)))))
           ((not (null (second fmean)))
              (cond ((atom (second (first fmean)))
   ; *** meaning = (((conj subord) --empty-conj))                             ; ------ case 5
   ;     fmean = ((conj subord) --empty-conj) 
   ;     first fmean = (conj subord)
                      (list-leggi meaning (list categ type)))
   ; *** meaning = (((noun (gender m)) ££boy) ((noun (gender f)) ££girl)) 
   ;     fmean = ((noun (gender m)) ££boy)
   ;     first fmean = (noun (gender m))
                    (t (feature-based-disamb meaning categ tree other-feat)))) ; ----- case 6
           (t (first fmean)))))                                                ; ----- case 7
   ; *** meaning = (((££conduct ££direct-movie))) 
   ; *** fmean = ((££conduct ££direct-movie)) 
   ; *** first fmean = (££conduct ££direct-movie)

; ***************************************************************************
; *** chooses the thematic grid that matches best with the dependents of tree
;     the match is based on the arc labels of the dependents of tree
(defun choose-best-grid (them-grids tree)
    (let* ((deps (remove-head-marker (get-actavm-dependents tree)))
           (best-grid (first them-grids))
           (best-match (them-grid-match (rest (second best-grid)) deps)))
   ; *** best-grid: (conc (thematic-grid (role1 role2 ...)))
     (do ((nxtgrid (second them-grids) (first them-grids))
          (them-grids (rest (rest them-grids)) (rest them-grids)))
         ((null nxtgrid) best-grid)
         (cond ((< (them-grid-match (rest (second nxtgrid)) deps) best-match)
                  (setq best-grid nxtgrid))))))

; ***************************************************************************
; *** evaluates the degree of match between them-grid and the actual dependents
(defun them-grid-match (them-grid deps)
   (let ((gridlab (mapcar #'first them-grid))
         (num-missing 0)
         (remdeps deps))
      (do ((nxtlab (first gridlab) (first gridlab))
           (gridlab (rest gridlab) (rest gridlab)))
          ((null nxtlab)
            (+ num-missing (* 3 (length remdeps))))
          (multiple-value-setq (num-missing remdeps) 
                  (search-grid-element nxtlab num-missing remdeps)))))

; ***************************************************************************
; *** looks for "label" among remdeps
;     if it founds it, the prev-missing is returned inchanged, and remdeps
;     is returned without "label"
;     otherwise, prev-missing is increased by 1, and remdeps is unchanged
(defun search-grid-element (label prev-missing remdeps)
   (do* ((prevdeps nil (cons nxtdep prevdeps))
         (nxtdep (first remdeps) (first remdeps))
         (remdeps (rest remdeps) (rest remdeps)))
       ((or (eq label (get-actavm-headlink nxtdep)) (null nxtdep))
          (cond ((eq label (get-actavm-headlink nxtdep))
                   (values prev-missing (append prevdeps remdeps)))
                (t (values (1+ prev-missing) prevdeps))))))

; ***************************************************************************
; *** meaning = (((noun (gender m)) ££boy) ((noun (gender f)) ££girl)) 
;     tree has a value in case the function is called during the semantic interpretation
;     in case it is called during parsing to apply semantic preferences, the tree does
;     not exist, but some features can be passed via other-feat (which has the form
;     ((gender m) (number pl)))
(defun feature-based-disamb (meaning headcateg tree other-feat)
   (cond ((null meaning) nil)
         (t (let* ((firstcase (first meaning))
                   (selector (first firstcase))
                   (concept (rest firstcase))
                   (selcateg (first selector))
                   (selfeat (second selector))
                   (featname (first selfeat))
                   (featval (second selfeat))
                   (treefeature 
                       (cond ((null tree) (first (leggi other-feat featname)))
                             (t (get-actavm-featval featname tree)))))
               (cond ((and (eq headcateg selcateg)
                           (eq featval treefeature)) concept)
                     (t (feature-based-disamb (rest meaning) headcateg tree other-feat)))))))

; ***************************************************************************
; *** this takes from *ANAPHORIC-REFERENCE* the last reference.
;     the form of *ANAPHORIC-REFERENCE* is as follows:
;     <turn-n turn-(n-1) turn-(n-2) ...>
;     Each turn may have the following form:
;     <§myself prompt-text prompt-parameters>
;       for system's turns, where "prompt-parameters" may be NIL
;     <§speaker annotated-tree ontological-repr parameters>
;       for user's turns
; *** returns a list including the concept followed by the possible cardinality
;     infos (e.g. (CONCERT (CARDINALITY SING)))
(defun get-last-anaph-ref (tree-up dep-role)
  (declare (special *ANAPHORIC-CONTEXT*))
  (let (found)
      (do ((nextturn (first *ANAPHORIC-CONTEXT*) (first remturns))
           (remturns (rest *ANAPHORIC-CONTEXT*) (rest remturns)))
          ((or (null nextturn) found)
            (cond ((null nextturn)
                    ;(exception 'semantic-error
                    ;          "PROC/seminterp: No anaphoric reference available")
                    nil
                     )
                  (t found)))
          (setq found (check-anaph-last-turn nextturn tree-up dep-role)))
             ))

; ***************************************************************************
(defun check-anaph-last-turn (turn tree-up dep-role)
   (cond ((eq (first turn) '§myself) nil)
  ; *** currently, the system's turns do not provide anaphoric referents
         (t (find-anaph-in-tree (second turn) 
                          (skip-question-tense-marker tree-up) dep-role nil))))

; ***************************************************************************
; *** tree is the tree within which the referent must be found
; *** target-tree is the tree whose "dep-role" dependent is the anaphor
; *** up-categ is the category of the governor of the possible referent (tree)
;     The trees must be travelled backwards, since the best referent is the
;     last "nominal" (i.e. noun or pronoun) node; note that the list of trees
;     is already given in reverse order (right to left)
; *** returns a list includig the concept followed by the possible cardinality
;     infos (e.g. (CONCERT (CARDINALITY SING)))
(defun find-anaph-in-tree (tree target-tree dep-role up-categ)
 (let (found)
    (cond ((not (equal tree '(#\#)))
            (let ((nextupcat (get-actavm-headcateg tree))
                  (deps (reverse (get-actavm-dependents tree))))
                (do ((nxtdep (first deps) (first deps))
                     (deps (rest deps) (rest deps)))
                   ((or (null nxtdep) found) found)
                  (cond ((equal nxtdep '(#\#))
   ; *** this is the position of the current tree head. Perform the actual checks
                      (cond ((memq (get-actavm-headcateg tree) '(noun pron))
       ; *** if this is a nominal node, take it as referent, unless the current
       ;     governor (in the sentence currently analyzed, not in the anaphoric
       ;     context) is a verb, so that the role compatibility must be checked
       ;     first
                              (cond ((eq 'noun up-categ) nil)
          ; *** in case of noun-noun modification, I assume that the lower noun
          ;     cannot be the referent (locutions and telegraphic forms)
                                    ((or (null target-tree)
                                         (neq 'verb (get-actavm-headcateg target-tree)))
                                       (setq found 
                                         (list
                                             (get-actavm-headsem tree)
                                             (get-actavm-headnumber tree))))
          ; *** check-complem-compatib in onto-reasoning.lisp
                                    ((check-complem-compatib
                                                  (get-actavm-headsem tree)
                                                  (get-actavm-headsem target-tree)
                                                  dep-role)
                                       (setq found 
                                         (list
                                             (get-actavm-headsem tree)
                                             (get-actavm-headnumber tree))))))
                            (t nil)))	; *** it is not a nominal node
                    (t	; *** it is not the position of the head: downward recursion
                      (setq found (find-anaph-in-tree 
                                     nxtdep target-tree dep-role nextupcat))))))))))

; ***************************************************************************
; *** this function has two tasks;
; *** the first is to includes in the tree the meaning of the co-referring traces.
;     It travels across the tree, and for each coreferential trace searches
;     the tree for its co-referent, picks its meaning and adds it to the
;     trace
;     N.B. It is assumed that the meaning of the coreferent already exists
;          this could not be the case for traces referring to other traces
; *** the second goal is to solve deictic references, both explicit (this moment)
;     and implicit (the city)
(defun solve-coreferences (tree wholetree governor)
  (cond ((equal tree '(#\#)) tree)
        (t (let ((thead (get-actavm-head tree)))
              (cond ((and (is-a-coref-trace? tree)
                          (null (get-actavm-headlexmean tree)))
            ; *** the meaning could be non-null in case this is a trace
            ;     coming from a modal. In this case, ("He will go t") the
            ;     solution of the anaphora is based on the verb governing
            ;     the trace, so that the trace has a meaning, while the
            ;     pronoun has an empty meaning
                      (cond ((eq #\p (get-actavm-headcoreftype tree))
                              (list
                                (list 'head
                                  (list (assoc 'form thead)
                                    (assoc 'position thead)
                                    (assoc 'syn thead)
                                    (assoc 'coref thead)
                                    (assoc 'link thead)
                                    (list 'sem 
                                         (subst-sem-val
                                             (first (leggi thead 'sem))
                                             (get-actavm-headlexmean
                                                 (find-coreferent
                                                     (get-actavm-headcorefline tree)
                                                     (list wholetree)))))))
                              '(dependents ((#\#)))))
                            ((eq #\f (get-actavm-headcoreftype tree))
                               (let ((coreferent 
                                        (find-coreferent
                                              (get-actavm-headcorefline tree)
                                              (list wholetree))))
                              (list
                                (list 'head
                                  (list (assoc 'form thead)
                                    (assoc 'position thead)
                                    (assoc 'syn thead)
                                    (assoc 'coref thead)
                                    (assoc 'link thead)
                                    (list 'sem 
                                         (subst-sem-val
                                             (first (leggi thead 'sem))
                                             (get-actavm-headlexmean coreferent)))))
                               `(dependents ,(get-actavm-dependents coreferent)))))
                             (t (exception 'semantic-error 
				         "PROC/seminterp: Word trace in seminterp"))))
                    ((and (eq 'pron (get-actavm-headcateg tree))
                          (eq 'relat (get-actavm-headtype tree)))
            ; *** this is a relative pronoun. The meaning is the same of the
            ;     referent of the pronoun, which is the element governing the verb
            ;     which governs the pronoun. This work should be made by the syntax, but
            ;     currently, the dependency tree does not include co-referents of
            ;     relative pronouns
                       (let* ((pron-up (find-actavm-parent tree (list wholetree)))
                              (pron-up-up (find-actavm-parent pron-up (list wholetree)))
                              relat-sem)
                 ; *** in pron-up the tree rooted in the governor of the pronoun
                 ; *** in pron-up-up the same, but two levels up
                           (cond ((eq 'verb (get-actavm-headcateg pron-up))
                                    (setq relat-sem
                                        (get-actavm-headlexmean pron-up-up)))
                                 ((eq 'prep (get-actavm-headcateg pron-up))
                      ; *** the theatres IN which ...
                                    (cond ((eq 'verb (get-actavm-headcateg pron-up-up))
                                             (setq relat-sem
                                                 (get-actavm-headlexmean
                                                      (find-actavm-parent pron-up-up
                                                                     (list wholetree)))))
                                          (t (exception 'semantic-error 
				               "PROC/seminterp: Rel Pron governed by a prep not governed by a verb"))))
                                 ((eq 'art (get-actavm-headcateg pron-up))
                                    (cond ((eq 'verb (get-actavm-headcateg pron-up-up))
                                             (setq relat-sem
                                                 (get-actavm-headlexmean pron-up-up)))
                                          ((eq 'prep (get-actavm-headcateg pron-up-up))
                                             (let ((pron-up-up-up
                                                    (find-actavm-parent pron-up-up (list wholetree))))
                                ; *** le componenti per LE quali ...
                                                (cond ((eq 'verb 
                                                           (get-actavm-headcateg pron-up-up-up))
                                                         (setq relat-sem
                                                             (get-actavm-headlexmean
                                                                  (find-actavm-parent pron-up-up-up
                                                                             (list wholetree)))))
                                                      (t (exception 'semantic-error 
				                              "PROC/seminterp: Rel Pron governed by an art and then a prep not governed by a verb")))))
                                          (t (exception 'semantic-error 
				               "PROC/seminterp: Rel Pron governed by a prep not governed by a prep or a verb"))))
                                 (t (exception 'semantic-error 
				          "PROC/seminterp: Rel Pron not governed by a prep, verb or art")))
                          (list
                             (list 'head
                                 (list (assoc 'form thead)
                                       (assoc 'position thead)
                                       (assoc 'syn thead)
                                       (assoc 'coref thead)
                                       (assoc 'link thead)
                                       (list 'sem 
                                            (subst-sem-val
                                                (first (leggi thead 'sem)) relat-sem))))
              ; *** the next assumes that traces and rel prons do not have dependents
                            '(dependents ((#\#))))))
         ; *** not a trace and not a relative pronoun
                    (t (let ((treehead (get-actavm-head tree)))
                          (list (list 'head treehead)
                                `(dependents 
                                   ,(mapcar #'(lambda (x)
                                                (solve-coreferences x wholetree treehead))
                                            (get-actavm-deps-with-traces tree)))))))))))

; ***************************************************************************
; *** looks for a subtree whose head has the line number equal to linenumb
(defun find-coreferent (linenumb treeset)
   (cond ((null treeset) nil)
         ((equal (first treeset) '(#\#))
            (find-coreferent linenumb (rest treeset)))
         ((equal (get-actavm-headnumb (first treeset)) linenumb)
            (first treeset))
         (t (let ((firstcoref 
                     (find-coreferent linenumb
                          (get-actavm-deps-with-traces (first treeset)))))
               (cond ((null firstcoref)
                         (find-coreferent linenumb (rest treeset)))
                     (t firstcoref))))))

; ***************************************************************************
; *** replaces a semantic value
;     If it is new, it creates the "lexmean" feature
(defun subst-sem-val (semvals newval)
  (cond ((null semvals) `((lexmean ,newval)))
        ((eq (first (first semvals)) 'lexmean)
            (cons (list 'lexmean newval) (rest semvals)))
        (t (cons (first semvals) (subst-sem-val (rest semvals) newval)))))

; ***************************************************************************
; *** replaces the syntactic value of the syntactic feature "feature"
(defun subst-syn-val (synvals feature newval)
  (cond ((null synvals) (list (list feature newval)))
        ((eq (first (first synvals)) feature)
            (cons (list feature newval) (rest synvals)))
        (t (cons (first synvals) 
                 (subst-syn-val (rest synvals) feature newval)))))

; ***************************************************************************
; *** changes the value of a feature of a syntactico-semantic head
(defun subst-head-val (head feat newval)
  (cond ((null head) (list (list feat newval)))
        ((eq (first (first head)) feat) (cons (list feat newval) (rest head)))
        (t (cons (first head) (subst-head-val (rest head) feat newval)))))

; *******************************************************************
; *** it traverses the tree looking for the leftmost item (i.e. the last word)
(defun get-leftmost-item (tree)
   (let ((deps (get-actavm-dependents tree)))
      (cond ((equal (ult deps) '(#\#)) tree)
            (t (get-leftmost-item (ult deps))))))

; *******************************************************************
(defun find-verb-person-or-number (tree selector)
  (let ((featureval 
          (cond ((eq selector 'person) (get-actavm-headperson tree))
                ((eq selector 'number) (get-actavm-headnumber tree)))))
      (cond ((null featureval)
           (let ((headmood (get-actavm-headmood tree)))
  ; *** if the person or the number is not in the verbal head, then the latter must
  ;     be a participle or a gerund and the person or the number must be found in a
  ;     depending auxiliary
              (cond ((eq headmood 'INFINITE) nil)
                    ((memq headmood '(PARTICIPLE GERUND))
                       (let ((auxiliary (find-actavm-dep 'AUX tree)))
                           (cond ((null auxiliary) nil)
                                 (t (let ((actfeatval 
                                           (cond ((eq selector 'person)
                                                     (get-actavm-headperson auxiliary))
                                                 ((eq selector 'number) 
                                                     (get-actavm-headnumber auxiliary))))
                                          vsubj)
                                      (cond ((null actfeatval)
                                              (exception 'semantic-error
                                                 "PROC/seminterp: Auxiliary without person or number"
                                                 actfeatval))
                                            ((eq actfeatval 'ALLVAL)
                                              (setq vsubj (find-actavm-dep 'VERB-SUBJ tree))
                                              (cond ((not (null vsubj))
                                                      (cond ((eq selector 'number) 
                                                               (get-actavm-headnumber vsubj))
                                                            ((eq selector 'person)
                                                               (cond ((eq 'pron (get-actavm-headcateg vsubj))
                                                                        (get-actavm-headperson vsubj))
                                                                     (t 3)))))
                                                    (t nil)))
                                            (t actfeatval)))))))
                    (t (exception 'semantic-error
                                 "PROC/seminterp: Verb without person, but not participle")))))
            ((eq featureval 'allval)
  ; *** for instance, English modals
               (let ((vsubj (find-actavm-dep 'VERB-SUBJ tree)))
                  (cond ((null vsubj)
                          (exception 'semantic-error
                                 "PROC/seminterp: verb without a subject"))
                        ((eq selector 'number) 
                            (get-actavm-headnumber vsubj))
                        ((eq selector 'person)
                           (cond ((eq 'pron (get-actavm-headcateg vsubj))
                                    (get-actavm-headperson vsubj))
                                 (t 3))))))
            (t featureval))))
 
; *******************************************************************
; *** this functions goes down one level if the head of the tree
;     is an article or a non-qualificative adjective
; *** the optional "multiple" specifies that the input is a forest
;     and not a single tree
(defun skip-determiner (tree &optional multiple)
 (cond (multiple (mapcar #'single-skip-determiner tree))
       (t (single-skip-determiner tree))))

(defun single-skip-determiner (tree)
 (let ((determ-head (is-a-actavm-noun-complex tree)))
      (cond ((not (null determ-head)) determ-head)
            (t tree))))

; *******************************************************************
; *** Takes care of modals in English. In case the root is a
;     "tense marker" (i.e. "will", "would", "shall")
;     or a "question or negative" marker ("do"), it returns the
;     its verbal dependent
; *** it returns two values: the remaining tree and some semantic infos associated
;     with the marker
(defun skip-question-tense-marker (tree)
  (let ((headmean (get-actavm-headlexmean tree)))
   (cond ((eq headmean '--question-or-neg-marker)
            (values (find-actavm-dep 'VERB+MODAL-INDCOMPL tree)
                    'eng-question-or-neg))
         ((eq headmean '--tense-marker)
            (values (find-actavm-dep 'VERB+MODAL-INDCOMPL tree)
                    'eng-future))
         (t tree))))

; *******************************************************************
(defun is-sem-interrogative (annotated-tree)
  (declare (special *LANGUAGE*))
  (let ((tree-deps (get-actavm-dependents annotated-tree)))
     (or (eq (get-actavm-headlemma (get-leftmost-item annotated-tree)) #\?)
             (eq '--question-or-neg-marker (get-actavm-headlexmean annotated-tree))
             (find-interr-adv tree-deps)
             (find-interr-pron tree-deps)
             (find-interr-adjec tree-deps)
             (and (eq *LANGUAGE* 'english)
                  (there-inversion tree-deps)))))

; *******************************************************************
; *** assumption that an English sentence where there is a "there"
;     occurring after the verb is interrogative also in absence of 
;     question mark
(defun there-inversion (tree-deps)
   (let (found-head result)
       (dolist (dep tree-deps result)
           (cond ((equal dep '(#\#))
                    (setq found-head t))
                 ((and found-head
                       (eq (get-actavm-headlink dep) 'PRON-RMOD-LOC+METAPH))
                    (setq result t))))))

; *******************************************************************
; +++ it gets a tree and returns a string representing the sentence
;     If the tree includes a first and last quote, they are removed
; *** the external function just to delete the trailing blank
(defun read-actavm-sent (tree level noquotes)
   (string (implode (rest (explode (read-t-s-int tree level noquotes))))))

(defun read-t-s-int (tree level noquotes)
  (let ((deps (get-actavm-dependents tree))
        (head (get-actavm-headform tree))
        (subword (not (numberp (get-actavm-headnumb tree))))
        (sentence ""))
    (cond ((and noquotes
                (= level 1)
                (eq head '|"|))    ; "
            "")
          (t (dolist (nxtdep deps sentence)
               (cond ((equal nxtdep '(#\#))
                       (cond ((not subword)
                                (setq sentence 
                                   (concatenate 
                                     'string sentence " " 
                                     (get-printable-tree-form head))))))
                     (t (setq sentence 
                            (concatenate 'string sentence 
                                (read-t-s-int 
                                      nxtdep (1+ level) noquotes))))))))))
; *******************************************************************
(defun get-printable-tree-form (form)
  (cond ((eq form #\t) "")
        (t (coerce (symbol-name form) 'string))))

; *******************************************************************
; *** this selects the part of the representation that must indergo the simplification
;     The actual woork is done by "remove-upper-classes"
(defun simplify-onto-repr (ontorepr)
 (let (inpform simplresult)
  (cond ((eq (first ontorepr) 'select)
      ; *** (SELECT x FROM y WHERE z)   z has to be simplified
           (setq inpform (sixth ontorepr))
           (setq simplresult (append1 (first-n 5 ontorepr) (act-simplify-onto inpform))))
        ((eq (first ontorepr) 'about)
      ; *** (ABOUT x WHERE y)   y has to be simplified
           (setq inpform (fourth ontorepr))
           (setq simplresult (append1 (first-n 3 ontorepr) (act-simplify-onto inpform))))
        (t (break "seminterp: simplify-onto-repr")))
   simplresult
   ))

; *******************************************************************
(defun is-simple-onto-item (item)
  (or (atom item) (eq 'synt (first item))))

; *******************************************************************
; *** the simpification is made in 4 steps:
;     1. All ands are removed, so that a set of "upper-level" conjuncts is produced
;        "flatten-all-ands": 
;           Ex:  (a (and ((b c (and ((d) (e f)))) (g h)))) --->
;                   ((a b c d) (a b c e f) (a g h))
;     2. From each list all up-down or down-up paths are removed
;        "shorten-loops":
;           Ex:  (... X A B C D C B A Y ...) --->
;                   ((... X A B C D) (... X A Y ...))
;     3. The "more general" classes are removed, when possible
;     4. The formula is simplified by inserting again "and" operators
(defun act-simplify-onto (semrepr)
  (let (flattened shortened noupper finres)
     (setq flattened (flatten-all-ands semrepr nil))
  ;(format t "After flatten-all-ands: ~a~%" flattened)
  ;(break "After flatten")
  ;   (setq shortened (flatten (mapcar #'(lambda (x) (shorten-loops x nil nil)) flattened)))
  ;(format t "After shorten-loops: ~a~%" shortened)
  ;(break "After shorten")
  ;   (setq noupper (mapcar #'remove-upper-classes shortened))
     (setq noupper (mapcar #'remove-upper-classes flattened))
  ;(format t "After remove-upper-classes: ~a~%" noupper)
  ;(break "After remove-upper")
     (setq finres (compose-and-repr noupper))
  ;(format t "After compose-and: ~a~%" finres)
  ;(break "After compose-and")
   finres))

; *******************************************************************
; *** this takes a set of paths, and checks if there are different starts
;     ex. ((a b c d) (a b e) (f g h))
;     In such a case it builds an and: ((and (a ...) (f ...)))
(defun compose-and-repr (pathlist)
                 ;  (format t "pathlist: ~a~%" pathlist)
                 ;  (break "")
 (let (firstp)
  (cond ((all-null pathlist) nil)
        ((and (eq 1 (length pathlist))
              (listp (remove-synt-pointer (first (first pathlist))))
              (neq 'eq (first (first (first pathlist)))))
           (setq firstp (first pathlist))
           (cond ((eq 'EVENT-AND (first (first firstp)))
                    (list (list 'event-and (mapcar #'compose-and-repr (second (first firstp))))))
                 (t (break "in seminterp.lisp: compose-and-repr"))))
        (t (let* ((allstarts (mapcar #'first pathlist)) grouped starts equivalences)
          (multiple-value-setq (starts equivalences)
              (merge-all-starts (mapcar #'remove-synt-pointer allstarts)))
      ; *** now we have in starts all beginnings of the path, without duplicates of
      ;     equal or subsuming concepts. If any pair of concepts has not been duplicated
      ;     because of subsumption, we have in equivalences the pair <specific generic>
      ;     Ex. allstars: (££SITUATION ££RAIN ££TEMPERATURE ££SITUATION ££RAIN ££TEMPERATURE)
      ;         --> starts = (££RAIN ££TEMPERATURE)
      ;             equivalences = ((££RAIN ££SITUATION))
          (cond ((eq (length starts) 1)
      ; *** in this case, all paths start with the same item, so no "and" is required
                 ;  (format t "single start; next step: ~a~%" (mapcar #'rest pathlist))
                 ;  (break "")
                   (cons (find-best-start allstarts equivalences) 
                         (compose-and-repr (mapcar #'rest pathlist))))
      ; *** find-best-start checks if any of the starts includes a synt pointer, in which case it
      ;     returns it
                (t (setq grouped (dropnil2 (group-same-start starts pathlist equivalences)))
                 ;  (format t "grouped: ~a~%" grouped)
                 ;  (break "")
      ; *** this may happen in case we have, at the previous level:
      ;     ((X) (A (A B C) (A B E)))
      ;     So, the "grouped" for A is  (NIL ((B C) (B E)))
                   (cond ((eq 1 (length grouped))
                            (compose-and-repr (first grouped)))
                         (t (list (list 'and (mapcar #'compose-and-repr grouped))))))))))))

; *******************************************************************
; *** this gets a list of concepts and builds a new list such that:
;     - there are no duplicates
;     - if conc1 and conc2 are subconcepts one of the other, only the most specific one remains
;       (first the most specific)
; *** returns the list and the set of pairs of replaced concept in case of subsumption
(defun merge-all-starts (concepts)
  (let (result equiv equivs)
     (do ((nxtconc (first concepts) (first concepts))
          (concepts (rest concepts) (rest concepts)))
         ((null nxtconc) (values (reverse result) equivs))
         (multiple-value-setq (result equiv)
             (insert-no-dupl-with-subsumption nxtconc result))
         (setq equivs (cond ((or (null equiv)
                                 (member equiv equivs :test #'equal)) equivs)
                            (t (cons equiv equivs)))))))

; *******************************************************************
(defun insert-no-dupl-with-subsumption (newitem lis)
  (let (repl res found)
     (do ((nxtitem (first lis) (first lis))
          (lis (rest lis) (rest lis)))
         ((or (null nxtitem) found)
            (cond (found (values (dropnil (append res (list nxtitem) lis)) repl))
                  (t (values (cons newitem (dropnil (append res (list nxtitem) lis))) nil))))
         (cond ((eq nxtitem newitem)
                  (setq found t)
                  (setq res (cons nxtitem res)))
               ((is-subclass-of nxtitem newitem)
                  (setq found t)
                  (setq repl (list nxtitem newitem))
                  (setq res (cons nxtitem res)))
               ((is-subclass-of newitem nxtitem)
                  (setq found t)
                  (setq repl (list newitem nxtitem))
                  (setq res (cons newitem res)))
               (t (setq res (cons nxtitem res)))))))

; *******************************************************************
; *** checks if any of the starts includes a synt pointer, in which case it returns it
;     it also applies equivalences, in order to return the more specific concept
(defun find-best-start (starts equivalences)
  (let ((begstart (first starts)))
      (do ((nxtstart (first starts) (first starts))
           (starts (rest starts) (rest starts)))
          ((or (null nxtstart) (and (listp nxtstart) (eq 'synt (first nxtstart))))
            (cond ((null nxtstart) begstart)
                  (t (find-start-equiv nxtstart equivalences)))))))
        
; *******************************************************************
; *** start is assumed to be a triple <SYNT pos conc>
;     is conc appears in an equivalence <more-spec-conc conc> then it is replaced in the
;     triple: <SYNT pos more-spec-conc>
(defun find-start-equiv (start equivs)
  (do ((nxtequiv (first equivs) (first equivs))
       (equivs (rest equivs) (first equivs)))
      ((null nxtequiv) start)
      (cond ((eq (third start) (second nxtequiv))
               (setq start (list 'synt (second start) (first nxtequiv)))))))

; *******************************************************************
(defun group-same-start (starts pathlist equivs)
   (cond ((null starts) nil)
         (t (let ((simplstart (remove-synt-pointer (first starts))))
                (cons 
                   (dropnil2 
                      (mapcar 
                         #'(lambda (x) 
                             (let ((simplx (remove-synt-pointer (first x))))
                                (cond ((equiv-equal simplstart simplx equivs)
                                         (cons (best-synt-conc (first starts) (first x))
                                               (rest x)))
                                       (t nil))))
                         pathlist))
                   (group-same-start (rest starts) pathlist equivs))))))

; *******************************************************************
(defun equiv-equal (conc1 conc2 equivs)
  (or (eq conc1 conc2)
      (member (list conc1 conc2) equivs :test #'equal)
      (member (list conc2 conc1) equivs :test #'equal)))

; *******************************************************************
; *** this removes, when possible, the subclass-of and has-subclass links
;     with the goal of focusing always on the more specific classes.
;     This means that "inheritance" has been implicitly applied
; *** a sequence A SUBCLASS-OF B SUBCLASS-OF ... SUBCLASS-OF C is replaced by A
; *** a sequence A HAS-SUBCLASS B HAS-SUBCLASS ... HAS-SUBCLASS C is replaced by C
;     This except for cases as A HAS-SUBCLASS B SUBCLASS-OF C (or viceversa), 
;     which is kept in order to preserve the down-up (or up-down) movement
(defun remove-upper-classes (ontorepr)
   (cond ((null ontorepr) nil)
         (t (let ((firstitem (first ontorepr))
                  (seconditem (second ontorepr))
                  (fourthitem (fourth ontorepr)))
                (cond ((or (is-simple-onto-item firstitem)
                           (eq 'eq (first firstitem)))
  ; *** the first item is simple: it can be a structural item (e.g. RANGE-OF) or an object
  ;     in the ontology. It can also contain a reference to the syntactic structure (SYNT ...)
                        (cond ((null seconditem)
                                (list firstitem))
                              ((is-a-structural-item firstitem)
       ; *** the first item is structural item (e.g. RANGE-OF SUBCLASS-OF): this should not happen
                                (break "path in the ontology starting with a structural item"))
                              ((eq seconditem 'SUBCLASS-OF)
                                 (cond ((and (eq fourthitem 'HAS-SUBCLASS)
                                             (neq firstitem (fifth ontorepr)))
                          ; *** in this case (path up-down) no simplification
                          ;     unless going up and down returns to the same concept
                                          (append (first-n 4 ontorepr)
                                                  (remove-upper-classes (nthcdr 4 ontorepr))))
                                       (t (remove-upper-classes 
                                             (cons firstitem (rest (rest (rest ontorepr))))))))
                              ((eq seconditem 'HAS-SUBCLASS)
                                 (remove-upper-classes (rest (rest ontorepr))))
                              (t (cons firstitem 
                                    (cons seconditem (remove-upper-classes (rest (rest ontorepr))))))))
                      ((eq 'event-and (first firstitem))
     ; *** the structure of the list is 
     ;     ((event-and ((----restr11---- restr1N) (restr21 --- restr2M) ---)))
     ;    where each ( --- restri1 --- restriN) is a set of paths representing
     ;    one of the participants in Eventi. Each restrik is a path in the ontology
     ;    linking the verb describing the event to one of its participants
                         (list (list 'event-and 
                                    (mapcar #'(lambda (x) (mapcar #'remove-upper-classes x))
                                                (second firstitem)))))
                      (t (break "Non-simple item in a flattened formula")))))))

(defun old-remove-upper-classes (inputrepr buff)
  ; (format t "Entering remove-upper-classes; inputrepr: ~a~%" inputrepr)
  ; (break "")
  (let* ((ontorepr (mapcar #'(lambda(x) (shorten-loops x nil nil)) inputrepr))
         (firstitem (first ontorepr))
         (seconditem (second ontorepr))
         (thirditem (third ontorepr))
         andlist andresult newsecoperands tempresult result)
   (setq result
    (cond ((null ontorepr) nil)
          ((is-simple-onto-item firstitem)
  ; *** the first item is simple: it can be a structural item (e.g. RANGE-OF) or an object
  ;     in the ontology. It can also contain a reference to the syntactic structure (SYNT ...)
            (cond ((null seconditem)
                    (list firstitem))
                  ((is-a-structural-item firstitem)
       ; *** the first item is structural item (e.g. RANGE-OF): go ahead
                    (cons firstitem (old-remove-upper-classes (rest ontorepr) nil)))
                  ((is-simple-onto-item seconditem)
       ; *** the first item is not a structural item, but the second item is also simple
                    (cond ((eq 'subclass-of seconditem)
                            (cond ((is-simple-onto-item thirditem)
           ; *** X SUBCLASS-OF simple-item: go ahead after SUBCLASS-OF
                                     (cond ((eq (fourth ontorepr) 'has-subclass)
                  ; *** except in case X SUBCLASS-OF Y HAS-SUBCLASS Z, to avoid that X is assumed to be a
                  ;     superclass of Z
                                              (append (first-n 4 ontorepr)
                                                      (old-remove-upper-classes (nthcdr 4 ontorepr) nil)))
                                           (t (old-remove-upper-classes 
                                                   (cons firstitem (nthcdr 3 ontorepr)) nil))))
               ; *** if the third item is not an atom, it could be an and
               ;     in this case, the situation is:
               ;     < conc1 subclass-of (and ((conc2 ...) (conc3 ...) ...)) ...>
               ;     The various conc-i should be removed
                                  ((eq 'and (first thirditem))
                                     (setq andlist (second thirditem))
             ;(format t "and in thirditem: thirditem= ~a~%" thirditem)
             ;(break "")
                                     (do ((nxtoperand (first andlist) (first andlist))
                                          (andlist (rest andlist) (rest andlist)))
                                         ((null nxtoperand)
                  ; *** and of the do on the operand of the and; the process continues, but the
                  ;     subclass-of link is removed and the new and-expression replaces the old one
             ;(format t "and in thirditem: andresult= ~a~%" andresult)
             ;(break "")
                                           (setq tempresult (dropnil2 andresult))
                                           (cond ((> (length tempresult) 1)
                                                    (cons firstitem
                                                        (cons (list 'and tempresult)
                                                             (old-remove-upper-classes 
                                                                         (nthcdr 3 ontorepr) nil))))
                                                 (t (append (first tempresult)
                                                          (old-remove-upper-classes 
                                                                      (nthcdr 3 ontorepr) nil)))))
                  ; *** body of the do
             ;(format t "and in thirditem: body of the do;~% nxtoperand= ~a~%" nxtoperand)
             ;(break "")
                                         (cond ((is-simple-onto-item (first nxtoperand))
                                                 (cond ((is-a-structural-item (first nxtoperand))
                                                          (break "seminterp: old-remove-upper-classes 1"))
                                                       ((is-simple-onto-item (second nxtoperand))
                                                          (setq andresult 
                                                            (append1 andresult 
                                                                (old-remove-upper-classes 
                                                                    (rest nxtoperand) nil))))
                       ; *** if everything is ok, it could be that the rest of nextoperand is another
                       ;     "and"; e.g.
                       ;     <conc1 subclass-of (and ((conc2 (and ((range-of ...) 
                       ;                                           (subclass-of conc3 range ...)))))) >
                       ;     In such a case, after the simplification, we would obtain, for the
                       ;     second operand (subclass-of conc3), but this is wrong, because also
                       ;     this subclass link should be removed, leaving 
                       ;     <conc1 (and ((range-of ...) (range ...))) >
                       ;     A (possible?) solution is to remove the inner and by flattening the
                       ;     structure
                       ;     Unfortunately, this requires to change the loop variables
                                                       ((eq 'and (first (second nxtoperand)))
                                                          (setq newsecoperands
                                                             (flatten-second-and
                                                                       (first nxtoperand)
                                                                       (second (second nxtoperand))))
                       ; *** now, we have in newsecoperands the list of the operands of the inner "and"
                       ;     with conc2 in front of them
                       ;     the "do" must continue by putting this list in front of andlist
                                                          (setq andlist 
                                                             (append newsecoperands andlist)))
                       ; *** if it is not an atom, but it does not start with "and", proceed
                                                      (t (setq andresult 
                                                            (append1 andresult 
                                                                 (cons (first nxtoperand)
                                                                     (old-remove-upper-classes 
                                                                           (rest nxtoperand) nil)))))))
                       ; *** the first of nextoperand is not an atom; but it must not be an adjacent
                       ;     "and"
                                               ((eq 'and (first (first nxtoperand)))
                                                  (break "seminterp: old-remove-upper-classes 2"))
                                               (t (setq andresult 
                                                     (append1 andresult 
                                                        (cons (first nxtoperand)
                                                           (cons (second nxtoperand)
                                                                (old-remove-upper-classes 
                                                                    (rest (rest nxtoperand)) nil)))))))))
                  ; *** the third item is not an atom, but it is not an and
                  ;     this should not happen: <conc-1 subclass-of (eq? ...) ...>
                                  (t (break "seminterp: old-remove-upper-classes 2"))))
              ; *** the first item is not a structural item, but the second item is also simple
              ; *** and is not subclass-of
                          ((eq 'has-subclass seconditem)
                            (cond ((is-simple-onto-item thirditem)
                  ; *** <simple HAS-SUBCLASS simple>
                                ;     (format t "second item is has-subclass")
                                ;     (break "")
                                     (old-remove-upper-classes (rest (rest ontorepr)) nil))
                  ; *** if the situation is 
                  ;     (X Y <simple HAS-SUBCLASS (AND (SUBCL1 ...) (SUBCL2 ...) ...)> )
                  ;         the result should be
                  ;     (X Y <(AND (SUBCL1 ...) (SUBCL2 ...) ...)> )
                                  (t (old-remove-upper-classes (rest (rest ontorepr)) nil))))
              ; *** THIS BRANCH MUST BE DONE !!!!
                          (t (cons firstitem 
                                (cons seconditem (old-remove-upper-classes (rest (rest ontorepr)) nil))))))
              ; *** the second item is not simple
                  ((eq 'and (first seconditem))
             ;(format t "and in seconditem: seconditem= ~a~%" seconditem)
                     (setq tempresult (flatten-second-and firstitem (second seconditem)))
              ; *** firstitem=item1; seconditem=(and (item211 item212 ...) (item221 item222 ...) ...)
              ;     the result is ((item1 item211 item212 ...) (item1 item221 item222 ...) ...)
             ;(format t "after flattening the and: tempresult= ~a~%" tempresult)
             ;(break "")
                    ; (setq tempresult (mapcar #'old-remove-upper-classes tempresult))
             ;(format t "after recursion: tempresult= ~a~%" tempresult)
             ;(break "")
                     (setq tempresult (dropnil2 (remove-inner-ands (mapcar #'non-single-rest tempresult)
                     ;tempresult
                      )))
             ;(format t "after removing-inner-ands: tempresult= ~a~%" tempresult)
             ;(break "")
                     (cond ((> (length tempresult) 1)
                              (list-non-single firstitem tempresult)
                            ;  (list firstitem (list 'and tempresult))
                            )
                           (t (cons firstitem (first tempresult)))))
                  (t (cons firstitem 
                          (cons seconditem (old-remove-upper-classes (rest (rest ontorepr)) nil))))))
       ; *** the first item is not an atom
          ((eq 'and (first firstitem)) nil)
            ;(format t "and in firstitem: firstitem= ~a~%" firstitem)
            ;(break "")
           ;  (cons (list 'and (mapcar #'old-remove-upper-classes (second firstitem)))
           ;       (old-remove-upper-classes (rest ontorepr)))
          ((null seconditem)
             (list firstitem))
          (t (cons firstitem 
                  (cons seconditem (old-remove-upper-classes (rest (rest ontorepr)) nil))))))
  ;    (format t "Exiting old-remove-upper-classes; result: ~a~%" result)
  ;    (break "")
      result))

; ***************************************************************************
; *** this function has the goal of removing some redundant movements on the
;     ontology. This may happen in order to find a connection with a word
;     in the sentence. The situation to detect is <... X A B C D C B A Y ...>
;     When this situation is found, the the paths are split in order to obtain
;     ((... X A B C D) (X Y ...))
;     The first of the two items is complete, while for the second one the process
;     must continue. Complete paths are kept in buff2, while active ones are kept in buff1
(defun shorten-loops (semrepr buff1 buff2)
  (let (result newshort restpath newbuff1 newbuff2)
     (cond ((null semrepr)
              (setq result (mapcar #'reverse (append buff1 buff2)))
           ; (format t "shorten-loop; exit 1; result = ~a~%" result)
           ; (break "")
            result)
         ((or (atom (first semrepr))
              (not (memq (first (first semrepr)) '(and event-and))))
            (multiple-value-setq (newshort restpath) (check-revpath semrepr))
            (cond ((null newshort)
                     (shorten-loops (rest semrepr) 
                               (flat-cons (first semrepr) buff1)
                               buff2))
                  (t
  ; *** if semrepr is (A B C D C B A Y E F...) 
  ;        buff1 is ((Z X) (T X))
  ;        buff2 is ((U V W))
  ;     the result of check-revpath is
  ;        newshort: (A B C D)
  ;        restpath: (Y E F ...)
  ;     the recursive call must be made on
  ;        semrepr = (Y E F...) 
  ;        buff1 is not changed
  ;        buff2 is ((D C B A Z X) (D C B A T X))
     ;  (format t "shorten-loops~%     newshort = ~a~%     restpath = ~a~%" newshort restpath)
     ;  (break "")
                     (setq newbuff1 (flat-cons (first newshort) buff1))
                     (setq newbuff2 
                         (cond ((null buff1)
                                  (list (list (reverse newshort))))
                               (t (append 
                                      (mapcar #'(lambda (x) 
                                               (append (reverse newshort) x))
                                          buff1)
                                      buff2))))
                     (shorten-loops restpath newbuff1 newbuff2))))
         ((eq (first (first semrepr)) 'event-and)
     ; *** the structure of the list is 
     ;     ((event-and ((----restr11---- restr1N) (restr21 --- restr2M) ---)))
     ;    where each ( --- restri1 --- restriN) is a set of paths representing
     ;    one of the participants in Eventi. Each restrik is a path in the ontology
     ;    linking the verb describing the event to one of its participants
           ;(break "event-and in shorten-loops")
            (flat-append1 (mapcar #'reverse buff1)
                  (list 'event-and 
                       (mapcar #'(lambda (x) 
                                      (mapcar #'(lambda (y) (flatten (shorten-loops y nil nil))) x))
                                 (second (first semrepr))))))
         (t (break "and-ed formula in shorten-loops")))))

; ***************************************************************************
; *** this eliminates all ands form inside a formula, by producing all simple
;     formulae (which, if put in and, are equivalent to the original formula)
;     Ex:  (a (and ((b c (and ((d) (e f)))) (g h)))) --->
;            ((a b c d) (a b c e f) (a g h))
(defun flatten-all-ands (semr buff)
  (cond ((null semr)
           (mapcar #'reverse buff))
        ((or (is-simple-onto-item (first semr))
             (eq 'eq (first (first semr))))
           (flatten-all-ands (rest semr) (flat-cons (first semr) buff)))
        ((eq 'and (first (first semr)))
           (int-fl-all-ands (mapcar #'(lambda (x) (flatten-all-ands x buff)) (second (first semr)))))
        ((eq 'event-and (first (first semr)))
           (flat-append1 (mapcar #'reverse buff)
                   (list 'event-and 
                     (mapcar #'(lambda (x) (flatten-all-ands x nil)) (second (first semr))))))
        (t (break "unknown complex item in flatten-all-ands"))))

; ***************************************************************************
(defun flat-append1 (buff el)
   (cond ((null buff) (list (list el)))
         (t (mapcar #'(lambda (x) (append1 x el)) buff))))

; ***************************************************************************
; *** if buffer is empty, it is initialized as ((el))
;     otherwise, el is cons-ed in front of all sublists of buff
;     (flat-cons x ((a b) (c) (d e f))) -->  ((x a b) (x c) (x d e f)))
(defun flat-cons (el buff)
   (cond ((null buff) (list (list el)))	; *** initialization of buff
         (t (mapcar #'(lambda (x) (cons el x)) buff))))

; ***************************************************************************
(defun int-fl-all-ands (lislislis)
   (cond ((null lislislis) nil)
         (t (append (first lislislis) (int-fl-all-ands (rest lislislis))))))

; ***************************************************************************
; *** in the example above, the function succeeds when
;     path = <A B C D C B A Y ...>
; *** returns the length of the common part; 3, in this example; 0 if no usable common part
;     It first collects in afterstart all pieces of path that finish in a second copy of
;     the first of path. Then checks if any of them is "symmetrical", in the sense shown
;     by the example. This avoids the detection of shorter loops, as in case of 
;     <A B A C D C A B A Y ...>
(defun check-revpath (path)
   (let ((afterstart (find-same-beg-start path (first path) nil nil))
         plength halfleng rem found prevpath)
;   (format t "afterstart: ~a~%" afterstart)
     (do ((nxtstart (first afterstart) (first afterstart))
          (afterstart (rest afterstart) (rest afterstart)))
         ((or (null nxtstart) found)
   ;(format t "check-revpath~%  path: ~a~%   prevpath: ~a~%   found: ~a~%" path prevpath found)
   ;(break "")
            (cond (found (values found (remove-rev-path path prevpath)))
                  (t nil)))
         (setq plength (length nxtstart))
         (multiple-value-setq (halfleng rem) (floor plength 2))
;   (format t "nxtstart: ~a~% plength: ~a~%  halfleng: ~a~%  rem: ~a~% "
;                nxtstart plength halfleng rem)
         (cond ((= rem 1)
                 (setq found (pathsymmetric nxtstart halfleng plength))
                 (setq prevpath nxtstart))))))

; ***************************************************************************
(defun remove-rev-path (path begin)
   (cond ((null begin) path)
         ((eq (first begin) (first path)) (remove-rev-path (rest path) (rest begin)))
         (t (break "different prefixes in remove-rev-path"))))

; ***************************************************************************
; *** checks if a path is split in two parts such that one is the inverse of the other.
;     Returns the first half:
;     (a subclass-of b subclass-of c has-subclass b has-subclass a)) --->
;           (a subclass-of b subclass-of c)
(defun pathsymmetric (path halfleng plength)
  (let ((begpath (first-n plength path)))
 ; *** the first condition to avoid success in case of <a has-subclass b subclass-of a>
 ;     since this is already handled in "remove-upper-classes" that is applied
 ;     immediately after
   (cond ((and (not (all-has-subclass begpath))
               (equal (first-n halfleng (inv-range-dom (reverse begpath)))
                      (first-n halfleng begpath)))
            (first-n (1+ halfleng) begpath))
         (t nil))))

; ***************************************************************************
; *** returns true if all structural links in a path are "has-subclass"
(defun all-has-subclass (path)
  (cond ((null path) t)
        ((atom (remove-synt-pointer (first path)))
           (cond ((and (is-a-structural-item (first path))
                       (neq (first path) 'has-subclass))
                    nil)
                 (t (all-has-subclass (rest path)))))
        (t nil)))

; ***************************************************************************
; *** path contains the path to inspect
;     init is the beginning of the original path
;     revpath is the reversed original part until the part inspected
;     buff contains all results collected so far
(defun find-same-beg-start (path init revpath buff)
   (cond ((null path) (mapcar #'reverse (butlast buff)))
   ; *** butlast because buff also contain the singleton path (A), which is useless
         ((eq (remove-synt-pointer (first path)) init)
            (find-same-beg-start (rest path) init (cons (first path) revpath) 
                   (cons (cons (best-synt-conc init (first path)) revpath) buff)))
         (t (find-same-beg-start (rest path) init (cons (first path) revpath) buff))))

; ***************************************************************************
; *** returns true if path includes a non-relational concept
(defun include-non-relational (path)
  (let (firstp)
   (cond ((null path) nil)
         (t (setq firstp (remove-synt-pointer (first path)))
            (cond ((and (atom firstp)
                        (is-subclass-of firstp '££entity)) t)
                  (t (include-non-relational (rest path))))))))

; *******************************************************************
; *** If concept=conc-X and 
;        andlist= ( (range-of conc-1 ...) (subclass-of conc-2 domain-of ...) (domain conc-3 ...))
;     produces: ( (conc-X range-of conc-1 ...) 
;                 (conc-X subclass-of conc-2 domain-of ...) 
;                 (conc-X domain conc-3 ...))
(defun flatten-second-and (concept andlist)
  (mapcar #'(lambda (x) (cons concept x)) andlist))

; *******************************************************************
; *** takes the rest, unless path has length 1
;     This is useful for paths coming from a previous path such that its end is
;     <and <has-subclass CONCX> <...>>>
(defun non-single-rest (path)
   (cond ((eq 1 (length path)) path)
         (t (rest path))))

; *******************************************************************
; *** this is useful in combination with the previous one: if any of follpaths
;     has length 1, then it comes from  <and <has-subclass CONCX> <...>>>
;     In this case, follpaths is <CONCX <...>>, and CONCX must be used instead of
;     "item" (since it is a subclass of it)
(defun list-non-single (item follpaths)
  (let (found rempaths)
      (do ((nxtpath (first follpaths) (first follpaths))
           (follpaths (rest follpaths) (rest follpaths)))
          ((null nxtpath)
            (cond (found 
                    (cond ((eq 1 (length rempaths))
                              (cons found (first rempaths)))
                          (t (list found (list 'and rempaths)))))
                  (t (list item (list 'and rempaths)))))
          (cond ((eq 1 (length nxtpath)) (setq found (first nxtpath)))
                (t (setq rempaths (append1 rempaths nxtpath)))))))

; *******************************************************************
; *** if reprlist is (repr1 (and (repr21 repr22)) repr3)
;     it returns (repr1 repr21 repr22 repr3)
(defun remove-inner-ands (reprlist)
   (cond ((null reprlist) nil)
         ((or (atom (first reprlist)) 
              (atom (first (first reprlist))))
            (cons (first reprlist) (remove-inner-ands (rest reprlist))))
         ((eq 'and (first (first (first reprlist))))
            (append (second (first (first reprlist))) (remove-inner-ands (rest reprlist))))
         (t (cons (first reprlist) (remove-inner-ands (rest reprlist))))))

; *******************************************************************
(defun remove-up-down (ontorepr)
  (let ((firstitem (first ontorepr))
        (seconditem (second ontorepr))
        (thirditem (third ontorepr))
        (fourthitem (fourth ontorepr))
        andlist andresult)
    (cond ((null ontorepr) nil)
          ((atom firstitem)
            (cond ((is-a-structural-item firstitem)
                    (cons firstitem (remove-up-down (rest ontorepr))))
                  ((atom (remove-synt-pointer seconditem))
                    (cond ((eq 'subclass-of seconditem)
                            (cond ((atom (remove-synt-pointer thirditem))
                                    (cond ((atom (remove-synt-pointer fourthitem))
                                            (cond ((check-final-updown-path firstitem)
                                                    (remove-up-down (nthcdr 4 ontorepr)))
                                                  (t (cons firstitem
                                                        (cons seconditem
                                                            (remove-up-down (rest (rest ontorepr))))))))
               ; *** if the fourth item is not an atom, it could be an and, but also in this
               ;     case no simplification is possible, since the situation is
               ;     < conc1 subclass-of conc2 (and ((...) (...) ....)) ....>
               ;     even in case one of the operand of AND starts with <has-subclass conc1>, it
               ;     cannot be changed (the lower subclass must be maintained)
                                          (t (cons firstitem
                                                (cons seconditem
                                                      (remove-up-down (rest (rest ontorepr))))))))
               ; *** this is similar to the above. The situation would be:
               ;     < conc1 subclass-of (and ((conc2 ...) (conc3 ...) ...)) ...>
               ;     even if the start of, say, the first operand were (conc2 has-subclass conc1)
               ;     the only possibility would be to have something as 
               ;     < conc1 subclass-of (and ((conc1 ...) (conc3 ...) ...)) ...>
                                  (t (cons firstitem
                                         (cons seconditem (remove-up-down (rest (rest ontorepr))))))))
               ; *** the second item is not "subclass-of": no simplification
                          (t (cons firstitem
                                 (cons seconditem (remove-up-down (rest (rest ontorepr))))))))
               ; *** the second item is not an atom, but here something could be done:
               ;     < conc1 (and ((...) (...) ...)) ...>
               ;     If one of the operands of the and starts with 
               ;     <subclass-of conc2 has-subclass conc1>, this prefix can be removed
                  ((eq 'and (first seconditem))
                     (setq andlist (second seconditem))
                     (do ((nxtoperand (first andlist) (first andlist))
                          (andlist (rest andlist) (rest andlist)))
                         ((null nxtoperand)
                  ; *** and of the do on the operand of the and; the process continues, but the new
                  ;     and-expression replaces the old one
                           (cons firstitem
                              (cons (list 'and andresult)
                                  (remove-up-down (rest (rest ontorepr))))))
                         (cond ((and (atom (first nxtoperand))
                                     (eq 'subclass-of (first nxtoperand))
                                     (or (atom (second nxtoperand))
                                         (eq 'synt (first (second nxtoperand))))
                                     (check-final-updown-path (rest (rest nxtoperand)) firstitem))
                                 (setq andresult 
                                     (append1 andresult (remove-up-down (nthcdr 4 nxtoperand)))))
                               (t (setq andresult 
                                     (append1 andresult 
                                           (cons (first nxtoperand)
                                                 (remove-up-down (rest nxtoperand)))))))))
               ; *** it is not an and
                  (t (cons firstitem
                          (cons seconditem (remove-up-down (rest (rest ontorepr))))))))
   ; *** firstitem is not an atom
          ((eq 'and (first firstitem))
            (list 'and (mapcar #'remove-up-down (second firstitem))))
          (t (cons firstitem (cons seconditem (remove-up-down (rest (rest ontorepr)))))))))

; *******************************************************************
; *** this checks if a path starts with <has-subclass concept>
(defun check-final-updown-path (path concept)
  (and (eq 'has-subclass (first path))
       (eq (second path) concept)))


