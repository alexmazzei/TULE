(in-package "USER")

; ***************************************************************************
(defun build-sem-query (annotated-tree &optional retry)
; *** in general (and in the comments below), it is assumed that the interpretation 
;     is carried out inside a dialogue. So, there are two possible situations:
;     1. We are at the beginning of the dialogue, so that the first user (§partner)
;        input aims to clarify what the dialogue is about
;     2. We are inside a dialogue, so that it is now concerned with the
;        details of the user goals.
;     In specific domains, the "dialogue" can be one-way only, as in the case of
;     ATLAS (where the translation concerns messages addressed by the TV speaker
;     - §myself - to the public - §partner)
; *** the optional "retry" says if the "standard" interpretation must be
;     carried out. This happens when, at start-dialogue, the user's
;     input is a full query. In this case, the "dialogue topic"
;     interpretation fails, and build-sem-query is restarted disregarding
;     the context. A second case where "retry" is used is when the final
;     question about dialogue continuation was answered by the user directly
;     asking something. In this case, a "yes" value is assumed 
;     for the YES-NO-ANSWER parameter.
; ***
; &&&&&& In the case of actual (information request) dialogue
; &&&&&& The overall organization is as follows:
;       *DIALOGUE-CONTEXT*?
;       ---> +REQUEST+ (the user is issuing a request of some infos)
;            HEADSEM?
;            --> ££get-info +++ verb-indcompl-theme
;            --> ££want +++ verb+modal-indcompl +++ ££know
;                  +++ rmod(verb) +++ adv(interr)
;            --> ££can +++ verb+modal-indcompl +++ ££tell
;                  +++ verb-obj(verb) +++ adv(interr)
;       ---> meta-level (the user is talking about what the dialogue is about)
;            --> ££get-info +++ verb-indcompl-theme(--about-relation)
; &&&&&& In the case of one-way communication (from §myself to §partner)
; &&&&&& The *DIALOGUE-CONTEXT* is fixed to +PROVIDE-INFO+
; ***
; *** the basic idea is to move downwards, starting from the root, to
;     identify the relevant concepts and their restrictions
 (declare (special *DIALOGUE-CONTEXT* *ALL-DIAL-CONTEXTS* restr-dial-parameters
           *SYSTEM-CONTEXT* annotated-tree))
 (let (theme-subtree obj-subtree topic-subtree full-topic-subtree
       act-topic-subtree (+FULL-TREE+ annotated-tree) temp-subtree
       topics mult-default-infos restrictions semrestr head-ident 
       curr-context topic-changes newrestrs qtense-marker det-mean obj-det-mean)
  (declare (special +FULL-TREE+ topic-changes))
  (cond ((eq *SYSTEM-CONTEXT* 'hops-dial)
           (setq curr-context (get-top-block-name *DIALOGUE-CONTEXT*)))
      ; *** question and tense markers are the English modals. The goal is to reach the
      ;     top-level content word
        ((eq *SYSTEM-CONTEXT* 'atlas)
           (setq curr-context '+PROVIDE-INFO+)))
      ; *** the next applies to English, where questions can be marked by modals (do)
      ;     and the same applies to the future tense (will, shall)
      ; *** the possible values of qtense-marker are "eng-question-or-neg" (for "do")
      ;     and "eng-future" (for will, shall)
  (multiple-value-setq (annotated-tree qtense-marker)
      (skip-question-tense-marker annotated-tree))
  (let ((headsem (get-actavm-headlexmean annotated-tree))
        result-query obj-subtree-mean to-be-search)
      ; *** if the context is "hops", "tule", "tocai", or "atlas" no check on the
      ;     dialogue context: this is always the branch to follow
      ; *** The only case where this is not the right branch is when we are in the 
      ;     beginning of a real dialogue (context "hops-dial" and not dialogue
      ;     context = +START-DIALOGUE+ or +END-OF-DIALOGUE+ and not "retry"; for
      ;     "retry" see the comments at the beginning of this function)
    (cond ((or (memq *SYSTEM-CONTEXT* '(hops tule tocai atlas))
               retry
               (and (memq curr-context (cons '+REQUEST+ *ALL-DIAL-CONTEXTS*))
            ; *** the next condition is removed in order to allow for an easier test
            ;     of the HOPS NLPP. In this way, the behavior of the hops-dial version
            ;     and the hops version should be the same
                 ;   (not (memq curr-context 
                 ;             '(+START-DIALOGUE+ +ASK-END-OF-DIALOGUE+)))
               ))
      ; *** ££get-info *******************************************************
      ;    "biglietteria settembre musica" 
      ;    The ££get-info node has been added when a head noun was encountered
      ;    The 'verb-indcompl-theme' link connects it with the noun (ex. biglietteria)
      ;    Interpreted as "give me the 'default-info' about 'ticket-counters' where
      ;    the 'ticket-counter' is 'of' the modifiers of 'biglietteria'.
             (cond ((eq '££get-info headsem)
                      (setq theme-subtree
                           (find-actavm-dep 'VERB-INDCOMPL-THEME annotated-tree))
                      (cond ((null theme-subtree)
            ; *** this could happen in case ££get-info comes from "looking for"
                               (multiple-value-setq (theme-subtree det-mean)
                                 (skip-determiner
                                   (find-actavm-dep 'VERB-OBJ annotated-tree)))))
                      (cond ((null theme-subtree)
            ; *** this is for "speak to someone about"
                               (setq theme-subtree
                                     (find-actavm-descendant 
                                        '((link lexmean))
                                        '((rmod --about-relation))
                                        (get-actavm-deps-with-traces annotated-tree)))))
                      (cond ((null theme-subtree)
                               (exception 'semantic-error
                                        "PROC/buildquery: no obj subtree for ££get-info *** 2 ***"))
                            ((eq '££information (get-actavm-headlexmean theme-subtree))
                 ; *** if this is the case, go down, assuming that the input is "information about"
                 ;     this covers also "information related to", and returns the subtree
                 ;     governed by "about" or "to"
                               (setq theme-subtree
                                   (find-information-topic theme-subtree)))))
      ; *** ££want ££know ****************************************************
      ;    "vorrei sapere dove è la biglietteria di settembre musica" 
      ;    It assumes that the 'verb-object' of "to tell" is a verb-headed group
      ;    and that the verb governs a question adverb
      ;    The question adverb identifies what is asked, while the other complements
      ;    identify the search conditions
                   ((eq '££want headsem)	; I want to know ...
                     (setq obj-subtree
      ; *** "Vorrei andare ...", "I want to ...", "pretendo di sapere" ...
      ;     get-sentential-object defined in seminterp
                           (get-sentential-object annotated-tree))
                     (cond ((null obj-subtree)
                             (multiple-value-setq (theme-subtree det-mean)
      ; *** "Vorrei un'informazione  ...", "I want an information ...",
                                 (skip-determiner
                                     (get-standard-object annotated-tree)))
                             (cond ((and (eq 'PRON (get-actavm-headcateg theme-subtree))
                                         (eq 1 (get-actavm-headperson theme-subtree)))
      ; *** this is the case of "mi servono informazioni", where "mi" is
      ;     (wrongly?) labelled as verb-obj, so we must take the verb-subj
      ; *** Now, theme-subtree could be headed by "information"
                                      (setq theme-subtree 
                                            (find-actavm-dep 'VERB-SUBJ annotated-tree))))
                             (cond ((eq '££information
                                       (get-actavm-headlexmean theme-subtree))
      ; *** If this is the case, go down two levels, assuming that the input is
      ;     "information about"
                                     (setq theme-subtree
                                           (find-information-topic theme-subtree)))))
                           ((eq '££know (get-actavm-headlexmean obj-subtree))
                             (setq theme-subtree (find-know-theme obj-subtree)))
      ; *** I would like to get information ...
                           ((eq '££obtain (get-actavm-headlexmean obj-subtree))
                             (multiple-value-setq (theme-subtree obj-det-mean)
                                 (skip-determiner (find-actavm-dep 'verb-obj obj-subtree)))
                             (cond ((eq '££information (get-actavm-headlexmean theme-subtree))
                                     (setq theme-subtree
                                           (find-information-topic theme-subtree)))))
      ; *** the next for "print it", translated as "the user wants that the system prints it"
                           ((or (one-is-subclass-of (get-actavm-headlexmean obj-subtree)
                                             '££system-operation)
                                (one-is-subclass-of (get-actavm-headlexmean obj-subtree)
                                             '££external-service))
                             (setq theme-subtree obj-subtree))
                           (t (exception 'semantic-error
                                        "PROC/buildquery: Unknown argument for ££want"
                                        (get-actavm-headlexmean obj-subtree))))
                     (cond ((null theme-subtree)
                              (exception 'semantic-error "PROC/buildquery: No theme subtree"))
                           ((eq '££possess (get-actavm-headlexmean theme-subtree))
              ; *** if "I want to know if you have information ..."
              ;     theme-subtree is "have information ..."
                              (setq temp-subtree
                                     (find-actavm-dep 'verb-obj theme-subtree))
                              (cond ((eq '££information
                                         (get-actavm-headlexmean temp-subtree))
                                       (setq theme-subtree
                                           (find-information-topic temp-subtree)))))
                           ((eq '££can (get-actavm-headlexmean theme-subtree))
              ; *** if "I want to know where I can buy ....", then the
              ;     theme-subtree is "I buy ..."
              ; *** Note that the "where" adv should have been moved below "buy"
                               (setq theme-subtree
                                     (find-actavm-dep 'verb+modal-indcompl theme-subtree)))))
      ; *** ££can ££tell *****************************************************
      ;    "puoi dirmi il titolo?"
      ;    "puoi dirmi dove è la biglietteria di settembre musica" 
      ;    It takes the 'verb-object' of "to tell"
      ;    - if is a verb-headed group, it assumes that the verb governs a question adverb
      ;      The question adverb identifies what is asked, while the other complements
      ;      identify the search conditions
      ;    - If it is a nominal group, it assumes it is the topic
                   ((eq '££can headsem)		; can you tell me
                     (setq obj-subtree
                           (find-actavm-dep 'verb+modal-indcompl annotated-tree))
                     (setq obj-subtree-mean (get-actavm-headlexmean obj-subtree))
                     (cond ((eq obj-subtree-mean '££tell)
              ; *** look for the object of tell
                             (setq theme-subtree (find-actavm-dep 'VERB-OBJ obj-subtree))
                             (cond ((eq (get-actavm-headlexmean theme-subtree) '££can)
                     ; *** can you tell me how I can ...
                                       (setq theme-subtree 
                                            (find-actavm-dep 'VERB+MODAL-INDCOMPL theme-subtree)))))
                           ((eq obj-subtree-mean '££occur)
              ; *** THIS is a big patch: In italian, I interpret "dare" as "occur"
              ;     for "dove danno lo spettacolo?"
                              (setq temp-subtree
                                     (find-actavm-dep 'VERB-OBJ obj-subtree))
                              (cond ((eq '££information (get-actavm-headlexmean temp-subtree))
                                       (setq theme-subtree
                                           (find-information-topic temp-subtree)))))
                           ((eq obj-subtree-mean '££get-info)
                ; *** In this case, instead of having the standard processing,
                ;     the result is found via a recursive call
                             (setq result-query 
                                 (int-build-sem-query obj-subtree retry)))
                           ((or (one-is-subclass-of obj-subtree-mean '££system-operation)
                                (one-is-subclass-of obj-subtree-mean '££external-service))   ; ex. ££collect
                             (setq theme-subtree obj-subtree))
                           (t (exception 'semantic-error
                                         "PROC/buildquery: Unknown argument for ££can")))
                     (cond ((and (null theme-subtree)
                                 (null result-query))
                              (exception 'semantic-error
                                    "PROC/buildquery: No theme subtree for ££can"))))
      ; *** ££to-be *****************************************************
      ;    "Are there cabaret events?"
      ;    "dove è la biglietteria di settembre musica" 
      ;    It takes the full annotated tree
                   ((eq headsem '££to-be)
                     (cond ((eq curr-context '+PROVIDE-INFO+)
                                  ; *** vediamo quali sono le previsioni
                                  ; *** quella di oggi è una situazione di transizione
                                  ; *** ci saranno correnti settentrionali
                                  ; *** i mari saranno mossi
                                  ; *** è nuvoloso sulla pianura
                                  ; *** al sud le nuvole sono poche
                                  ; *** il nord sarà all'insegna di un miglioramento
                                  ; *** ci sarà un graduale peggioramento
                                  ; *** X sarà senza precipitazioni
                                  ; *** la colonnina di mercurio sarà in risalita
                                  ; *** sull'Appennino ci sarà qualche annuvolamento
                                  ; *** E' tutto
                             )
        ; *** we are in the ATLAS context, or we are answering a question
                           (t (let ((prep-case 			; I'm after rock concerts
                                     (find-actavm-descendant 
                                          '((link lexmean))
                                          '((rmod --after-relation))
                                    (get-actavm-deps-with-traces annotated-tree))))
                               (cond ((null prep-case)
                                        (setq theme-subtree annotated-tree))
                                     (t (setq to-be-search t)
                                       ; *** the variable to-be-search is used below to determine
                                       ;     if the verb "to be" is used for "look for" (in
                                       ;     "to be after")
                                        (setq theme-subtree 
                                           (skip-determiner
                                              (find-actavm-dep 'prep-arg prep-case)))))))))
                   ((eq headsem '££put)		; ho trovato (buttato) una poltrona
                      (setq theme-subtree annotated-tree)))
; ********************************************************************************************
; *** Now, the theme has been found for ££get-info, ££can, ££tell, ££to-be *******************
; *** and ££want+££know
             (cond ((not (null result-query)) result-query)
    ; *** the branch above concerns the recursive call occurred with ££can+££get-info
                   ((eq headsem '££give-info)		; "al Regio" (answer to system)
    ; *** ££give-info is the only case where we get no topic, but just an obj-subtree
                     (setq obj-subtree (find-actavm-dep 'VERB-INDCOMPL-THEME annotated-tree))
              ; *** look for the provided information
                     (cond ((null obj-subtree)
                              (setq obj-subtree
              ; *** look for the provided information
                                 (find-actavm-dep 'VERB-OBJ annotated-tree)))))
                   ((eq headsem '££see)				; "vediamo qual è la situazione"
                     (setq obj-subtree (find-actavm-dep 'VERB-OBJ annotated-tree))
                     (cond ((null obj-subtree)
                              (exception 'semantic-error "PROC/buildquery: No obj subtree for ££see"))
                           (t (setq result-query (int-build-sem-query obj-subtree)))))
   ; *** otherwise, use the theme for identifying topic and restrictions
                   ((memq headsem '(££get-info ££want ££can ££to-be ££put))
                      (multiple-value-setq 
                            (topic-subtree full-topic-subtree topics restrictions)
                        (find-topic-subtree theme-subtree))
                      (setq restrictions (mapcar #'(lambda (x) (list (list x) 'top)) restrictions))
                      (cond ((eq (get-actavm-headlexmean theme-subtree)
                                 '££to-be)   ; Per quali item ci sono sostituzioni?
                              (setq restrictions 
                                   (append restrictions
                                         (get-verb-restrictions theme-subtree full-topic-subtree)))
                              (break "build-sem-query: get-verb-restrictions 1")
                            ))
                      (cond ((or (eq headsem '££get-info)
                                 (and (eq headsem '££to-be)
                                      to-be-search)
                                 (and (eq headsem '££want)
                                      (eq '££information
                                          (get-actavm-headlexmean
                                               (find-actavm-dep 'VERB-OBJ annotated-tree)))))
                               ; *** the first disjunct of this branch aims at coping with
                               ;     a syntax error. In case of "I am looking for events
                               ;     tomorrow", "tomorrow" is attached to "looking", instead 
                               ;     of to "events". The grammar cannot be changed, since an
                               ;     adverb must, in general, be attached to a verb rather than
                               ;     to a noun. The solution is to consider as restrictions also
                               ;     the modifiers of the main verb.
                               ; *** the second disjunct aims at coping with "to be" used for
                               ;     "looking for", as in "I'm after ..."
                               ; *** the third disjunct for "I want information about ... for ..."
                               ;     where "for ..." is attached to "want"
                              (setq restrictions 
                                (append restrictions
                                 (get-verb-restrictions
                                         annotated-tree full-topic-subtree)))
                              (break "build-sem-query: get-verb-restrictions 2")
                             )))
    ; **** THE NEXT BRANCH REMOVED, SINCE IN THE MENTIONED CASE headsem	IS &give-info *********
       ;            ((eq headsem '££to-have)
        ; *** ATLAS: for "... we have X " the interpretation is the same as "... is X"
        ;            or "... there is X"
        ;            "Today we have last day of june" --> "Today is last day of June"
       ;               (multiple-value-setq 
       ;                     (topic-subtree full-topic-subtree restrictions)
       ;                 (find-topic-subtree annotated-tree))
        ; *** ATLAS: for "to have", find topic subtree returns the "topic" described by the
        ;     VERB-OBJ (i.e. "last day ...")
       ;              (setq restrictions 
       ;                   (append restrictions
       ;                         (get-verb-restrictions annotated-tree full-topic-subtree)))
       ;               (break "build-sem-query: get-verb-restrictions 3")
       ;                )
    ; **** END OF REMOVED BRANCH FOT ££to-have **************************************************
                   ((one-is-subclass-of headsem '££system-operation)
                      (multiple-value-setq 
                            (topic-subtree full-topic-subtree topics restrictions)
                        (find-topic-subtree annotated-tree))
                      (setq restrictions (mapcar #'(lambda (x) (list (list x) 'top)) restrictions)))
                   (t (exception 'semantic-error "PROC/buildquery: Undefined top-level operator")))
 ; ********************************************************************************************
 ; *** end of the analysis of the tree and of the collection of information from its structure
 ;     In general, we now have in topic-subtree the goal of the query and in restrictions the
 ;     constraints ("where" part) for the selection of the relevant items
 ; *** now, actual start of query construction
   ; *** case 1: the branch below concerns the recursive call occurred with ££can+££get-info
             (cond ((not (null result-query)) result-query)
   ; *** case 2: ££give-info: we don't have the topic, but an obj-subtree (see above)
                   ((eq '££give-info headsem)
                      (setq semrestr 
                            (first (build-restr-sem 'TOP '££DIALOGUE 0 nil
                                      (list (list (find-obj-all-conjuncts obj-subtree) 'top)) nil t)))
            ; *** the first list, since this is a list of restrictions; the second list to put
            ;     together the "actual restrictions" and 'top
                      (final-build-givinfo '££DIALOGUE semrestr))
   ; *** case 3: requests to the system (includes also "list something", interpreted as "print")
                   ((and (not (null theme-subtree))
                         (or (one-is-subclass-of (get-actavm-headlexmean theme-subtree)
                                          '££system-operation)
                             (one-is-subclass-of (get-actavm-headlexmean theme-subtree)
                                          '££external-service)))
                      (setq semrestr 
                            (first (build-restr-sem 'TOP '££DIALOGUE 0 nil
                                               (list (list (list theme-subtree) 'top)))))
                      (final-build-givinfo '££DIALOGUE semrestr))
   ; *** case 4: error: the topic subtree is missing; since not ££give-info, a topic-subtree
   ;             had to be found before
                   ((null topic-subtree)    
                      (exception 'semantic-error "PROC/buildquery: No topic subtree found"))
   ; *** case 5: standard processing (e.g. for queries: get-info)********************
                   (t 
      ; *** the next cond sets the variable head-number **********************************
      ; *** REMOVED AFTER INSERTION OF LINK TO SYNTAX ************************************
       ;              (cond ((and (eq 'NOUN (get-actavm-headcateg topic-subtree))
       ;                          (neq 'proper (get-actavm-headtype topic-subtree)))
       ;                       (setq head-number (get-actavm-headnumber topic-subtree)))
       ;                    ((eq 'PRON (get-actavm-headcateg topic-subtree))
         ; *** for interrogative pronouns, even if they are singular, it is assumed that
         ;     the answer can be a list, so that the number is forced to 'pl
       ;                       (cond ((eq 'interr (get-actavm-headtype topic-subtree))
       ;                                (setq head-number 'pl))
       ;                             (t (setq head-number 
       ;                                      (get-actavm-headnumber topic-subtree)))))
       ;                    (t (setq head-number nil)))
      ; *** the next fragment refers to topics that are proper names *********************
      ; *** currently this happens just in case the sentence is a single identifier,
      ;     as "Settembre Musica", so the topic also includes the identifier restriction
                     (setq head-ident (get-actavm-headlexident topic-subtree))
                     (cond ((not (null head-ident))
                              (setq restrictions (cons (list (list topic-subtree) 'top) restrictions))))
   ; *** "restrictions" is a list of lists, each of which inclcudes all the subtrees that
   ;     are conjuncts of one dependent of the element being interpreted. The last parameter
   ;     (head-number) is not nil, in case the topic is associated to a common noun or a pronoun
              ; (format t " Topics: ~a~% " topics)
              ; (format t " Topics: ~a~% Topic subtree: ~a~% Restrictions: ~a~%"
              ;            topics topic-subtree restrictions)
              ; (break "")
                     (setq semrestr 
                          (first (build-restr-sem 'TOP topics
                                      (get-actavm-headnumb topic-subtree)
                                      topic-subtree restrictions)))
                     (setq mult-default-infos (mapcar #'get-default-infos topics))
   ; *** "semrestr" is a list of paths on the ontology; one for each
   ;     syntactic restriction 
                     (final-build-query mult-default-infos semrestr))))
; .... FROM NOW TO THE END: HOPS ............................................
        ((and (eq *SYSTEM-CONTEXT* 'hops-dial)
              (eq curr-context '+START-DIALOGUE+))
   ; *** at +START-DIALOGUE+ the topic of the dialogue is the goal of the
   ;     dialogue itself, and not a domain concept; in fact, when the system
   ;     starts the dialogue, it tries to determine if the user wants some
   ;     specific or generic info
      ; *** ££get-info ......................................................
      ;    "su un concerto"
      ;    "vorrei informazioni sugli spettacoli di domani"
      ;    The ££get-info node has been added when a head noun or prep was
      ;    encountered
      ;    The 'verb-indcompl-theme' link connects it with the "su" preposition
      ;    In this dialogue context, it is interpreted as "I want to talk about X"
             (cond ((eq '££get-info headsem)
                      (setq topic-subtree
                           (find-actavm-dep 'VERB-INDCOMPL-THEME annotated-tree))
                      (cond ((null topic-subtree)
            ; *** this could happen in case ££get-info comes from "looking for"
                               (setq act-topic-subtree
                                   (find-actavm-dep 'VERB-OBJ annotated-tree)))
                            ((memq (get-actavm-headlexmean topic-subtree)
                               '(--dummy-prep --about-relation))
      ; *** the sentence is "informazioni su xxx" or "su xxx": the useful topic
      ;     is not "su", but "xxx" or "interested in"
         ; *** in act-topic-subtree the noun (or pronoun) governed by the
         ;     preposition (skipping possible determiners)
                             (setq act-topic-subtree 
                                  (skip-determiner 
                                       (find-actavm-dep 'PREP-ARG topic-subtree))))
                            (t (exception 'semantic-error
                                   "PROC/buildquery: Unknown preposition in build-sem-query"
                                   (get-actavm-headlemma topic-subtree))))
                      (setq head-ident 
                           (get-actavm-headlexident act-topic-subtree))
         ; *** if the topic is a common noun, get its syntactic number
         ;             (cond ((and (eq 'NOUN (get-actavm-headcateg act-topic-subtree))
         ;                         (neq 'PROPER (get-actavm-headtype act-topic-subtree)))
         ;                     (setq head-number (get-actavm-headnumber act-topic-subtree))))
                      (setq restrictions 
                           (cons act-topic-subtree 
                                 (get-verb-restrictions 
                                       annotated-tree act-topic-subtree)))
                       (break "build-sem-query: get-verb-restrictions 4")
                      (setq semrestr (first (build-restr-sem 'TOP '£DIALOGUE 0 nil restrictions)))
                      (final-build-givinfo (list '££DIALOGUE) semrestr))
                   ((eq '££see headsem)
     ; *** "I've seen the poster of ..."
                      (setq obj-subtree
                         (skip-determiner
                            (find-actavm-dep 'VERB-OBJ annotated-tree)))
                      (cond ((is-subclass-of (get-actavm-headlexmean obj-subtree)
                                             '££advertisement)
                              (setq theme-subtree
                                   (find-actavm-dep 'PREP-ARG 
                                          (find-actavm-dep 'PREP-RMOD obj-subtree))))
                            (t (setq theme-subtree obj-subtree)))
                      (cond ((null theme-subtree)
                               (exception 'semantic-error
                                    "PROC/buildquery: Unknown argument for ££see")))
                      (setq restrictions (list (list theme-subtree)))
                      (setq semrestr (first (build-restr-sem 'TOP '££DIALOGUE 0 nil restrictions)))
                      (final-build-givinfo '££DIALOGUE semrestr))
     ; *** in case the form of the form of the input is not as expected
     ;     at start of dialogue, perhaps the user has directly posited
     ;     the question (user initiative), so try a full interpretation
                   (t (int-build-sem-query annotated-tree t))))
        ((and (eq *SYSTEM-CONTEXT* 'hops-dial)
              (eq curr-context '+ASK-END-OF-DIALOGUE+))
          (cond ((eq '££give-info headsem)	; "yes" or "no"
                  (setq obj-subtree
              ; *** look for the "yes" or "no" subtree
                        (find-actavm-descendant
                                '((link cat))
                                '((VERB-OBJ phras))
                                     (get-actavm-deps-with-traces annotated-tree)))
                  (cond ((null obj-subtree)
                          (setq obj-subtree
                 ; *** look for an adverbial argument (es. "tomorrow")
                            (find-actavm-descendant
                                '((link cat))
                                '((VERB-OBJ adv))
                                     (get-actavm-deps-with-traces annotated-tree)))))
                  (setq head-ident (get-actavm-headlexident obj-subtree))
                  (cond ((not (null head-ident))
                           (setq restrictions (list (list obj-subtree))))
                        (t (exception 'semantic-error
                                "PROC/buildquery: Problems in end of dialogue")))
                  (setq semrestr (first (build-restr-sem 'TOP '££DIALOGUE 0 nil restrictions)))
                  (cond ((and (listp semrestr) (eq 1 (length semrestr)))
                           (setq semrestr (first semrestr))))
                  (final-build-givinfo '(££DIALOGUE) semrestr))
                (t (int-build-sem-query annotated-tree t))))
    ; *** in this case, I assume that the final yes/no anwer about the
    ;     continuation of the dialogue has been skipped by the user, who
    ;     has directly asked something new; so a "yes" answer to this
    ;     question was understood
    ; *** the desired behavior is obtained by forcing an (YES-NO-ANSWER yes)
    ;     as a parameter value, and then going on with the analysis
        (t (exception 'semantic-error
                 "PROC/buildquery: Undefined dialogue context in build-sem-query"))))))

; ***************************************************************************
(defun apply-topic-changes (topics topic-changes)
   (dolist (nxtchange topic-changes topics)
       (setq topics (substitute (second nxtchange) (first nxtchange) topics))))

; ***************************************************************************
; *** This looks for the subtree describing the second argument of "££know"
;     (what one wants to know)
(defun find-know-theme (obj-subtree)
   (let ((theme
              ; *** look for the verb object
              ;     I want to know "where IS ..."
           (skip-question-tense-marker (find-actavm-dep 'VERB-OBJ obj-subtree))))
        (cond ((null theme)
                (setq theme
              ; *** look for a sentence governed by "if": "I want to know if"
                    (find-actavm-descendant
                           '((link cat))
                           '((VERB-OBJ conj))
                           (get-actavm-deps-with-traces obj-subtree)))
                (cond ((not (null theme))
                        (setq theme
                          ; *** go down another level
                               (skip-question-tense-marker
                                     (find-actavm-descendant
                                            '((link cat))
                                            '((CONJ-ARG verb))
                                            (get-actavm-deps-with-traces theme))))))))
        theme))

; ***************************************************************************
; *** This looks for the subtree describing the topic about which a piece of
;     information is looked for;
; *** it inspects all dependents of the node whose meaning is ££information
;     and looks for a (prep-rmod --about-relation) dependent (i.e. "about",
;     "on") or for a (adjc-rmod &related-to) dependent (i.e. "relative a")
(defun find-information-topic (info-subtree)
   (let ((info-dependents (get-actavm-deps-with-traces info-subtree))
         info-topic)
       (do ((nxtdep (first info-dependents) (first info-dependents))
            (info-dependents (rest info-dependents) (rest info-dependents)))
           ((or (null nxtdep) (not (null info-topic)))
              info-topic)
           (cond ((equal nxtdep '(#\#)) nil)
                 ((and (eq (get-actavm-headlink nxtdep) 'PREP-RMOD)
		       (eq (get-actavm-headlexmean nxtdep) '--about-relation))
                    (setq info-topic (find-actavm-dep 'PREP-ARG nxtdep)))
                 ((and (eq (get-actavm-headlink nxtdep) 'ADJC+QUALIF-RMOD)
		       (eq (get-actavm-headlexmean nxtdep) '&related-to))
                    (setq info-topic (find-actavm-dep 'ADJC-ARG nxtdep)))))))

; ***************************************************************************
; *** Given a tree which is the argument of query, it returns four values:
;     - the subtree (complement or adjunct) which acts as topic of the
;       input tree, 
;     - the full subtree of which this topic is a part (they are different in
;       case the head of the actual topic is an article or a question adjective),
;     - the topics, i.e. the items that describe what is under consideration
;       without their restrictions (i.e. in "the name and address of the owner",
;       the topics are the ontology concepts of "name" and "address")
;     - a possible set of restrictions extracted from inside that
;       subtree. For instance, it is assumed that in NP's, the actual topic is
;       just the NP head, while all dependents are restrictions.
; *** e.g. for "I want to know where I can buy ..."
;          1. the "where" subtree
;          2. the "where" subtree
;          3. (££location)
;          4. nil
;     while for "Which concerts directed by Abbado are there tomorrow?"
;          1. the "concerts directed by Abbado" subtree
;          2. the "which concerts directed by Abbado" subtree
;          3. (££concerts)
;          4. the "directed by Abbado" subtree
;     Note that the restriction carried by "tomorrow" is handled outside this function
;     for "can you tell me the place and time of the concerts directed by Abbado?"
;          1. the "place and time of the concerts directed by Abbado" subtree
;          2. the "the place and time of the concerts directed by Abbado" subtree
;          3. (££location ££startTime)
;          4. the "concerts directed by Abbado" subtree
(defun find-topic-subtree (tree)
  (declare (special *LANGUAGE*))
  (let (topic-subtree restrictions act-topic-subtree temp-subtree temp-deps topics
        (treeheadsem (get-actavm-headlexmean tree)) 
        (treeheadcat (get-actavm-headcateg tree)) 
        (treedeps (get-actavm-deps-with-traces tree)) 
        prep-subtree prep-mean other-topic-subtree topic-categ)
   (cond ((and (eq 'VERB treeheadcat) 
               (eq '££can treeheadsem))			; ----------------- ££can
         ; *** can you tell me how I can arrange
         ;     here, we are working now on "can arrange ...", but the topic must 
         ;     be looked for in "arrange ..."
            (find-topic-subtree (find-actavm-dep 'VERB+MODAL-INDCOMPL tree)))
         ((and (eq 'VERB treeheadcat) 
               (eq '££to-be treeheadsem)
               (eq '££entity (get-actavm-headlexmean (find-actavm-dep 'VERB-PREDCOMPL+SUBJ tree))))
         ; *** "which are the items ...?"
         ;     the meaning of "which" is "££entity"; the actual topic are "items ..."
            (find-topic-subtree (find-actavm-dep 'VERB-SUBJ tree)))
         ((and (eq 'VERB treeheadcat) 
               (eq '££to-have treeheadsem))
         ; *** today, we have some rain (ATLAS)
            (find-topic-subtree (find-actavm-dep 'VERB-OBJ tree)))
         (t 
; ...........................................................................................
; ... in this last case, the work is made in two steps: first look for the full topic subtree
; ... then split it into topics and restrictions
         ; *** the head category of the input tree is a preposition *************************
            (cond ((and (eq 'PREP treeheadcat)			; --- PREP: about or dummy
                        (memq treeheadsem '(--about-relation --dummy-prep)))
               ; *** --about-relation for "information on"
               ; *** --dummy-prep for "interested in"
               ; ***** the tree refers to "about something". "something is the topic, while
               ;       the restrictions are empty
               ; ***** get-preposition-arg in "seminterp"
                    (setq topic-subtree (skip-determiner (get-preposition-arg tree))))
         ; *** the head category of the input tree is a conjunction *************************
                  ((and (eq 'CONJ treeheadcat) (eq '££manner treeheadsem))	; - CONJ: manner
               ; *** in "can you tell me how ..", "how" can be interpreted as a conjunction
                    (setq topic-subtree tree))
                  (t (setq topic-subtree (find-interr-adv treedeps))
                     (cond ((and (not (null topic-subtree))
                                ; *** ADVERBIAL QUESTION ELEMENT ********************
                                 (eq '£manner (get-actavm-headlexmean topic-subtree)))
                             (setq topic-subtree tree)))
                     (cond ((null topic-subtree)              ; ... not adv QE ........
                                ; *** try PRONOMINAL question elements
                             (setq topic-subtree (find-interr-pron treedeps))))
                     (cond ((and (not (null topic-subtree))
                                ; *** Which are the ...?
                                ;     In this case, the topic is not "which", but the subject
                                ;     of "to be"
                                 (eq '--q-pron (get-actavm-headlexmean topic-subtree))
                                 (eq '££to-be treeheadsem))
                             (setq other-topic-subtree (find-actavm-dep 'VERB-SUBJ tree))
                             (cond ((or (null other-topic-subtree)
                                        (equal topic-subtree other-topic-subtree))     
                                      (setq topic-subtree
                                                   (find-actavm-dep 'VERB-PREDCOMPL+SUBJ tree)))
                                   (t (setq topic-subtree other-topic-subtree)))))
                     (cond ((null topic-subtree)              ; ... not adv or pron QE.......
                                ; *** try ADJ+ADV (how long?) question elements
                              (setq topic-subtree (find-interr-adj+adv treedeps))))
                     (cond ((null topic-subtree)            ; ... not adv or pron or adj+adv QE
                                ; *** try ADJECTIVAL question elements
                                ; *** Which concerts are there tomorrow?
                                ; **** N.B. This works for "which", but not for "for which"
                              (setq topic-subtree (find-interr-adjec treedeps))))
                     (cond ((null topic-subtree)
                              ; *** the topic subtree is still empty for "Vorrei un'informazione"
                              ;     (I want an information) or "Ci sono concerti di Abbado?"
                              ;     (are there concerts directed by Abbado?)
                             (cond ((memq treeheadcat '(ART NOUN))	   
                                       ; --- nominal root of the tree
                                       ;     if "tree" is "an information" (first example above),
                                       ;     it is already the topic
                                     (setq topic-subtree tree))
                               ; *** but if the head of the tree is ££to-be, then the sentence
                               ;     is a yes/no question. However, in some cases, it is an
                               ;     implicit request for infos
                               ;     It is not cooperative to answer to "Are there concerts
                               ;     conducted by Abbado" with "yes". In these cases, some extra
                               ;     info must be provided, as if the question were "Which
                               ;     concerts ..."
                               ; *** Currently, only "Are there ..." is handled
                               ;     ££to-have for Catalan "Hi ha ..."
                               ;     ££to-have without loc-metaph for Spanish "Hay algun events ..."
                                   ((or (and (memq *LANGUAGE* '(italian english)) ; --- yes/no question
                                             (eq treeheadsem '££to-be)
                                             (not (null (find-actavm-dep 'PRON-RMOD-LOC+METAPH tree))))
                                        (and (eq *LANGUAGE* 'catalan)
                                             (memq treeheadsem '(££to-be '££to-have))
                                             (not (null (find-actavm-dep 'PRON-RMOD-LOC+METAPH tree))))
                                        (and (eq *LANGUAGE* 'spanish)
                                             (memq treeheadsem '(££to-be '££to-have))))
                                     (setq topic-subtree (find-actavm-dep 'VERB-SUBJ tree)))
                                   ((and (eq '££tell treeheadsem)	     ; --- the System "tells"
                                         (member 
                                            (get-actavm-headlexmean (find-actavm-dep 'VERB-SUBJ tree))
                                            '(§myself nil)))
                              ; *** NIL because in the polite third person ("Puo' dirmi ..."),
                              ;     the system is currently not able to recover itself as the
                              ;     addressee
                                     (setq topic-subtree (find-actavm-dep 'VERB-OBJ tree)))
                                   ((and (eq '££know treeheadsem)  	     ; --- the System "knows"
                                         (member (get-actavm-headlexmean (find-actavm-dep 'VERB-SUBJ tree))
                                            '(§myself nil)))
                                     (setq topic-subtree (find-actavm-dep 'VERB-OBJ tree)))
                              ; *** the next for "can you give me some information ..."
                                   ((and (eq '££give treeheadsem)	     ; --- the System "gives"
                                         (member (get-actavm-headlexmean (find-actavm-dep 'VERB-SUBJ tree))
                                             '(§myself nil)))
                                      (setq temp-subtree 
                                              (skip-determiner (find-actavm-dep 'VERB-OBJ tree)))
                                      (cond ((eq (get-actavm-headlexmean temp-subtree) '££information)
                                              (setq temp-deps 
                                                 (remove '(#\#) 
                                                   (get-actavm-deps-with-traces temp-subtree)))
                                              (cond ((null temp-deps)
                                     ; *** the word "information" without modifiers
                                     ;     possibly, the sentence is "give information for ..."
                                     ;     where "for" is attached to the main verb
                                                      (setq temp-subtree (find-actavm-dep 'RMOD tree)))
                                            (t (setq prep-subtree 
                                                   (find-information-topic temp-subtree))
                                     ; *** otherwise there must be a preposititonal modifier
                                     ;     "about" or "on" (but this does not cover "information
                                     ;     concerning" or "related to")
                                               (cond ((not (null prep-subtree))
                                                       (setq prep-mean (get-actavm-headlexmean prep-subtree))
                                                       (cond ((memq prep-mean 
                                                                 '(--about-relation --on-relation))
                                                               (setq topic-subtree prep-subtree))
                                                             (t (exception 'semantic-error
                                                                   "PROC/buildquery: 'information' with a wrong preposition"))))
                                                     (t (exception 'semantic-error
                                                           "PROC/buildquery: 'information' with modifiers but no preposition"))))))
                                     ; *** currently, the only thing that the system can give is 
                                     ;     information
                                    (t (exception 'semantic-error
                                               "PROC/buildquery: asking the system to give something which is not information")))))))
                 ; *** now, we must have in topic-subtree the full topic; *********************
                 ;     but possible prepositions and/or determiners must be skipped
                 ; *** except in case the head of act-topic-subtree is a noun, the
                 ;     (local) restrictions are empty (nil)
                     (setq topic-categ (get-actavm-headcateg topic-subtree))
                     (cond ((eq topic-categ 'PREP)
                              (setq act-topic-subtree 
                                  (skip-determiner
                                         (get-preposition-arg topic-subtree)))
                              (multiple-value-setq (topics restrictions)
                                       (get-nonverbal-restrictions act-topic-subtree t)))
                           ((eq topic-categ 'VERB)
                              (setq act-topic-subtree (skip-determiner topic-subtree))
                              (setq restrictions (get-verb-restrictions act-topic-subtree nil))
                             (break "find-topic-subtree: get-verb-restrictions 1")
                               )
                           ((and (eq topic-categ 'CONJ)
                                 (eq (get-actavm-headtype topic-subtree) 'subord))
                              (setq act-topic-subtree topic-subtree)
                              (setq topics (list (get-actavm-headlexmean tree)) )
                              (setq restrictions (list (find-actavm-dep 'CONJ-ARG topic-subtree))))
                           (t (setq act-topic-subtree (skip-determiner topic-subtree))
                              (multiple-value-setq (topics restrictions)
                                       (get-nonverbal-restrictions act-topic-subtree t))))
                    (values act-topic-subtree topic-subtree topics restrictions)))))))

; ***************************************************************************
; *** looks for a question adverb, among the dependents of a verb
(defun find-interr-adv (deps)
   (let ((qadv (find-actavm-descendant
     ; *** there is no constraint on the link, since this works both
     ;     for adverbial complements (I want to know "where" is ...)
     ;     and for adverbial adjuncts (I want to know where I can buy ...)
     ; **** N.B. This works for "where", but not for "from where"
                        '((cat type))
                        '((adv interr))
                        deps)))
      (cond ((null qadv)
     ; *** if not found, try an explicit link; this is inserted for traces
     ;     in seminterp; for instance, in "I want to buy", we get "I want
     ;     I KNOW WHERE to buy ...". Here, WHERE is a trace, so it cannot
     ;     have syntactic infos (cat, type), and it is identified by the
     ;     link
               (find-actavm-dep 'ADVB+INTERR-RMOD deps))
            (t qadv))))

; ***************************************************************************
; *** looks for a question adverb, among the dependents of a verb
(defun find-interr-pron (deps)
   (find-actavm-descendant
         '((cat type)) '((pron interr)) deps))

; ***************************************************************************
; *** looks for a dependent (of a verb) that is headed by (or includes)
;     an interrogative adjective
(defun find-interr-adjec (deps)
  (declare (special +FULL-TREE+))
   (let ((result (find-actavm-descendant '((cat type)) '((adj interr)) deps)))
      (cond ((null result)
    ; *** the next for "On which ..."
              (setq result 
                  (find-actavm-descendant 
                     '((link cat))
                     '((rmod prep))
                     deps))
              (cond ((is-a-actavm-trace? result)
                      (setq result
                            (find-coreferent (get-actavm-headcorefline result)
                                             (list +FULL-TREE+)))))
              (setq result 
                  (find-actavm-descendant 
                        '((link cat type)) 
                        '((prep-arg adj interr))
                        (get-actavm-deps-with-traces result)))))
      (cond ((null result) nil)
            ((is-a-actavm-trace? result)
         ; *** if the found interrogative adjective is a trace, look in the
         ;     full tree for its referent and returns it as the result
              (find-coreferent (get-actavm-headcorefline result)
                               (list +FULL-TREE+)))
            (t result))))

; ***************************************************************************
; *** looks for a dependent (of a verb) that is headed by (or includes)
;     an interrogative adjective
(defun find-interr-adj+adv (deps)
  (declare (special +FULL-TREE+))
   (let ((adj-subtree (find-actavm-descendant '((cat)) '((adj)) deps))
         adv-subtree)
      (cond ((not (null adj-subtree))
               (setq adv-subtree 
                  (find-actavm-dep 'ADVB+INTERR-RMOD adj-subtree))))
      (cond ((not (null adv-subtree)) adj-subtree)
            (t nil))))

; ***************************************************************************
; *** main function for building the interpretation of a subtree
; *** INPUT:
;  >>> governor-cat: the category of the governor whose restrictions are being
;      interpreted, i.e. of the item whose senses are in semtopics
;  >>> semtopics: a list of possible meanings of the head node of the tree
;  >>> topic-subtree: the tree that is being interpreted
;  >>> restrictions: a list of syntactic restrictions of the head node;
;      A restriction is a pair <subtree head-node>
;      where applic-range can take the values:
;  >>> the optional "not-simplify" serves to block and-simplification in case the
;      restrictions come from an "and" of give-info topics (e.g. "tomorrow it will rain
;      and the day after tomorrow there will be sun")
; *** OUTPUT:
;  >>> a list of possible interpretations, which are pairs:
;      - the first sublist is a full interpretation of the tree, while the second is
;        the interpretation of [some of] its restrictions
;        So, the result of build-restr-sem is "ambiguous"
;        The result represents the possible interpretations of the structure 
;        semtopics + restrictions: it does not only build the semantic of the restrictions but
;        also links them to the governing node
(defun build-restr-sem (governor-cat semtopics synt-node-pos topic-subtree restrictions 
                                &optional up-ident not-simplify)
                                            ;     (format t " Semtopics: ~a~% " semtopics)
                                            ;     (break "Entering build-restr-sem")
 (let ((multresult 
          (mapcar 
             #'(lambda (x) 
                 (catch 'semantic-error 
                        (build-sing-restr-sem governor-cat x synt-node-pos topic-subtree
                                   restrictions up-ident not-simplify)))
             (inlist semtopics)))
       actresult)
     (setq actresult (remove-failure multresult))
                                            ;    (format t 
                                            ;        "Build-restr-sem. up-ident: ~a~% actresult: ~a~%"
                                            ;         up-ident actresult)
                                            ;    (break "yby ************ build-restr-sem")
     (cond ((null actresult)
              (format t "PROC/buildquery: failure in building the meaning of a restriction~%")
              (format t " Semtopics: ~a~%  Restrictions: ~a~% Results: ~a~%"
                             semtopics restrictions multresult)
              (exception 'semantic-error
                    "PROC/buildquery: failure in building the meaning of a restriction"))
         ; *** the next tries to further simplify the query (see comments in move-up-subclass-and)
           (t (mapcar #'move-up-subclass-and actresult)))))

; ***************************************************************************
; *** this removes from a list of result all failures;
;     Since each result should be a path in the ontology, an atom (e.g. 'fail)
;     is a failure, as well as a list starting with a string (i.e. information
;     received from an exception)
(defun remove-failure (resultlist)
   (cond ((null resultlist) nil)
         ((or (atom (first resultlist))
              (stringp (first (first resultlist))))
            (remove-failure (rest resultlist)))
         (t (cons (first resultlist) 
                  (remove-failure (rest resultlist))))))

; ***************************************************************************
; *** this builds the interpretation concerning a single "root meaning" (i.e.
;     just one of the possible meanings of the upper node).
;     the result is the interpretation of the structure singrootsem + restrictions
;     So, this does not only build the semantic of the restrictions but also
;     links them to the governing node
; *** INPUT [ex. "a good friend of John"]:
;  >>> rootcat: the category of the governor whose restrictions are being
;      interpreted, i.e. of the item one of whose senses is in singrootsem
;      [ex. NOUN [for "friend"]]
;  >>> singrootsem: a single concept (a node in the ontology), which is one of the possible
;      meanings of the head [ex. ££friend]
;  >>> root-syntpos: the "linear" position of the root in the sentence [e.g. 3 for ££friend]
;  >>> synt-subtree: the subtree of the head [ex. the subtree rooted in "friend"]
;  >>> restrictions: the syntactic restrictions (subtrees) of the head node
;      [ex. a list of two restrictions, being the syntactic trees associated with
;      "good" and "of John"]
;  >>> noun-number (optional): the syntactic number of the head node [ex. SING for "friend"]
;  >>> up-ident (optional): the possible identifier (instance) of the head node
;      [NIL, in our example]
;  >>> not-simplify (optional): blocks and-simplification in case the
;      restrictions come from an "and" of give-info topics (e.g. "tomorrow it will rain")
; *** OUTPUT:
;  >>> a pair including:
;      - a list representing the chosen interpretation for the tree headed in the node whose
;        associated meaning is singrootsem, including all the restrictions
;      - a list of representations for all the restrictions following the head. This could be
;        useful in case the head is governed by a coordinating conjunction
(defun build-sing-restr-sem (rootcat singrootsem root-syntpos synt-subtree restrictions 
                                     &optional up-ident not-simplify)
    (let ((tempres (xbuild-sing-restr-sem rootcat singrootsem root-syntpos synt-subtree 
                                  restrictions up-ident not-simplify)))
  ; *** this function is defined just for debugging goals; the actual function is 
  ;     the one below
         ;  (format t "Exiting build-sing-restr-sem~% result: ~a~%" tempres)
         ;  (break "")
            tempres))

(defun xbuild-sing-restr-sem (rootcat singrootsem root-syntpos synt-subtree restrictions 
                                     &optional up-ident not-simplify)
 (declare (special topic-changes))
                               ;  (format t "rootcat: ~a; singrootsem: ~a; root-syntpos: ~a;~%"
                               ;              rootcat singrootsem root-syntpos)
                               ;  (format t "synt-subtree: ~a;~% restrictions: ~a;~%"
                               ;               synt-subtree restrictions)
                               ;  (break "Entering build-sing-restr-sem")
            ;(format t "&&&&& build-sing-restr-sem~%  &&& rootcat: ~a~%  &&& singrootsem: ~a~%"
            ;                rootcat singrootsem)
 (cond ((listp singrootsem)		; *** the concept is ambiguous
          (exception 'semantic-error "Multiple singrootsem in buildquery: build-sing-restr-sem")))
 (cond ((and (neq 'TOP rootcat)
             (neq rootcat (get-actavm-headcateg synt-subtree)))
          (exception 'semantic-error "Buildontorepr: mismatch between synt-subtree and rootcat")))
 (cond ((null restrictions)
; *** if there are no restrictions
         (cond ((is-deictic-concept singrootsem)   
            ; *** this branch is used in case we are interpreting "the city"
            ;     which is deictic (or, possibly, anaphoric, ut this case is currently not covered). 
            ;     Actually, deictic reference is treated either via the data in /KB-ALL/deictic-ref.dat
            ;     or via the semantic dictionary (by associating an ident to a common noun; see the
            ;     next branch of the cond). This should be revised
                  (let ((referent (get-deictic-referent singrootsem)))
						 ; e.g. referent = £Torino
                     (list
                       (cons (build-sem-to-synt-ref referent (get-actavm-headnumb synt-subtree))
      ; *** the rest, since the result includes the singrootsem
                             (rest (append 
                                      (choose-best-ontpath 
                                            (find-shortest-path singrootsem (get-instance-class referent)))
                                      (list 'has-instance referent))))
      ; *** the next nil is the second item of the result, i.e. the list of restrictions of "the city"
                          nil)))
               ((not (null up-ident))
                  (list (cons (build-sem-to-synt-ref singrootsem (get-actavm-headnumb synt-subtree))
                                      (list 'has-instance up-ident))
                        nil))
               (t (list (list (build-sem-to-synt-ref singrootsem root-syntpos)) nil))))
; *** if there are restrictions
       (t (let (paths newpaths them-grid firstpath restrpos coordrestrs coordpaths restr-link
                new-coord-path them-role (restrictions (sort-restr restrictions nil nil))
                fullfirstpath subfirstpath actual-restr firstcoord down-mean)
       ; (cond ((eq rootcat 'verb)
       ;          (format t "restrictions: ~a;~%" restrictions)
       ;          (break "UUU: build-sing-restr-sem2")))
    ; *** the next sets the thematic grid for verbs (and for some nouns)
              (cond ((memq rootcat '(NOUN VERB))
                       (setq them-grid (get-actavm-headthemgrid synt-subtree))))
    ; *** the next do collects all the paths going from a concept (singrootsem) to
    ;     all of its restrictions
    ; *** each restriction is actually a set of restrictions, each of which comes from a dependent,
    ;     but may include more than one item in case the dependent includes a coordinating conjunction
    ;     "Un temporale disturba la Francia e l'Italia" -->
    ;     (((un temporale))         			[VERB-SUBJ]
    ;      ((la Francia e l'Italia) (l'Italia)))	[VERB-OBJ]
    ; *** here, we should take into account information regarding the reading (collective, distributive,
    ;     and so on) of the possible conjuncts.
    ; *** currently, only cases appearing in the corpus are handled:
    ;     case 1: una perturbazione interessa la Francia e l'Italia
    ;             Here, nxtrestr is ((la Francia e l'Italia) (l'Italia))
    ;             corresponding to "la Francia e l'Italia"
    ;             The result is
    ;                (path (interessare --> Francia),
    ;                 path (interessare --> Italia))
    ;     case 2: aria calda e umida
    ;             Here, nxtrestr is ((calda e umida) (calda))
    ;             corresponding to "calda e umida"
    ;             The result is
    ;                (path (aria --> (and calda umida)))
    ; *** the interpretation is based on a nested loop, where all conjuncts appearing in nxtrestr
    ;     are collected. According to the governor type, a verb produces the result of case 1 above,
    ;     while any other governor produces case 2.
              (do ((nxtrestr (first restrictions) (first restrictions))
                   (restrictions (rest restrictions) (rest restrictions)))
                  ((null nxtrestr)
    ; *** exit from the do on the restrictions: now, we have in paths the interpretations of
    ;     all restrictions: put them together in a single representation
    ; *** also, we have in coordrestrs all the right modifiers of a possible last conjunct
                 ;(format t "Exiting Build-sing-restr-sem. up-ident: ~a~% singrootsem: ~a~% paths: ~a~%"
                 ;       up-ident singrootsem paths)
                 ;(break "zbz")
                    (setq paths (dropnil paths))
                    (cond ((null paths)
              ; *** this branch in case no dependent produces a real constraint
              ;     The function calls itself after setting restrictions to NIL, in order to treat
              ;     the subtree as if no dependent were present
                             (build-sing-restr-sem rootcat singrootsem root-syntpos synt-subtree nil 
                                     up-ident not-simplify)
                             ;(list
                             ;   (list (build-sem-to-synt-ref singrootsem root-syntpos))
                             ;   nil)
                              )
                          ((eq 1 (length paths)) 
              ; *** this branch in case of just one restriction: no composition is needed
              ;     A path is a pair: representation of the restriction + representation of 
              ;     downer subrestrictions
              ; *** however, in case the single restriction includes a coordination, it has a
              ;     further level of parentheses:
              ;     no coord: (((c11 c12 ... c1n) (sub11 sub12 sub1m)))
              ;                 where ((c11 c12 ... c1n) (sub11 sub12 sub1m)) is the single restr
              ;     coord: ((((c11 c12 ... c1n) (sub11 sub12 sub1m)))
              ;             (((c21 c22 ... c2p) (sub21 sub22 sub1q))))
              ;                 where ((c11 c12 ... c1n) (sub11 sub12 sub1m)) is the first conjunct
              ;                 of the single restr ((c21 ...) (sub21 ...)) is the second one
                             (setq fullfirstpath (first paths))
                             (setq firstpath (first fullfirstpath))
                             (cond ((listp (remove-synt-pointer (first firstpath)))
                    ; *** there is a coordination: use standard-compose-restr
                                      (standard-compose-restrs 
                                            paths singrootsem rootcat root-syntpos not-simplify))
                                   (t (setq subfirstpath (second fullfirstpath))
                                      (cond ((equal (first firstpath) singrootsem)
                                               (list
                                                  (cons (build-sem-to-synt-ref singrootsem root-syntpos)
                                                        (rest firstpath))
                                                  subfirstpath))
                                            ((and (eq (second firstpath) 'has-instance)
                                                  (equal (third firstpath) singrootsem))
              ; *** this  branch for restrictive relative clauses attached to a proper noun
              ;     (a case rare but not impossible)
                                               (list
                                                  (list (build-sem-to-synt-ref singrootsem root-syntpos)
                                                        `(and ((has-instance ,(first firstpath))
                                                              ,(nthcdr 3 firstpath))))
                                                  subfirstpath))
              ; *** in case of no match, I assume there has been an inversion (e.g. "Nubi in aumento")
             ;         --> "Aumento di nubi"
                                            (t fullfirstpath)
                                            ;(t (exception 'semantic-error
                                            ;      "PROC/buildquery: mismatch in build-restr-sem 1"))
                                             ))))
                          ((has-ordinal-desc paths) 
              ; *** this branch in case of an ordinal description (il primo giorno del mese)
   ;(break "ordinal")
                             (setq newpaths (dropnil (build-ordinal-description singrootsem paths)))
                             (cond ((eq 1 (length newpaths)) 
                                 ; *** the only actual restriction is the ordinal
                                 ;     the first of newpaths is the usual pair:
                                 ;     <repr of restriction, repr of subrestrictions>
                                      (setq fullfirstpath (first newpaths))
                                      (setq actual-restr (first fullfirstpath))
                                      (setq subfirstpath (second fullfirstpath))
                                      (cond ((equal (first actual-restr) singrootsem)
                                               (list 
                                                  (cons (build-sem-to-synt-ref singrootsem root-syntpos)
                                                        (rest actual-restr))
                                                  subfirstpath))
                                            (t (exception 'semantic-error
                                                  "PROC/buildquery: mismatch in build-restr-sem 2"))))
                                   (t (standard-compose-restrs 
                                            newpaths singrootsem rootcat root-syntpos not-simplify))))
              ; *** last branch: standard case
                          (t (standard-compose-restrs paths singrootsem rootcat root-syntpos not-simplify))))
    ; *** body of the do on the restrictions ************************************
    ;     recall the format of the restrictions
    ;     ( ( (coord11 coord12 ... coord1N1) link1)
    ;       ( (coord21 coord22 ... coord2N2) link2)
    ;            .....                              )
    ;     So, in the next, actual-restr = (coord1 coord2 coordN1)
    ;     The position of the dependent is the position of coord1
                (setq actual-restr (first nxtrestr))
                (setq restrpos (get-actavm-headnumb (first actual-restr)))
                (setq restr-link (second nxtrestr))
       ; *** now, we try to find a "semantic connection" between the dependent and the head
       ;     - in case there is a thematic-grid, then (in case the dependent is a complement
       ;       of the governing verb - or noun) the thematic grid specifies the connection
       ;     - in case the link upward of the restriction is a list, then the restriction
       ;       is associated with a prepositional modifier, and has the form (RMOD prep-meaning).
       ;       In this case, the semantic connection is looked for in the preplate table
       ;       (and in this case, the name them-role for the variable is a bit improper)
                (cond ((listp restr-link)
                         (setq firstcoord 
                              (skip-determiner 
                                    (find-actavm-dep 'PREP-ARG (first actual-restr) nil)))
                         (setq down-mean (get-actavm-headlexmean firstcoord))
                         (setq them-role (get-act-prep-mean (second restr-link) singrootsem down-mean))
                         (cond ((eq them-role 'fail) (setq them-role nil))))
                      (t (setq them-role (get-grid-role them-grid restr-link))))
                (setq coordrestrs nil)
                (setq coordpaths nil)
         ;(format t "Before the do on coordinations;~%  nxtrestr = ~a~%" nxtrestr)
         ;(break "twt")
                (do ((nxtcoord (first actual-restr) (first actual-restr))
                     (actual-restr (rest actual-restr) (rest actual-restr)))
                    ((null nxtcoord)
                ; *** each item in coordpaths is actually a pair consisting in two semantic paths in
                ;     the ontology. Usually, the second item is NIL. It is non-NIL in case the last
                ;     conjunct has some modifiers. 
                ; *** coordrestrs is the same as coordpaths, but it contains only interpretations
                ;     of restrictions to the right of the head
                ; *** each new-coord-path is non-ambiguous, so that its form is <path restrpath>
        ;(format t "Exiting (1) the do on the coordinations. coordpaths: ~a~% coordrestrs: ~a~%" 
        ;         coordpaths coordrestrs)
        ;(break "uvu")
                       (setq new-coord-path (build-poss-coord-path coordpaths singrootsem root-syntpos))
                ; *** build-poss-coord-path is first called on "locale precipitazione", then on "debole
                ;     precipitazione", then on "precipitazione in Abruzzo". In all these cases it has no
                ;     effect. So, when the loop is exited, it has these three paths as foundpaths,
                ;     and the third of them as coordrestrs
                       (setq paths (append1 paths new-coord-path))
        ; (format t "Exiting (2) the do on the coordinations. paths: ~a~%" paths)
        ; (break "ubu")
                      )
             ; *** body of the internal do on the coordinations *****************************
                    (let (nodemean foundpath)
                        (cond ((not (null synt-subtree)) 
                                (setq nodemean (get-actavm-headlexmean synt-subtree))))
    ; (cond ((eq rootcat 'VERB)
        ;(format t " Before calling sing-r-sem; ~%nxtcoord: ~a~%" (first nxtcoord))
        ;(break "yuy")))
                        (setq foundpath
                                (build-sing-r-sem 
                                     rootcat singrootsem synt-subtree nxtcoord up-ident them-role))
    ; (cond ((eq rootcat 'VERB)
    ;     (format t "   AFTER CALLING SING-R-SEM *** foundpath: ~a~%nxtcoord: ~a~%"
    ;                   foundpath nxtcoord)
    ;     (break "ywy")))
                        (setq coordpaths (append1 coordpaths foundpath))
                        (cond ((index-precedes root-syntpos restrpos)
                                (setq coordrestrs (append1 coordrestrs foundpath))))
                         ))
     ; *** if the just found path (now stored in first coordpaths) has length 1, then it
     ;     refers to a subtype restriction; so the goal topic must become the new
     ;     concept (in "stato di obsolescenza", "££status" is changed into
     ;     "££generalConfigurationState")
                    (setq coordpaths (remove-failure coordpaths))
                    (cond ((eq 1 (length (first coordpaths)))
                             (setq topic-changes 
                                 (cons (list singrootsem (first (first coordpaths))) topic-changes))
                             (setq singrootsem (first (first coordpaths))))))))))

; *******************************************************************
; *** coordpaths is a list of pairs <interp,further-restrs>, one for each conjunct
;     where "interp" is the standard semantic interpretation of a subtree,
;     while "further-restrs" is the set of interpreted restrictions of each subtree
;     for "the old man and the young girl with a hat saw ..."
;     the two pairs are <sem(old man),NIL> and <sem(young girl), sem(with a hat)>
;        N.B. "further-restrs" include only restrictions that follow the head
; *** Actually, the function enters into play when the conjuncts are being linked to their
;     head, so, in the example, the two coordpaths are
;     <sem(old man see),NIL> and <sem(young girl see), sem(with a hat)>
; *** in case the length of coordpaths is 1, there is no coordination, so that the result
;     is simply the first item of the first pair (i.e. the sem of the expression)
; *** otherwise, we check if the last item includes a further-restrs (i.e. sem(with a hat))
;     in our example. If it does, then a check (whose rules are still unclear to me) is carried
;     out in order to ascertain if the further restrictions are compatible with previous conjuncts
;     In such a case, the previous conjuncts are all extended with further-restrs (i.e.
;     sem(old man)--> sem(old man with a hat)), otherwise the second component of all pairs are
;     simply thrown away. Then, the result is "standardly composed"
(defun build-poss-coord-path (coordpaths singrootsem root-syntpos)
 ;(format t "build-coord-path: coordpaths = ~a~% singrootsem = ~a~%" coordpaths singrootsem)
 ;(break "txt 1")
  (let (extended-conjuncts newpaths)
   (cond ((eq 1 (length coordpaths)) (first coordpaths))
       ; *** there is no coordination
         (t (let ((revcoordpaths (rest (reverse coordpaths)))
                  (further-restrs (second (ult coordpaths))) 
                  (stop nil) newnextconj savconjunct result 
                  (newcoordpaths (last coordpaths)))
       ; *** remember that we are working at an upper level, in the sense that the first conjunct
       ;     is not sem(the boy) and the second one sem(the girl with the hat); rather, the first
       ;     conjunct is sem(the boy saw) and the second one is sem(the girl with the hat saw)
       ;     This means that it makes sense to call sem(girl sith the hat) "further-restrs",
       ;     but they come with the "girl" head
       ; *** the situation can be mixed; in "l'anziano signore e la ragazza incinta col cappello"
       ;     (theold man and the pregnant girl with the hat; the example is made in Italian
       ;     because the possible common restriction (incinta: pregnant") must follow the head,
       ;     which is not the case in English). Note also that in Italian, the attachment of the
       ;     restriction (pregnant) is excluded also on syntactic grounds ("incinta" is feminine)
       ; *** so the function must take the restrictions one at a time (starting from the last)
       ;     and check the compatibility of each of them. As soon as one is not compatible, all
       ;     the preceding ones are also excluded
       ;      ((conjunct1 (restrs ...) (conjunct2 (restr1 restr2 restr3))))
       ;      if restr2 is not compatible with conjunct1, then restr1 cannot be included
       ; *** however, now this cannot be accomplished, since the restrictions have not been taken
       ;     apart
                 (do ((nxtconjunct (first revcoordpaths) (first revcoordpaths))
                      (revcoordpaths (rest revcoordpaths) (rest revcoordpaths)))
                     ((or (null nxtconjunct) stop)
                        (setq result
                           (cond (stop (append (reverse revcoordpaths)
                                              (cond ((null nxtconjunct) nil)
                                                    (t (list nxtconjunct)))
                                              (list savconjunct) newcoordpaths))
                                 (t newcoordpaths))))
                     (setq savconjunct nxtconjunct)
                     (setq newnextconj (build-new-firstconj further-restrs nxtconjunct))
 ;(format t "build-coord-path: newnextconj = ~a~%" newnextconj)
 ;(break "txt 2")
                     (cond ((neq 'fail newnextconj)
                              (setq newcoordpaths (cons newnextconj newcoordpaths)))
                           (t (setq stop t))))
 ;(format t "build-coord-path: result = ~a~%" result)
 ;(break "txt 3")
                 ;(setq newpaths (append1 extended-conjuncts (first (ult coordpaths))))
                 ;     (t (setq newpaths (mapcar #'first coordpaths)))
                 ; (standard-compose-restrs newpaths singrootsem rootcat root-syntpos nil)
  result)))))

; *******************************************************************
; *** this checks if the restrictions of the last conjunct can also be applied to 
;     a previous one. For "the boy and the girl with the hat saw ...", we have that
;     last-conj-restr = (££girl subclass-of ££person range-of &wearer ... ££dress-item has-subclass ££hat)
;     next-conj = (((to-see-1 domain-of &seer range ££living has-subclass ££person has-subclass ££boy)
;                    (££boy)))
;     where the first line of next-conj is the representation of "the boy saw" and the second one are
;     the restrictions of that (which are not relevant in this example)
; *** in case the last conjunct has no restrictions, returns 'fail
; *** the problem is considerably difficult and requires further investigation.
;     Currently, what I do is to check if the first non-logical relation of last-conj-restr (in our
;     case &wearer) is connected with the head of the second conjunct (££girl) via a link (range)
;     that is restricted to a concept (££person) which is compatible (i.e. subsumes) the first
;     conjunct (££boy). In this case, the modifier is attached also to the first argument.
(defun build-new-firstconj (last-conj-restr next-conj)
   (let ((firstconjhead (ult (first next-conj)))
         secondconjrestr initpath oldfirstconj newfirstconj oldrestrs oldprefix newrestrs)
    (cond ((null last-conj-restr) 'fail)
          (t (multiple-value-setq (initpath secondconjrestr) (search-relation-restr last-conj-restr nil))
 ;(format t "build-new-firstconj: firstconjhead = ~a~%" firstconjhead)
 ;(break "xqx")
             (cond ((is-subclass-of (remove-synt-pointer firstconjhead) (first secondconjrestr))
   ; *** in our example, firstconjhead = ££boy, secondconjrestr = (££person range-of-&wearer ...)
   ;     initpath = (££girl subclass-of)
   ; *** the result must be 
   ;      (((to-see-1 domain-of &seer range ££living has-subclass ££person has-subclass ££boy
   ;            subclass-of ££person range-of &wearer ... ££dress-item has-subclass ££hat)
   ;        (££boy subclass-of ££person range-of &wearer ... ££dress-item has-subclass ££hat)))
                     (setq oldfirstconj (first next-conj))
                  ; *** oldfirstconj = (to-see-1 domain-of &seer ... ££person has-subclass ££boy)
                     (setq oldrestrs (second next-conj))
                  ; *** oldrestrs = (££boy)
                     (setq oldprefix (remove-fc-restrs oldfirstconj oldrestrs))
                  ; *** oldprefix = (to-see-1 domain-of &seer ... ££person has-subclass ££boy)
                  ;     this is the same as oldfirstconj just because ££boy has no restrictions
                  ;     if the sentence were "the young boy and the girl with the hat saw", 
                  ;     oldrestrs would be "saw boy[young]", and oldprefix would be "saw boy"
                  ;     (see the comments in remove-fc-restrs)
                     (setq newrestrs (simple-include-restr 
                                          oldrestrs (append (rest initpath) secondconjrestr)))
                  ; *** newrestrs = (££boy subclass-of ££person range-of ... ££hat)
                     (setq newfirstconj (append oldprefix (rest newrestrs)))
                     (list newfirstconj newrestrs))
   ; *** if the two conjuncts are not compatible, the restrictions cannot be shared
                    (t 'fail))))))

; *******************************************************************
; *** looks for a relation node in a path
; *** if the path is (c0 c1 c2 R c3 c4 c5)
;       returns (c0) and (c1 c2 R c3 c4 c5)
(defun search-relation-restr (path initpath)
   (cond ((null path) (break "sem mod compatible"))
         ((is-relation (third path))
            (values initpath path))
         (t (search-relation-restr (rest path) (append1 initpath (first path))))))

; *******************************************************************
; *** this function detaches from a semantic representation the part associated
;     with some restrictions. The detached part must be merged with the new
;     restrictions coming from a possible second conjunct
;     oldfirstconj = (to-see-1 domain-of &seer ... ££person has-subclass ££boy r1 r2 ... rN)
;     oldrestrs = (££boy r1 r2 ... rN)
; *** the result is (to-see-1 domain-of &seer ... ££person has-subclass ££boy)
; *** I currently assume that oldrestrs includes a single sublist
(defun remove-fc-restrs (oldfirstconj oldrestrs)
   (let ((revoldrestr (reverse oldrestrs))
         (revoldfirst (reverse oldfirstconj)) savold)
      (do ((nxtoldr (first revoldrestr) (first revoldrestr))
           (revoldrestr (rest revoldrestr) (rest revoldrestr))
           (nxtoldf (first revoldfirst) (first revoldfirst))
           (revoldfirst (rest revoldfirst) (rest revoldfirst)))
          ((null nxtoldr)
             (reverse (cons savold (cons nxtoldf revoldfirst))))
          (cond ((not (equal (remove-synt-pointer nxtoldr) (remove-synt-pointer nxtoldf)))
                   (break "Restriction misalignment in remove-fc-restrs"))
                (t (setq savold nxtoldf))))))
         
; *******************************************************************
; *** oldrestrs is (pivot-conc ro1 ro2 ro3 ...)
; *** newrestr is (rn1 rn2 rn3 ...)
;     The result is (pivot-conc (and ((ro1 ro2 ro3 ...) (rn1 rn2 rn3 ...))))
(defun simple-include-restr (oldrestr newrestr)
  (cond ((null (rest oldrestr))
           (append oldrestr newrestr))
        (t (list (first oldrestr)
                 (cons 'and (list (rest oldrestr) newrestr))))))

; *******************************************************************
; *** the function takes care of getting the deep syntrole from link
;     and finding the corresponding mapping (if any) in them-grid
; *** link could be a list in case of prepositional RMOD. Since it is
;     an RMOD, it is not a complement, so the result is NIL
(defun get-grid-role (them-grid link)
   (cond ((listp link) nil)
         (t (let ((deepr (first (expl+cats (explode link) #\/))))
                (first (leggi them-grid deepr))))))

; *******************************************************************
; *** this is similar to compose-restr-path, but it does not attach in front
;     of the joined interpreted restrictions (paths) the path to the governor
; *** a standard interpreted restriction (a "path") has the form:
;     ((ca1 ca2 ... cax) (cra1 cra2 ... cray))
;        where (ca1 ... cax) is the actual interpretation, while (cra1 ... cray) is
;        the interpretation of sub-restrictions
;     In case of ambiguity, the form is as follows:
;     (((cb1 cb2 ... cbx) (crb1 crb2 ... crby))
;      ((cd1 cd2 ... cdz) (crd1 crd2 ... crdy))
;        ...)
; *** According to the starting concept of the restriction, in case of coordinations
;     different things may happen: in case the starting concept is a verbal one,
;     distributivity is applied, in other cases the different coordinated restrictions
;     are merged as if they were parallel ones
;     Let's suppose we have that the different paths are:
;     (((p1 r1) (p2 r2))
;      (p3 r3)
;      (p4 r4))
;     If the start of p1 (and p2, p3, p4) is verbal the result is: 
;     ((and (p1 p3 p4) (p2 p3 p4)) (r1234))
;     Otherwise, it is
;     ((and (p1 p2 p3 p4)) (r1234'))
; *** it is assumed that all paths starts with the same ontology concept
; *** semtopic is used just to check consistency
; *** the optional in-event specifies that the call has been made inside inside-compose-restrs
;     to compose the arguments of and-ed events. In such a case, the distribution on events
;     should not be repeated
;     returns a single path (non-ambiguous)
(defun standard-compose-restrs (paths semtopic rootcat topic-syntpos not-simplify &optional in-event)
   (let (there-is-coordpath allpaths allrestrs simpl-f allevents result)
     ; (format t "!!!!!!! Entering standard-compose-restrs~% paths: ~a~% semtopics ~a~%"
     ;        paths semtopic)
     ; (break "")
      (setq paths (dropnil paths))
   ; *** null paths can come from, e.g. parts of locutions
      (dolist (nxtpath paths)
         (cond ((listp (remove-synt-pointer (first (first nxtpath))))
      ; *** The first of the next path is the actual interp; the second are the restrictions
      ;     if the first of the actual interp is a list, there is a coordination
                  (setq there-is-coordpath t))))
    (setq result
      (cond ((and (not in-event)
                  (distributive-interp? rootcat))
               (cond (there-is-coordpath
                       (multiple-value-setq (allpaths allrestrs) (distribute-interps paths))
                       (setq allpaths 
                             (mapcar #'(lambda (x) 
                                          (mapcar #'(lambda (y) 
                                                  (cons (build-sem-to-synt-ref 
                                                                 (first y) topic-syntpos)
                                                          (rest y)))
                                                   x))
                                      allpaths))
                       (setq allevents 
                           (mapcar #'(lambda (x) (simplify-bq-and (list 'and x) not-simplify))
                                   allpaths))
      ; *** each item in the list "allevents" is a list of arguments describing the participants
      ;     in the event. Each group of participants must be "composed" in oredr to obtain the
      ;     compact descriptions of the involved events
                     ;  (setq allevrepr 
                     ;      (mapcar #'(lambda (x)
                     ;                    (standard-compose-restrs x 
                     ;                             semtopic rootcat topic-syntpos not-simplify t))
                     ;              allevents))
      ;(format t "!!!!!!! standard-compose-restrs 1~% allevents: ~a~%" allevents)
      ;(break "")
                       (list (list (list 'event-and allevents)) nil))
                     (t (setq allpaths
                           (simplify-bq-and (list 'and (mapcar #'first paths)) not-simplify))
      ; (format t "!!!!!!! standard-compose-restrs 2~% allpaths: ~a~%" allpaths)
      ; (break "")
                        (list (cons (build-sem-to-synt-ref (first allpaths) topic-syntpos)
                                    (rest allpaths))
                              nil
                              ;(mapcar #'second paths)
                                ))))
      ; *** here, I assume that in verbal (distributive?) interpretation, no restriction can be passed
      ;     up in coordinated structures. Actually, this can be useful in cases as
      ;     "I met him and talked to him yesterday"
      ;     But currently I do not know how to keep apart useful restrictions (e.g. "yesterday")
      ;     from other ones (e.g. "to him")
            (t (setq allpaths nil)
               (setq allrestrs nil)
      ; *** here, we are not in a distributive context (e.g. a nice and kind girl)
      ;     coords and non-coords can be mixed: "a nice and kind blonde girl"
               (do ((nxtpath (first paths) (first paths))
                    (paths (rest paths) (rest paths)))
                   ((null nxtpath))
   ; (format t "In standard-compose-restrs; nxtpath-f= ~a~%" nxtpath)
   ; (break "")
                   (cond ((listp (remove-synt-pointer (first (first nxtpath))))
          ; *** (first nxtpath): the restriction repr (no coord) or the first conjunct (coord)
          ; *** (first (first nxtpath)): the first item of the path (no coord) or the 
          ;        restriction repr of the first conjunct (coord)
                           (setq allpaths (append allpaths (mapcar #'first nxtpath)))
                           (setq allrestrs (append allrestrs (mapcar #'second nxtpath))))
                         (t (setq allpaths (append1 allpaths (first nxtpath)))
                            (cond ((not (null (second nxtpath)))
                                     (setq allrestrs (append1 allrestrs (second nxtpath))))))))
               (setq simpl-f (simplify-bq-and (list 'and allpaths) not-simplify))
   ; (format t "In standard-compose-restrs; simpl-f= ~a~%" simpl-f)
   ; (break "")
               (cond ((and (atom (first simpl-f))
                           (member (first simpl-f) (inlist semtopic)))
      ; *** the first item of the result is the actual representation
      ;     the second one is restriction
                        (list (cons (build-sem-to-synt-ref (first simpl-f) topic-syntpos)
                                    (rest simpl-f))
                              (simplify-bq-and (list 'and allrestrs) not-simplify)))
                     (t (exception 'semantic-error
                                 "PROC/buildquery: simplification of formula in build-restr-sem"))))))
      ;(format t "!!!!!!! Exiting standard-compose-restrs~% result: ~a~%" result)
      ;(break "")
   result))

; *******************************************************************
(defun distributive-interp? (categ)
   (eq categ 'verb))

; *******************************************************************
; *** this makes a kind of cartesian product
(defun distribute-interps (paths)
  (let (firstinterp firstrestr otherinterps otherrestrs newinterps newrestrs nxtpath
        newsinginterp newsingrestr)
   (cond ((null paths) (list nil))
         (t (multiple-value-setq (otherinterps otherrestrs) (distribute-interps (rest paths)))
            (cond ((atom (remove-synt-pointer (first (first (first paths)))))
  ; *** this is a simple (non coordinated) restriction: add a level of parentheses
                     (setq nxtpath (list (first paths))))
                  (t (setq nxtpath (first paths))))
   ; *** the next do loops on all interpretations of the first path
   ;     if we have: (((p1 r1) (p2 r2)) (p3 r3) ((p4 r4) (p5 r5)))
   ;     nxtpath = ((p1 r1) (p2 r2))
   ;     nxtcoord is, at the first step (p1 r1), at the second step (p2 r2)
            (dolist (nxtcoord nxtpath)
                 (setq firstinterp (first nxtcoord))
                 (setq firstrestr (second nxtcoord))
                 (setq newsinginterp nil)
                 (setq newsingrestr nil)
      ; *** at the first step firstinterp=p1 firstrestr=r1,
      ;     at the first step firstinterp=p2 firstrestr=r2
      ; *** the next do loops on all interpretations of the rest of the paths
      ;     The recursive hypothesis is that otherinterps=((p3 p4) (p3 p5))
      ; *** after the first step, newsinginterp=((p1 p3 p4))
      ;     after the first step, newsinginterp=((p1 p3 p4) (p1 p3 p5))
                 (dolist (nxtnew otherinterps)
                       (setq newsinginterp (cons (cons firstinterp nxtnew) newsinginterp)))
                 (dolist (nxtnew otherrestrs)
                       (setq newsingrestr (cons (cons firstrestr nxtnew) newsingrestr)))
                 (setq newinterps (append newsinginterp newinterps))
                 (setq newrestrs (append newsingrestr newrestrs)))
            (values newinterps newrestrs)))))

; *******************************************************************
(defun has-ordinal-desc (paths) 
  ;(format t "Entering has ordinal; paths: ~a~%" paths)
  ;(break "hasord")
   (cond ((null paths) nil)
         ((member '££ordinal-descriptor (first(first paths))) t)
   ; *** recall that each path is a pair <repr of the tree, repr of its restrictions>
         (t (has-ordinal-desc (rest paths)))))

; *******************************************************************
; *** ordinals are managed in the following way:
;     1. One of the restrictions refers to the ordinal. If, for instance, the 
;        expression is "last day", the form of this restriction is:
;        ((££day ... ££ordinal-description domain-of &ordinal-desc-selector range
;          ££ordinal-descriptor has-instance £last) sub-restrs1)
;        This restriction is identified by the presence of ££ordinal-descriptor
;     2. Among the other restrictions there can be one of the form
;        ((££day range-of &day-in-daymonth domain ££day-month-part-of domain-of
;          &month-in-daymonth range ££month) sub-restrs2)
;        This is characterized by the presence of ££day-month-part-of, which is
;        a sub-concept of ££part-of
;     3. The resulting representation is built assuming that the larger entity
;        (here ££month, since &month-in-daymonth restricts &part-bigger) is 
;        the reference sequence for the ordering, so that the direct ££part-of
;        connection is replaced by a connection through ££ordinal-description,
;        where ££day is the &ord-described-item and ££month is the &reference-sequence
;        The result is:
;        ((££day subclass-of ££time-interval subclass-of ££sequenceable-entity
;            range-of &ord-described-item domain 
;          ££ordinal-description
;            (and (domain-of &ordinal-desc-selector range ££ordinal-descriptor 
;                  has-instance £last)
;                 (domain-of &reference-sequence range ££entity-sequence has-subclass
;                  ££day-sequence has-subclass ££month)))
;          sub-restrs2)
; *** Comments:
;     a. Actually, the second restriction includes the full representation of the
;        the specified sequence (e.g. the month name)
;     b. In case no sequence description is found, the second conjunct of the
;        specification of ££ordinal-description is missing (left unspecified)
;     c. all other restrictions of the top noun (e.g. ££day) are treated in the
;        standard way
; *** the function returns a list of paths such that the paths concerned with the ordinal
;     (one of two depending on the fact that the reference sequence appears explicitly)
;     are replaced with the representation of the ordinal
(defun build-ordinal-description (semtopic paths)
; *** the next mapcar throws away the info about the subrestrictions used for coordination
;     This has to be revised !!!
  (let (found other-restrs found2 rem-restrs res first-part seq-paths
        selector seq-descr sequence)
; *** the external do looks for the ordinal modifier. It must exist since the function
;     is evaluated just in case such a modifier has been found
   (do ((nxtpath (first paths) (first paths))
        (paths (rest paths) (rest paths)))
       ((or (null nxtpath) found)
          (cond ((not found) 
                  (exception 'semantic-error "Ordinal not found in build-ordinal-description")))
          (cond ((not (null nxtpath))
                   (setq other-restrs (append other-restrs (list nxtpath) paths))))
         ; *** now, we have in "found" the ordinal (e.g. £last) and in other-restrs
         ;     the other (if any) restrictions of the "ordinalized" noun
         ; *** look, among them, for one that is a sub-relation of ££part-of
          (do ((nxtrestr (first other-restrs) (first other-restrs))
               (other-restrs (rest other-restrs) (rest other-restrs)))
              ((or (null nxtrestr) found2)
                (cond ((not (null nxtpath))
                        (setq rem-restrs (append rem-restrs (list nxtrestr) other-restrs))))
                (cond (found2
     ;(format t "build ordinal: Exiting the internal loop; ~% found: ~a~% found2: ~a~%" found found2)
     ;(break "build ord")
                        (setq first-part 
                           (reverse (member '££ordinal-description (reverse (first found)))))
                  ; *** first-part is the connection between the described entity
                  ;     (e.g. ££day) and the concept ££ordinal-description
                  ;        <££day subclass-of ££time-interval subclass-of ££sequenceable-entity
                  ;         range-of &ord-described-item domain ££ordinal-description>
                        (setq selector (rest (member '££ordinal-description (first found))))
                  ; *** selector is the selector in the ordinal representation 
                  ;        <domain-of &ordinal-desc-selector range ££ordinal-descriptor
                  ;        has-instance (synt pos1 £last)>
                        (setq seq-descr (find-sequence-descr (first found2)))
                  ; *** seq-descr is the portion of found2 that comes after the "larger"
                  ;     concept (e.g. the description of the month)
                  ;        <(synt pos2 month) subclass-of ££time-interval domain-of &has-time-interv-descr
                  ;         range ££time-interval-description has-subclass ££month-description
                  ;         has-instance (synt pos3 june)>
                        (setq seq-paths (find-shortest-path semtopic 
                                                (remove-synt-pointer (first seq-descr))
                                                '&reference-sequence))
                  ; *** the next one choose the best path, but only omong the ones that pass
                  ;     through ££ordinal-description
                        (setq sequence 
                            (rest (choose-best-ontpath 
                                     (dropnil
                                        (mapcar #'(lambda (x) (member '££ordinal-description x))
                                                seq-paths)))))
                  ; *** sequence is the new part. that should connect ££ordinal-description
                  ;     to the larger part (the sequence)
                  ;        <domain-of &reference-sequence range ££entity-sequence has-subclass
                  ;         ££day-sequence has-subclass ££month>
                  ; *** second found2 are the previous subrestrictions
                        (setq res
                           (list 
                              (append1 first-part
                                  (list 'and (list selector 
                                               (sing-comp-restr-path sequence (list seq-descr nil)))))
                              (second found2)))
                  ; *** seq-descr is put in a list, since it must have the form of a restriction
                  ;     representation, i.e. <restriction repr, sub-restrictions repr>
             ; (format t "Seq-descr: ~a~%sequence: ~a~%first-part: ~a~%" seq-descr sequence first-part)
             ; (break "")
                        (cons res rem-restrs))
                     (t (cons found rem-restrs))))
       ; *** body of the internal loop: look for a sub-concept of ££part-of
       ;     "first" of nxtrestrs, since it also includes the sub.restrictions
              (cond ((includes-subclass-of (first nxtrestr) '££part-of)
                       (setq found2 nxtrestr)))))
 ; *** body of the external loop: look for the concept ££ordinal-descriptor
 ;     "first" of nxtpath, since it also includes the sub.restrictions
   (cond ((member '££ordinal-descriptor (first nxtpath))
           (setq found nxtpath))
         (t (setq other-restrs (append1 other-restrs nxtpath)))))))

; *******************************************************************
; *** this looks for the "larger" argument of the "part-of" relation between
;     the item and the sequence
(defun find-sequence-descr (path)
  (cond ((null path)
           (exception 'semantic-error "Ordinal not found in build-ordinal-description"))
        (t (let ((firstp (remove-synt-pointer (first path))))
               (cond ((or (is-a-structural-item firstp)
                          (not (is-relation firstp))
                          (not (is-restriction-of firstp '&part-bigger)))
                        (find-sequence-descr (rest path)))
                     ((equal (second path) 'range)
                        (rest (rest path)))
                     (t (exception 'semantic-error "Found bigger part in sequence-descr, but missing range")))))))

; *******************************************************************
; *** checks if any of the concepts in path is subsumed by class
(defun includes-subclass-of (path target-class)
  (cond ((null path) nil)
        ((is-a-structural-item (first path))
          (includes-subclass-of (rest path) target-class))
        ((is-subclass-of (first path) target-class) t)
        (t (includes-subclass-of (rest path) target-class))))

; *******************************************************************
(defun sort-restr (restrictions single non-single)
   ;(format t "sort-restr; restrictions: ~a~%" restrictions)
   ;(break "")
   (cond ((null restrictions) (append single non-single))
         ((eq (first (first restrictions)) 'single)
            (sort-restr (rest restrictions) (append1 single (first restrictions)) non-single))
         (t (sort-restr (rest restrictions) single (append1 non-single (first restrictions))))))

; *******************************************************************
(defun is-countable (concept)
   (is-subclass-of concept '££countable))

; ***************************************************************************
; ***************************************************************************
; *** The situation is:
;     (... C1 has-subclass C2 (and COND1 ... (subclass-of C1 CONDi) ... CONDn))
;     what happens is that from C1 we go down to C2, and then (among the
;        constraints on C2), there is one that refers to its C1 upper class
; *** The result should be:
;     (... C1 (and CONDi (has-subclass C2 (and COND1 ... CONDn))))
;     so, the constraint related to C1 has been attached directly to C1
(defun move-up-subclass-and (semrestr)
   ;(break "move-up-subclass-and")
    (int-move-up-s-a semrestr nil))

(defun int-move-up-s-a (semrestr prevpath)
  (let (subcl nosubcl remconds newand)
    (cond ((null semrestr) (reverse prevpath))
          ((or (atom (first semrestr))
               (neq (first (first semrestr)) 'and)
               (neq (second prevpath) 'has-subclass))
            (int-move-up-s-a 
                (rest semrestr) (cons (first semrestr) prevpath)))
  ; *** the dolist looks for all arguments of and that start with "subclass-of C",
  ;     where the previous path was "C has-subclass X" (reversed in prevpath)
          (t (dolist (nxtandarg (rest (first semrestr)))
                   (cond ((and (eq (first nxtandarg) 'subclass-of)
                               (eq (second nxtandarg) (third prevpath)))
                           (setq subcl (cons nxtandarg subcl)))
                         (t (setq nosubcl (cons nxtandarg nosubcl)))))
             (cond ((null subcl)
                     (int-move-up-s-a 
                         (rest semrestr) (cons (first semrestr) prevpath)))
                   (t (setq remconds
                         (cond ((null nosubcl) nil)
                               ((eq 1 (length nosubcl))
                                  (append (list 'has-subclass (first prevpath))
                                          (first nosubcl)))
                               (t (append (list 'has-subclass (first prevpath))
                                          (cons 'and nosubcl)))))
                      (setq newand
                         (cons 'and 
                               (append2 (mapcar #'(lambda (x) (rest (rest x))) subcl)
                                        remconds)))
                      (int-move-up-s-a 
                         (rest semrestr) (cons newand (rest (rest prevpath))))))))))

; ***************************************************************************
; *** it converts an and-ed formula in a simplified form, such that
;     all prefixes appear just once
;     INPUT: (and ((alfa beta gamma) (alfa (and ((beta) (delta)))) (delta omega)))
;     FLAT: ((alfa beta gamma) (alfa beta) (alfa delta) (delta omega))
;     GROUPED: ((alfa (beta (nil gamma)) delta) (delta omega))
;     RESULT: (and ((alfa (and ((beta gamma) (delta)))) (delta omega)))
;     paths is a list of ontology paths
; *** simplify-bq-and
;          |----------> basic-bq-simpl      <for the topics of the interaction>
;          |    |---------> get-common-prefixes 
;          |         |--------> all-bq-equal
;          |              |-------> synt-sem-equal 
;          |----------> flatten-bq-and 
;          |    |---------> act-bq-flat 
;          |    |---------> int-flat-bq-and 
;          |         |--------> mult-bq-cons
;          |----------> group-bq-paths
;          |    |---------> elimdup-with-synt
;          |    |    |--------> include-with-synt
;          |    |         |-------> synt-sem-equal 
;          |    |---------> collect-bq-rests
;          |         |--------> find-all-rests
;          |----------> add-bq-and
;               |---------> int-a-bq-and
(defun simplify-bq-and (anded-f &optional not-simplify)
   ;(break "simplify-bq-and")
   (cond ((not not-simplify)
            (let* ((flat (flatten-bq-and anded-f))
                   (grouped (group-bq-paths flat))
                   (result (add-bq-and grouped)))
                (cond ((listp (remove-synt-pointer (first result)))
                         (first result))
                      (t result))))
         (t (basic-bq-simpl anded-f))))

; ***************************************************************************
; *** when a formula must not be simplified, it is assumed that it refers to the
;     topics of the interaction, so the prefixes (all referring to the ££dialogue
;     topics) must be unified anyway
(defun basic-bq-simpl (formula)
   (cond ((eq (first formula) 'and)
            (let (common-pref remainders)
                (multiple-value-setq (common-pref remainders) 
                     (get-common-prefixes (second formula) nil))
                (append1 common-pref (list 'and remainders))))
         (t formula)))

; ***************************************************************************
; *** extract the prefix which is common to all formulae (accounting for possible
;     synt pointers)
(defun get-common-prefixes (formulae prefix)
  (let ((firstequal (all-bq-equal (first (first formulae)) (mapcar #'first formulae))))
    (cond (firstequal
            (get-common-prefixes (mapcar #'rest formulae) (append1 prefix firstequal)))
          (t (values prefix formulae)))))

; ***************************************************************************
; *** it checks if all items are the same, and equal to firstitem
;     It accounts also for "synt" pointers: it returns the item 
;     including the "synt" info (if any) or the standard item
(defun all-bq-equal (firstitem items)
  (cond ((null items) firstitem)
        (t (let ((nxtres (synt-sem-equal firstitem (first items))))
               (cond ((null nxtres) nil)
                     (t (all-bq-equal nxtres (rest items))))))))

; ***************************************************************************
; *** flatten-bq-and works on (and ((a b c))) or (and ((x y) (z)))
;     ARG: (and (conj1 conj2 ... conjN))
;     RES: (flatconj1 flatconj2 ... flatconjN)
(defun flatten-bq-and (anded-f)
   ;(break "flatten-bq-and")
   (act-bq-flat (mapcar #'int-flat-bq-and (second anded-f))))

; *** int-flat-bq-and works on (a b c) or (x (and ((y t) (w))))
;     ARG: (simplelem1 simplelem2 ... simplorcomplemlemN)
;     RES: (flatconj1 flatconj2 ... flatconjN)
(defun int-flat-bq-and (conjunct)
 (let ((firstconj (remove-synt-pointer (first conjunct))))
   (cond ((null conjunct) nil)
         ((null (rest conjunct))
            (cond ((listp firstconj)
                     (cond ((eq 'and (first firstconj))   ; it must be an "and"
                              (flatten-bq-and firstconj))
                           ((eq (first firstconj) 'eq)
                              conjunct)
                           (t (exception 'semantic-error 
                                        "PROC/buildontorepr: in flattening a formula 1"))))
                  (t conjunct)))
         (t (let* ((restres (int-flat-bq-and (rest conjunct)))
                   (firstrest (remove-synt-pointer (first restres))))
                (cond ((and (listp firstrest)
                            (not (eq (first firstrest) 'eq)))
                          (mult-bq-cons (first conjunct) restres))
                      (t (cons (first conjunct) restres))))))))

(defun mult-bq-cons (firstelem list-of-lists)
   (cond ((null list-of-lists) nil)
         (t (cons (cons firstelem (first list-of-lists))
                  (mult-bq-cons firstelem (rest list-of-lists))))))

(defun act-bq-flat (lists)
  (cond ((null lists) nil)
        ((atom (remove-synt-pointer (first (first lists))))
          (cons (first lists) (act-bq-flat (rest lists))))
        (t (append (first lists) (act-bq-flat (rest lists))))))

; ***************************************************************************
; *** group-bq-paths works on ((a b c) (a e f) (a b d g) (h e))
;     in order to produce: ((a ((b ((c)
;                                   (d ((g)))))
;                               (e ((f))))
;                           (h ((e)))))
;     ARG: (list1 list2 ... listN)
;     RES: (conj1 conj2 ... conjM)
;          where each conjI has the form: (firstel rests)
;          where firstel is a concept, and rests has the same form of RES
(defun group-bq-paths (paths)
  (cond ((null paths) nil)
        ((equal paths '(nil)) nil)
        (t (let ((firsts (elimdup-with-synt (mapcar #'first paths))))
               (collect-bq-rests firsts paths)))))
   
; ***************************************************************************
; *** given a set of starts and the paths, it groups the paths according to 
;     their starts
;     It takes the first start and looks for all its continuations, then 
;     goes on with the remaining starts
(defun collect-bq-rests (firsts paths)
  (cond ((null firsts) nil)
        (t (let* ((tails (group-bq-paths (find-all-rests (first firsts) paths)))
                  (starts (cond ((null tails) (list (first firsts)))
                                (t (list (first firsts) tails)))))
               (cons starts (collect-bq-rests (rest firsts) paths))))))

; ***************************************************************************
; *** this removes duplicate concepts from a list. It takes care of synt pointers:
;     (c1 c2 c3 (synt c1 n) c2 c4 c1) --> (c2 c3 (synt c1 n)  c4)
(defun elimdup-with-synt (elems)
   (let ((res nil))
     (do* ((nxtelem (first elems) (first elems))
           (elems (rest elems) (rest elems)))
          ((null nxtelem) res)
         (setq res (include-with-synt nxtelem res)))))

; ***************************************************************************
; *** adds an element to a list, if not already present. The comparison is made
;     by taking into account the possible presence of "synt" infos:
;     c1 (c3 c2 c4) --> (c3 c1 c4)
;     c5 (c3 c1 c4) --> (c3 c1 c4 c5)
;     (synt n c4) (c3 c1 c4) --> (c3 c1 (synt n c4))
(defun include-with-synt (elem conclist)
  (cond ((null elem) conclist)
        ((null conclist) (list elem))
        (t (let ((cfr (synt-sem-equal (first conclist) elem)))
             (cond ((null cfr)
                     (cons (first conclist) (include-with-synt elem (rest conclist))))
                   (t (cons cfr (rest conclist))))))))

; ***************************************************************************
; *** given an elem and a set of paths, returns the rest of all the paths
;     starting with elem (or with its synt variant)
(defun find-all-rests (elem paths)
  (cond ((null paths) nil)
        ((synt-sem-equal elem (first (first paths)))
           (cond ((eq 1 (length (first paths)))
                     (find-all-rests elem (rest paths)))
                 (t (cons (rest (first paths)) (find-all-rests elem (rest paths))))))
        (t (find-all-rests elem (rest paths)))))

; ***************************************************************************
; *** add-bq-and works on: ((a ((b ((c)
;                                   (d ((g)))))
;                               (e ((f))))
;                           (h ((e)))))
;     and produces:
;                  (and ((a (and ((b (and ((c) (d g))))
;                                 (e f))))
;                        (h e)))
(defun add-bq-and (grouped-f)
   (cond ((null grouped-f) nil)
         ((= 1 (length grouped-f))     ; no "and" is needed
            (cons (first (first grouped-f))
                  (add-bq-and (second (first grouped-f)))))
         (t (list (list 'and (mapcar #'int-a-bq-and grouped-f))))))

(defun int-a-bq-and (single-g-f)
; *** single-g-f has the form (prefix restform)
   (cons (first single-g-f) (add-bq-and (second single-g-f))))

; ***************************************************************************
; *** this function builds the semantic representation for a governing
;     concept and a single restriction of it. The restriction is given as
;     a full annotated subtree (but not as a triple, as it happens
;     in build-sing-restr-sem)
; *** Note that the functions gets as input also information about the object
;     whose restriction is being interpreter. For instance, in "good friend",
;     the fundtion takes care of interpreting "good", but it gets as inputs also:
;     - rootcat (the category of "friend", i.e. NOUN)
;     - singrootsem (i.e. ££friend: on of the possible ontology concepts associated with
;       the word "friend")
;     - up-ident (if non-nil) is the possible identifier associated with singrootsem
; *** currently, the result must be a single list, since given one singrootsem and
;     one restriction, ambiguities should be solved inside the function. This
;     could be changed afterwards
; *** INPUT:
;  >>> rootcat: the category of the governor whose restriction is being
;      interpreted, i.e. of the item one of whose senses is in singrootsem
;  >>> singrootsem: a single possible meaning of the head node
;  >>> syntroot-tree: the whole subtree whose root is in the governing word
;      (e.g. "friend" in "good friend")
;  >>> singrestr: a single syntactic restriction (subtree; e.g. the one of "good")
;  >>> up-ident (optional): a possible identifier (individual) associated with the head
;  >>> them-role (optional): a possible thematic role associated with this restriction (with
;      respect to its governor)
; *** OUTPUT:
;  >>> a pair such that 
;      - its first element is a list representing a single path in the ontology (non-ambiguous)
;        this is the interpretation of the current singrestr, but already linked to its head
;      - its second element is the same path, but excluding the connection to the head
; *** EXAMPLE:
;      "[I] saw the girl with the hat"; Suppose that build-sing-r-sem is evaluated on the restriction
;       "the girl with the hat" (i.e. the direct object). Then
;      singrootsem = ££to-see-1
;      singrestr = tree["the girl with the hat"]
;      ----> ((££to-see-1 &seen ... ££girl ... ££to-wear-1 ... ££hat)
;             (££girl ... ££to-wear-1 ... ££hat))
(defun build-sing-r-sem (rootcat singrootsem syntroot-tree singrestr &optional up-ident them-role)
  (declare (special +FULL-TREE+))
; *** if the restrictions refers to a trace, then use in its place the referent
  (cond ((is-a-actavm-trace? singrestr)
          (let ((corefline (get-actavm-headcorefline singrestr)))
             (cond ((and (not (null corefline))
                         (memq (get-actavm-headcoreftype singrestr) '(#\f f)))
                     (setq singrestr (find-coreferent corefline (list +FULL-TREE+))))))))
  (let ((categ (get-actavm-headcateg singrestr))
        (meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr))
        (restr-syntpos (get-actavm-headnumb singrestr))
        (head-syntpos (get-actavm-headnumb syntroot-tree))
        traceresult result)
  ; (format t "£££££ build-sing-r-sem~%  £££ rootcat: ~a~%  £££ singrootsem: ~a~%  £££ categ: ~a~%  £££ meaning: ~a~%" 
  ;               rootcat singrootsem categ meaning)
  ; (break "")
     (cond ((eq meaning 'verbal-locut) nil)
   ; *** the restriction is a component of a verbal locution; its meaning is
   ;     already included in the verbal head
           ((stringp ident)
              (append          ; *** it is a descriptor enclosed in quotes
                  (choose-best-ontpath (find-shortest-path singrootsem meaning))
                  (list 'has-instance (build-sem-to-synt-ref ident restr-syntpos))))
           ((eq 'ART categ)	
   ; *** a determiner is the head: determiners are currently ignored, so the
   ;     tree is travelled downward
              (build-sing-r-sem rootcat singrootsem 
                        syntroot-tree (skip-determiner singrestr) up-ident them-role))
           ((eq 'ADJ categ)
              (setq result
                  (build-adj-sem rootcat singrootsem syntroot-tree singrestr nil them-role))
         ;     (format t "build-adj-sem; result = ~a~%" result)
         ;     (break "xax")
              result)
           ((eq 'NOUN categ)
              (setq result
                  (build-noun-sem rootcat singrootsem head-syntpos singrestr up-ident nil them-role))
         ;     (format t "build-noun-sem; ~% rootsem: ~a~% singrestr: ~a~% result = ~a~%"
         ;               singrootsem (get-actavm-headlemma singrestr) result)
         ;     (break "xnx")
              result)
           ((eq 'PREP categ)
              (setq result
                  (build-prep-sem rootcat singrootsem syntroot-tree singrestr up-ident them-role))
         ;     (format t "build-prep-sem; ~% rootsem: ~a~% singrestr: ~a~% result = ~a~%"
         ;               singrootsem (get-actavm-headlemma singrestr) result)
         ;     (break "xpx")
              result)
           ((eq 'ADV categ)         		; *** this happens for "domani"
              (build-adv-sem rootcat singrootsem singrestr up-ident))
                                                ;     up-ident is required for intensifiers
           ((eq 'VERB categ)
              (setq result
                  (build-verb-sem rootcat singrootsem
                          (get-actavm-headlexmean syntroot-tree)
                           syntroot-tree singrestr))
             ; (format t "build-verb-sem; result = ~a~%" result)
             ; (break "xvx")
              result)
           ((eq 'PRON categ)
              (build-pron-sem rootcat singrootsem singrestr them-role))
           ((or (eq 'PHRAS categ)         ; *** this happens for "yes/no"
                (eq 'INTERJ categ))       ; *** this happens for "buongiorno", ...
              (build-phras-sem singrootsem singrestr))
           ((eq 'NUM categ)         ; *** this happens for italian descriptions of
                                    ;     dates (Il 10 luglio)
              (build-num-sem singrootsem singrestr))
           ((eq categ 'PUNCT) nil) 
           ((eq categ 'CONJ)
              (build-conj-sem rootcat singrootsem syntroot-tree singrestr))
           ((and (null categ) 
                 (eq meaning '--about-relation))
             ; *** this is a trace for the "about" pronoun, inserted when the user
             ;     answers a request with an NP (e.g. "Abbado")
             (build-sing-restr-sem rootcat singrootsem (get-actavm-headnumb syntroot-tree) singrestr
                 (mapcar #'(lambda (x) (list 'single x nil))
                        (skip-determiner (find-actavm-dep 'PREP-ARG singrestr t) t))))
           (t (exception 'semantic-error "PROC/buildquery: Unknown top category" categ)))))

; ***************************************************************************
; *** INPUT:
;  >>> governor-cat: the category of the word governing the adjective (e.g. NOUN, when
;      interpreting "good" in "a good friend")
;  >>> governor-sem: the meaning (a single, non ambiguous, one) of the word governing the
;      adjective (e.g. ££friend)
;  >>> governor-tree: the syntactic tree headed in "friend"
;  >>> singrestr: the adjectival restriction which is being interpreted (e.g. "good")
;  >>> governor-ident: the (possible) semantic instance id associated with the governor
;  >>> themrole: the (possible) thematic role associated with the adjective (usually nil)
(defun build-adj-sem (governor-cat governor-sem governor-tree singrestr governor-ident themrole)
  (let ((adjtype (get-actavm-headtype singrestr))
        (meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr))
        (syntnode-pos (get-actavm-headnumb singrestr))
        tempsem tempsem2 restrs newtopics tempsem3 tempsem4 actrestrs)
;(format t "Entering build-adj-sem; Meaning= ~a~% Ident= ~a~%" meaning ident)
;(break "")
      (cond ((eq meaning '-dummy-adj) nil)    
             ; *** adjectives (provisionally) not interpreted; this is, for instance, the case
             ;     of "la NOSTRA penisola"
            ((eq adjtype 'interr)
             ; *** interrogative adjectives (quale, che, ...)
             ;     currently, they are simply skipped
               (build-sing-r-sem governor-cat governor-sem governor-tree (skip-determiner singrestr)))
            ((memq adjtype '(deitt demons))
             ; *** deictic and demonstrative adjectives (this, next ...)
               (list (find-path-to-deictic governor-sem singrestr) nil))
            ((eq adjtype 'indef)
             ; *** indefinite adjectives (some ...)
             ;     currently, they are simply skipped
               (build-sing-r-sem governor-cat governor-sem governor-tree (skip-determiner singrestr)))
            ((eq adjtype 'ordin)
             ; *** ordinal adjectives
               (cond ((not (null (find-actavm-dep 'noun-rmod-month singrestr)))
                       ; *** august 3rd
                        (list (build-date-repr governor-sem singrestr) nil))
                       ; *** the next is the standard case: e.g. "first course"
                     (t (setq tempsem 
                            (reverse (choose-best-ontpath (find-shortest-path governor-sem ident))))
                        (list (reverse 
                                 (cons (build-sem-to-synt-ref (first tempsem) syntnode-pos) (rest tempsem)))
                             nil))))
            ((null ident)
             ; *** for standard adjectives, the restriction is interpreted by finding
             ;     the shortest path between the governing noun (governor-sem) and the
             ;     adjective (meaning). 
               (multiple-value-setq (newtopics restrs) (get-nonverbal-restrictions singrestr))
               (setq tempsem (build-restr-sem 'ADJ meaning syntnode-pos singrestr restrs))
                     ;   (break "adj 1")
               (list (compose-restr-path
                          (choose-best-ontpath (find-shortest-path governor-sem meaning themrole))
                          tempsem
                          'build-adj-1)
                     (second tempsem)))
             ; *** the last are adjectives specifying a type (e.g. classical music). In this case,
             ;     the meaning of the adjective has <lexmean, property-name>, <ident, property-value>
             ;     For instance: classical music --> <lexmean ££musical-type> <ident £classic>
             ; *** this also apply to comparison adjectives (ex. greater)
            (t (multiple-value-setq (newtopics actrestrs) (get-nonverbal-restrictions singrestr))
                    ;    (break "adj 2")
             ;  (setq actrestrs (remove-coord-conj restrs))
               (cond ((null actrestrs)			; *** no extra adverbials
             ; *** the next checks if there is a specific adj interpretation (see the comments in
             ;     db-adj-lookup)
                        (setq tempsem (db-adj-lookup governor-sem meaning ident))
                        (cond ((null tempsem)
                                 (list (append 
                                         (choose-best-ontpath 
                                               (find-shortest-path governor-sem meaning themrole))
                                         (list 'has-instance (build-sem-to-synt-ref ident syntnode-pos)))
                                       nil))
                              (t tempsem)))
             ; *** the next is the branch that includes also "greater than"
                     ((one-is-subclass-of meaning '££comparison-operator)
                       ; (break "Comparison operator 1")
                        (setq tempsem 
                              (build-restr-sem 'ADJ meaning syntnode-pos singrestr actrestrs ident))
                      ;  (setq tempsem2 (choose-best-ontpath (first tempsem)))
             ; *** tempsem is the semantic interpretation of a restriction, so it has the form
             ;     ((restrinterp subrestrinterp) link-up)
             ;     Now, what we need is "restrinterp"
                        (setq tempsem2 (first (first tempsem)))
                        (setq tempsem3
                           (choose-best-ontpath (find-shortest-path governor-sem meaning '&compar-arg1)))
                    ;(format t "before building comparison:~%")
                    ;(break "")
                        (cond ((synt-sem-equal (ult tempsem3) (first tempsem2))
                                 (setq tempsem4 (put-up-and tempsem3 tempsem2 (list 'eq ident))))
                              (t (exception 'semantic-error 
                                     "PROC/buildquery: comparison operator in build-adj-sem")))
                    ;(format t "after building comparison:~% ~a~%" tempsem4)
                    ;(break "")
                        (list tempsem4 nil)
                        )
                     (t 
          ; *** this is the standard case for adjectives corresponding to a value (ident not = NIL)
                        (setq tempsem (build-restr-sem 'ADJ meaning syntnode-pos singrestr actrestrs ident))
                     ;   (cond ((null (second tempsem))
                     ;            (setq tempsem (first tempsem)))
                     ;         (t (format t "Warning: multiple paths in build-adj-sem. Random choice")
                     ;            (setq tempsem (first tempsem))))
               ; *** tempsem is a list of restriction interpretations, each having the form
               ;     (restrinterp subrestr-interp)
               ;     if the list is longer than 1, then there is an ambiguity, which is 
               ;     possibly due to the fact that the head adjective is ambiguous (e.g. "high")
               ; *** N.B. The result of build-restr-sem always refers to a single restriction
               ;          in the sense that multiple restrictions have already been attached
               ;          to the root. In "very high in my opinion" each item in the result list
               ;          express the full interpretation, including the two restrictions "very"
               ;          and "in my opinion". So, multiple sublists always denote ambiguity
                        (setq tempsem3 (first (first tempsem)))
               ; *** the first interpretation is chosen random, then its restrinterp part is taken
               ;     The idea is that in checking "intensification", if it occurs, it appears in
               ;     all restrinterps, so that any of them is ok
               ; *** Actually, tempsem3 is also used in put-up-and, but this case should be checked
                        (setq tempsem2 (find-shortest-path governor-sem meaning))
                   ;     (break "adj; xbx")
                        (cond ((intensification-found tempsem3)
          ; *** in this case, the restriction has produced an "intensification" of the original
          ;     adjective (e.g. "very strong" wrt. "strong"), so that the intensified value
          ;     is used in place of the original one
          ;     N.B. The "list tempsem" depends on the Note above: tempsem is a single (possibly
          ;          ambiguous) interpretation, not a list of them
                                 (mult-compose-restr-path tempsem2 tempsem 'build-adj-2))
                              (t (list (put-up-and (choose-best-ontpath tempsem2) tempsem3 (list 'eq ident))
                                       tempsem3)))))))))
                                      
; ***************************************************************************
; *** this function takes a list of restrictions and removes from that the ones concerning
;     coordinating conjunctions; in fact they should have already been handled at an upper
;     level, since they are not true restrictions
; *** A restriction has the structure:
;     (coordinated-trees up-label) where
;     coordinated-trees = (tree1 tree2 tree3 ...)
; *** For instance, in "a nice and sweet girl", "nice" and "sweet" are restrictions of "girl"
;     For interpreting this phrase, the standard recursion gets "nice and sweet" and "sweet"
;     as subtrees restricting "girl", the recurs on the two adjectives. This function removes
;     from the first adjectival phrase "and sweet"
(defun remove-coord-conj (restrs)
  (let (result coordsubtrees)
     (do ((nxtrestr (first restrs) (first restrs))
          (restrs (rest restrs) (rest restrs)))
         ((null nxtrestr) result)
         (setq coordsubtrees (first nxtrestr))
         (cond ((not (lab-subsumes 'COORD (get-actavm-headlink subtree)))
                 (setq result (append1 result nxtrestr)))))))

; ***************************************************************************
; *** an intensification relation has been found if the found path includes a
;     relation instance of ££intensification-rel
(defun intensification-found (path)
   (cond ((null path) nil)
         ((atom (first path))
            (cond ((or (eq (first path) '££intensification-rel)
                       (is-instance-of (first path) '££intensification-rel)) t)
                  (t (intensification-found (rest path)))))
         ((eq 'and (first (first path)))
           (let (found)
            (do* ((nxtarg (first (second (first path))) (first andargs))
                  (andargs (rest (second (first path))) (rest andargs)))
                 ((or (null nxtarg) found) found)
                 (setq found (intensification-found nxtarg)))))
         (t (intensification-found (rest path)))))

; ***************************************************************************
; *** this aims at interpreting structures as "musica classica" or "settore orientale"
;     In the first example we have upconc=££music, downconc=££music-type, downident=£classical
;     In the second we have upconc=££it-geogr-area, downconc=££it-area-spec, downident=£eastern
; *** A simple "find-shortest-path" would return (££music &has-music-type ££music-type (eq £classical))
;     or (££it-geogr-area &has-it-area-spec ££it-area-spec (eq £eastern))
;     So, in both cases, we fail to retrieve the "interesting" concept (£classical-music and
;     £it-eastern-area, respectively)
; *** what I do is to force a "lookup" of £classical (or £eastern) in the "DB table" corresponding
;     to the relation &has-music-type (or &has-it-area-spec), and, if it exists, to retrieve the
;     other item of the relation (provided it is an instance or subclass of ££music, or ££it-geogr-area)
; *** So the structure is:
;      UPCONC<-----domain-------xxxrel-----range-->DOWNCONC
;    (££music)             (&has-music-type)    (££music-type)
;         ^                        ^                   ^
;         |                        |                   |
;      firstarg<---argument-- nxtrelinst---value-->downident
;  (£classic-music)         (&has-m-type7)        (£classical)
;
; *** the returned path will be:
;     (££music HAS-INSTANCE £classic-music ARG-OF &has-m-type7 RELINSTANCE &has-music-type)
;     (RANGE ££music-type HAS-INSTANCE £classical)
;
(defun db-adj-lookup (upconc downconc downident)
   (let ((relinstance (get downident 'VALUE-OF)) firstarg actrel relrange found newpath)
  ; (break "ghg in db-adj-lookup")
       (do ((nxtrelinst (first relinstance) (first relinstance))
            (relinstance (rest relinstance) (rest relinstance)))
           ((or (null nxtrelinst) found) found)
           (setq firstarg (first (get nxtrelinst 'ARGUMENT)))
           (setq actrel (first (get nxtrelinst 'RELINSTANCE)))
           (setq relrange (first (get actrel 'RANGE)))
   ; *** the next loop, because when a lexmean is determined on the basis of an identifier
   ;     (i.e. it is its direct class) it is a list of possible concepts (for instance,
   ;     £northern is an instance both of ££cardinal-direction and of ££it-area-spec)
   ;     This happens also in case there is no ambiguity (downconc is a list of length 1)
           (do ((nxtdownconc (first downconc) (first downconc))
                (downconc (rest downconc) (rest downconc)))
               ((or (null nxtdownconc) found) found)
               (cond ((and (is-instance-or-subclass-of firstarg upconc)	; *** £classic-music - ££music
                           (is-instance-or-subclass-of nxtdownconc relrange))
                        (setq newpath (first (find-shortest-path relrange nxtdownconc)))
                        (setq found
                           (list
                              (append (first (find-shortest-path upconc firstarg))
                                 (list 'ARG-OF nxtrelinst 'RELINSTANCE actrel 'RANGE)
                                 newpath
                                 (list (list 'EQ downident)))
                              (append1 newpath
                                 (list 'EQ downident))))))))))

; ***************************************************************************
; *** this performs the following operation:
;     1. path1 = [x, a, b, c, d, e]
;     2. path2 = [e, d, c, f, g, h]
;     3. eq-expr = [eq k]
;       ---->
;      (x, a, b, c (and ((d, e (eq k)) (f, g, h)))) 
; *** the idea is that (x, a, b, c, f, g, h) is the relevant expression
;        and (d, e) is simply a move allowing to reach the identifier k
;     so, this simplification removes the loop cutting the return path from e to c
(defun put-up-and (path1 path2 eq-expr)
   ;  (format t "entering put-up-and;~% path1:~a;~% path2;~a;~%" path1 path2)
   ;  (break "")
   (let ((revpath1 (inv-range-dom (reverse path1))) begcommon endcommon
         andfirstarg andsecondarg beforeand)
     (do* ((prev1 nil nxt1)		                ; *** at the end, prev1 is c
           (common nil (cons best-eq common))		; *** common is [c, d, e]
           (nxt1 (first revpath1) (first revpath1))	; *** nxt1 is b
           (revpath1 (rest revpath1) (rest revpath1))	; *** revpath1 is [a, x] 
           (nxt2 (first path2) (first path2))		; *** nxt2 is f
           (path2 (rest path2) (rest path2))		; *** path2 is [g, h]
           (best-eq (synt-sem-equal nxt1 nxt2) (synt-sem-equal nxt1 nxt2)))
    ; *** in best-eq the richest description (the one including the pointer to syntax,
    ;     if any) or nil
        ((or (null nxt1) (null nxt2) (not best-eq))
                   (multiple-value-setq (endcommon begcommon)
                       (split-common-part common nil))
   ;  (format t "exiting put-up-and;~% nxt1: ~a;~% nxt2: ~a;~% common:~a;~% revpath1:~a;~% path2;~a;~% begcommon:~a;~% endcommon:~a;~%" nxt1 nxt2 common revpath1 path2 begcommon endcommon)
   ;  (break "")
           (cond ((not (null nxt2))
    ; *** in begcommon the start of "common" up to (and including) the first non-relational
    ;     item; in endcommon the remaining part
    ;     e.g. begcommon = [domain ££comparison-relation]
    ;          endcommon = [domain-of ££compar-op range ££comparison-operator]
    ;          nxt1 = &compar-arg1
    ;          nxt2 = &compar-arg2
    ;          revpath1 = [range ££math-value has-subclass ££measured-value ...]
    ;          path2 = [range ££math-value range-of £applied-function-value ...]
    ;            from which we must get:
    ;            [... ££measured-value subclass-of ££math-value range-of £compar-arg1
    ;              domain ££comparison-relation
    ;                [and [[domain-of &compar-op range ££comparison-operator]
    ;                      [domain-of &compar-arg2 range ££math-value range-of ...]]]]
    ; *** so we must produce three segmants: 
    ;     1. the original part until the fork. This corresponds to the concatenation of the
    ;        inverted revpath1 (i.e. the remaining part of path1), nxt1, and begcommon
    ;     2. the first argument of the and. This is endcommon
    ;     3. the second argument of the and. This is the "continuation" of the original path
    ;        it is composed by the part of begcommon up to (but excluding) the non-relational
    ;        item, nxt2 and path2
                   (setq beforeand 
                       (cond ((null nxt1)
                                (append (reverse (inv-range-dom revpath1)) 
                                        (inv-range-dom begcommon)))
                             (t (append (reverse (inv-range-dom revpath1)) 
                                           (list (inv-r-d nxt1)) (inv-range-dom begcommon)))))
                   (setq andfirstarg (append2 (inv-range-dom endcommon) eq-expr))
                   (setq andsecondarg (append (butlast begcommon) (list nxt2)
                                           path2))
                   (append1 beforeand (list 'and (list andfirstarg andsecondarg))))
   ; *** the next branch must be done
                 (t (append (reverse (cons prev1 (cons best-eq revpath1)))
                            (append2 (rest common) eq-expr)))))
   ;  (format t "put-up-and loop;~% nxt1: ~a;~% nxt2: ~a;~% common:~a;~%" nxt1 nxt2 common)
   ;  (break "")
        )))
                         
; ***************************************************************************
; *** splits a path in two parts; the separator (included in the first part) is the
;     first non-relational concept
(defun split-common-part (restpath begpath)
  ;(format t "split-common-part; restpath= ~a;~% begpath= ~a~%" restpath begpath)
  ;(break "")
   (cond ((null restpath)
            (break "non-relational concept not found in buildquatlas: split-common-part"))
         ((and (or (atom (first restpath))
                   (eq 'synt (first (first restpath))))
               (is-subclass-of (remove-synt-pointer (first restpath)) '££entity))
            (values (rest restpath) (append1 begpath (first restpath))))
         (t (split-common-part (rest restpath) (append1 begpath (first restpath))))))

; ***************************************************************************
;  >>> governor-cat: the category of the governor whose restriction is being
;      interpreted, i.e. of the item one of whose senses is in semtopic
; *** in case of governing adj (ex. mare molto mosso),
;     -- governor-cat = adj 
;     -- semtopic is the meaning of the adj (e.g. "mosso": ££sea-status-description)
;     -- singrestr is the adverbial modifier (e.g. molto)
;     -- up-ident is the identifier of the adj (e.g. "mosso": £mare-agitato)
; *** In case of governing verb: (ex cantava spesso)
;     -- governor-cat = verb 
;     -- semtopic is the meaning of the verb (e.g. "cantare": ££to-sing)
;     -- singrestr is the adverbial modifier (e.g. spesso)
;     -- up-ident is the identifier of the verb (presumably NIL)
(defun build-adv-sem (governor-cat semtopic singrestr up-ident)
  (let ((meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr))
        (adv-syntpos (get-actavm-headnumb singrestr))
        (adv-type (get-actavm-headtype singrestr))
        (adv-semtype (get-actavm-headsemtype singrestr))
        temp-path temp2-path subrestr)
     ;(format t "Entering build-adv-sem; meaning: ~a~%" meaning)
     ;(break "xkx")
     (cond ((not (null (get-actavm-headthemrole singrestr)))
  ; *** this branch is for adverbials having a relational meaning, as "quindi". In this case,
  ;     meaning is: (££to-cause (thematic-role &causee))
  ;     If the fragment is "è caduta quindi" (has fallen therefore), then I look for the path
  ;     from "to-fall" to "to-cause", passing through &causee 
              (setq temp-path
                  (reverse
                    (choose-best-ontpath 
                        (find-shortest-path semtopic 
                              meaning  (get-actavm-headthemrole singrestr)))))
              (setq temp2-path 
                   (reverse (cons (build-sem-to-synt-ref (first temp-path) adv-syntpos)
                                  (rest temp-path))))
              (list temp2-path (list (build-sem-to-synt-ref meaning adv-syntpos))))
           ((eq meaning '--max-) nil)
	     ; *** the above is for "il piu' vicino". Currently, "più" is ignored
           ((memb-or-eq '££intensifier meaning)
	     ; *** this is for, e.g., "molto".
             ;     I assume that in this case ident and up-ident have a value
              (setq temp-path (find-subtype-path up-ident ident 'intens))
              (setq temp2-path (choose-best-ontpath (find-shortest-path semtopic meaning)))
       ;  (format t "build-adv-sem; temp2-path: ~a~%" temp2-path)
       ;  (break "xux")
              (cond ((null temp-path)
      ; *** temp2-path goes from the property at hand to the "££intensifier" concept
      ;     What is produced is 
      ;     (property (and (has-instance "non-intensified.meaning")
      ;                    (... intensifier path ... has-instance --intensifier-)))
                       (setq subrestr (append (rest temp2-path) (list 'has-instance ident)))
                       (list `(,(first temp2-path)
                                   (and ((has-instance ,up-ident) ,subrestr)))
                             subrestr))
                    (t (list
                          (cons semtopic
                              (cons (list 'temp-eq (first temp-path))
                                    (rest temp-path)))
                          (list (ult temp2-path) 'instance-of up-ident)))))
           ((memb-or-eq '££empty-conc meaning) nil)
             ; *** the above for adverbs which do not carry information (e.g. "in programma",
             ;     where "in programma" is taken as an adverbial locution)
           ((memb-or-eq '£manner meaning) nil)
             ; *** the above for how. It is ignored, since in "how can I buy"
             ;     the actual topic is "buy"
           ((null ident)	; *** standard adverb
             (cond ((or (eq adv-semtype 'TIME)
                        (eq adv-type 'TIME))
                      (setq temp-path 
                        (reverse (choose-best-ontpath 
                                     (find-shortest-path semtopic meaning '&has-situation-time)))))
                   (t (setq temp-path 
                        (reverse (choose-best-ontpath (find-shortest-path semtopic meaning))))))
             (list (reverse (cons (build-sem-to-synt-ref (first temp-path) adv-syntpos)
                                  (rest temp-path)))
                   (list meaning)))
           (t (cond ((or (eq adv-semtype 'TIME)
                         (eq adv-type 'TIME))
                       (setq temp-path 
                            (choose-best-ontpath 
                                     (find-shortest-path semtopic meaning '&has-situation-time))))
                    (t (setq temp-path (choose-best-ontpath (find-shortest-path semtopic meaning)))))
              (cond ((null temp-path)
                       (exception 'semantic-error
                                "PROC/buildquery: Empty path in build-adv-sem"))
                    (t (list (append temp-path 
                                (list 'has-instance (build-sem-to-synt-ref ident adv-syntpos)))
                             (list (ult temp-path) 'has-instance 
                                   (build-sem-to-synt-ref ident adv-syntpos)))))))))

; ***************************************************************************
;  >>> governor-cat: the category of the governor whose restriction is being
;      interpreted, i.e. of the item one of whose senses is in semtopic
;  >>> semtopic: its meaning
(defun build-conj-sem (governor-cat semtopic topic-subtree singrestr)
  (declare (special annotated-tree))
  (let ((meaning (get-actavm-headlexmean singrestr))
        (downtree (find-actavm-dep 'CONJ-ARG singrestr))
        categ downtree tempsem tempsem2 restrs adjtype ident syntnode-pos)
     (cond ((eq meaning '--and-operator)
              ;(format t "In build-conj-sem; coordination have already be handled at the upper level")
              ;(break "")
              ;(setq downtree (find-actavm-dep 'COORD2ND singrestr))
              ;(setq categ (get-actavm-headcateg downtree))
              ;(cond ((and (eq 'ADJ governor-cat)
              ;            (eq 'ADJ categ))
              ;         (setq adjtype (get-actavm-headtype downtree))
              ;         (setq syntnode-pos (get-actavm-headnumb downtree))
              ;         (setq ident (get-actavm-headlexident downtree))
              ;         (cond ((null ident)
              ;                 (multiple-value-setq (newtopics restrs) 
              ;                               (get-nonverbal-restrictions downtree))
              ;                 (setq tempsem (build-restr-sem 'ADJ meaning syntnode-pos downtree restrs))
              ;                 (list 'adj-coord tempsem))))
      ; *** adjective coordination. Assuming all standard qualifying adjectives
              ;     (t (exception 'semantic-error "PROC/buildquery: conj (under development)")))
              nil)
           ((eq meaning '--neutral-conj)
    ; *** subordinating conjunctions
              (setq downtree (find-actavm-dep 'CONJ-ARG singrestr))
              (cond ((eq (get-actavm-headcateg downtree) 'VERB)
                       (build-verb-sem governor-cat semtopic 
                                (get-actavm-headlexmean topic-subtree) topic-subtree downtree))
                    (t (exception 'semantic-error 
                              "PROC/buildquery: conj subord not governing a verb"))))
           (t (exception 'semantic-error "PROC/buildquery: Unknown top conj")))))
                                       
; ***************************************************************************
; *** it builds the semantic representation of "singrestr" and links it to 
;     semtopic. singrestr has a nominal head
(defun build-noun-sem (governor-cat semtopic topic-syntpos singrestr 
                            &optional semtopic-ident quantif them-role)
  (let ((is-proper (eq (get-actavm-headtype singrestr) 'proper))
        (meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr))
        (syntnode-pos (get-actavm-headnumb singrestr))
        (deps (remove-continuations (get-actavm-deps-with-traces singrestr)))
        street-numb best-ontpath prep-dep restr-meaning qmean temppath 
        topics restrs actmeaning upper-path end-concept)
            ; (format t "Enter build noun sem~%  semtopic: ~a~% meaning: ~a~% themrole: ~a~%" 
            ;          semtopic meaning them-role)
            ; (break "")
   ; *** the next is to "modify" the meaning or the identifier in case the "quantif" is
   ;     non-null. This applies to "un po' di pioggia" or "un po' di instabilità". The
   ;     assumption (to be verified) is that in case the noun is associated with an
   ;     identifier (as, in the current implementation "£instabilità"), then the
   ;     identifier has to be replaced by a related modifier (e.g. "£moderata-instabilità"),
   ;     while in case it is associated just to a generic concept (e.g. "££rain"), a path
   ;     must be found (subtype path) to another concept (e.g. "££moderate-rain"). The two
   ;     situations seem to be the same, and probably this is true, and depends on some
   ;     wrong decisions about the presence of an identifier for £instabilità. However,
   ;     at the present stage, I prefer to keep both roads open.
     (cond ((null ident)
             (cond ((not is-proper)	; *** it is not a proper name
           ; *** interpret the restriction, and link it to the parent
           ;     now, topics is the meaning of the head noun
                      (multiple-value-setq (topics restrs)
                                     (get-nonverbal-restrictions singrestr))
                      (setq restr-meaning
                           (build-restr-sem 'NOUN topics syntnode-pos singrestr restrs))
           ; *** but if, from the upper level, a quantifier has been sent below, include
           ;     it in the resulting structure
                    ;(format t "Standard common noun; restr-meaning 1: ~a~%" restr-meaning)
                    ;(break "jxj")
                      (cond ((not (null quantif))
           ; *** see the function put-quantif below, for an explanation on quantifiers
                              (setq temppath 
                                  (choose-best-ontpath 
                                      (find-shortest-path meaning (first quantif))))
                              (setq restr-meaning (put-quantif restr-meaning quantif temppath))))
                      (cond ((neq meaning (remove-synt-pointer (first (first (first restr-meaning)))))
                   ; *** there has probably be an inversion "Avere nubi in aumento" --> "Avere aumento
                   ;     di nubi"
                                (setq meaning (remove-synt-pointer 
                                                     (first (first (first restr-meaning)))))))
                      (cond ((null them-role)
                               (setq upper-path (find-shortest-path semtopic meaning)))
                            (t (setq upper-path (find-shortest-path semtopic meaning them-role))))
                      ;(break "in build-noun-sem 1")
                      (mult-compose-restr-path upper-path restr-meaning 'build-noun-1))
                   (t 		; *** it has a null ident, but it is a proper name
                     ; (break "in build-noun-sem 2")
                     (let (downtopics downrestrs new-interp-restr ident-repr)
                         (multiple-value-setq (downtopics downrestrs)
                                         (get-nonverbal-restrictions singrestr))
                         (cond ((not (null downrestrs))
                                 (exception 'semantic-error
                                        "PROC/buildquery: Proper name with restrictions"))
                               (t (setq meaning (get-default-ident semtopic))
                                  (cond ((null meaning)
                                          (exception 'semantic-error
                                                 "PROC/buildquery: Unknown identifier"))
                                        (t (cond ((null them-role)
                                                    (setq upper-path (find-shortest-path semtopic meaning)))
                                                 (t (setq upper-path 
                                                           (find-shortest-path semtopic meaning them-role))))
                         ; *** what must now be done is:
                         ;     upper-path ((C1 C2 C3) (C1 C4 C5 C3))
                         ;     result of build-restr-sem: (((C3 X Y Z) (Y Z)) ((C3 V Z) (Z)))
                         ;     ---> choose one connection: ((C1 C2 C3 V Z) (C3 V Z))
                         ;     ---> attach to it the identifier information:
                         ;              ((C1 C2 C3 V Z has-instance I) (C3 V Z has-instance I))
                                           (setq new-interp-restr
                                               (mult-compose-restr-path upper-path
                                                   (build-restr-sem 
                                                       'NOUN meaning syntnode-pos singrestr nil)
                                                    'build-noun-2))
                                           (setq ident-repr
                                                (list 'has-instance 
                                                    (build-sem-to-synt-ref
                                                        (get-actavm-headlemma singrestr) syntnode-pos)))
                                           (list (list
                                              (append (first new-interp-restr) ident-repr)
                                              (append (second new-interp-restr) ident-repr)))))))))))
           ((equal '((#\#)) deps)   ; *** it has a non-null ident and no dependents
    ; (format t "Proper name: ~% semtopic: ~a~% meaning: ~a~% ident: ~a~%" semtopic meaning ident)
    ; (break "")
              (cond ((null them-role)
                       (setq upper-path (choose-best-ontpath (find-shortest-path semtopic ident))))
                    (t (setq upper-path 
                           (choose-best-ontpath (find-shortest-path semtopic ident them-role)))))
              (cond ((null upper-path)
                       (exception 'semantic-error
                                  "PROC/buildquery: No path from topic to meaning"))
                    (t (list (append1 (butlast upper-path)
                                      (build-sem-to-synt-ref ident syntnode-pos))
                              nil))))
           (t 	; *** ident is non-null, but there are dependents
                ;     This is, for instance, the case of "South of ..."
                ; *** Here, for "weather in the South of Italy", semtopic=weather
                ;     and meaning=cardinal-direction
          ; *** First, we interpret the restrictions.
          ;     We pass downward "ident", which is necessary for the correct
          ;     interpretation of the restriction "of Italy", in the phrase
          ;     "South of Italy", where a subpart path is involved,
          ;     since "South of Italy" is not a special type of South, but an
          ;     identifier of the Italian Southern Regions.
          ; *** the next cond for quantifier nouns (as "un poco di X" - a little of X)
          ;     it checks the structure depending on "little", and then interprets the X structure
               (cond ((member ident '(--q-little))
                        (setq deps (remove-head-marker deps))
                        (cond ((null deps)
                                 (exception 'semantic-error
                                    "PROC/buildquery: Quantifier noun without dependents")))
                        (cond ((> (length deps) 1)
                                 (exception 'semantic-error
                                    "PROC/buildquery: Quantifier noun with many dependents")))
                        (setq prep-dep (first deps))
                        (cond ((neq (get-actavm-headcateg prep-dep) 'prep) 
                                 (exception 'semantic-error
                                    "PROC/buildquery: Quantifier noun with a non-prep dependent")))
                        (setq singrestr 
                               (skip-determiner (find-actavm-dep 'PREP-ARG prep-dep)))
                        (setq qmean (choose-best-ontpath 
                                           (find-shortest-path meaning ident)))
           ; *** now, we have moved down in the tree, skipping the nominal quantifier structure:
           ;     apply again "build-noun-sem", keeping the upper node (semtopic) unchanged
           ; *** the extra argument (qmean) enables the interpreter to add the
           ;     information that the depending noun has a "modifying" quantifier
           ;     (e.g. "un po' di pioggia" "a little [of] rain")
                        (build-noun-sem governor-cat semtopic topic-syntpos
                                                singrestr semtopic-ident qmean))
                     (t		; *** it is not a quantifier noun
                        (multiple-value-setq (topics restrs)
                                         (get-nonverbal-restrictions singrestr))
                        (setq restr-meaning 
                              (build-restr-sem 'NOUN meaning syntnode-pos singrestr restrs ident))
   ;(format t "Semtopic: ~a; meaning: ~a~% restr-meaning: ~a~%" semtopic meaning restr-meaning)
   ;(break "build noun sem 2")
                        (cond ((null restr-meaning)
    ; *** if have not found a subtype or subpart path, then try standard interpretation
    ; *** this is the case of "instabilità al Sud", where "instabilità" is a weather
    ;     description with identifier £instabilità
                                (setq best-ontpath 
                                     (choose-best-ontpath 
                                          (cond ((null them-role) (find-shortest-path semtopic meaning))
                                                (t (find-shortest-path semtopic meaning them-role)))))
                                (cond ((not (null best-ontpath))
                                         (multiple-value-setq (topics restrs)
                                                             (get-nonverbal-restrictions singrestr))
                                         (setq restr-meaning
                                             (build-restr-sem 'NOUN meaning syntnode-pos singrestr restrs
                                                       ident))
                                         (list (compose-restr-path
                                                  (append best-ontpath (list 'instance-of semtopic-ident))
                                                  restr-meaning 'build-noun-3)
                                               restr-meaning))
                                      (t nil)))
    ; *** a restr-meaning is the full path from the down concept, including all of its
    ;     restrictions. It is assumed that in case of restrictions, just one solution
    ;     exists (which is "first restr-meaning"). Since a solution is a pair of paths
    ;     (one for the restriction, the other for its sub-restrictions), we use
    ;     the first of its first, to find the connections with the node above
                              (t (setq end-concept (first (first (first restr-meaning))))
                                 (setq best-ontpath
                                    (cond ((null them-role) 
       ; *** the append1+butlast is required to re-introduce the possible synt pointer, in case it was
       ;     removed to carry out the search for the shortest path
                                              (append1 
                                                 (butlast
                                                    (choose-best-ontpath
                                                       (find-shortest-path 
                                                            semtopic (remove-synt-pointer end-concept))))
                                                 end-concept))
                                          (t (append1 
                                                 (butlast
                                                    (choose-best-ontpath
                                                       (find-shortest-path 
                                                             semtopic (remove-synt-pointer end-concept)
                                                             them-role)))
                                                 end-concept))))
          ;(format t "Build-noun-sem; best-ontpath: ~a~%" best-ontpath)
          ;(break "")
                                 (cond ((null semtopic-ident) 
                                          (compose-restr-path 
                                                      best-ontpath restr-meaning 'build-noun-4))
                                       (t (list (append (list (first best-ontpath)
                                                        'instance-of semtopic-ident)
                                                     (compose-restr-path 
                                                          (rest best-ontpath) 
                                                          restr-meaning 'build-noun-5))
                                                restr-meaning)))))))))))

; ***************************************************************************
; *** restr-meaning is a set of interpreted restrictions (ambiguities)
;     ((interp1 restr1) (interp2 restr2) ...)
;          ((££weather-status-unstable <subpath1> £south) r1)    [instabilità al sud]
;     quantif is a triple (property-measured HAS-INSTANCE value)
;          (££degree has-instance --q-little)
;     temppath is a path connecting the upper concept to the property measured
;          (££weather-status-unstable <subpath2> ££degree)
; *** the result should include the quantifier in the representation:
;          ((££weather-status-unstable (and ((<subpath1> £south)
;                                            (<subpath2> (££degree (eq --q-little))))))
;           r1)
(defun put-quantif (restr-meaning quantif temppath)
  (let (result actinterp restrinterp)
   (dolist (nxtmean restr-meaning result)
      (setq actinterp (first nxtmean))
      (setq restrinterp (second nxtmean))
      (setq result 
           (append1 result
                (list 
                   (list (first actinterp)
                         (list 'and (list (append (rest temppath)
                                              (replace-has-instance (rest quantif)))
                                    (rest actinterp))))
                   restrinterp))))))

; ***************************************************************************
; *** replaces all pairs "... has-instance instanceid ..." with "... (eq instanceid) ..."
(defun replace-has-instance (path)
   (cond ((null path) nil)
         ((eq (first path) 'has-instance)
            (cons (list 'eq (second path))
                  (replace-has-instance (rest (rest path)))))
         (t (cons (first path) (replace-has-instance (rest path))))))

; ***************************************************************************
(defun get-default-ident (semtopic)
  (declare (special *DEFAULT-CONCEPT-INFOS*))
  ; *** *DEFAULT-CONCEPT-INFOS* has the form:
  ;     ( (conc1 (c11 c12 .... ident-conc1 subclass-of datatype))
  ;       (conc2 (c21 c22 .... ident-conc2 subclass-of datatype)) ...)
  ;     Here, we must extract ident-concK, if semtopic=concK
  (third (reverse (first (leggi *DEFAULT-CONCEPT-INFOS* semtopic)))))

; ***************************************************************************
(defun build-num-sem (semtopic singrestr)
  (let ((meaning (get-actavm-headlexmean singrestr)))
     (cond ((eq meaning '££day-numb-descr)
             (build-date-repr semtopic singrestr))
           ((eq semtopic '££number)
             ; *** This is for "number 3"
              `(,semtopic (eq ,(get-actavm-headvalue singrestr))))
           ((eq semtopic '££dialogue)
             ; *** This is for "3"
              (append1 
                  (choose-best-ontpath (find-shortest-path semtopic '££number))
                  `(eq ,(get-actavm-headvalue singrestr))))
           ((not (null (find-actavm-dep 'contin singrestr)))
             ; *** this for "the 3rd", where "rd" is linked to 3 via "contin". We
             ;     should also check it is of category "ordinsuff"
              (append1 
                  (choose-best-ontpath (find-shortest-path semtopic '££number))
                  `(eq ,(get-actavm-headvalue singrestr))))
           ((is-subclass-of semtopic '££location)   ; assume that the number is a street number
                                                    ; so it has already been analyzed in the
                                                    ; governing street name
              `(,semtopic
                range-of
                &has-street-number    ; *** Via Roma, 33
                domain
                ££street-number
                (eq ,(get-actavm-headvalue singrestr))))
           (t nil 
   ; *** this case refers to "meteo 2" in the ATLAS context. Now, the number 2 is simply ignored
            ; (exception 'semantic-error
            ;             "PROC/buildquery: Unknown number use" meaning)
              ))))

; ***************************************************************************
(defun build-phras-sem (semtopic singrestr)
  (let ((meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr)))
     (cond ((null ident)
              (cond ((lab-subsumes 'EMPTYCOMPL (get-actavm-headlink singrestr)) nil)
                    (t (exception 'semantic-error
                   "PROC/buildquery: Phrasal category or interjection, but without a specific meaning"))))
           (t (list
                (append          ; *** it is "yes" or "no"
                  (choose-best-ontpath (find-shortest-path semtopic meaning))
                  (list 'has-instance ident))
                nil)))))

; ***************************************************************************
(defun build-prep-sem (governor-cat semtopic topic-subtree singrestr &optional up-ident them-role)
  (declare (special *PREP-TEMPLATES* *LANGUAGE*))
  (let ((meaning (get-actavm-headlexmean singrestr)) downtree tempres up-path result downresult
        topics restrs inv-dep other-deps newtree down-syntpos topic-syntpos new-syntpos
        target-tree inv-modif)
    ; *** a query beginning with "on"
     ;(format t "Entering build-prep-sem ~% semtopic:  ~a~% singrestr: ~a~%"
     ;        semtopic (get-actavm-headlemma singrestr))
     ;(break "jej")
 ; *** CASE 1: top of the tree; analysis of the dummy prep --about-relation identifying the 
 ;             topic of the dialogue
     (cond ((and (memq meaning '(--about-relation --on-relation))
                 (eq semtopic '££dialogue))
              (setq downtree (skip-determiner (find-actavm-dep 'PREP-ARG singrestr)))
              (cond ((eq 'PREP (get-actavm-headcateg downtree))
       ; *** This is for "per buttare dei mobili"
                       (setq result
                          (build-sing-r-sem governor-cat semtopic topic-subtree 
                            (skip-determiner (find-actavm-dep 'PREP-ARG downtree)))))
                    (t (setq result
                          (build-sing-r-sem governor-cat semtopic topic-subtree downtree up-ident)))))
 ; *** CASE 2: preposition marking a thematic role ************************************************
           ((not (null them-role))
     ; *** in this case, the preposition marks a given semantic role. This can happen both for
     ;     verbs "He went TO Rome" and for nouns "an infiltration OF air"; I assume that the 
     ;     preposition has already fulfilled its function, by identifying the thematic role, so
     ;     it is simply skipped
    ;(format t "Build-prep-mean; them-role: ~a~%" them-role)
    ;(break "zero")
             (setq result
              (build-sing-r-sem governor-cat semtopic topic-subtree 
                            (skip-determiner (find-actavm-dep 'PREP-ARG singrestr)) up-ident them-role)))
           (t (let ((prep-mean (first (leggi *PREP-TEMPLATES* meaning)))
                    (downtree (skip-determiner (get-preposition-arg singrestr)))
                    down-mean down-ident act-prep-mean down-type downdowntree)
    ;(format t "Build-prep-mean; Prep sem - downtree: ~a~%" downtree)
    ;(break "Uno")
 ; *** CASE 3: preposition without an entry in the preplate table (should we account for this?) ***
                 (cond ((null prep-mean)
                        ; *** in case the preposition has no associated preplate entry,
                          (setq down-ident (get-actavm-headlexident downtree))
                          (cond ((and (not (null up-ident))
                                      (not (null down-ident)))
                            ; *** if it links two items having an identifier, try to
                            ;     link the identifiers, via a subpart relation
                                   (setq result
                                      (build-subpart-repr 
                                        governor-cat up-ident down-ident semtopic topic-subtree downtree)))
                                (t (setq result
                                      (build-sing-r-sem governor-cat semtopic topic-subtree downtree)))))
 ; *** CASE 4: preposition linking to an upper relational noun (should this be a subcase of 3?)
                       ((and (is-relational-conc semtopic)
                             (has-gramm-type (get-actavm-headlemma singrestr) '&neutral-prep))
                        ; *** if the upper concept is "relational", as "mother", the
                        ;     connecting preposition (mother of) simply marks an argument
                        ;     of the "mother" relation, without carrying any specific meaning
                        ; *** has-gramm-type in chunk-parser
                          (setq result
                              (build-sing-r-sem governor-cat semtopic topic-subtree downtree)))
 ; *** CASE 5: the PP refers to a trace; should we do something here? **************************
                       ((is-a-actavm-trace? singrestr) nil)
                       (t (setq down-mean (get-actavm-headlexmean downtree))
                          (setq down-ident (get-actavm-headlexident downtree))
                          (setq down-type (get-actavm-headtype downtree))
                          (setq down-syntpos (get-actavm-headnumb downtree))
    ;(format t "Build-prep-mean; down mean: ~a~%      down-ident:~a~%" down-mean down-ident)
    ;(break "Due")
 ; *** CASE 6: standard PP **************************************************************************
    ; *** substep 6.1: pick the subtree governed by the preposition, skipping determiners
                          (cond ((and (eq 'ADJ (get-actavm-headcateg downtree))
                                      (memq down-type '(DEITT DEMONS)))
                                  ; *** in case the head of downtree is a demonstrative or
                                  ;     deictic adjective, move down one level (events of THIS
                                  ;     saturday; events of NEXT weekend)
                                   (cond ((eq down-type 'DEITT)
                                            (setq downdowntree
                                                   (find-actavm-dep 'DET+INDEF-ARG downtree)))
                                         ((eq down-type 'DEMONS)
                                            (setq downdowntree
                                                   (find-actavm-dep 'DET+DEF-ARG downtree))))
                                   (cond ((not (null downdowntree))
                                            (setq downtree downdowntree)
                                            (setq down-mean (get-actavm-headlexmean downtree))
                                            (setq down-ident (get-actavm-headlexident downtree))))))
                           ; *** now we have found the actual argument of the preposition, so
                           ;     we look for the actual meaning in the preplate table
    ; *** substep 6.2: inspection of the preplate entry
                          (cond ((eq 'unambiguous prep-mean)
                                   (setq act-prep-mean meaning))
                                (t (setq act-prep-mean 
                                        (select-prep-mean prep-mean semtopic down-mean))))
         ; *** case 6.2.1: no match in the preplate table
                          (cond ((eq act-prep-mean 'fail)
                           ; *** a first case of failure could be the occurrence of a subtype or
                           ;     subpart relation. For instance in "Sud d'Italia", we do not
                           ;     have in the preplate table that "di" could mean that "Sud d'Italia"
                           ;     is a part-of Italia. So special processing
                                  (cond ((and (listp down-mean)
                                              (eq 1 (length down-mean)))
                                           (setq down-mean (first down-mean))))
             ; *** case 6.2.1.1: the items linked by the prep have an ident 
                                  (cond ((and (not (null down-ident))
                                              (not (null up-ident)))
                            ; *** if it links two items having an identifier, try to
                            ;     link the identifiers, via a subpart relation
                                           (let ((subpart-spec
                                                   (build-subpart-repr governor-cat
                                                       up-ident down-ident semtopic topic-subtree downtree)))
                                               (cond ((null subpart-spec)
                                                       (exception-internal 'semantic-error
                                                          "PROC/buildquery: no meaning selected for a preposition"
                                                          meaning prep-mean))
                                                     (t (setq result subpart-spec)))))
             ; *** case 6.2.1.2: the depending item is a property-value or a measure unit
                                        ((is-subclass-of-one-of down-mean 
                                                   '(££property-value ££measureunit))
                   ; *** check if there is an inversion in property val ("giorno con valori di 
                   ;     temperatura elevati" or "temperature in aumento");
                   ;     target-tree is in the first case the one rooted in "valori" in the second one
                   ;     the tree rooted in "temperature"
                                           (setq target-tree downtree)
                                           (setq inv-modif nil)
                                           (multiple-value-setq 
                                                (inv-dep down-mean act-prep-mean other-deps)
                                                (find-inverted-property target-tree semtopic meaning))
                   ;     inv-dep is in the first case the one rooted in "temperatura" in the second one
                   ;     the tree rooted in "aumento"
                                           (cond ((null inv-dep)
                                                   (exception 'semantic-error
                                                     "PROC/buildquery: no meaning for a preposition 1"))
                   ; *** the inverted property has been found:
                   ;     in inv-dep there is the subtree of the depending noun related with the property
                   ;     (di temperatura), while in other-deps we have all other dependents of it 
                   ;     (elevati)
                                                 (t (setq newtree
                                                       (build-inverted-tree 
                                                           target-tree inv-dep other-deps inv-modif))
                                  ;  (break "newtree in build-prep-sem")
                                                    (setq new-syntpos (get-actavm-headnumb newtree))
                                                    (multiple-value-setq (topics restrs)
                                                                (get-nonverbal-restrictions newtree))
                                        ;   (format t "Inverted property;~% inv-dep: ~a;~% other-deps: ;~a~% down-mean: ~a;~% down-syntpos: ~a;~% restrs: ~a~%"
                                        ;              inv-dep other-deps down-mean down-syntpos restrs)
                                        ;   (break "")
                                                    (setq downresult
                                                        (build-restr-sem
                                                             'NOUN topics new-syntpos newtree restrs))
                                                    (setq result
                                                      (compose-restr-path
                                                        (choose-best-ontpath
                                                            (find-shortest-path 
                                                                semtopic down-mean act-prep-mean))
                                                        downresult 'build-prep-1))
                                                   ; (setq result 
                                                   ;     (compose-restr-path (list down-mean) downresult))
                                                    )))
             ; *** case 6.2.1.3: the depending item refers to an amount-modification
                                        ((is-subclass-of-one-of down-mean 
                                                   '(££amount-modification))
                   ; *** check if there is an inversion in property val ("temperature in aumento");
                   ;     this case is different from the previous one, since the "trigger" of the
                   ;     inversion is the dependent (aumento) not the head (valori)
                                           (setq target-tree topic-subtree)
                                           (setq inv-modif t)
                                           (multiple-value-setq 
                                                (inv-dep down-mean act-prep-mean other-deps)
                                                (find-inverted-property target-tree semtopic meaning))
                   ;     inv-dep is in the first case the one rooted in "temperatura" in the second one
                   ;     the tree rooted in "aumento"
                                           (cond ((null inv-dep)
                                                    (exception 'semantic-error
                                                        "PROC/buildquery: no meaning selected for a preposition 1"))
                   ; *** the inverted property has been found:
                   ;     in inv-dep there is the subtree of the depending noun related with the property
                   ;     (di temperatura), while in other-deps we have all other dependents of it 
                   ;     (elevati)
                                                 (t (setq newtree
                                                       (build-inverted-tree 
                                                           target-tree inv-dep other-deps inv-modif))
                                  ;  (break "newtree in build-prep-sem")
                                                    (setq new-syntpos (get-actavm-headnumb newtree))
                                                    (multiple-value-setq (topics restrs)
                                                                (get-nonverbal-restrictions newtree))
                                        ;   (format t "Inverted property;~% inv-dep: ~a;~% other-deps: ;~a~% down-mean: ~a;~% down-syntpos: ~a;~% restrs: ~a~%"
                                        ;              inv-dep other-deps down-mean down-syntpos restrs)
                                        ;   (break "")
                                                    (setq downresult
                                                        (build-restr-sem
                                                             'NOUN topics new-syntpos newtree restrs))
                                                  ;  (setq result
                                                  ;    (compose-restr-path
                                                  ;      (choose-best-ontpath
                                                  ;          (find-shortest-path 
                                                  ;              semtopic down-mean act-prep-mean))
                                                  ;      downresult 'build-prep-1))
                                                     (setq result 
                                                         (compose-restr-path (list down-mean) downresult))
                                                    )))
                                        (t (exception-internal 'semantic-error
                                             "PROC/buildquery: no meaning selected for a preposition 2"))))
         ; *** case 6.2.2: there is a preplate entry, but the restriction is not relevant
                   ; *** the next for restrictions which do not carry information
                   ;     (e.g. "in programmazione"). These must be encoded in the preplate table
                                ((eq down-mean '££empty-conc) nil)
         ; *** case 6.2.3: Demontrative adjectives: Deictic referent?
                                ((and (eq 'ADJ (get-actavm-headcateg downtree))
                                      (eq 'demons (get-actavm-headtype downtree)))
                                   (setq result
                                     (find-path-to-deictic semtopic downtree act-prep-mean)))
         ; *** case 6.2.4: there is a preplate entry and the dependent has no associated identifier
         ;     ************ standard case *****************************************************
                                ((null down-ident)
                   ; *** the argument of the preposition is not a proper name (standard case)
                                  (multiple-value-setq (topics restrs)
                                                         (get-nonverbal-restrictions downtree))
     ; (format t "Build-prep-sem: downtree = ~a~% downmean: ~a~%" downtree down-mean)
     ; (break "Due bis")
                                  (setq downresult
                                       (build-restr-sem 'NOUN down-mean down-syntpos downtree restrs))
     ; (format t "Build-prep-sem: downresult = ~a~% downmean: ~a~%" downresult down-mean)
     ; (break "Due ter")
                                  (setq result
                                    (compose-restr-path
                                       (choose-best-ontpath
                                            (find-shortest-path semtopic down-mean act-prep-mean))
                                       downresult 'build-prep-2))
     ;(format t "Build-prep-mean; down mean: ~a~%      down-ident:~a~%" down-mean down-ident)
     ;(break "Tre")
                                                    )
         ; *** case 6.2.5: there is a preplate entry and the dependent has an associated identifier
         ;     Usually, this means that the preposition governs a proper name
                                (t 
                                  (setq downresult
                                       (list (build-noun-sem
                                                'NOUN down-mean (get-actavm-headnumb downtree)
                                                 downtree up-ident)))
                ;    (format t "Noun sem in build-prep-sem:~% downtree: ~a~% downtree sem:~a~%"
                ;             downtree (build-noun-sem 'noun down-mean (get-actavm-headnumb downtree)
                ;                                    downtree up-ident))
                ;    (break "")
                                  (setq result
                                    (compose-restr-path
                                       (choose-best-ontpath
                                            (find-shortest-path semtopic down-mean act-prep-mean))
                                       downresult 'build-prep-5)))))))))
                                ; *** the third argument of find-shortest path is intended to be a
                                ;     constraint, provided by the preposition, on the path to find
                                ;     in the ontology
     ; (format t "Build-prep-mean; semtopic: ~a~% singrestr: ~a~% result: ~a~%" 
     ;            semtopic (get-actavm-headlemma singrestr) result)
     ; (break "Quattro")
       result
        ))

; ***************************************************************************
; *** given the "meaning" of a preposition, the governing concept and the
;     depending concept, evaluates the actual meaning of the preposition, as
;     specified in the preplate table
(defun get-act-prep-mean (prep-meaning upper-conc downer-conc)
   (declare (special *PREP-TEMPLATES*))
   (let (prep-entry)
      (cond ((is-relation prep-meaning) prep-meaning)
   ; *** this happens when the preposiation has a well specified meaning (as "during")
            (t (setq prep-entry (first (leggi *PREP-TEMPLATES* prep-meaning))) 
               (select-prep-mean prep-entry upper-conc downer-conc)))))

; ***************************************************************************
; *** checks the presence of an inverted property, as in case
;     "a pool with three meters of depth"
;     In this case, we have:
;     1. deptree = "three meters of depth" (with "meters" as root)
;     2. up-noun-sem = ££pool
;     3. up-prep-sem = --with-relation
(defun find-inverted-property (deptree up-noun-sem up-prep-sem)
  (declare (special *PREP-TEMPLATES*))
  (let ((down-deps (get-actavm-deps-with-traces deptree)) 
         prep-mean act-prep-mean savdownsem savprepmean down-ident
         actdep down-sem path found-deps other-deps otherpath
         them-grid them-role direct-role)
      ;(format t "Entering find-inverted-property")
      ;(break "trt")
; *** the do loops on all dependents of the intermediate noun (meters)
      (do ((nxtdep (first down-deps) (first down-deps))
           (down-deps (rest down-deps) (rest down-deps)))
; *** exit from the loop
         ((null nxtdep)
     ; *** at the end, we have in found-deps the dependents involved in the inversion
     ;     (of depth) and in other-deps the other dependents (three)
           (cond ((null found-deps) nil)
                 ((= (length found-deps) 1)
     ; *** this is the case at hand; in found-deps we have a single dependent that
     ;     connects as a property the down-down item (depth) with the upper item
     ;     (pool). Now check if the intermediate item (meter) can be a value of the
     ;     down-down one (depth).
     ; *** In the case of "temperature in growth at north", down-sem is ££to-increase (growth)
     ;     deptree is "at north" 
                   (setq otherpath 
                      (choose-best-ontpath
                           (find-shortest-path down-sem (get-actavm-headlexmean deptree))))
      ;(format t "Exiting find-inverted-property; found-deps: ~a~% downsem: ~a~% prepmean:~a~%"
      ;           found-deps savdownsem savprepmean)
      ;(break "tft")
                   (cond ((or direct-role
                              (includes-subclass-of otherpath '££has-value))
     ; *** now everything is ok: return the found dependent (of depth) and the other ones
                            (values (first found-deps) savdownsem savprepmean other-deps))))
                 (t (exception 'semantic-error "more than one path in inverted properties"))))
; *** body of the loop
         (cond ((equal nxtdep '(#\#)) nil)
               ((eq (get-actavm-headcateg nxtdep) 'PREP)
                  (setq actdep (skip-determiner (find-actavm-dep 'PREP-ARG nxtdep)))
                  (setq down-sem (get-actavm-headlexmean actdep))
                  (setq down-ident (get-actavm-headlexident actdep))
      ; *** check if "pool with depth" has any meaning
                  (cond ((null down-ident)
                           (setq act-prep-mean (get-act-prep-mean up-prep-sem up-noun-sem down-sem)))
                        (t (setq act-prep-mean (get-act-prep-mean up-prep-sem up-noun-sem down-ident))))
      ; *** the next for "temperatura in aumento" (lit. "temperature in growth")
      ;     in this case, it is assumed that the depending noun (growth) express a change of
      ;     value in the measurement of the upper noun. So that a thematic-grid is looked for,
      ;     which is associated to the depending noun, for which the upper noun can be an agent
                  (cond ((or (null act-prep-mean)
                             (eq act-prep-mean 'fail))
                           (setq them-grid (get-actavm-headthemgrid actdep))
                           (setq them-role (get-grid-role them-grid 'NOUN-SUBJ))
                           (cond ((null them-role)
                                    (setq other-deps (append1 other-deps nxtdep)))
                                 (t (setq path 
                                      (choose-best-ontpath
                                         (find-shortest-path down-sem up-noun-sem them-role)))
                                    (cond ((is-direct-role-path path)
                                             (setq found-deps (append1 found-deps nxtdep))
                                             (setq savdownsem down-sem)
                                             (setq savprepmean them-role)
                                             (setq direct-role t))
                                          (t (setq other-deps (append1 other-deps nxtdep)))))))
      ; *** check if the path found for "pool with depth" is related with properties
      ;     i.e. if "depth" is a property of "pool"
                        (t (setq path 
                             (choose-best-ontpath
                                (find-shortest-path up-noun-sem down-sem act-prep-mean)))
                ;  (format t "Find-inverted-property; path:~a;~% down-sem:~a;~% act-prep-mean:~a;%"
                ;             path down-sem act-prep-mean)
                ;  (break "")
                           (cond ((includes-subclass-of path '££has-property)
                                    (setq found-deps (append1 found-deps nxtdep))
                                    (setq savdownsem down-sem)
                                    (setq savprepmean act-prep-mean))
                                 (t (setq other-deps (append1 other-deps nxtdep)))))))
               (t (setq other-deps (append1 other-deps nxtdep)))))))

; ***************************************************************************
; *** the function returns true is the path has the form:
;     <c1 SUBCL-RELa c2 SUBCL-RELa ... DOMAIN-OF thr RANGE c2 SUBCL-RELb c4 SUBCL-RELb ...>
;     where c1 is any dependent of "££situation"
; *** currently, I do not check that "thr" actually is a thematic-role
(defun is-direct-role-path (path)
  (let (failure (status 'start))
  ;(break "is-direct-role")
    (do ((nxtitem (first path) (first path))
         (path (rest path) (rest path)))
        ((or failure (null path))
           (cond ((memq status '(hsubcl-after subclo-after)) (not failure))
                 (t nil)))
        (cond ((is-a-structural-item nxtitem)
                 (case status
                    (start
                         (cond ((eq nxtitem 'HAS-SUBCLASS) (setq status 'hsubcl-before))
                               ((eq nxtitem 'SUBCLASS-OF) (setq status 'subclo-before))
                               ((eq nxtitem 'DOMAIN-OF) (setq status 'found-domain))
                               (t (setq failure t))))
                    (hsubcl-before
                         (cond ((eq nxtitem 'HAS-SUBCLASS) nil)
                               ((eq nxtitem 'DOMAIN-OF) (setq status 'found-domain))
                               (t (setq failure t))))
                    (subclo-before
                         (cond ((eq nxtitem 'SUBCLASS-OF) nil)
                               ((eq nxtitem 'DOMAIN-OF) (setq status 'found-domain))
                               (t (setq failure t))))
                    (found-role
                         (cond ((eq nxtitem 'RANGE) (setq status 'found-range))
                               (t (setq failure t))))
                    (found-range
                         (cond ((eq nxtitem 'HAS-SUBCLASS) (setq status 'hsubcl-after))
                               ((eq nxtitem 'SUBCLASS-OF) (setq status 'subclo-after))
                               (t (setq failure t))))
                    (hsubcl-after
                         (cond ((eq nxtitem 'HAS-SUBCLASS) nil)
                               (t (setq failure t))))
                    (subclo-after
                         (cond ((eq nxtitem 'SUBCLASS-OF) nil)
                               (t (setq failure t))))))
   ; *** it is not a structural item
              (t (case status 
                    (found-domain (setq status 'found-role))
      ; *** in the branch above there is the assumption that it is in fact a role
                    (otherwise nil)))))))

; ***************************************************************************
; *** this function reshuffles a tree, by extracting the portion related to the
;     measured property. The original structure is as follows (downtree)
;     ("valori di temperatura elevati")      ("nubi in aumento al nord")
;
;              head-noun (valori)                    head-noun (nubi)
;                   |        \                           |      
;                prep (di)   depX (elevati)           prep (in)
;                   |                                    |
;        property-noun (temperatura)            change-noun (aumento)
;                   |                                    |
;                 depY (---)                          depY (al nord)
;
;    - inv-dep is the subtree rooted in "prep". 
;    - other-deps are all other dependents (depX)
;    - In the example, there is just one other dependent, but in principle there could be
;      more than one.
;    - In the example, property-noun has no dependents, but one or more of them could be
;      present.
; *** The reshuffled trees should be:
;
;        property-noun (temperatura)                 change-noun (aumento)
;                   |            \                        |            \
;          head-noun (valori)   depY (---)       head-noun (nubi)   depY (al nord)
;                   |
;              depX (elevati)
;
(defun build-inverted-tree (downtree inv-dep other-deps &optional change-noun?)
; *** downtree is, in the first case, the one rooted in "valori", in the second one
:     in "nubi"
; *** inv-dep is the tree rooted in the linking preposition ("di" and "in", respectively)
:     in "aumento"
   (let ((newtree (find-actavm-dep 'PREP-ARG inv-dep)) newsubtree downhead
         (uplink (get-actavm-headlink downtree)))
; *** newtree is, in the first case, the one rooted in "temperatura", in the second one
:     in "aumento"
       (setq other-deps (cons '(#\#) other-deps))
       (cond (change-noun?
                (setq downhead (assoc 'head (set-actavm-featval 'link 'NOUN-SUBJ downtree))))
             (t (setq downhead (assoc 'head downtree))))
       (setq newsubtree (list downhead (list 'dependents other-deps)))
; *** newsubtree is the subtree rooted in head-noun (valori) without the "di temperatura"
;     dependent
       (list (assoc 'head (set-actavm-featval 'link uplink newtree))
             (list 'dependents (cons newsubtree (get-actavm-deps-with-traces newtree))))))

; ***************************************************************************
(defun build-subpart-repr (governor-cat up-ident down-ident semtopic topic-subtree downtree)
  (let ((tempres (find-subtype-path up-ident down-ident 'subpart)) up-path)
      (cond ((null tempres)
               (build-sing-r-sem governor-cat semtopic topic-subtree downtree))
   ; *** in the last tests, tempres is the path connecting up-ident (excluded) to the
   ;     subpart, so the part below substitutes the last portion
            (t (list (append (list semtopic 'has-instance up-ident) tempres) nil))
   ; *** if tempres is not null, then the restriction has been
   ;     interpreted as a subtype (e.g. ££It-southern-region)
   ;     So, the result involves the composition of the path
   ;     from semtopic to the found subtype
            ;(t (setq up-path
            ;        (choose-best-ontpath (find-shortest-path semtopic tempres)))
   ; *** up-ident must be included in the result
            ;   (cons (first up-path) (replace-has-instance (rest up-path))))
     )))

; ***************************************************************************
; *** this is for "stato di obsolescenza" (obsolescence status)
;     In this example, upmean is "££status" and downmean is "£obsolescence"
; *** it is also used for finding the involved subpart in the case of the
;     connection between £North (upmean) and £Italy (downmean). In this
;     case, we first find (via find-shortest-path) a sequence as 
;     <£North range-of &has-it-north-selector domain £North-Italy-part-of
;      domain-of &has-it-north-bigger range £Italy>
; *** then we see if in this sequence there is an instance of ££geogr-part-of
;     (in this example £North-Italy-part-of), then we look for its
;     'smaller-part' (in this example ££it-northern-region)
; *** interestingly, this also applies to adverbial modifications, where we 
;     have that "molto" (--intensifier-adv) is is an "Intensifier", so we have:
;     <--intensifier-adv- is-a ££intensifier range-of &intensification-selector
;      domain ££intensification-rel domain-of &intensification-arg range
;      £mare-agitato>
;     In this case, what need be found is the &intensification-value, which in
;     this case is £mare-molto-agitato
;     In order to apply te same procedure, we must have:
;     upmean=--intensifier-adv;   downmean=£mare-agitato
(defun find-subtype-path (upmean downmean searchtype)
   (let ((path (choose-best-ontpath
                    (find-shortest-path upmean downmean)))
          found)
    ; (format t "Entering find-subtype-path; upmean: ~a; downmean: ~a~% path: ~a~%" upmean downmean path)
    ; (break "kwk")
       (cond ((null path) nil)
             (t (do* ((prevpath nil (cons nxtelem prevpath))
                      (nxtelem (first path) (first path))
                      (path (rest path) (rest path)))
                   ((or found (null nxtelem))
              ;  (format t "Exiting find-subtype-path~%  nxtelem: ~a~% prevpath: ~a~%" nxtelem prevpath)
              ;  (break "xdx")
                     (cond ((or (null found)
                                (eq found 'fail)
                                (and (neq found searchtype)
                                     (not (and (eq searchtype 'subpart)
                                               (eq found 'subpart-2))))) nil)
                           ((eq found 'subtype)
    ; *** what has been found is something as
    ;           <££status domain-of &has-status-subtype range ££statusType ...>:
    ;     where: path = <££statusType>
    ;            nxtelem = range
    ;            prevpath = <&has-status-subtype domain-of ££status>
                              (cond ((eq nxtelem 'domain) (list (first path)))
                                    ((eq (second prevpath) 'domain-of)
                                       (list (third  prevpath)))
                                    (t (exception 'semantic-error
                                              "PROC/buildquery: ontology relation without domain"))))
                           ((memq found '(subpart subpart-2))
        ; *** subpart-2 refers to a subpart specification given via a relation instance
        ;     (currently restricted to &has-it-area-spec instances)
        ;     for "South of Italy", find-result-role returns: 
        ;         (value-of &has-it-area-6ter argument £it-southern-area)
        ;     while the original path was:
        ;         (£SOUTH VALUE-OF &HAS-IT-AREA6TER ARGUMENT £IT-SOUTHERN-AREA INSTANCE ££IT-GEOGR-AREA
        ;          SUBCLASS-OF ££GEOGRAPHIC-AREA HAS-INSTANCE £ITALY)
        ;     In this case, the returned subpart representation will be:
        ;         (£it-southern-area (and ((argument-of &has-it-area-ter value £south))
        ;                                  (instance 
                             (find-result-role (first prevpath) found))
                           ((eq found 'intens)
                             ;(format t "find-subtype-path: prevpath = ~a~%" prevpath)
                             ;(break "xbx")
        ; *** for "molto mosso", we have now:
        ;     prevpath = (&intens-rel-1 argument &moved-sea-intens-1 value-of £mare-agitato)
        ;     path = (arg-of &moved-sea-intens-3 value --intensifier-adv)
        ;     find-result-role -> (arg-of &moved-sea-intens-2 value £mare-molto-agitato)
        ; *** so, the final result is
        ;     (£mare-agitato value-of &moved-sea-intens-1
        ;                 argument £intens-rel-1
        ;                     (and ((arg-of &moved-sea-intens-3 value --intensifier-adv)
        ;                           (arg-of &moved-sea-intens-2 value £mare-molto-agitato))))
                             (append1 (reverse prevpath)
                                   (list 'and
                                        (list path
                                            (find-result-role 
                                                  (first prevpath) found)))))))
    ; **** body
                            ; (format t "find-subtype-path: nxtelem = ~a~%" nxtelem)
                            ; (break "xsx")
                  (cond ((is-a-structural-item nxtelem) nil)
                        ((and (is-restriction-of nxtelem '&has-subtype)
                              (eq searchtype 'subtype))
                           (setq found 'subtype))
                        ((and (is-instance-of nxtelem '££intensification-rel)
                              (eq searchtype 'intens))
                           (setq found 'intens))
                        ((and (is-instance-of nxtelem '££geogr-part-of)
                              (eq searchtype 'subpart))
                           (setq found 'subpart))
                        ((and (is-relinstance-of nxtelem '&has-it-area-spec)
                              (eq searchtype 'subpart))
                           (setq found 'subpart-2))
        ; *** if in the path there is a relation which is not a description, 
        ; *** and it is not a role of a part-of, then is not a subtype path
                        ((and (is-relation nxtelem)
                              (not (is-restriction-of nxtelem '&has-description))
                              (not (is-restriction-of nxtelem '&part-bigger))
                              (not (is-restriction-of nxtelem '&part-smaller))
                              (not (is-restriction-of nxtelem '&part-selector)))
                           (setq found 'fail))
                         ))))))

;****************************************************************************
; *** a concept is a subtype of another concept if they are linked via a
;     relation that is a restriction of &has-subtype. In the involved path,
;     it may occur also a restriction of &has-description. This is partially
;     incorrect, but it enables me to take an ££obsolescenceStatusDescription
;     as a subtype of a ££status
(defun is-subtype (conc1 conc2)
 (cond ((ont-siblings conc1 conc2) nil)
       (t (let ((path (choose-best-ontpath (find-shortest-path conc1 conc2))) fail)
              (do ((nxtitem (first path) (first path))
                   (path (rest path) (rest path)))
                  ((or (null path) fail) (not fail))
                  (cond ((and (is-relation nxtitem)
                              (not (is-restriction-of nxtitem '&has-subtype))
                              (not (is-restriction-of nxtitem '&has-description)))
                           (setq fail t))))))))

; ***************************************************************************
; *** as above, but on a list of concepts as second argument
(defun is-subtype-of-one-of (conc1 conclist)
   (cond ((null conclist) nil)
         ((is-subtype conc1 (first conclist)) t)
         (t (is-subtype-of-one-of conc1 (rest conclist)))))

; ***************************************************************************
; *** given a concept that is a reification of an instance of the 
;     &geogr-part-of relation, returns 
;     1. If selecttype = subpart:
;        the item that is its "smaller part" (i.e. from £south-Italy-part-of, it
;        returns ££it-southern-region)
;     2. If selecttype = intens:
;        the item that is its "intensified value" (i.e. from £intens-rel-1, it
;        returns £mare-molto-agitato)
;        relation = £intens-rel-1
(defun find-result-role (relation selecttype)
  (let ((roles
     ; *** the next cond, because subtype works on relations, while intens works
     ;     on relation instances
           (cond ((member selecttype '(subpart subtype)) (get relation 'domain-of))
                 ((eq selecttype 'subpart-2) (list relation))
                 ((eq selecttype 'intens) (get relation 'arg-of))))
         found)
     (do ((nxtrole (first roles) (first roles))
          (roles (rest roles) (rest roles)))
        ((or (null nxtrole) found)
   ; *** in found, we have the name of the "smaller part" relation
          (cond (found 
                  (cond ((member selecttype '(subtype subpart))
                           (first (get found 'range)))
                        ((eq selecttype 'subpart-2)
     ; *** now, in found we have &has-it-area-6ter
     ;     so, what is returned is 
     ;     (value-of &has-it-area-6ter argument £it-southern-area)
                           ;(list 'value-of found 'argument
                           ;    (first (get found 'argument)))
                           (list (first (get found 'argument))
                               'argument-of found 'value)
                           )
                        ((eq selecttype 'intens)
     ; *** now, in found we have &moved-sea-intens-2
     ;     so, what is returned is 
     ;     (arg-of &moved-sea-intens-2 value £mare-molto-agitato)
                           (list 'arg-of found 'value
                               (first (get found 'value))))))
                (t nil)))
        (cond ((or (and (member selecttype '(subtype subpart))
                        (is-restriction-of nxtrole '&geogr-part-smaller))
                   (and (eq selecttype 'subpart-2)
                        (is-relinstance-of nxtrole '&has-it-area-spec))
                   (and (eq selecttype 'intens)
                        (is-relinstance-of nxtrole '&intensification-value)))
                 (setq found nxtrole))))))

; ***************************************************************************
(defun build-pron-sem (governor-cat semtopic singrestr &optional them-role)
  (let ((meaning (get-actavm-headlexmean singrestr))
        (pronsyntpos (get-actavm-headnumb singrestr))
        (prontype (get-actavm-headtype singrestr)))
          ;   (format t "Enter build pron sem~%  semtopic: ~a~% meaning: ~a~% governor-cat: ~a~%" 
          ;            semtopic meaning governor-cat)
          ;   (break "")
     (cond ((eq prontype 'ordin)
            ; *** ordinals standing alone, as "third", are pronouns
             (let ((down-restr (find-actavm-dep 'PREP-RMOD-MONTH singrestr)))
                (cond ((null down-restr)
                       ; *** not a date expression
                        (append1 
                            (choose-best-ontpath (find-shortest-path semtopic '££number them-role))
                            `(eq ,(get-actavm-headvalue singrestr))))
                      (t (let ((down-mean (get-actavm-headlexmean down-restr)))
                            (cond ((eq down-mean '--di-relation)
                                     ; *** third of january
                                    (let* ((down-down-restr (find-actavm-dep 'PREP-ARG down-restr))
                                           (down-down-mean 
                                                 (get-actavm-headlexmean down-down-restr))
                                           (down-down-ident 
                                                 (get-actavm-headlexident down-down-restr))
                                           (res1 
                                              (append
                                                 (choose-best-ontpath 
                                                    (find-shortest-path semtopic '££day-numb-descr them-role))
                                                 (list 'instance-of 
                                                       (get-actavm-headvalue singrestr))))
                                           (res2 
                                              (append
                                                 (choose-best-ontpath 
                                                    (find-shortest-path semtopic down-down-mean them-role))
                                                 (list 'instance-of down-down-ident))))
                                        ; *** both res1 and res2 begin with "semtopic",
                                        ;     which is merged
                                        (list (first res1) 
                                              (list 'and 
                                                    (list (rest res1) (rest res2))))))))))))
           ((eq prontype 'interr)
             ; *** for interogative pronouns, even if they are singular, it is assumed that
             ;     the answer can be a list, so that the number is forced to 'pl
             (compose-restr-path
                 (choose-best-ontpath (find-shortest-path semtopic meaning them-role))
                 (build-restr-sem 'PRON meaning pronsyntpos
                                    singrestr (get-verb-restrictions singrestr nil))
                 'build-pron-1)
                             (break "find-topic-subtree: get-verb-restrictions 1")
                  )
          ; ((memq (get-actavm-headlexmean singrestr)
          ;        '(§speaker §generic-ag §indef-ref NIL))
          ;   ; *** "I" and "you" are not interpreted
          ;   ;     NIL can occur in 3rd person polite questions ("Puo' dirmi ...")
          ;    nil)
          ; ((and (eq (get-actavm-headperson singrestr) 3)
          ;       (eq (get-actavm-headnumber singrestr) 'pl))
          ;    nil)
           ((eq (get-actavm-headlexmean singrestr) '--neutral-pron)
             ; *** this is "one" in "next one"
              (let* ((ordin-node (find-actavm-dep 'ADJC-RMOD singrestr))
                     (ordin-cat (get-actavm-headcateg ordin-node))
                     (ordin-ident (get-actavm-headlexident ordin-node)))
                  (cond ((and (eq ordin-cat 'adj)
                              (not (null ordin-ident)))
                          ; *** "the fourth one"
                          (choose-best-ontpath (find-shortest-path semtopic ordin-ident them-role)))
                        ((or (eq ordin-cat 'num)
                             (null ordin-ident))
                          (append1 
                              (choose-best-ontpath (find-shortest-path semtopic '££number them-role))
                              `(eq ,(get-actavm-headvalue ordin-node)))))))
             ; *** other pronouns?
           ((eq (get-actavm-headlexmean singrestr) '§indef-ref)
               nil)
           (t (compose-restr-path
                   (choose-best-ontpath (find-shortest-path semtopic meaning them-role))
                   (build-restr-sem 'PRON meaning pronsyntpos singrestr
                          (get-verb-restrictions singrestr nil))
                          'build-pron-2)))))

; ***************************************************************************
(defun build-verb-sem (governor-cat semtopic topic-syntpos topic-subtree verbsubtree)
  (declare (special *PREP-TEMPLATES* *LANGUAGE* *SYSTEM-CONTEXT*))
  (let ((meaning (get-actavm-headlexmean verbsubtree))
       ; (topic-syntpos (get-actavm-headnumb topic-subtree))
        (syntpos (get-actavm-headnumb verbsubtree))
        (ident (get-actavm-headlexident verbsubtree)) 
        downtree preparg tempsem vrestrs pronsem othersem inv-pron-sem
        relpron-data deptype relpron-path vsubj vpred)
          ;   (format t "Enter build verb sem~%  semtopic: ~a~% meaning: ~a~% governor-cat: ~a~%" 
          ;            semtopic meaning governor-cat)
          ;   (break "")
      (cond ((is-a-relclause verbsubtree)
    ; *** this applies to both reduced and non-reduced relative clauses
               (cond ((eq (get-actavm-headlink verbsubtree) 'VERB-RMOD+RELCL)
                       (setq relpron-data (find-relpron-dep verbsubtree))
                       (setq deptype (first relpron-data))
                       (setq relpron-path (second relpron-data)))
                     ((eq (get-actavm-headlink verbsubtree) 'VERB-RMOD+RELCL+REDUC)
                       (setq relpron-path (find-relpron-trace verbsubtree))
                       (setq deptype 'direct))
                     (t (break "Buildquery: Unknown relative clause type")))
               (cond ((eq 'direct deptype)
            ; *** in this case, the pronoun is an unmarked case (subj or obj)
                        (setq vrestrs (get-verb-restrictions verbsubtree nil t))
                        (setq tempsem
                            (build-restr-sem 'VERB meaning syntpos verbsubtree vrestrs))
                        (cond ((neq 1 (length tempsem)) (break "tempsem in build-verb-sem")))
                        (multiple-value-setq (pronsem othersem)
                            (detach-relpron-sem (first tempsem) semtopic))
                        (setq inv-pron-sem (inv-range-dom (reverse (rest (first pronsem)))))
                        ;(compose-restr-path
                        ;    (choose-best-ontpath (find-shortest-path semtopic meaning))
                        ;    (build-restr-sem 'VERB meaning syntpos verbsubtree
                        ;             (get-verb-restrictions verbsubtree nil t)))
                ;      (format t "In verb sem relative clause;~% tempsem:~a~% pronsem: ~a~% othersem= ~a~%"
                ;               tempsem pronsem othersem)
                ;      (break "")
                        (list (append inv-pron-sem (first othersem)) (second othersem))
                         )
                     ((eq 'simple deptype)
                ; *** the table on which you put the lamp
                ; *** relpron-path includes the single node "on"
                ;     what must be done is to interpret "put on table", and then reverse
                ;     the result
                        (let* ((act-prep-mean (get-act-prep-mean relpron-path meaning semtopic)))
                  ; *** meaning is the meaning of the verb; semtopic of the governing noun
                  ;     "the table on which you put the book" -->
                  ;     meaning=££put semtopic=££table
                            (cond ((eq act-prep-mean 'fail)
                                     (exception-internal 'semantic-error
                                         "PROC/buildquery: no meaning 2 selected for a preposition"))
                                  (t (compose-restr-path
                                         (choose-best-ontpath 
                                                (find-shortest-path 
                                                         semtopic meaning act-prep-mean))
                                         (build-restr-sem 'VERB meaning syntpos verbsubtree
                                                (get-verb-restrictions verbsubtree nil t))
                                         'build-verb-1)))))
                     (t (exception-internal 'semantic-error
                             "PROC/buildquery: complex path to a relpron"))))
            ((eq meaning '@dummy)
       ; *** This is the predicative (neutral) meaning of the verb "to be"
       ;     (as in "this movie is nice"). The interpretation is made by looking for
       ;     the subject of the verb, and specifying that it has the value given
       ;     by the verb-precompl+subj
               (setq vsubj (find-actavm-dep 'VERB-SUBJ verbsubtree))
               (setq vpred (find-actavm-dep 'VERB-PREDCOMPL+SUBJ verbsubtree))
               (cond ((and (not (null vsubj))
                           (not (null vpred)))
                        (build-to-be-sem governor-cat semtopic topic-subtree vsubj vpred nil))
                     (t (exception 'semantic-error
                            "PROC/buildquery: @dummy (copula) verb without right arguments"))))
            ((eq meaning '££modal-can)
       ; *** The modal "can" is skipped ("local rains can fall" --> "local rains fall")
               (build-verb-sem governor-cat semtopic 
                      (get-actavm-headlexmean topic-subtree)
                      topic-subtree (find-actavm-dep 'VERB+MODAL-INDCOMPL verbsubtree)))
       ;    ((eq 'participle (get-actavm-headmood verbsubtree))
       ; *** reduced relatives (?????)
       ;      (compose-restr-path
       ;          (choose-best-ontpath (find-shortest-path semtopic meaning))
       ;          (build-restr-sem 'VERB meaning syntpos verbsubtree 
       ;                           (get-verb-restrictions verbsubtree nil t))))
       ;    ((or (one-is-subclass-of meaning '££system-operation)
       ;         (one-is-subclass-of meaning '££external-service))
       ;       (cond ((not (null ident))
       ;               ; *** "stop" and similar include their ident
       ;               (append
       ;                    (choose-best-ontpath (find-shortest-path semtopic meaning))
       ;                    (list 'instance-of ident)))
       ;               ; *** other system op (as "reserve") require a full treatment
       ;             (t (compose-restr-path
       ;                    (choose-best-ontpath (find-shortest-path semtopic meaning))
       ;                    (build-restr-sem 'VERB meaning syntpos verbsubtree
       ;                                    (get-verb-restrictions verbsubtree nil t))))))
       ; ********** The next branch is probably obsolete (treatment of "to be" in HOPS)
            ((eq meaning '££to-be)
              (let ((rmodpreparg (find-actavm-dep 'RMOD verbsubtree)) 
                    (locpreparg (find-actavm-dep 'VERB-INDCOMPL-LOC+IN verbsubtree)) )
                 ; the first condition for "I'm in Shaftsbury Avenue"
                 ; the second for "Sono in Shaftsbury Avenue"
             ; *** this is a special HOPS construction
                 (cond ((or (and (not (null rmodpreparg))
                                 (memq (get-actavm-headlexmean rmodpreparg)
                                       '(--in-relation --on-relation)))
                            (not (null locpreparg)))
                         (let* ((actpreparg
                                  (cond ((not (null rmodpreparg)) rmodpreparg)
                                        (t locpreparg)))
                                (vsubj (find-actavm-dep 'VERB-SUBJ verbsubtree))
                                (subjmean (get-actavm-headlexmean (skip-determiner vsubj))))
                             (cond ((eq subjmean '§speaker)
                                      (build-sing-r-sem governor-cat semtopic topic-subtree actpreparg))
                                   (t (build-restr-sem governor-cat semtopic topic-syntpos
                                           topic-subtree nil
                                          (list (list 'single vsubj actpreparg)))))))
             ; *** the next should be the standard treatment of the copula
                       (t (setq vsubj (find-actavm-dep 'VERB-SUBJ verbsubtree))
                          (setq preparg (find-actavm-dep 'VERB-PREDCOMPL+SUBJ verbsubtree))
                          (cond ((and (not (null vsubj))
                                      (not (null preparg)))
                                   (build-to-be-sem governor-cat semtopic topic-subtree vsubj preparg nil))
		                (t (compose-restr-path
			                  (choose-best-ontpath
                                              (find-shortest-path semtopic meaning))
			                  (build-restr-sem 
			                     'VERB meaning syntpos verbsubtree
                                             (get-verb-restrictions verbsubtree nil t))
                                          'build-verb-2)))))))
	    ((eq meaning '££to-have)
      ; *** in case of Catalan "Hi Ha .."
               (cond ((and (eq *LANGUAGE* 'catalan)
                           (not (null (find-actavm-dep 'pron-rmod-loc+metaph verbsubtree))))
                       (setq downtree (find-actavm-dep 'VERB-SUBJ verbsubtree))
                       (cond ((is-a-actavm-trace? downtree)
         ; *** here, we should look for the actual subject of "there is", which is sometimes
         ;     attached as RMOD
                               (exception 'semantic-error 
                                   "PROC/buildquery: No match between default infos and semrestrs in final-build-query"))))
                     ((eq *SYSTEM-CONTEXT* 'atlas)
         ; *** in the ATLAS context, "to have" is normally used in expressions as "we have nice weather"
         ;     this is encoded in the ontology simply by specifying that ££to-have may be a useful
         ;     ££dialogue-topic
                        (compose-restr-path
                             (choose-best-ontpath (find-shortest-path semtopic meaning))
                             (build-restr-sem 'VERB meaning syntpos verbsubtree
                                   (get-verb-restrictions verbsubtree nil t))
                             'build-verb-3))))
; *** top of the tree; the structure remains the same but the interpreted restrictions are set to nil,
;     since they are useless
	    ((and (eq semtopic '££dialogue)
                  (eq governor-cat 'TOP))
               (list 
                  (append (butlast (choose-best-ontpath (find-shortest-path semtopic meaning)))
                          (first 
                             (first (build-restr-sem 'VERB meaning syntpos verbsubtree
                                          (get-verb-restrictions verbsubtree nil t)))))
    ; *** first is the first interpretation (in case of ambiguity); first-first is the actual
    ;     interpretation part
                  nil))
; *** standard case: not a relative clause, not "to be", not "to have", not "modal can"
            (t (compose-restr-path
                    (choose-best-ontpath (find-shortest-path semtopic meaning))
                    (build-restr-sem 'VERB meaning syntpos verbsubtree
                               (get-verb-restrictions verbsubtree nil t))
                    'build-verb-4)))))

; ***************************************************************************
; *** moves inside a semantic representation, in order to find a path from the
;     root ending in semtopic
(defun detach-relpron-sem (tempsem semtopic)
  (let* ((actsem (first tempsem))
         (restrsem (second tempsem))
         (path (find-sem-path actsem semtopic nil)) rempaths)
    (setq rempaths (remove-sem-path actsem path nil))
    (values (list path restrsem) (list rempaths restrsem))))

; ***************************************************************************
; *** moves across a semantic representation in order to find a path ending in "semtopic"
(defun find-sem-path (tempsem semtopic acc)
   (let ((firstitem (remove-synt-pointer (first tempsem))) found)
     (cond ((null firstitem) nil)
           ((atom firstitem)
             (cond ((eq firstitem semtopic)
                      (cond ((eq (second tempsem) 'has-instance)
                               (append acc (first-n 3 tempsem)))
                            (t (append1 acc firstitem))))
                   (t (find-sem-path (rest tempsem) semtopic (append1 acc firstitem)))))
           ((eq 'and (first firstitem))
              (do ((nxtpath (first (second firstitem)) (first otherpaths))
                   (otherpaths (rest (second firstitem)) (rest otherpaths)))
                 ((or (null nxtpath) found) found)
                  (setq found (find-sem-path nxtpath semtopic acc))))
           (t (format t "Non atomic non-and path in find-sem-path: ~a~% acc= ~a~%" firstitem acc)
              (break "")
             ))))

; ***************************************************************************
; *** acc keeps a portion of path that we still do not know if it must be included in
;     the result
(defun remove-sem-path (tempsem path acc)
   (let ((firstitem (remove-synt-pointer (first tempsem)))
         (firstinpath (remove-synt-pointer (first path)))
         found result rempath)
     (cond ((null firstitem) 
              (cond ((null firstinpath) nil)
                    (t acc)))
           ((atom firstitem)
              (cond ((eq firstitem firstinpath)
                       (remove-sem-path (rest tempsem) (rest path) (append1 acc (first tempsem))))
                    (t tempsem)))
           ((eq 'and (first firstitem))
              (do ((nxtpath (first (second firstitem)) (first otherpaths))
                   (otherpaths (rest (second firstitem)) (rest otherpaths)))
                  ((null nxtpath)
   ; (format t "Exiting and loop: result: ~a~% acc: ~a~%" result acc) (break "")
                    (cond ((null result) (break "Remove-sem-path 1"))
                          ((eq 1 (length result))
                             (append acc (first result)))
                          (t (append1 acc (list 'and result)))))
                  (cond ((not found)
                           (setq rempath (remove-sem-path nxtpath path nil))
                           (cond ((null rempath)
                                    (setq found t))
                                 (t (setq result (append1 result rempath)))))
                        (t (setq result (append1 result nxtpath))))))
           (t (format t "Non atomic non-and path in remove-sem-path: ~a~%" firstitem)))))

; ***************************************************************************
; *** a verbal dependent is a relative clause if its link
;     to the governor is subsumed by "verb-rmod+relcl"
(defun is-a-relclause (verbsubtree)
   (lab-subsumes 'VERB-RMOD+RELCL (get-actavm-headlink verbsubtree)))

; ***************************************************************************
; *** given a subtree associated with a reduced relative clause, returns the
;     dependent trace that refers to the governing noun
(defun find-relpron-trace (verbsubtree)
   (declare (special +FULL-TREE+))
   (let ((deps (get-actavm-deps-with-traces verbsubtree)) 
         (parent (find-actavm-parent verbsubtree (list +FULL-TREE+)))
         parentpos found)
      (setq parentpos (get-actavm-headnumb parent))
      (do ((nxtdep (first deps) (first deps))
           (deps (rest deps) (rest deps)))
         ((or found (null nxtdep))
            (cond (found found)
                  (t (exception 'semantic-error
                         "PROC/buildquery: reduced relative clause without trace to governor"))))
         (cond ((equal nxtdep '(#\#)) nil)
               ((and (is-a-actavm-trace? nxtdep)
                     (eq (get-actavm-headcorefline nxtdep) parentpos))
                  (setq found nxtdep))))))

; ***************************************************************************
; *** given a subtree associated with a non-reduced relative clause, returns a pair, where
;     the second element is its subtree (dependent) that includes a relative pronoun
;     while the first element can be:
;     direct: the relpron is a direct dependent of the verb
;     simple: the relpron is a dependent of a preposition linked to the verb
;     multiple: the relpron is included in a complex PP (the table on the top of which
;               you put the lamp) 
(defun find-relpron-dep (verbsubtree)
   (let ((deps (get-actavm-deps-with-traces verbsubtree)) checkres found)
      (do ((nxtdep (first deps) (first deps))
           (deps (rest deps) (rest deps))
           (deppos 1 (1+ deppos)))
         ((or found (null nxtdep))
            (cond (found found)
                  (t (exception 'semantic-error
                         "PROC/buildquery: relative clause without relative pronoun"))))
         (cond ((equal nxtdep '(#\#)) nil)
               (t (setq checkres (check-relpron-dep nxtdep 1))
                  (cond ((= checkres 0) nil)
                        ((= checkres 1) (setq found (list 'direct nxtdep)))
                        ((= checkres 2) (setq found (list 'simple nxtdep)))
                        (t (setq found (list 'multiple nxtdep)))))))))

; ***************************************************************************
; *** returns 0 if the subtree does not includes a relative pronoun
;     otherwise returns the depth of the relpron (starting from 1)
;     "with the man" --> 0
;     "which" --> 1
;     "whose" --> 1 !!! ERROR: A CASE TO HANDLE !!!
;     "on which" --> 2
;     "the top of which" --> 3
;     "on the top of which" --> 4
;     ....
(defun check-relpron-dep (subtree depth)
   (cond ((null subtree) 0)
         ((and (eq 'pron (get-actavm-headcateg subtree))
               (eq 'relat (get-actavm-headtype subtree)))
             (cond ((and (memb-or-eq 'poss (get-actavm-headcase subtree))
                         (eq depth 1))
     ; *** this should apply to "whose" and "cui". It actually does not work, since
     ;     the lexicon does not associate the "poss" case with these forms
                      2)
                   (t depth)))
         ((eq 'prep (get-actavm-headcateg subtree))
             (let ((preparg (find-actavm-dep 'prep-arg subtree)))
                 (cond ((not (null preparg)) 
                          (check-relpron-dep (skip-determiner preparg) (1+ depth)))
                       (t (exception 'semantic-error
                              "PROC/buildquery: preposition without argument")))))
         ((eq 'noun (get-actavm-headcateg subtree))
             (check-relpron-dep (find-actavm-dep 'PREP-RMOD subtree) (1+ depth)))
         (t 0)))

; ***************************************************************************
; *** it builds the semantic representation of a copula
; *** semtopic is the item governing the verb "to be"
;     vsubj is the subject of "to be"
;     predcompl is the verb-predcompl+subj
;     other-deps are other dependents (if any) of "to be"
(defun build-to-be-sem (governor-cat semtopic topic-subtree vsubj predcompl other-deps)
  (let* ((vsubj-nodet (skip-determiner vsubj))
         (subjmean (get-actavm-headlexmean vsubj-nodet))
         (subjcat (get-actavm-headcateg vsubj-nodet))
         (subjpos (get-actavm-headnumb vsubj-nodet))
         (topic-syntpos (get-actavm-headnumb topic-subtree))
         (subj-interp (build-noun-sem governor-cat semtopic topic-syntpos vsubj-nodet))
         (predcat (get-actavm-headcateg predcompl))
         (predmean (get-actavm-headlexmean predcompl))
         downpred predsem
        )
     (cond ((eq 'ART predcat)	
   ; *** a determiner is the head: determiners are currently ignored, so the
   ;     tree is travelled downward
              (setq downpred (skip-determiner predcompl))
              (cond ((eq 'NOUN (get-actavm-headcateg downpred))
                       (setq predsem (build-noun-sem governor-cat semtopic topic-syntpos downpred nil)))
                    (t (exception 'semantic-error "Strange predcompl in build-to-be-sem"))))
           ((eq 'ADJ predcat)
    ; *** in case of adjectives, I try to restructure the tree, in order to get the subject as 
    ;     root and all other dependents of the verb under it:
    ;     In the new tree, the predcompl adjective is put under the subject
              (setq predsem (build-adj-sem subjcat subjmean vsubj-nodet predcompl nil nil)))
           ((eq 'NOUN predcat)
              (setq predsem (build-noun-sem subjcat subjmean subjpos predcompl nil)))
           ((eq 'PREP predcat)
              (setq predsem (build-prep-sem subjcat subjmean vsubj-nodet predcompl nil)))
           ((eq 'PRON predcat)
              (setq predsem (build-pron-sem subjcat subjmean predcompl)))
           (t (exception 'semantic-error "Unknown predcompl category in build-to-be-sem")))
   ; *** now, we have in subj-interp the semantic interpretation of the subject (wrt to the
   ;     item governing the copula) and in predsem the interpretation of the verb predcompl
   ; *** the just have to be put together
     ;(mult-compose-restr-path
     ;      (find-shortest-path semtopic meaning) restr-meaning)
   ; *** subj-interp should be something as (semtopic ... subjmean path1)
   ; *** predsem something as (subjmean path2)
   ;     So, we obtain (semtopic ... subjmean (and path1 path2))
 ;(format t "build-to-be-sem; ~% subj-interp: ~a~% predsem:~a~%" subj-interp predsem)
 ;(break "")
    (merge-paths-with-pivot subj-interp predsem) 
      ))

; ***************************************************************************
; *** full-subj-interp should be something as 
;     ((semtopic ... subjmean path1) subrestr-interp1)
; *** full-predsem something as ((subjmean path2) subrestr-interp2)
;     We obtain ((semtopic ... subjmean (and path1 path2)) subrestr-interp2)
; *** currently, no treatment of coordination
;     (e.g. temperature and pressure are high, or the temperature is high and hard)
(defun merge-paths-with-pivot (full-subj-interp full-predsem) 
  (list (int-merge-paths-with-piv (first full-subj-interp) (first full-predsem))
        (second full-predsem)))

; ***************************************************************************
(defun int-merge-paths-with-piv (subj-interp predsem) 
   (cond ((null subj-interp)
            (exception 'semantic-error "in merge-paths-with-pivot"))
         ((synt-sem-equal (first subj-interp) (first predsem))
            (cons (best-synt-conc (first subj-interp) (first predsem))
                  (list (list 'and (list (rest subj-interp) (rest predsem))))))
         (t (cons (first subj-interp)
                  (int-merge-paths-with-piv (rest subj-interp) predsem)))))

; ***************************************************************************
; *** builds the representation of a date, assuming that singrestr has, as
;     top element, a number (expressed either as a number or as an ordinal
;     adjective) and a dependent expressing a month
(defun build-date-repr (semtopic singrestr)
   (let ((down-restr (find-actavm-dep 'NOUN-RMOD-MONTH singrestr)))
       (cond ((null down-restr)
                (setq down-restr
                   (find-actavm-dep 'PREP-ARG (find-actavm-dep 'PREP-RMOD singrestr)))))
       (let* ((down-mean (get-actavm-headlexmean down-restr))
              (down-ident (get-actavm-headlexident down-restr))
              (res1 (append
                       (choose-best-ontpath (find-shortest-path semtopic 
                                                 '££day-numb-descr))
                       (list 'instance-of (get-actavm-headvalue singrestr))))
              (res2 (append
                       (choose-best-ontpath (find-shortest-path semtopic down-mean))
                       (list 'instance-of down-ident))))
                  ; *** both res1 and res2 begin with "semtopic", which is merged
           (list (first res1) 
                 (list 'and (list (rest res1) (rest res2)))))))

; ***************************************************************************
(defun final-build-query (default-infos semrestrs)
   (cond ((= 1 (length default-infos))
            (final-build-simple-query (first default-infos) (first semrestrs)))
         (t (let ((compl-query (mapcar #'final-build-simple-query default-infos semrestrs)) temp)
   ; *** compl-query actually is a list of queries, one for each search target
   ;     now, the attempt is to merge them in a single query
                (setq temp (final-build-complex-query compl-query))))))
   ; *** temp used just to debugging purposes
 
; ***************************************************************************
(defun final-build-simple-query (default-infos semrestrs)
; *** default-infos is a set of paths
;     (e.g. ( (££cinema-loc subclass-of ££location ...)
;             (££cinema subclass-of ££genre ...)))
;     The final element of default-infos must be a subclass of ££datatype
;     The first element of default-infos must be equal to the first element of semrestrs
; *** semrestrs is another path (starting from concept)
; *** the final query must be:
;     <select <concept last-four-elements-of-default-infos>
;      from concept
;      where concept <inverted-rem-path> semrestrs>
;     where inverted-rem-path is the inversion of the initial part of the
;      default-infos, apart from the last four elements
; *** so that the last elements must be:
;     <concept range-of relation domain descr-type subclass-of $some-datatype>
;     unless its length is 6  
; *** another possibility is that default-infos is just
;     <subclass-of some-datatype>
 (let ((taillength 6) topic act-default-infos found act-semr)
  (setq act-default-infos
       (do ((first-default (first default-infos) (first default-infos))
            (default-infos (rest default-infos) (rest default-infos)))
           ((or found (null first-default))
               (cond (found found)
                     (t (exception 'semantic-error 
                              "PROC/buildquery: No match between default infos and semrestrs in final-build-query"))))
           (cond ((atom (first semrestrs))
                   (cond ((eq (first semrestrs) (first first-default))
                            (setq found first-default)
                            (setq act-semr semrestrs))))
   ; *** semrestrs could be a list e.g. "Voldria informació sobre teatre",
   ;     where semrestrs is ((££theater) (££theater-loc))
                 (t (do ((first-semr (first semrestrs) (first semrestrs))
                         (semrestrs (rest semrestrs) (rest semrestrs)))
                        ((or found (null first-semr)))
                       (cond ((eq (first first-semr) (first first-default))
                               (setq found first-default)
                               (setq act-semr first-semr))))))))
  (setq topic (first act-default-infos))
  (setq act-default-infos (rest act-default-infos))
  (cond ((is-subclass-of (ult act-default-infos) '££datatype)
          (cond ((or (= 2 (length act-default-infos))
                     (> (length act-default-infos) (1- taillength)))
                  (let (sel-clause from-clause wh-clause)
                     (cond ((<= (length act-default-infos) taillength)
                             (setq sel-clause act-default-infos)
                             (setq from-clause topic)
                             (setq wh-clause act-semr))
                           (t (let ((split-pos (- (length act-default-infos)
                                                  (1+ taillength))))
                                  (setq sel-clause 
                                        (nthcdr (1+ split-pos) act-default-infos))
                                  (setq from-clause 
                                        (nth split-pos act-default-infos))
                                  (setq wh-clause 
                                     (rem-subclass-of-itself
                                        (append 
                                           (inv-range-dom
                                               (reverse (first-n (1+ split-pos)
                                                                 act-default-infos)))
                                           act-semr)
                                        nil)))))
                     `(select ,sel-clause from ,from-clause where ,wh-clause)))
                ((<= (length act-semr) 1)
        ; *** The next added for "Puo' dirmi il titolo?"
                     `(select ,default-infos from ,topic))))
       (t (exception 'semantic-error "PROC/buildquery: Problems in final-build-query")))))

; ***************************************************************************
(defun final-build-complex-query (queries)
   (let* ((common-restr-path (ult (find-common-path queries)))
          (prefixes (remove-tail queries common-restr-path))
          newprefixes)
       (cond ((null prefixes) (break "in final-build-complex-query"))
    ; *** prefixes and queries are assumed to be parallel lists
             (t (do ((nxtprefix (first prefixes) (first prefixes))
                     (prefixes (rest prefixes) (rest prefixes))
                     (nxtquery (first queries) (first savqueries))
                     (savqueries (rest queries) (rest savqueries)))
                   ((null nxtquery))
                  (setq newprefixes 
                      (cons (rem-subclass-of-itself
                               (append (reverse (inv-range-dom nxtprefix)) (second nxtquery))
                               nil)
                            newprefixes)))
                `(select ,(put-in-and newprefixes)
                  from ,(first common-restr-path)
                  where ,common-restr-path)))))

; ***************************************************************************
(defun remove-tail (queries tail)
   (int-rem-tail (mapcar 'sixth queries) (reverse tail) (length tail)))

(defun int-rem-tail (restrs revtail taillength)
   (cond ((null restrs) nil)
         (t (let ((remresult (int-rem-tail (rest restrs) revtail taillength)))
                 (cond ((eq remresult 'fail) 'fail)
                       ((is-a-prefix revtail (reverse (first restrs)))
                          (cons (first-n (- (length (first restrs)) taillength) 
                                         (first restrs))
                                remresult))
                       (t 'fail))))))

; ***************************************************************************
(defun put-in-and (args)
   (cond ((eq 1 (length args)) args)
         (t (let ((result (list (first args))))
               (dolist (nxtarg (rest args) result)
                   (setq result (append result (list 'and nxtarg))))))))
                   
; ***************************************************************************
; *** this looks for common sublists
;     e.g. queries = < <x1, x2, x3, x4, x5, x6> <y1, x3, x4, x5, y2, y3> <z1, x2, x3, x4, z2> >
;     result: <x3, x4>
(defun find-common-path (queries)
; *** the operation is made on the "where" part of the queries, i.e. the sixth element
   ;(format t "find-common-path: queries = ~a ~%" queries)
   ;(break "")
   (let ((restrictions (mapcar #'sixth queries))
         found-common diff all-found-common tempmatch 
         (useless-starts '(domain range has-subclass domain-of range-of subclass-of)))
       (setq tempmatch (list (first restrictions)))
   ; *** tempmatch is the list of matches found until the n-th list
       (dolist (nxtrestr (rest restrictions) tempmatch)
     ; *** for all further lists in queries, update tempmatch
   ;(format t "find-common-path: dolist1; tempmatch = ~a ~%" tempmatch)
   ;(format t "                           nxtrestr = ~a ~%" nxtrestr)
   ;(break "")
        (setq tempmatch
         (dolist (nxtmatch tempmatch all-found-common)
   ;(format t "find-common-path: dolist2; nxtmatch = ~a ~%" nxtmatch)
   ;(break "")
             (setq all-found-common nil)
     ; *** in nxtmatch one previous match
            (do ((nxtconc (first nxtrestr) (first nxtrestr))
                 (nxtrestr (rest nxtrestr) (rest nxtrestr)))
        ; *** each element of a new query list is inspected
                ((null nxtconc) all-found-common)
   ;(format t "find-common-path: do3; nxtconc = ~a ~%" nxtconc)
   ;(break "")
                (setq found-common nil)
          ; *** and matched against a previous result
                (do ((nxtmatchel (first nxtmatch) (first nxtmatch))
                     (nxtmatch (rest nxtmatch) (rest nxtmatch)))
                    ((null nxtmatchel))
   ;(format t "find-common-path: do4; nxtmatchel = ~a ~%" nxtmatchel)
   ;(break "")
                    (cond ((and (eq nxtconc nxtmatchel)
                                (not (member nxtconc useless-starts)))
          ; *** we are at the beginning of a match
                             (setq found-common (list nxtconc))
          ; *** if the element is part of the list (e.g. x3 in the example above), then
          ;     in nxtmatch there is the part of the previous match following (and
          ;     including) the element in question (e.g. <x3, x4, x5, x6>)
          ; *** the two sublists are scanned in parallel until one ends or a different
          ;     element is found
          ; *** in found-common, we add common elements
                             (setq diff nil)
                             (do ((nxtpart (first nxtrestr) (first temprestr))
                                  (temprestr (rest nxtrestr) (rest temprestr))
                                  (nxtpartold (first nxtmatch) (first tempmatch))
                                  (tempmatch (rest nxtmatch) (rest tempmatch)))
                                 ((or (null nxtpart) (null nxtpartold) diff)
                                    (setq all-found-common 
                                      (cons (reverse found-common) all-found-common)))
   ;(format t "find-common-path: do5; nxtpart = ~a ~%" nxtpart)
   ;(break "")
                                 (cond ((eq nxtpart nxtpartold)
                                          (setq found-common
                                                (cons nxtpart found-common)))
                                       (t (setq diff t))))))))))
        (setq tempmatch 
           (remove-sublists 
                (sort tempmatch #'(lambda (x y) (< (length x) (length y)))))))))

; ***************************************************************************
(defun remove-sublists (lists)
   (cond ((null lists) nil)
         ((is-a-sublist (first lists) (rest lists))
             (remove-sublists (rest lists)))
         (t (cons (first lists)
                  (remove-sublists (rest lists))))))

; ***************************************************************************
; *** returns t if l is a sublist of one of the lists
(defun is-a-sublist (l lists)
   (cond ((null lists) nil)
         ((sublist? l (first lists)) t)
         (t (is-a-sublist l (rest lists)))))

; ***************************************************************************
; *** returns t if l1 is a sublist of l2
(defun sublist? (l1 l2)
  (let (found)
      (do ((nl2 t (first xl2))
           (xl2 l2 (rest xl2)))
          ((or (null nl2) found) found)
         (setq found (is-a-prefix l1 xl2)))))

; ***************************************************************************
; *** returns t if l1 is equal to the first part of l2
(defun is-a-prefix (l1 l2)
   (cond ((null l1) t)
         ((eq (first l1) (first l2))
            (is-a-prefix (rest l1) (rest l2)))
         (t nil)))

; ***************************************************************************
(defun final-build-givinfo (topic semrestrs)
; *** this is a simplified version of final-build-query above:
;     no query target!
; *** the input is a pair <actual interp, coord interps>
; *** it is applied when the semantic head is ££get-info or ££see
;     The idea is that the user is not asking anything, but s/he is
;     just providing the system with new information. An example is
;     at end of dialogue, when the user must simply tell the system
;     if s/he wants to continue or not
; *** topic is a concept (currently, ££dialogue)
; *** semrestrs is a path (starting from concept)
; *** the final query must be:
;      about concept
;      where concept semrestrs
  (cond ((and (= 2 (length semrestrs))
              (listp (first semrestrs)))
          (setq semrestrs (first semrestrs)))
        (t (exception 'semantic-error "PROC/buildquery: in final-build-givinfo")))
  (cond ((atom topic)
           `(about ,topic where ,semrestrs))
        ((= 1 (length topic))
           `(about ,(first topic) where ,semrestrs))
        (t `(about ,(build-with-topic-list) where ,semrestrs))))

; ***************************************************************************
;     this removes "class subclass-of class" and "class has-subclass class"
(defun rem-subclass-of-itself (path prevpath)
  (cond ((null path) (reverse prevpath))
        ((and (memq (first path) '(has-subclass subclass-of))
                    (eq (second path) (first prevpath)))
           (rem-subclass-of-itself (rest (rest path)) prevpath))
        (t (rem-subclass-of-itself (rest path) (cons (first path) prevpath)))))

; ***************************************************************************
; *** this changes "domain" into "domain-of", "domain-of" into "domain" and so on
;     it also removes "class subclass-of class" and "class has-subclass class"
(defun inv-range-dom (path)
  (mapcar #'inv-r-d path))

; ***************************************************************************
(defun inv-r-d (singitem)
  (case singitem
      (range 'range-of)
      (domain 'domain-of)
      (range-of 'range)
      (domain-of 'domain)
      (has-subclass 'subclass-of)
      (subclass-of 'has-subclass)
      (otherwise singitem)))

; ***************************************************************************
; *** this should choose the best of the paths connecting two nodes in
;     the ontology
; *** the preferred path is the shorter one, after removing all subclass links
; *** In case two paths are of equal no-subclass length, the shorter one (with
;     suclasses) is preferred: this gives preminence to the most "local" paths
;     This gives preference to 
;     (££MOVIE HAS-SUBCLASS ££COMEDY-MOVIE RANGE-OF &HAS-COMEDY-M-T DOMAIN £COMEDY)
;     With respect to
;     (££MOVIE RANGE-OF &HAS-MOVIE-TYPE DOMAIN ££MOVIE-TYPE HAS-INSTANCE £COMEDY)
; *** the variable "targetbest" records the fact that a path corresponds to something
;     that has been stored in "query-target". For instance, if the competing paths 
;     are 
;       <$STRING HAS-SUBCLASS ££AUTHORIZATIONREQUESTREASON RANGE-OF &HAS-AUTHORIZ-REASON
;        DOMAIN ££AUTHORIZATIONREQUEST SUBCLASS-OF ££REQUEST>
;     and 
;       <$STRING HAS-SUBCLASS ££AUTHORIZATIONREQUESTCODE RANGE-OF &HAS-AUTHORIZ-CODE
;        DOMAIN ££AUTHORIZATIONREQUEST SUBCLASS-OF ££REQUEST>
;     and the path through the CODE is the standard path to report the infos about
;     a ££REQUEST, then the second path must be preferred
(defun choose-best-ontpath (ont-semrestrs)
  (let ((best (first ont-semrestrs)) bestshort bestlength no-subcl-nxt 
        targetbest targetnxt instancebest instancenxt initgreetbest initgreetnxt
        bestdescrsubcl nextdescrsubcl valuebest valuenxt)
  ; *** INITIALIZATION: if the first path is 'fail, default worst case, otherwise
  ;        initialize "best" with the first path
     (cond ((eq best 'fail) 
              (setq bestshort 'fail)
              (setq bestlength 1000)
              (setq targetbest nil)
              (setq instancebest nil)
              (setq initgreetbest nil)
              (setq valuebest nil)
              (setq bestdescrsubcl 100))
           (t (setq bestshort (remove-subcl-links best))
              (setq bestlength (length bestshort))
              (setq targetbest (is-target-path best))
              (setq instancebest (is-relinstance-path best))
              (setq initgreetbest (is-initgreet-path best))
              (setq valuebest (includes-hasvalue best))
              (setq bestdescrsubcl (count-descr-subclass best))))
     (do ((next (second ont-semrestrs) (first ont-semrestrs))
          (ont-semrestrs (rest (rest ont-semrestrs)) (rest ont-semrestrs)))
         ((null next) best)
         (cond ((neq 'fail next)
                  (cond ((eq 'fail best) (setq best next))
                        (t (setq no-subcl-nxt (remove-subcl-links next))
                           (cond ((< (length no-subcl-nxt) bestlength)
                                    (setq best next)
                                    (setq bestlength (length no-subcl-nxt)))
                                 ((= (length no-subcl-nxt) bestlength)
                                    (setq targetnxt (is-target-path next))
                                    (setq instancenxt (is-relinstance-path next))
                                    (setq nextdescrsubcl (count-descr-subclass next))
                                    (setq initgreetnxt (is-initgreet-path next))
                                    (setq valuenxt (includes-hasvalue next))
                                    (cond ((and targetnxt (not targetbest))
                                             (setq best next)
                                             (setq targetbest t))
                                          ((and instancenxt (not instancebest))
                                             (setq best next)
                                             (setq instancebest t))
                                          ((< nextdescrsubcl bestdescrsubcl)
                                             (setq best next)
                                             (setq bestdescrsubcl nextdescrsubcl))
                                          ((and initgreetnxt (not initgreetbest))
                                             (setq best next)
                                             (setq initgreetbest t))
                                          ((< (length next) (length best))
                                             (setq best next))
      ; *** the next is relevant in case what is searched is the connection from
      ;     ££temperature to ££temperature-value: two links of equal length are
      ;     involved. The first one is the standard ££has-value, the second one
      ;     passes through ££feel-1. The first one must be preferred.
                                          ((and (= (length next) (length best))
                                                valuenxt (not valuebest))
                                             (setq best next)
                                             (setq valuebest t))
                                          ))))))))))

; *******************************************************************
; *** returns true if the path involves the ££has-value relation
;     This is useful for inverted properties, wher the connection between, e.g.
;     ££temperature and ££property-value (values of temperature), is best if
;     passes through ££has-value rather than ££to-feel-1
(defun includes-hasvalue (path)
   (member '££has-value path))

; *******************************************************************
; *** returns true if the path involves a relation instance. It is recognized
;     by the fact that the path ends as:
;     (... xxx ARG-OF rrr VALUE yyy)
;     An example could be "£classical-music ARG-OF &has-music-type33 VALUE £classic"
; *** These paths could be preferred if the goal is to retrieve the node xxx on
;     the basis of yyy.
(defun is-relinstance-path (path)
   (let ((revpath (reverse path)))
      (and (eq 'value (second revpath))
           (eq 'arg-of (fourth revpath)))))

; *******************************************************************
; *** returns the number of subclass links connecting descriptions.
;     the goal is to choose 
;     time-interval --has-subclass--> day --&has-day-descr--> day-description
;     rather than
;     time-interval --&has-time-interval-descr--> time-interval-descr
;        --has-subclass--> day-description
; *** it seems that the first one is more relavant in connection with what is
;     actually being described (i.e. a day)
(defun count-descr-subclass (path)
   (cond ((null path) 0)
         ((and (memq (second path) '(subclass-of has-subclass))
               (is-subclass-of (remove-synt-pointer (first path)) '££description)
               (is-subclass-of (remove-synt-pointer (third path)) '££description))
            (1+ (count-descr-subclass (rest (rest path)))))
         (t (count-descr-subclass (rest (rest path))))))

; *******************************************************************
; *** this checks if a path has, as a sublist, one of the default query targets
(defun is-target-path (path)
   (declare (special *DEFAULT-CONCEPT-INFOS*))
   (let* ((string-path (reverse (member '$string path)))
          (inv-string-path (inv-range-dom string-path))
          (string-rev-path (reverse (member '$string (reverse path))))
          (inv-string-rev-path (inv-range-dom string-rev-path))
          found nxtcheck)
       (do ((nxttarget (first *DEFAULT-CONCEPT-INFOS*) (first defaults))
            (defaults (rest *DEFAULT-CONCEPT-INFOS*) (rest defaults)))
           ((or found (null nxttarget)) found)
           (setq nxtcheck (cons (first nxttarget) (second nxttarget)))
           (setq found (or (part-of-list nxtcheck string-path)
                           (part-of-list nxtcheck inv-string-path)
                           (part-of-list nxtcheck string-rev-path)
                           (part-of-list nxtcheck inv-string-rev-path))))))

; *******************************************************************
; *** this checks (in ATLAS) if there are no previos sentence (beginning of a 
;     report) and the path includes the ££greetings concept
(defun is-initgreet-path (path)
  (declare (special *PREV-SENTENCES*))
  (and (null *PREV-SENTENCES*) (member '££greetings path)))

; *******************************************************************
; *** removes any subclass link together with the following concept
(defun remove-subcl-links (path)
  (cond ((null path) nil)
        ((memq (first path) '(has-subclass subclass-of))
            (remove-subcl-links (rest (rest path))))
        (t (cons (first path) (remove-subcl-links (rest path))))))

; *******************************************************************
; *** this repeats the next functions on different first segments
;     This may happen when the first segment is ambiguous
; *** This returns a single (unambiguous) path
; !!! See below for the format of the interpreted restrictions (interprestrs);
;     - for each item in first-segment, the best connection with an interpreted
;       restriction is looked for. The result (all-interp-restrs) is a set of
;       extended interpreted restrictions. So, each of them is a pair
;       <full-interpretation, restriction-only-interp> 
;     - the best full-interpretation is found (restr-selector)
;     - using it as a search key, the best interpreted-restriction is returned
(defun mult-compose-restr-path (first-segment interprestrs &optional source)
   ; *** source used for debug
  (let (all-interp-restrs restr-selector)
     (cond ((listp (remove-synt-pointer (first first-segment)))		; *** ambiguous concept
             (dolist (nxtfirst first-segment)
                   (setq all-interp-restrs 
                       (cons (compose-restr-path nxtfirst interprestrs source) all-interp-restrs)))
             (setq restr-selector (choose-best-ontpath (mapcar #'first all-interp-restrs)))
             (list-assoc all-interp-restrs restr-selector))
           (t (compose-restr-path first-segment interprestrs source)))))

; *******************************************************************
; *** this takes a path on the ontology, representing the interpretation of a search restriction,
;     and composes it with a set of interpreted sub-restrictions.
; *** this only takes the different sub-restrictions and looks for the first one
;     that matches first-segment
; *** Input: first-segment (e.g. (££ticket-counter subclass-of ££office))
;            interprestrs (e.g. (((££office xxx) interp("with the window")) ((££room yyy)) nil) or 
;                            (((£office (and xxx zzz)) nil)) )
;     The first example above could refer to phrases as "the office and the room with the window"
; *** Remember that an interpreted restriction is a pair where the first item is the 
;     interpretation of the head+restriction, while the second is the interpretation of the
;     restriction, not connected to the head (i.e. the interpretation of its sub-restrictions)
;     In case of conjuncts, the interpretation of the modifiers of the other conjuncts
;     are kept in the second element of the interpreted restriction
;     of the top-level restriction, with all of its modifiers, except the conjuncts.
; ***        Note that restrictions can include some relevant path, but
;            also some wrong interpretation (in this context) of the
;            restrictions; this "join" should cut off these "wrong" paths
; *** Output:  a pair including the composed path and the chosen second segment
;            e.g. ((££ticket-counter subclass-of ££office (and xxx zzz)) nil)
;            e.g.2 ((interp[office-with-window], interp[window]))
(defun compose-restr-path (first-segment interprestrs &optional source)
  ; (format t "///////// Compose-restr-path called from ~a~% first-segment: ~a~% interpreted-restrictions: ~a~%" 
  ;       source first-segment interprestrs)
  ; (break "")
; *** it takes as good the first possible connection between first-segment and the restrictions
  (cond ((null interprestrs) 'fail)
        (t (let ((first-res (sing-comp-restr-path first-segment (first interprestrs) source)) 
                 final-res)
               (cond ((null first-res)
                        (setq final-res 
                            (compose-restr-path first-segment (rest interprestrs) source)))
                     (t (setq final-res 
                            (list first-res (select-constr-paths (first interprestrs))))))
   ;(format t "///////// result of Compose-restr-path~% ~a~%" final-res)
   ;(break "")
                final-res))))

;*****************************************************************
; *** interp is a pair (restriction-repr subrestrs-repr). This function extracts from
;     restriction-repr the portion related with subrestrs-repr
; *** the rationale is to build a representation for the possible conjunct common modifiers
;     restriction-repr could come from "nice young girl with a hat". In this case, subrestrs-repr
;     is the one of "with the hat", since the other two restrictions (i.e. nice and young)
;     cannot be used for being distributed in conjuncts (currently we handle only movements
;     from the last conjunct to the previous ones: in "a good boy and a nice young girl with a
;     hat" the only shareable modifier is "with a hat", which could be copied from the girl
;     to the boy).
; *** This is done in the following way:
;     1. The representation for "nice young girl with a hat" is decomposed in all its subpaths
;        so that the subpaths of he modifiers are kept apart (nice girl + young girl + girl
;        with a hat). This is accomplished by flatten-all-ands (in seminterp)
;     2. The represention of the subrestrs is also flattened (in our example there is just
;        "with the hat", but ther could be more of them
;     3. All parts obtained from 1 that include a path in 2 are selected
;     4. The possible ands are re-inserted
(defun select-constr-paths (interp)
   (let ((flatrepr (flatten-all-ands (first interp) nil))
         (flatsubr (flatten-all-ands (second interp) nil))
         tempres finres)
       (dolist (nxtsub flatsubr)
          (dolist (nxtrep flatrepr)
              (cond ((part-of-list nxtsub nxtrep)
                       (setq tempres (append1 tempres nxtrep))))))
    ; *** Now, in tempres, we have all relevant paths (point 3 above)
       (setq finres (compose-and-repr tempres))
    ; *** assignment to finres for debug
       finres))

;*****************************************************************
; *** first-segment is a simple path
;     sing-interp-restr is the representation of a restriction, given, as usual, as
;     a pair < interp of the restriction, interp of its subrestrictions>
(defun sing-comp-restr-path (first-segment sing-interp-restr &optional source)
 (let ((second-segment (apply-conj-distributivity sing-interp-restr)))
    ; *** currently this function does nothing: it simply takes the first, i.e. the
    ;     actual interpretation, disregarding sub-restrictions
  ; (Format t "Entering sing-comp-restr-path: ~% first-segment: ~a~% second-segment ~a~%"
  ;          first-segment second-segment)
  ; (break "")
  (cond ((eq 'and (first second-segment))
    ; *** an and as first item of the second conjunct can come out from the analysis of
    ;     coordinations
             (list 'and
                  (mapcar #'(lambda (x) (sing-comp-restr-path first-segment 
                                               (list x (second sing-interp-restr))))
                         (second second-segment))))
        ((and (listp (ult first-segment))
              (eq 'eq (first (ult first-segment)))
              (eq (ult (butlast first-segment)) (first second-segment)))
    ; *** first-segment is obtained from a proper name, so that its
    ;     last element is (eq name)
           (cond ((null (rest second-segment)) first-segment)
                 (t (append (butlast first-segment)
                           `((and (,(ult first-segment)
                                   ,(rest second-segment))))))))
        ((listp (remove-synt-pointer (first second-segment)))		; *** ambiguous concept
           (let ((chosen-second 
                      (select-segment (ult first-segment) second-segment)))
              (cond ((null chosen-second) 
                       'fail)
                    (t (compose-restr-path first-segment chosen-second source)))))
        (t (let ((match? (tail-head-match first-segment second-segment)))
           ;(Format t "After tail-head-match: ~% first-segment: ~a~% second-segment ~a~% RESULT: ~a~%"
           ;         first-segment second-segment match?)
           ;(break "")
               (cond ((eq match? 'fail)
    ; *** in case there is no match, another possibility is that second-segment starts with
    ;     a subtype specification of the last item of first-segment
                        (setq match? (subtype-match first-segment second-segment))
                        (cond ((eq match? 'fail) nil)
        ; *** in the next case, match? includes the full path, since first-segment had to be
        ;     re-computed inside subtype-match
                              (t match?)))
                     (t match?)))))))
   
; *******************************************************************
; *** this function should take care to extend the interpretation of a restriction with the
;     intrepretation of subrestrictions coming from other conjuncts. Currently, it ignores
;     such sub-restrictions
(defun apply-conj-distributivity (sing-interp-restr)
   (first sing-interp-restr))

; *******************************************************************
; *** the tail of the first list must overlap with the beginning of the
;     second list (ex. (a b c d e f) (d e f g h))
; *** it returns the resulting full list (a b c d e f g h)
(defun tail-head-match (firstl secondl)
  (let ((revfirst (reverse firstl)) found rempart count)
   ; *** moves ahead on the reversed first list (f e d c b a)
    (setq rempart
       (do* ((tailfirst nil (cons nxtfirst tailfirst))
             (nxtfirst (first revfirst) (first revfirst))
             (revfirst (rest revfirst) (rest revfirst)))
     ; *** when the reversed first list is empty no match has been found
     ; *** if a match was found ("d" in the example above), then the
     ;     tail collected in tailfirst (i.e. (f e d)) is matched against the beginning
     ;     of secondl; actually, what is matched is just (f e), since "d" could
     ;     include the "synt" pointer
     ; *** if secondl is ((synt num d) e f g h) and firstl is (a b c d e f)
     ;     tailfirst is (d e f); (length tailfirst) is 3; 
     ;     (first-n 3 secondl) is ((synt num d) e f); found is (synt num d)
           ((or (null nxtfirst) found)
              (cond (found 
                      (cond ((equal (rest tailfirst)
                                    (rest (first-n (length tailfirst) secondl)))
                               (append (first-n (- (length firstl) (length tailfirst)) firstl)
                                       (cons found (nthcdr (length tailfirst) secondl))))
                            (t 'fail)))
                    (t 'fail)))
           (setq found (synt-sem-equal nxtfirst (first secondl)))))
   ; *** The two items are synt-sem-equal if they are equal or one of them includes
   ;     a pointer to the syntactic tree and the concepts are the same
   ; *** If ok, found includes the richer item (with synt), if any
   ; *** now, it checks if the remaining part is the reverse of the end of the
   ;     first list (ex. (a b c d) (d c))
   ;     This case may happen if the second list comes from a subclass restriction
   ;     (as in "richieste di acquisto in deroga" [lit. requests of purchase in
   ;     waiver"]). Here, just the extra-purchases (purchase in waiver) require a
   ;     request, so that "richieste di acquisto" is interpreted as
   ;     "requests of an extrapurchase, which is a purchase" (actually <££REQUEST
   ;     HAS-SUBCLASS ££AUTHORIZATIONREQUEST RANGE-OF &HAS-PURCHASE-REQUEST DOMAIN
   ;     ££EXTRAPURCHASE SUBCLASS-OF ££PURCHASE>) where "which is a purchase" is 
   ;     included since the lexical meaning of the word "acquisto" is ££PURCHASE.
   ; *** But "acquisto in deroga" is interpreted as <££PURCHASE HAS-SUBCLASS 
   ;     ££EXTRAPURCHASE> so that the info is repeated twice.
   ; *** N.B. The arc labels are reversed (e.g. HAS-SUBCLASS vs. SUBCLASS-OF)
   ; !!!!!!!!!!!!!!!!!!!!!!!!!!!! N.B. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ; *** The part below has been removed since, though it is in general ok, it may
   ;     happen that the result of this function is just a piece of a more complex
   ;     structure. For instance, in "Stato e posizione delle componenti installate"
   ;     what happens is that "posizione delle componenti installate" could undergo
   ;     a simplification of the type foreseen here, since it actually means
   ;     "the position where have been installed the items that have been installed"
   ;     But the first conjunct is interpreted as "the status of the item that have
   ;     been installed", so that the final "that have been installed" must be kept
   ;     also in the second conjunct in order to put together the two pieces.
    ;(setq count (1- (- (length firstl) (length rempart))))
    ;(cond ((< count 0) rempart)
    ;      ((equal (inv-range-dom (reverse rempart))
    ;              (nthcdr count (butlast firstl)))
    ;         nil)
    ;      (t rempart))
    rempart))

; *******************************************************************
; *** this compares two concepts; they are synt-sem-equal if they are
;     equal or if one of them is <SYNT POS concx> and the other is concx
(defun synt-sem-equal (conc1 conc2)
  (cond ((equal conc1 conc2) conc1)
        ((equal (remove-synt-pointer conc1) conc2) conc1)
        ((equal (remove-synt-pointer conc2) conc1) conc2)))

; *******************************************************************
; *** If two concepts a synt-sem-equal, it returns the one with "synt", if any
(defun best-synt-conc (conc1 conc2)
   (cond ((listp conc1) conc1)
         ((listp conc2) conc2)
         (t conc1)))

; *******************************************************************
; *** This adds a pointer to the syntactic structure to a semantic concept
;     [££rain, pos] --> (synt pos ££rain)
;     Where pos is the line number of the syntactic item associated with a concept
(defun build-sem-to-synt-ref (sem syntpos)
   (list 'synt syntpos sem))

; *******************************************************************
; *** if the description of the concept is (SYNT pos actconc) it returns actconc
;     otherwise it returns the input description
(defun remove-synt-pointer (concdescr)
  (cond ((and (listp concdescr) (eq (first concdescr) 'synt))
           (third concdescr))
        (t concdescr)))

; *******************************************************************
; *** this tries to ascertain if, second-segment
;     starts with a subtype of the last item of first-segment 
(defun subtype-match (first-segment second-segment)
  (let ((firstsecond (remove-synt-pointer (first second-segment))))
   (cond ((is-subtype-of-one-of firstsecond (inlist (ult first-segment)))
           (let ((newfirst (choose-best-ontpath 
                               (find-shortest-path (first first-segment) firstsecond))))
   ; *** newfirst replaces the old "first-segment"; it was a path going from some
   ;     concept to the "unrestricted" start of the second segment; since it has
   ;     been restricted by the lower dependents, it must replace the unrestricted
   ;     one, i.e. a new "best path" has to be looked up
               (append newfirst (rest second-segment))))
         (t 'fail))))

;(defun subtype-match (first-segment other-segments)
;   (cond ((null other-segments) nil)
;         ((is-subtype-of-one-of (first (first other-segments)) (ult first-segment))
;            (let ((newfirst (choose-best-ontpath 
;                               (find-shortest-path
;                                  (first first-segment)
;                                  (first (first other-segments))))))
;                (cons (append newfirst (rest (first other-segments)))
;                      (subtype-match first-segment (rest other-segments)))))
;         (t (subtype-match first-segment (rest other-segments)))))

; *******************************************************************
; *** chooses, among "segments", the one that begins with "concept"
;     Either "concept" or the first of any of "segments" can include a pointer to syntax
(defun select-segment (concept segments)
 (declare (special topic-changes))
 (let ((start-segment (first (first segments))))
  (cond ((null segments) nil)
        ((equal concept start-segment)
            (first segments))
        ((equal (remove-synt-pointer concept) start-segment) concept)
        ((equal (remove-synt-pointer start-segment) concept) (first segments))
        ((member (list concept start-segment) topic-changes :test #'equal)
            (append `(,concept has-subclass) (first segments)))
        ((member (list start-segment concept) topic-changes) :test #'equal
            (append `(,concept subclass-of) (first segments)))
        (t (select-segment concept (rest segments))))))

; *******************************************************************
(defun get-default-infos (topic)
  (declare (special *DEFAULT-CONCEPT-INFOS*))
  (let ((alltopics (inlist topic)) found)
      (do ((nxttopic (first alltopics) (first alltopics))
           (alltopics (rest alltopics) (rest alltopics)))
          ((null nxttopic)
             (cond ((null found)
                      (exception 'semantic-error
                           "PROC/buildquery: missing default infos 1 for" topic))
                   (t found)))
          (setq found 
              (cons (cons nxttopic (int-get-def-inf nxttopic))
                    found)))))

(defun int-get-def-inf (topic)
  (declare (special *DEFAULT-CONCEPT-INFOS*))
  (let ((path (find-default-path (list (list nil topic)))) lastnode)
      (cond ((null path)
                (exception 'semantic-error
                         "PROC/buildquery: missing default infos 2 for" topic))
            (t (setq lastnode (ult path))
               (cond ((is-subclass-of lastnode '££datatype) path)
                     (t (append path (int-get-def-inf lastnode))))))))

; **************************************************************************
; *** looks for a default path for any concept in the given list
;     Actually, topics is a list of pairs <subclass-path concept>
;     where subclass-path is the path of subclass-of links that have been
;     traversed to reach the concept under analysis
;     The functions goes up in a breadth-first fashion, in order to find,
;     for ££trn-location, also a possible definition of ££location
;     Unfortunately a node can have multiple parents, so the search
;     becomes a little more complex
(defun find-default-path (topics)
  (declare (special *DEFAULT-CONCEPT-INFOS*))
  (let (found upconcs result)
    (cond ((null topics) nil)
          (t (setq result
                (do ((nxttopic (first topics) (first topics))
                     (topics (rest topics) (rest topics)))
                    ((or (second found) (null nxttopic))
                        found)
                    (cond ((eq (second nxttopic) '££datatype)
                            (setq found (list (first nxttopic) (list '££datatype))))
                          (t (setq found 
                               (list (first nxttopic)
                                     (first (leggi *DEFAULT-CONCEPT-INFOS* (second nxttopic)))))))
                  ; *** if, for a given concept, no def is found, try with its superconcepts
                    (cond ((null (second found))
                              (setq upconcs 
                                   (append upconcs 
                                       (mapcar #'(lambda (x) 
                                                    (list (append1 (first nxttopic)
                                                                   (second nxttopic))
                                                           x))
                                               (get (second nxttopic) 'subclass-of))))))))
            (cond ((null (second result))
                    (find-default-path upconcs))
                  (t (build-subclass-path result)))))))

; *******************************************************************
(defun build-subclass-path (foundpath)
   (let ((subclass-path (first foundpath))
         (target-path (second foundpath))
         resultpath)
     (do ((nxtconc (first subclass-path) (first subclass-path))
          (subclass-path (rest subclass-path) (rest subclass-path)))
         ((null nxtconc) (append resultpath target-path))
         (setq resultpath
             (append resultpath `(subclass-of ,nxtconc))))))

; *******************************************************************
; *** returns all restrictions of theme-subtree, which is assumed to be a verbal
;     subtree. All dependents are included, except the already considered
;     topic-subtree, punctuation marks, and auxiliaries.
; *** in "looking for information about the opera", theme-subtree is "about ..." and 
;     topic-subtree is "opera ..."
; ** INPUT:
;  >>> theme-subtree: the subtree (usually headed by the verb) inside which the
;      restrictions must be found
;  >>> topic-subtree: the subtree (usually a dependent of the verb) that has
;      already been recognized as the topic of the question
;  >>> skip-head: used for getting the actual restrictions of the verb. When it
;      is NIL, then the full verb subtree could act as the resulting restriction
; ** OUTPUT: a list of restrictions.
;     Each restriction is a pair <actual-restr label-up>
;     where:
;     - actual-restr is a list of subtrees which are the restriction
;       It is a list, since it may include multiple conjuncts 
;     - label-up is the label of the link connecting the restriction to its governor
;       (this is currently used just in case of verbs)
;       In case of prepositional RMOD, label-up is a pair of the form (RESTR prep),
;       where "prep" is the meaning of the linking preposition 
(defun get-verb-restrictions (theme-subtree topic-subtree &optional skip-head)
  (declare (special +FULL-TREE+))
  (cond ((equal theme-subtree topic-subtree) nil)
   ; *** the two inputs may be equal, for instance, in case of "vorrei
   ;     un'informazione". In this case, because of the search process
   ;     both "theme-subtree" and "topic-subtree" refer to "un'informazione"
        (t (let ((deps (remove-head-marker (get-actavm-deps-with-traces theme-subtree)))
                  restrs newrestrs finrestrs temp-prep headmean up-link)
             (setq finrestrs
               (dolist (nxtdep deps restrs)
                   (setq up-link (get-actavm-headlink nxtdep))
                   (cond ((eq (get-actavm-headlink nxtdep) 'CONTIN+PREP)
                            (setq nxtdep (find-actavm-dep 'PREP-ARG nxtdep))))
                   (setq headmean (get-actavm-headlexmean nxtdep))
   ; *** in up-link the actual link up of the dependent
   ; *** in headmean the meaning of the actual filler of the restriction
   ;     this means that determiners and dummy prepositions are skipped
                   (cond ((equal nxtdep topic-subtree) nil)
                         ((memq headmean '(--dummy-prep --about-relation --after-relation))
                           ; *** --after-relation has been included to cope with
                           ;     "I'm after ..." in the sense of "looking for"
                           ;     It must be handled more properly in case more standard
                           ;     senses are involved
                            (let ((down-tree (skip-determiner (find-actavm-dep 'PREP-ARG nxtdep))))
                                (cond ((equal topic-subtree down-tree) nil)
                                      ((is-a-date-descr? down-tree)
                                         (setq restrs (append1 restrs (list (list down-tree) up-link))))
                                      (t (setq restrs (append1 restrs (list (list nxtdep) up-link)))))))
                         ((is-a-actavm-trace? nxtdep)
       ; *** the next dependent is a trace of the topic
                           (cond ((or (neq 'empty (get-actavm-headcoref nxtdep))
                                      (not (null (get-actavm-headlexmean nxtdep))))
       ; *** if the coref is not empty, then the referent is in the sentence
       ;     if its lexmean is not null, it was probably assigned as a default referent
       ;     (e.g. in a trace of "we" or "I")
                                    (setq restrs (append1 restrs (list (list nxtdep) up-link))))
                                 (t (break "Empty trace in Buildontorepr: get-verb-restrictions"))))
                         ((eq (get-actavm-headcateg nxtdep) 'PREP)
    ; *** in case the next dependent is headed by a preposition,
                            (cond ((equal (find-actavm-dep 'PREP-ARG nxtdep) topic-subtree)
            ; it could be the involved topic ("A che ora comincia ...")
            ; in this case, the preposition saved in temp-prep
                                     (setq temp-prep (get-actavm-head nxtdep)))
            ; or any other dependent
                                  ((lab-subsumes 'RMOD (get-actavm-headlink nxtdep))
                                     (setq restrs
                                         (append1 restrs 
                                               (list (build-conj-restrs nxtdep)
                                                     (list 'rmod (get-actavm-headlexmean nxtdep))))))
            ; *** if the link is not rmod, the syntax hass already chosen a semantic role for the PP
                                  (t (setq restrs 
                                         (append1 restrs 
                                               (list (build-conj-restrs nxtdep) up-link))))))
                         ((memq (get-actavm-headlink nxtdep) 
       ; *** the last one for "ci" in "ci sono" ("there are")
                                '(END SEPARATOR PARTICLE PRON-RMOD-LOC+METAPH))
                            nil)
                         ((and (eq (get-actavm-headlink nxtdep) 'VERB-PREDCOMPL+SUBJ)
                               (eq (get-actavm-headlexmean nxtdep) '££entity))
       ; *** this for "Which are ...?", where "which" should not count as a restriction
                            nil)
                         ((eq (get-actavm-headtype nxtdep) 'AUX) nil)
       ; *** the next for the first person subject of the verb
                ;         ((and (eq (get-actavm-headcateg nxtdep) 'PRON)
		;	       (memq (get-actavm-headlexmean nxtdep) 
                ;                     '(§speaker §myself §indef-ref)))
                ;            nil)
       ; *** the next for "Quale è" ("what is ...")
                         ((subtree-member topic-subtree nxtdep) nil)
       ; *** the next for "Quale è" ("what is ..."), but should be covered by the previous one
                         ((and (eq (get-actavm-headcateg nxtdep) 'PRON)
			       (eq (get-actavm-headlexmean nxtdep) '--q-pron))
                           (exception 'semantic-error
                                 "Question pronoun not topic in SEMANT-PROC-ALL:buildquery"))
       ; *** the next for locutions
                         ((eq (get-actavm-headlemma nxtdep) 
                              (get-actavm-headlemma theme-subtree)) 
                           (exception 'semantic-error
                                 "Strange verbal locution in SEMANT-PROC-ALL:buildquery"))
       ; *** the next for standard cases, where the dependent acts as a restriction
                         (t (setq newrestrs (build-conj-restrs nxtdep))
                            (setq restrs (append1 restrs (list newrestrs up-link)))
       ; *** with append1, the resulting restrs are ( ((conj11 case1) (conj12 case1) ...) ... 
       ;       ((conjN1 caseN) (conjN2 caseN) ... )) In this way, have collected all conjuncts
       ;     of the various dependents. Now, depending on the reading of the verb, we can force
       ;     a distributive or cumulative reading; this is left to build-restr-sem (the calling
       ;     function)
                                                      ))))
; ************ Now, all restriction have been collected in finrestrs
            (setq finrestrs
               (cond ((and (eq 'VERB (get-actavm-headcateg theme-subtree))
                           (is-content-verb (get-actavm-headlexmean theme-subtree))
                           (member-or-prep topic-subtree (get-actavm-deps-with-traces theme-subtree)))
   ; *** in case the theme-subtree refers to a content verb (e.g. "conduct")
   ;     the restriction is given by the same tree, apart of the removed
   ;     dependents. For instance, in "Which concerts were conducted by Abbado
   ;     last week", "concerts" is the topic, and "conducted by Abbado last week"
   ;     is the (only) restriction.
   ; *** The last condition is to avoid that when handling "conducted by Abbado 
   ;     next week", this branch is followed, leading to infinite recursion
                       (cond ((null temp-prep)
                                `(verb 
                                  ((head ,(get-actavm-head theme-subtree))
                                   (dependents ,finrestrs))
                                  nil nil))
   ; *** In case the topic was headed by a preposition, it is inserted as
   ;     head of the restriction: "In which theater they play La Locandiera"
   ;     topic --> "theater"
   ;     restriction --> "play La Locandiera"
                             (t `(verb
                                  ((head ,temp-prep)
                                   (dependents
                                       ((#\#)
                                        ((head ,(subst-head-val 
                                                   (get-actavm-head theme-subtree)
                                                    'link 'prep-arg))
                                         (dependents ,finrestrs)))))
                                  nil nil))))
                     (t finrestrs)))
        ;(format t "Result 2 of get-verb-restrictions: ~% ~a~%" finrestrs)
        ;(break "wbw")
           finrestrs
         ))))

; *******************************************************************
; *** returns true if any of the elements in deps is equal to topic-subtree
;     or is a PP whose argument is topic-subtree
(defun member-or-prep (topic-subtree deps)
   (cond ((null deps) nil)
         ((equal (first deps) '(#\#))
            (member-or-prep topic-subtree (rest deps)))
         ((equal topic-subtree (first deps)) t)
         ((and (eq (get-actavm-headcateg (first deps)) 'prep)
               (equal topic-subtree
                      (find-actavm-dep 'prep-arg (first deps))))
            t)
         (t (member-or-prep topic-subtree (rest deps)))))

; *******************************************************************
(defun is-content-verb (meaning)
   (memq meaning 
        '(££conduct ££buy ££begin ££to-cost ££collect ££end ££possess)))

; *******************************************************************
; *** returns two values: the first one are the topics, the second one are all restrictions
; *** Each restriction is a pair <actual-restr label-up>
;     where:
;     - actual-restr is the subtree which is the restriction
;     - label-up is the label of the link connecting the restriction to its governor
;       In case of PREP-RMOD, label-up is the meaning of the linking preposition 
; *** All dependents are included except the marker of the head position and locution continuations.
(defun get-nonverbal-restrictions (subtree)
  (let ((deps (remove-head-marker (get-actavm-deps-with-traces subtree)))
        restrs newtopic newrestrs distributivity linklab up-link result1 result2
        (head-node (get-actavm-head subtree))
        (topics (list (get-actavm-headlexmean subtree))))
  ;(format t "Entering get-nonverbal-restrictions: headnode= ~a~%" head-node)
  ;(break "huh")
    (multiple-value-setq (result1 result2)
      (dolist (nxtdep deps (values topics restrs))
          (setq up-link (get-actavm-headlink nxtdep))
          (cond ((eq (get-actavm-headlemma nxtdep) 
                     (get-actavm-headlemma subtree)) nil)
   ; *** the previous for locutions
                ((eq (get-actavm-headcateg nxtdep) 'PUNCT) nil)
   ; *** punctuation marks are ignored
                ((and (eq (get-actavm-headcateg nxtdep) 'CONJ)
                      (eq (get-actavm-headtype nxtdep) 'COORD))
   ; *** this for coordinations: they are not true restrictions
     ;              (let ((second-conjunct (skip-determiner (find-actavm-dep 'COORD2ND nxtdep))))
     ;                 (cond ((memq (get-actavm-headcateg second-conjunct) '(NOUN PREP ADJ))
     ;                          (setq coords 
     ;                             (append1 coords 
     ;                                  (list second-conjunct head-node (get-actavm-headlexmean nxtdep)))))
     ;                       (t (exception 'semantic-error
     ;                                 "PROC/buildquery: unknown categ for a 2nd conjunct"))))
                   nil)
                ((eq (get-actavm-headcateg nxtdep) 'PREP)
                    ; *** the next for subtype descriptions (Pseudo-locutions); they refer to 
                    ;     expressions as "stato di obsolescenza", where "obsolescence" does not seem
                    ;     to be a "restriction" in the sense of search condition, but rather a
                    ;     specification of a status type
                   (let* ((downnode (skip-determiner (find-actavm-dep 'PREP-ARG nxtdep)))
                          (down-ident (get-actavm-headlexident downnode)))
                        (cond ((not (null down-ident))
                                (let ((subtype-spec
                                        (find-subtype-path (ult topics) down-ident 'subtype)))
                                    (cond ((not (null subtype-spec))
                                            (setq topics (append (butlast topics) subtype-spec)))
                    ; *** if it is a preposition, but not a subtype spec, it is a true restriction
                                          (t (setq restrs 
                                                 (append1 restrs 
                                                     (list (build-conj-restrs nxtdep) up-link)))))))
                    ; *** if it is a preposition, but without a down ident, it is a true restriction
                              (t (setq restrs 
                                      (append1 restrs (list (build-conj-restrs nxtdep) up-link)))))))
   ; *** in the standard case, the dependent is added to the restrictions. 
                (t (setq restrs (append1 restrs (list (build-conj-restrs nxtdep) up-link)))))))
      ; (format t "Result of get-nonverbal-restrictions: ~% Topics. ~a~% Restrs: ~a~%"
      ;        result1 result2)
      ; (break "qtq")    
       (values result1 result2)
   ))

; *******************************************************************
; *** returns all conjuncts in a list
;     Tree[John, Bill and Mary] --> 
;         (Tree[John, Bill and Mary] Tree[Bill and Mary] Tree[Mary])
(defun build-conj-restrs (subtree)
  (let ((conjrestrs (find-all-conjuncts subtree)))
  ;(format t "Enter build-conj-restrs; conjrestrs = ~a~% ***** ~a~%" (first conjrestrs) (second conjrestrs))
  ;(break "wfw")
   ; *** find-all-conjuncts (in avm-transf) returns the various (if any) coordination-headed 
   ;     subtrees except for the first one, which is not headed by a coordination:
   ;     Tree[John, Bill and Mary] --> 
   ;         (Tree[John, Bill and Mary] Tree[, Bill and Mary] Tree[and Mary])
   ; *** currently, it is assumed that all of them are conjunctions, so that the info
   ;     about the coordination type is ignored
      (cons (first conjrestrs)
            (mapcar #'(lambda (x) (find-actavm-dep 'COORD2ND x))
                   (rest conjrestrs)))))

; *******************************************************************
; *** this checks if a "downnode" modification of "upnode" has to be
;     interpreted as a subtype specification of "upnode". 
; *** This should be checked on the ontology, but it anticipates
;     ontological interpretation; so, either the result is cached somewhere
;     or the search is made twice
(defun is-subtype-restr (downnode upnode)
   (declare (special *SUBTYPES-SPEC*))
   (cond ((eq (get-actavm-headcateg downnode) 'prep)
            (is-subtype-restr (find-actavm-dep 'PREP-ARG downnode) upnode))
         ((memq (get-actavm-headcateg downnode) '(noun adj))
            (member (list (get-actavm-headlemma upnode) 
                          (get-actavm-headlemma downnode))
                  *SUBTYPES-SPEC* :test #'equal))
         (t nil)))
   
; *******************************************************************
(defun is-deictic-concept (concept)    ; e.g. "the city"
  (declare (special *DEICTIC-REFERENCE*))
   (leggi *DEICTIC-REFERENCE* concept))

(defun get-deictic-referent (concept)
  (declare (special *DEICTIC-REFERENCE*))
   (first (leggi *DEICTIC-REFERENCE* concept)))

; *******************************************************************
; *** this function aims at handling expressions including deictics,
;     as "this", "next". They can occur in two syntactic positions:
;     1. As governors of the noun, in case no article appears ("for
;        next week" "in this month" "in this city")
;     2. As modifiers of the noun ("for the next week" "la prossima
;        settimana")
; *** in the first case, semtopic is something above the whole expression
;     (often "££dialogue"), and singrestr is the expression. 
;     - ident is £next
;     - down-node is the one of "week"
;       it is reached via the "det+def-arg" arc for "this" and via the
;       det+indef-arg for "next". The reason is that the first is taken
;       as a demonstrative (type=demons), while the second is a deictic
;       (type=deitt). Perhaps, this could be changed at the lexical level.
;     - down-mean is ££week
; *** in the second case, semtopic is "££week" and singrestr is the
;     subtree including only "next". Note that "this" cannot occur in 
;     this case, neither in Italian, nor in English ("*the this week"
;     "*la questo mese"). This means that, since true deictics, in the
;     present context are the demonstratives, there is no possibility of
;     having a deictic referent (as ££city) here. "the next city" is
;     syntactically correct, but does not make sense in the present
;     context. Consequently:
;     - ident is £next
;     - down-node is nil
;     - down-mean is nil
(defun find-path-to-deictic (semtopic singrestr &optional prep-mean)
; *** this branch refers to structures as "next week", "this month", where
;     "singrestr" is the whole expression (and semtopic is, usually,
;     ££dialogue). In these cases, the resulting expression comes from
;     finding the path from "££dialogue" to "££week" (or "££month")
;     and adding the condition (eq £next), or (eq £this).
; *** Here, ££week is down-mean, and ident is £this
  (let ((meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr))
        (categ (get-actavm-headcateg singrestr))
        (type (get-actavm-headtype singrestr))
        down-node down-mean)
     (cond ((or (memq '££deictic-descr meaning)
                (memq '££deictic-specification meaning))	; "other" items
             (cond ((eq 'demons type)
                      (setq down-node (find-actavm-dep 'DET+DEF-ARG singrestr)))
                   ((eq 'deitt type)
                      (setq down-node (find-actavm-dep 'DET+INDEF-ARG singrestr)))
                   (t (exception 'semantic-error
                           "PROC/buildquery: unknown deictic adj type " 
                                type)))
             (cond ((null down-node)
    ; *** we are in case 1 (the next week) 
                      (cond ((is-subclass-of semtopic '££time-interval)
                               `(,semtopic (eq ,ident)))
                            ((memq '££deictic-specification meaning)
                               `(,semtopic (eq ,ident)))
                            (t (exception 'semantic-error
                                "PROC/buildquery: unknown deictic concept 1: " 
                                semtopic))))
                    (t (setq down-mean (get-actavm-headlexmean down-node))
    ; *** we are in case 1 (next week, this week, this city) 
                      (cond ((is-deictic-concept down-mean)   
                                     ; e.g. "this city", "this moment"
                              (let ((deict-ref 
                                        (get-deictic-referent down-mean)))
                                 (cond ((eq deict-ref '%neutral)
                                  ; *** the next are for, e.g., "this monday"
                                         (let* ((down-ident 
                                                    (get-actavm-headlexident
                                                               down-node)))
                                            (cond ((null down-ident)
                                                    (choose-best-ontpath
                                                      (find-shortest-path
                                                           semtopic
                                                           down-mean
                                                           prep-mean)))
                                                  (t (choose-best-ontpath
                                                       (find-shortest-path
                                                           semtopic
                                                           down-ident
                                                           prep-mean))))))
                                  ; *** the next are for, e.g., "this city"
                                       (t (choose-best-ontpath
                                             (find-shortest-path
                                                 semtopic deict-ref
                                                 prep-mean))))))
                            ((is-subclass-of down-mean '££time-interval)
            ; *** this branch is for "next week", "this month", where
            ;     "singrestr" is the whole expression (and semtopic is, usually,
            ;     ££dialogue). In these cases, the resulting expression comes from
            ;     finding the path from "££dialogue" to "££week" (or "££month")
            ;     and adding the condition (eq £next), or (eq £this).
            ; *** Here, ££week is down-mean, and ident is £this
                               (append1
                                  (choose-best-ontpath 
                                     (find-shortest-path semtopic down-mean))
                                     `(eq ,ident)))
                            (t (exception 'semantic-error
                                  "PROC/buildquery: unknown deictic concept 2: " 
                                   down-mean))))))
           (t (exception 'semantic-error
                    "PROC/buildquery: unknown deictic adjective " meaning)))))

; *******************************************************************
; *** this sould return true if "concept" is a relational concept (e.g. type,
;     friend, ...). Currently, the only such concepts are "types"
(defun is-relational-conc (concept)
 ; *** "concept" may be a list in case of semantically ambiguous words
 ;     (e.g. "direct": direct-movie, conduct-concert)
   (cond ((atom concept)
            (is-subclass-of concept '££type))
         (t (let (found)
               (do ((nextc (first concept) (first concept))
                    (concept (rest concept) (rest concept)))
                   ((or (null nextc) found)
                      found)
                   (setq found (is-subclass-of nextc '££type)))))))

; *******************************************************************
; *** given the meaning of preposition, chooses the actual ontology concept
;     on the basis of the upper and lower concepts
(defun select-prep-mean (prep-mean up-conc down-conc)
  (let (found (up-conc (inlist up-conc)) (down-conc (inlist down-conc)))
      (do ((next-p-mean (first prep-mean) (first prep-mean))
           (prep-mean (rest prep-mean) (rest prep-mean)))
          ((or found (null next-p-mean))
             (cond (found
                     (cond ((eq found 'no-rel) nil)
                           (t found)))
                   (t 'fail)))
          (cond ((and (match-prep-subclasses up-conc (first (first next-p-mean)))
                      (match-prep-subclasses down-conc (second (first next-p-mean))))
                  (setq found (second next-p-mean)))))))

; *******************************************************************
; *** checks if any of the concepts in conclist is a subconcept of conc
(defun match-prep-subclasses (conclist conc)
  (cond ((null conclist) nil)
        ((is-subclass-of (first conclist) conc) t)
        (t (match-prep-subclasses (rest conclist) conc)))) 

; *******************************************************************
(defun is-a-date-descr? (tree)
   (let ((headcat (get-actavm-headcateg tree))
         (headtyp (get-actavm-headtype tree)))
      (or (and (or (eq headcat 'num)
      ; *** 26 June, June 26, 26th June, June 26th
                   (and (eq headcat 'adj)
                        (eq headtyp 'ordin)))
      ; *** seventh June, June seventh
               (not (null (find-actavm-dep 'NOUN-RMOD-MONTH tree))))
          (and (eq headcat 'num)
      ; *** 1 of June
               (eq '££month-descr
                  (get-actavm-headlexmean
                     (find-actavm-dep 'PREP-ARG
                            (find-actavm-dep 'PREP-RMOD tree)))))
          (and (and (eq headcat 'pron)
                    (eq headtyp 'ordin)))
      ; *** seventh of June
               (not (null (find-actavm-dep 'PREP-RMOD-MONTH tree))))))

; *******************************************************************
(defun subtree-member (subtree tree)
   (cond ((null tree) nil)
         ((equal '(#\#) tree) nil)
         ((equal subtree tree) t)
         (t (int-subtree-member subtree (get-actavm-deps-with-traces tree)))))

; *******************************************************************
(defun int-subtree-member (subtree tree-list)
  (let (found)
      (do ((nxttree (first tree-list) (first tree-list))
           (tree-list (rest tree-list) (rest tree-list)))
          ((or (null nxttree) found) found)
          (setq found (subtree-member subtree nxttree)))))

; *******************************************************************
; *** this inspects a tree, checking if the root is the first element of a 
;     "and" sequence of conjuncts (John and Mary and Bill).
; *** in any case, it returns a list including all meanings of the various
;     conjuncts (a singleton list, if no such conjunct exists)
(defun get-topic-concepts (topic-tree)
   (let ((conjunct (find-actavm-dep 'COORD topic-tree)))
      (cond ((null conjunct)
               (list (get-actavm-headlexmean topic-tree)))
            ((eq '--and-operator (get-actavm-headlexmean conjunct))
               (let ((secondarg (find-actavm-dep 'COORD2ND conjunct)))
                   (cond ((null secondarg)
                             (exception 'semantic-error
                                  "PROC/buildquery: no second argument for an --and-operator"))
                         (t (cons (get-actavm-headlexmean topic-tree)
                                  (get-topic-concepts secondarg)))))))))
      
; *******************************************************************
; *** removes from a list of nodes all that have an upward link of the "CONTIN"
;     type, i.e. that are continuations of locutions
(defun remove-continuations (dependents)
  (cond ((null dependents) nil)
        ((or (equal (first dependents) '(#\#))
             (not (member 'contin (get-lab-ancestors (get-actavm-headlink (first dependents))))))
           (cons (first dependents) (remove-continuations (rest dependents))))
        (t (remove-continuations (rest dependents)))))
        
