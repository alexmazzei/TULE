
(in-package "USER")

(defvar *CHUNK-LEVELS* 
  ; *** the triple (english noun noun-engl) has to be read:
  ;     If the current language is "english" then apply the rule with
  ;     language=noun-engl to the entries of category "noun"
	'(special punct phras adv predet conj (english noun noun-engl) 
  ; *** The next refer to a rule for examples as "subject-centered"
  ;     Here, if the verb is preceded by a hyphen and then a noun,
  ;     the noun must be attached to the verb as noun-rmod
          (english verb verb-engl) 
         ; adj
          (adj (qualif compar)) 
          (adj (indef poss deitt interr demons))
          num interj noun pron art prep verb))

; *** in *PARSE-CONTEXT*, the first element is the context name, which can
;     be 'sentence' or 'parenth'; the second element is the level of
;     nesting in parentheses;
(defvar *PARSE-CONTEXT*)

;**********************************************************************
; *** This file contains the chunk parser; it includes the following
;     sections and functions
; ****** LOAD THE KB OF NON-VERBAL PARSE RULES ************************
; --> load-parserules ()  [in PROC(all languages)/loadfunctions]
; ****** READ INPUT DATA AND CONVERSION IN INTERNAL FORMAT ************
; --> chunk-parser ()
; --> parse-single-f (tbf parsedf casefrf &optional extended-labels)
; ****** MAIN PARSE FUNCTION AND APPLICATION OF NON-VERBAL RULES ******
; --> parse-sentences (data)
; --> remove-parentheses (data)
; --> rem-single-par (lines par-stack skipped-lines)
; --> add-parentheses (data links par-data)
; --> apply-loc-parserules (data links)
; --> apply-parserules (curline curlink prevlines prevlinks remlines remlinks)
; --> apply-posparserules (posrules head dependent beforedep afterdep succ)
; --> apply-chunk-rules (direction rules line linelink data labels prev prevlabs)
; --> check-parsecond (rule head dependent prevlines succlines head-gov)
; --> eval-parsecond (condit head dependent prevlines succlines head-gov)
; --> test-next-words (conditions head succlines)
; --> test-singl-nw (condition head succlines)
; --> adj-prep-govern (adj prep)
; --> noun-prep-govern (noun prep)
; --> adv-prep-govern (adv prep)
; --> chunk-head-cat (direction nxtword lab prev succ prevlab succlab chunkcats)
; --> find-chunk-word (direction pos word curlink prev succ prevlinks succlinks)
; ****** ANALYSIS OF CONJUNCTIONS *************************************
; --> link-conjunctions (data labels)
; --> find-second-conj (conjline nxtlines nxtlinks prevline)
; --> find-first-conj (secondc prevlines prevlinks nextlines nextlinks)
; --> find-avail-verb (nxtsent)
; --> find-conj-seq (firstconj prevlines prevlinks)
; --> synt-compatible-conjuncts (prevconjhead prevconjunct newlines newhead)
; --> build-lab-list (prevconjs)
; ****** ANALYSIS OF VERBAL DEPENDENTS ********************************
; --> find-v-complements (data links)
;	(prevlines prevlinks curline curlabs succlines succlabs vmood vtense)
; --> apply-verbal-rules (curline curlink prevlines prevlinks nxtlines nxtlinks)
; --> find-aft-compl (nxtlines nxtlabs)
; --> find-bef-compl (prevlines prevlinks)
; --> match-caseframes (found-deps curline curlab nxtlines nxtlabs allines allabs
;					 unlinked verbcl)
; --> exch-obj-subj (caseframe)
; --> get-cf-verbclass (verbline)
; --> remove-dummies (cllist)
; --> ins-trace (govlinumb referent tracelab succ succlab)
; --> int-ins-trace (traceline traceref succ succlab)
; --> conjoin-equal-deps (words allines allinks)
; --> test-barriers (nxtlines nxtlinks verblinumb)
; --> foll-relpron (nxtlines)
; --> find-derived-cl (vclass transfs notransfs)
; --> cancel-found-cases (caseframes verbcf assignedlabels)
; --> canc-sing-foundc (surfcf surflab deeplab verbcf assigned)
; --> get-role-def (surfcf surflab deeplab roles)
; --> filter-cf (surfcf surflab deeplab asslab scfacc slacc dlacc)
; --> check-caseframes
;     (unlinked caseframes verbline verblink allines allinks &optional preferences)
; --> move-kb-first (lab target lablist acc)
; --> check-sing-casefr
;	(unlinked kbvp kbcases kbroles verbline verblab allines allabs)
; --> match-in-casefr
;	(kbvp kbcases kbroles verbline verblab depline allines allabs)
; --> test-cf-condit (cf-condit verbline verblab depline allines allabs)
; --> apply-adjunct-rules (depline verbline allines allabs)
; --> count-unknown (labels count)
; --> choose-best-assign (verbline unlinked cf-match allines allinks)
; --> is-best-assign (verbline unlinked match1 match2 allines allinks)
; --> apply-criterium (verbline match1 match2 ind unlinked allines allabs)
; --> find-bestassign-case (lab match unlinked)
; --> altern-obj-subj (lablist1 lablist2 unlinked)
; --> locut-member (labellist)
; --> merge-cf-assign 
;	(best-match unlinked prevlines prevlinks nxtlines nxtlinks verblinumb)
; --> count-adjunct (labellist count)
; --> merge-sing-cf-assign (label unlink lines links verblinumb)
; ****** POSTPROCESSING: SOLVE UNLINKED ELEMENTS **********************
; --> attach-unlinked (data labels)
; --> dual-par (par)
; --> check-listpos (prevlines nxtlines allines allinks)
; --> find-gov-verb (nxtlines nxtlinks)
; --> attach-punct-and-others (nxtsent nxtsentlab)
; --> solve-rem-puncts (allines allinks)
; --> find-attachment (wrdcat categs allines allabs)
; --> find-main-v (link-up allines allinks)
; --> int-find-attach 
;	(wrdcat curline leftcat leftlines leftlabs 
;		        rightcat rightlines rightlabs allines allabs)
; --> choose-arc-label (headcat depcat)
; --> find-up-cluster (char)
; --> int-find-up-clust
;	(prevpoint nxtpoint curline curlink prevlines prevlinks nxtlines nxtlinks)
; --> climb-tree (start allines allabs)
; --> find-lowest (left right allines allabs)
; --> attach-unlinked-verbs (allines allabs)
; --> default-attach-verbs (lines links unlinked)
; --> assign-root (allines allabs)
; --> put-root-arc (allines allabs index)
; --> set-final-end (rootnumb lines labs)
; --> find-relcl-ref (curline pronline pronlink prlines prlinks)
; --> attach-double-relpron (allines allinks)
; --> find-verb-after-cluster (cluster-head+rem allines allinks)
; --> belong-to-chunk 
;	(depline deplink cluster-head-pos seendep allines allinks loops)
; --> count-unlinked (links)
; --> inspect-prep-art [for ATLAS]
; ****** AUXILIARY FUNCTIONS ******************************************
; --> find-a-line (condition lines labels &optional all)
; --> find-prec-lines (line allines)
; --> find-prec-line (line allines allinks)
; --> apply-line-cond (condition line link)
; --> chunk-ch-agree (line feat-vals)
; --> change-lab (lines links linumb newlab)
; --> mult-change-labs (lines links newlinks)
; --> rem-surf-marker (surflabel)
; --> is-an-adjunct (lab)
; --> get-all-lab-ancestors (label)
; --> merge-flatten (listlist)
; ****** PRINTING ON OUTPUT FILE **************************************
; --> write-parsed-file (data parseresult buff parsedport)
; --> print-data-line (line label outport)

;**********************************************************************
; ****** LOAD THE KB OF NON-VERBAL PARSE RULES
;**********************************************************************
; ***  It loads the rules from the file 'ALLLANG/KB-ALL/GRAMM-KB-ALL/parserules.dat"
;      The input format is described in that file. The output is the
;	attachment to the 'head category' of a list representing a tree,
;	which is stored in the property 'chunk-rules'.
;	The root of each subtree is a syntactic subtype (e.g. 'demons',
;	'qualif'). At the next level, we have the position information,
;	possibly augmented with extra infos, and at the next level a set of
;	pairs <conditions, arc-label>
; *** Ex.
;	ADJ
;        |
;	chunk-rules
;	 	|
;	 	|--demons-------|
;		|		|---after-------|
;		|				|-- <NOUN(agree), NBAR-DEF>
;		|
;		|--indef--------|
;		|		|--follows------|
;		|		  (adj qualif*) |-- <NOUN(agree), NBAR-QUANTIF>
;		|
;	 	|---qualif------|
;		 		|---before------|
;		 		|		|-- <ADV(compar), ADVBMOD-COMPAR>
;		 		|		|-- <ADV(quant), ADVBMOD-QUANT>
;		 		|
;		 		|---after-------|
;		 				|-- <CONJ(compar), COORD-COMPAR>
;		

;**********************************************************************
; ****** READ INPUT DATA AND CONVERSION IN INTERNAL FORMAT
;**********************************************************************
; ***  It parses the sentences present in one or more files
; ***  THIS FUNCTION IS NOT USED ANY MORE
; (defun chunk-parser ()
;  (let (answ xinput xoutpt tempx filelist)
;     (format t " ***** CHUNK PARSER ******************** ~% ~%")
;     (format t " Do you want to carry out the work on all the files of the tagged corpus? (y/n)~%")
;     (setq answ (checkanswer '(y n)))
;     (cond ((eq answ 'n)
; 	    (format t "~% Name of the input file containing the automatic data (between quotation marks) ~%")
; 	    (format t "     Home Directory: *HOME-DIR* ~%")
; 	    (setq xinput (build-file-name (read)))
;;  *** merge-pathnames is a system function
; 	    (setq xoutpt (build-file-name (change-extens tempx ".prs")))
; 	    (parse-single-f xinput xoutpt))
; 	  (t (with-open-file (iport "/gull/GULL/CORPUS/tagcorpus.dat"
; 				:direction :input :if-does-not-exist :error)
; 	    (setq filelist (read iport))
; 	    (dolist (filen filelist)
;                 (format t " ~%  $$$$$$$$$$$$$ Next file: ~a $$$$$$$ ~%" filen)
; 		(parse-single-f (build-file-name (change-extens filen ".tb"))
; 				(build-file-name (change-extens filen ".prs")))))))))
 
;;**********************************************************************
;; ***  It parses the sentences present in a single file
;;      It reads the file and stores the lines in 'buff' and 'data'in readable format
;; ***  buff is organized as a list including all the lines; also non-data lines
;;       are included; buff is used just for data output
;; ***  data is organized as a two-dimensional array. Each line is associated with
;;	a sentence, whose columns are the words. Non-data lines are skipped
;;      Then the content of 'data' is parsed and the result is written on the
;;	output file
;;   tbf is the name of the tb (input) file
;;   parsedf is the name of the result (output) file
;;   casefrf is the name of the file continaing the verbal dependents
; ***  THIS FUNCTION IS USED ONLY IN TULE, SINCE THE ACCESS TO THE PARSER IS MADE DIRECTLY
;      VIA "PARSE-SENTENCES" IN HOPS
(defun parse-single-f (tbf parsedf casefrf &optional extended-labels)
  (declare (special *SYSTEM-CONTEXT*))
  (with-open-file (csfport casefrf :direction :output
                                   :if-exists :overwrite
                                   :if-does-not-exist :create)
    (with-open-file (tbport tbf :direction :input
                                    :if-does-not-exist :error)
     (with-open-file (parsedport parsedf :direction :output
                                    :if-exists :overwrite
                                    :if-does-not-exist :create)
      (let (buff data sent iline (init-time (get-internal-run-time)) 
 	   nowri-time tot-time parseresult (begfile t) expline)
         (do ((line (read-line tbport nil #\Escape)
                    (read-line tbport nil #\Escape)))
             ((equal line #\Escape)
 	       (setq data (reverse (mapcar #'reverse (cons sent data)))))
 ; *** process lines ***********
           (cond ((and begfile (not (equal line "")))
                   (setq expline (explode line))
      ; *** the sequence (239 187 191) is a marker of the Utf-8 encoding
                   (cond ((and (eq (char-code (first expline)) 239)
                               (eq (char-code (second expline)) 187)
                               (eq (char-code (third expline)) 191))
                            (setq line (string (implode (rest (rest (rest expline))))))
                            (setq begfile nil)))))
           (cond ((or (string= (string-trim '(#\Space #\Tab #\Return) line) "")
                      (same-chars? (read-from-string line) #\?))
 		   (setq buff (cons line buff)))
 	      	((is-sentence-heading line nil)
 ; *** if sent=nil, this is the first sentence: go ahead
                   (cond ((memq *SYSTEM-CONTEXT* '(tule legal tocai-test atlas))
                            (setq buff (cons line buff))))
 		   (cond ((not (null sent))
 		   	    (setq data (cons sent data))
 		   	    (setq sent nil))))
 ; *** the line in data is a regular data line: convert it in readable format
 ;     interp-newtb-line in "MORPHO/tb-functions"
 ; *** the 'cond' is needed to work on the 'man' file, which include traces that
 ;     must not appear among the data to analyse (traces must be added by the
 ;     parser, not found in the input text!)
                 (t (setq iline (interp-newtb-line line))
                    (cond ((not (is-a-newtb-trace? iline))
                            (setq sent (cons iline sent))
                            (setq buff (cons iline buff)))))))
 ; *** parseresult is a list including two sublists;
;      the first one are the original data with the possible insertion of traces
;      the second one is an array of labels, parallel to the first list
        (setq parseresult (start-parse-sentences data csfport))
   ; (format t "after parsing; data: ~%~a~% links: ~%~a~% buff: ~%~s~%"
   ;            (first parseresult) (second parseresult) buff)
   ; (break "")
 	(setq nowri-time (- (get-internal-run-time) init-time))
        (write-parsed-file (first parseresult) (second parseresult)
 				 (reverse buff) parsedport)
 	(setq tot-time (- (get-internal-run-time) init-time))
 	(format t "Parsing timing:~%  Actual parse: ~s~%  File output: ~s~%      TOTAL-TIME: ~s~%~%"
 	    (print-time nowri-time)
 	    (print-time (- tot-time nowri-time))
 	    (print-time tot-time))
        parseresult)))))

;**********************************************************************
; ****** MAIN PARSE FUNCTION AND APPLICATION OF NON-VERBAL RULES ******
;**********************************************************************
; *** the next is used to introduce the global *PARSE-CONTEXT*
(defun start-parse-sentences (data &optional (csfport nil))
    (setq *PARSE-CONTEXT* '(sentence 0))
    (parse-sentences data csfport))

;**********************************************************************
; *** It parses the sentences present in a single file
; *** In case *TREE-FORMAT* = AVM "Data" has the following format:
;     ((wordsent1 wordsent2 ... wordsentn) (linksent1 linksent2 ... linksentn))
;     each wordsenti is the list of items of the i-th sentence, and the 
;     linksenti are the corresponding links to parents.
;     The form of wordsenti (a sentence) is:
;     (word1 word2 ... wordn), and each word is represented as:
;     ((posit .) (form .) (syn ((lemma .) (cat .) ...)) (sem (...)))
; *** it returns a pair, whose first element are the input data with the possible
;     insertion of traces, and the second element are the corresponding links
(defun parse-sentences (data csfport)
 (declare (special *SYSTEM-CONTEXT* *TREE-FORMAT* *PARSE-CONTEXT*))
 (let* (links tempres finalres
        (nomarker-data (cond ((memq *SYSTEM-CONTEXT* '(tule legal tocai-test atlas))
                               (remove-markers data))
                             (t data)))
        (marker-list (cond ((memq *SYSTEM-CONTEXT* '(tule legal tocai-test atlas))
                              (second nomarker-data))
                           (t nil)))
        (nopar-data-and-par (cond ((memq *SYSTEM-CONTEXT* '(tule legal tocai-test atlas))
                                     (remove-parentheses (first nomarker-data)))
                                  (t nil)))
        (nopar-data (cond ((memq *SYSTEM-CONTEXT* '(tule legal tocai-test atlas))
                             (first nopar-data-and-par))
                          (t nomarker-data))))
;	numel1 numel2 numel3 numel4 numel5 numel6 numunl1 numunl2 numunl3
;	numunl4 numunl5 numunl6 tab
; *** The following function builds an array of lists of NIL's, isomorphic to 'data'
       (setq links (mult-makeli nopar-data nil))
; *** in some cases prep-art (dei, delle, ...) are indef articles. We try here to
;     identify at least some of them (in the ATLAS environment)
;      (multiple-value-setq (numel1 numunl1) (count-unlinked links))
; *** the locutions are identified and the suitable links set up
      (setq links (set-locution-links nopar-data links))	; ********* SET-LOCUTION-LINKS
      (check-link-triples links 'locutions-1)
; *** application of local parsing rules (file parserules)
;      (multiple-value-setq (numel1 numunl1) (count-unlinked links))
  (setq *PRINT-LEVEL* nil)
  (setq *PRINT-LENGTH* nil)
      ; (format t "dati: ~a~% links: ~a~%" nopar-data links)
      ; (break "After locution links")
      (setq links (apply-loc-parserules nopar-data links))	; ********* CHUNKING RULES
      ; (format t "links: ~a~%" (first links))
      ; (break "After local parse rules")
      (check-link-triples links 'chunkrules)
; *** it may happen that the tag rules introduce a loop; the next removes them
      (multiple-value-setq (nopar-data links)
                  (remove-loops nopar-data links))		; ********* REMOVE LOOPS
; *** check if the parenthesis includes an item list, i.e. a sequence of items
;     separated by commas. In such a case, take them as a sequence of coordinates
      (setq links (find-item-list nopar-data links))		; ********* ITEM LISTS
     ; (format t "links: ~a~%" (first links))
     ; (break "After find item list")
      (check-link-triples links 'item-list)
; *** check for unattached auxiliaries; this could happen if a verb was
;     tagged as AUX, but a following past participle was then tagged as ADJ
      (multiple-value-setq (nopar-data links)
                  (adjust-unattached-aux nopar-data links))	; ********* UNATTACHED AUXILIARIES
; *** insert a verbal trace for a special legal construct 
      (multiple-value-setq (nopar-data links)
                  (add-verbal-traces nopar-data links))		; ********* SPECIAL VERBAL TRACE
; *** check and possible adjustement for prepositional links
;      (multiple-value-setq (numel2 numunl2) (count-unlinked links))
      (setq tempres (move-locution-links (list nopar-data links))) ; ********* LOCUTION LINKS
      (setq nopar-data (first tempres))
      (setq links (second tempres))
      (check-link-triples links 'locutions-2)
     ; (format t "Links: ~a~%" links)
     ; (break "after move locution links")
      (setq links (check-missing-links nopar-data links))	; ********* MISSING LINKS
      (check-link-triples links 'prepositions-1)
; *** analysis of conjunctions ********************
;      (multiple-value-setq (numel2 numunl2) (count-unlinked links))
     ;  (format t "Links: ~a~%" links)
     ;  (break "after check missing links")
      (setq links (link-conjunctions nopar-data links))		; ********* CONJUNCTIONS
      (check-link-triples links 'conjunctions)
;      (multiple-value-setq (numel3 numunl3) (count-unlinked links))
; *** analysis of verbal complements ***************
;     The function 'find-v-complements' also inserts some traces, so that not
;     only the links can be modified, but also the original 'data' can include
;     some extra lines. The result is a pair <new-data-lines, new-links>
     ;     (format t "Data: ~a~%" nopar-data)
     ;     (format t "Links: ~a~%" links)
     ;     (break "After conjunctions")
       (multiple-value-setq (nopar-data links)
		 (find-v-complements nopar-data links csfport))	; ********* VERBAL FRAMES
      (check-link-triples links 'verbal-rules)
;      (multiple-value-setq (numel4 numunl4) (count-unlinked links))
     ;     (format t "Links: ~a~%" links)
     ;     (break "After verbal analysis")
; *** Post-processing: try to solve unattached words 
;     Meanwhile, assignment of sentence head 
      (multiple-value-setq (nopar-data links)
           (attach-unlinked nopar-data links))			; ********* UNATTACHED WORDS
      (check-link-triples links 'post-unlinked)
     ;      (format t "Links: ~a~%" links)
     ;      (break "After Attach Unlinked")
;      (multiple-value-setq (numel5 numunl5) (count-unlinked links))
    ; *** Post-processing 1: remove implausible PP-attachments 
      (setq links (move-prepositions nopar-data links))
      (check-link-triples links 'prepositions-2)
     ;    (format t "Lines: ~a~%" nopar-data)
     ;    (format t "Links: ~a~%" links)
     ;    (break "after move-prepositions")
; *** analyse and re-insert parenthesized material
      (cond ((memq *SYSTEM-CONTEXT* '(tule legal tocai-test atlas))
               (setq finalres 
                 (add-parentheses nopar-data links (second nopar-data-and-par) csfport)))
            (t (setq finalres (list nopar-data links))))
     ;     (format t "result: ~a~%" finalres)
     ;     (break "after add-parentheses")
    ; *** Post-processing 2: move locution links *************
    ; *** finalres is a pair of data and links
    ;     now, all links to the end of a locution are moved to the beginning of that
    ;     locution
      (check-link-triples (second finalres) 'add-parentheses)
      (setq finalres (move-locution-links finalres))
     ;       (format t "result: ~a~%" finalres)
     ;       (break "after move-locution-links")
      (check-link-triples (second finalres) 'locutions-2)
    ; *** Post-processing 3: insert some top traces (for the ATLAS project) **
      (cond ((eq *SYSTEM-CONTEXT* 'atlas)
               (setq finalres 
                  (insert-top-trace (first finalres) (second finalres)))))
      (check-link-triples (second finalres) 'top-traces)
    ; *** the function "insert-top-trace" is in the file "insert-top-traces.lisp"
     ;        (format t "result: ~a~%" finalres)
     ;        (break "after insert-top-trace")
      (cond ((memq *SYSTEM-CONTEXT* '(tule legal tocai-test atlas))
               (setq finalres 
                  (add-markers (first finalres) (second finalres) marker-list))))
     ;     (format t "result: ~a~%" finalres)
     ;     (break "after add-markers")
      (check-link-triples (second finalres) 'add-markers)
    ; *** Post-processing 4: connect possibly disconnected pieces of tree ****
    ; *** finalres is a pair of data and links
       (cond ((memq *SYSTEM-CONTEXT* '(tule legal tocai-test atlas))
                (setq finalres 
                   (force-tree-connection (first finalres) (second finalres)))))
     ;       (format t "result: ~a~%" finalres)
     ;       (break "after force Tree connection")
;      (multiple-value-setq (numel6 numunl6) (count-unlinked (second finalres)))
;      (format t " *** ATTACHED ITEM COUNT ***~%")
;    (cond ((eq (first *PARSE-CONTEXT*) 'sentence)
;	      (setq tab " "))
;	    (t (setq tab "    ")))
;      (format t "~aInitial items: Total: ~a; Unlinked ~a~%" tab numel1 numunl1)
;      (format t "~aParserules:    Total: ~a; Linked ~a~%" 
;		tab numel2 (- numunl1 numunl2))
;      (format t "~aConjunctions:  Total: ~a; Linked ~a~%" 
;		tab numel3 (- numunl2 numunl3))
;      (format t "~aVerbal:        Total: ~a; Linked ~a~%" 
;		tab numel4 (- numunl3 numunl4))
;      (format t "~aFinal:         Total: ~a; Linked ~a~%" 
;		tab numel5 (- numunl4 numunl5))
;      (format t "~aComplete:      Total: ~a; Linked ~a~%" 
;		tab numel6 (- numunl5 numunl6))
   ; *** finalres is the result in the form that depends on the variable
   ;     *TREE-FORMAT*
   ;     If the format is tut, then the result is printed on file, otherwise
   ;     it is used for subsequent processing. In the first case, the character
   ;     encoding is adjusted in the printing functions. In the second case, it
   ;     is done here; in avm (but note that this is the "flat" avm representation,
   ;     see avm-transf.lisp for more details), the format is
   ;     ((wordsent1 wordsent2 ... wordsentn) (linksent1 linksent2 ... linksentn))
   ;     each wordsenti is the list of items of the i-th sentence, and the 
   ;     linksenti are the corresponding links to parents.
   ;     The form of wordsenti (a sentence) is:
   ;     (word1 word2 ... wordn), and each word is represented as:
   ;     ((posit .) (form .) (syn ((lemma .) (cat .) ...)) (sem (...)))
  ; (break "finalres")
      ;(cond ((eq *TREE-FORMAT* 'tut) finalres)
      ;      (t (adjust-lemma-encoding finalres)))
     finalres
	))

;**********************************************************************
(defun check-link-triples (links source)
  (let ((countsent 0) (countline 0))
   (dolist (nxtsentlinks links)
      (setq countline 0)
      (setq countsent (1+ countsent))
      (dolist (nxtlink nxtsentlinks)
          (setq countline (1+ countline))
          (cond ((not (or (null nxtlink)
                          (= 3 (length nxtlink))))
                   (format t "Link without source: Sent.num.: ~a; line numb: ~a; link: ~a~%"
                            countsent countline nxtlink)
                   (format t "   after the execution of ~a~%" source)
                   (break "")))))))
       

;**********************************************************************
(defun adjust-lemma-encoding (finalres)
  ;(break "lemma encoding")
  (let ((data (first finalres))
        (links (second finalres))
        finaldata newsent)
    ; *** loop on all sentences
      (dolist (nxtsent data (list (reverse finaldata) links))
        (setq newsent nil)
        (setq finaldata
           (cons 
    ; *** loop on all words
              (dolist (nxtword nxtsent (reverse newsent))
                (cond ((characterp (get-synt-word nxtword)) 
                         (setq newsent (cons nxtword newsent)))
                      (t (setq newsent
                           (cons 
                             (set-synt-lemma nxtword 
                                 (convert-base-to-currscheme-uppercase
                                       (get-synt-word nxtword)))
                             newsent)))))
              finaldata)))))

;**********************************************************************
; *** checks if the last application of the chunk rules has produced
;     any loop; in such a case, it cuts the chain somewhere
(defun remove-loops (data links)
 (let (newdata newlinks)
   ; *** loops on all sentences
   (do ((cursent (first data) (first remsents))
        (remsents (rest data) (rest remsents))
        (cursentlinks (first links) (first remsentlinks))
        (remsentlinks (rest links) (rest remsentlinks)))
       ((and (null cursent) (null remsents))
          (values (reverse newdata) (reverse newlinks)))
      ; *** loops on all lines
       (do* ((prevlines nil (cons curline prevlines))
             (curline (first cursent) (first remlines))
	     (remlines (rest cursent) (rest remlines))
	     (prevlinks nil (cons curlink prevlinks))
             (curlink (first cursentlinks) (first remlinks))
	     (remlinks (rest cursentlinks) (rest remlinks)))
	   ((null curline) 
	     (setq newdata (cons (reverse prevlines) newdata))
	     (setq newlinks (cons (reverse prevlinks) newlinks)))
           (cond ((and (not (null curlink))
                       (check-loop curline curlink 
                             (append (reverse prevlines) (list curline) remlines)
                             (append (reverse prevlinks) (list curlink) remlinks)))
      ; *** if a loop is detected, the first link of the loop is cancelled
                    (setq curlink nil)))))))

;**********************************************************************
(defun adjust-unattached-aux (data links)
 (let (newdata newlinks)
   ; *** loops on all sentences
   (do ((cursent (first data) (first remsents))
        (remsents (rest data) (rest remsents))
        (cursentlinks (first links) (first remsentlinks))
        (remsentlinks (rest links) (rest remsentlinks)))
       ((and (null cursent) (null remsents))
          (values (reverse newdata) (reverse newlinks)))
      ; *** loops on all lines
       (do* ((prevlines nil (cons curline prevlines))
             (curline (first cursent) (first remlines))
	     (remlines (rest cursent) (rest remlines))
             (prevlinks nil (cons curlink prevlinks))
             (curlink (first cursentlinks) (first remlinks))
	     (remlinks (rest cursentlinks) (rest remlinks)))
	   ((null curline) 
	     (setq newdata (cons (reverse prevlines) newdata))
	     (setq newlinks (cons (reverse prevlinks) newlinks)))
           (cond ((and (null curlink)
                       (eq (get-synt-categ curline) 'VERB)
                       (eq (get-synt-type curline) 'AUX))
                   (let ((near-verb (find-near-verb remlines remlinks)))
                ;      (format t " Remlines: ~a~%" remlines)
                ;      (break "untattached aux")
                      (cond ((null near-verb) 
                              (setq curline (change-synt-type curline 'MAIN)))
                            (t (setq curlink 
                                  (make-link (get-synt-numb near-verb) 
                                             'AUX 'unattached-aux)))))))))))

;**********************************************************************
; *** looks for a verb in the next few lines; the items that can be crossed are
;     adverbs, quotation marks
(defun find-near-verb (lines links)
  (let ((linecateg (get-synt-categ (first lines))))
   (cond ((null lines) nil)
         ((eq linecateg 'VERB) (first lines))
         ((memq (get-synt-word (first lines)) '(#\" #\'))
            (find-near-verb (rest lines) (rest links)))
         ((eq linecateg 'ADV)
            (find-near-verb (rest lines) (rest links)))
         (t nil))))

;**********************************************************************
(defun check-loop (startline startlink allines allinks)
  (let ((startnumb (get-synt-numb startline)))
   ;(break " in check-loop")
     (do* ((upline-link 
              (list startline startlink)
              (find-a-line `(position ,(first uplink)) allines allinks))
           (upline (first upline-link) (first upline-link))
           (uplink (second upline-link) (second upline-link))
           (seenlines (list startnumb) (cons (get-synt-numb upline) seenlines)))
           ; *** stop the search when the path upward is interrupted or when 
           ;    it has come back to startline 
         ((or (null uplink)
              (eq 0 (first uplink))
              (member (first uplink) seenlines))
   ; *** the search is stopped either when an empty pointer is detected
   ;     or when the pointer is to one line already encountered (loop)
   ; *** however the function returns true just in case the loop closes to
   ;     the starting line. In other words, for (a b c d e b) it returns
   ;     false. This is because the pointer that is removed in the calling
   ;     function is the one associated with the first item (a), which is
   ;     not the case if the loops closes to b. The correct link (from b to
   ;     c will be removed anyway in one of the next calls)
           (eq (first uplink) startnumb))
   ;     (format t "Check-loop: upline = ~a~%  uplink = ~a~% seenlines = ~a~%" upline uplink seenlines)
   ;     (break "")
        )))

;**********************************************************************
(defun remove-markers (data)
; *** for all sentences in 'data'
 (let (newdata marker-set)
   (do* ((prevsents nil (cons cursent prevsents)) 
         (cursent (first data) (first remsents))
	 (remsents (rest data) (rest remsents)))
	((null cursent) (list newdata marker-set))
      (let (marker-onesent skiplines)
; *** for all lines of the next sentence
    	(do* ((prevlines nil (cond (skip prevlines)
				   (t (cons curline prevlines))))
; *** after a skip, curline is the open marker, which must not be attached
;     to 'prevlines'
	      (curline (first cursent) (first remlines))
	      (remlines (rest cursent) (rest remlines))
	      (skip nil nil))
	     ((null curline) 
		(setq newdata (cons (reverse prevlines) newdata))
		(setq marker-set (cons marker-onesent marker-set)))
; *** if the next line is the < symbol, and the category is MARKER, then skip
	  (let ((nxtword (get-synt-word curline)))
	      (cond ((and (memq nxtword '(#\< <))
                          (eq (get-synt-categ curline) 'marker))
		      (multiple-value-setq
			      (remlines skiplines)
			      (rem-single-par remlines 
				      (list nxtword) (list curline)))
		      (setq marker-onesent (append1 marker-onesent skiplines))
	 	      (setq skip t)))))))))

;**********************************************************************
(defun add-markers (data links markers)
; *** INPUT:
;  ---> data: the data lines (excluding markers)
;  ---> links: the corresponding links
;  ---> markers: the lines of the markers. It is composed of lists
;       (one for each sentence). Inside each such list there is one sublist for
;	each pair of parentheses.
; *** for all sentences in 'data'
  (let (newdata newlinks)
   (do* ((cursent (first data) (first data))
	 (data (rest data) (rest data))
	 (cursentlinks (first links) (first links))
	 (links (rest links) (rest links))
	 (curmark (first markers) (first markers))
	 (markers (rest markers) (rest markers)))
	((and (null cursent) (null curmark)) (list newdata newlinks))
; *** for all lines of the next sentence
    	(do* ((prevlines nil (cond (insert prevlines)
				   (t (cons curline prevlines))))
; *** after an insertion, curline is the line after the parenthesis. 
;     No movement ahead
	      (curline (first cursent) (cond (insert curline)
					     (t (first cursent))))
	      (cursent (rest cursent) (cond (insert cursent)
					     (t (rest cursent))))
	      (prevlinks nil (cond (insert prevlinks)
				   (t (cons curlink prevlinks))))
	      (curlink (first cursentlinks) (cond (insert curlink)
					          (t (first cursentlinks))))
	      (cursentlinks (rest cursentlinks) (cond (insert cursentlinks)
					     	      (t (rest cursentlinks))))
	      (insert nil nil))
; *** curmark includes all the markers of the current sentence
	     ((null curmark)
   ; *** if no more markers, end processing of sentence, by attaching to the
   ;     lines and links buffers all remaining data
		(cond ((null curline)
			(setq newdata (cons (reverse prevlines) newdata))
			(setq newlinks (cons (reverse prevlinks) newlinks)))
		      (t (setq newdata 
		      	   (cons
			      (append 
				 (reverse prevlines) (list curline) cursent)
			      newdata))
			 (setq newlinks 
		      	     (cons
			       (append 
				  (reverse prevlinks) (list curlink) cursentlinks)
			       newlinks)))))
; *** if there are no more data lines, or
;     if the index of the marker precedes the index of the data line, then
;     it's time to insert the data concerning the marker
	     (cond ((or (null curline)
			(index-precedes (get-synt-numb (first (first curmark)))
				    (get-synt-numb curline)))
; *** extend prevlines and prevlinks with the data and links of the marker
			 (setq prevlines 
                               (append (reverse (first curmark)) prevlines))
			 (setq prevlinks 
                               (append (makeli (length (first curmark)) nil) 
                                       prevlinks))
		         (setq curmark (rest curmark))
		         (setq insert t)))))))

;**********************************************************************
(defun remove-parentheses (data)
; *** for all sentences in 'data'
 (let (newdata par-set)
   (do* ((prevsents nil (cons cursent prevsents)) 
         (cursent (first data) (first remsents))
	 (remsents (rest data) (rest remsents)))
	((null cursent) (list newdata par-set))
      (let (par-onesent skiplines)
; *** for all lines of the next sentence
    	(do* ((prevlines nil (cond (skip prevlines)
				   (t (cons curline prevlines))))
; *** after a skip, curline is the open parenthesis, which must not be attached
;     to 'prevlines'
	      (curline (first cursent) (first remlines))
	      (remlines (rest cursent) (rest remlines))
	      (skip nil nil))
	     ((null curline) 
		(setq newdata (cons (reverse prevlines) newdata))
		(setq par-set (cons par-onesent par-set)))
; *** if the next line is an open parenthesis, then skip
	  (let ((nxtword (get-synt-word curline)))
	      (cond ((or (member nxtword '(#\( #\[))	; )
      ; *** the next is for treating as a parenthesis a quoted text, in case it is referred
      ;     to with the term "word", as in 'the words "in this case" are not relevant1
                         (and (eq nxtword #\")			;"
                              (has-gramm-type (get-synt-word (first prevlines)) '&word-ref)))
		      (multiple-value-setq
			      (remlines skiplines)
			      (rem-single-par remlines 
				      (list nxtword) (list curline)))
		      (setq par-onesent (append1 par-onesent skiplines))
	 	      (setq skip t)))))))))

;**********************************************************************
; *** removes all lines until the closed parenthesis coupled with the one
;     passed initially in par-stack
(defun rem-single-par (lines par-stack skipped-lines)
  (cond ((or (null lines) (null par-stack))
	   (values lines skipped-lines))
	((equal (dual-par (first par-stack)) (get-synt-word (first lines)))
	   (rem-single-par 
		(rest lines) (rest par-stack) 
		(append1 skipped-lines (first lines))))
; *** if more than one level of nested equal parentheses, store them into 
;     par-stack, to get the correct match
	((equal (first par-stack) (get-synt-word (first lines)))
	   (rem-single-par (rest lines)
			 (cons (get-synt-word (first lines)) par-stack)
			 (append1 skipped-lines (first lines))))
	(t (rem-single-par (rest lines) par-stack 
			(append1 skipped-lines (first lines))))))

;**********************************************************************
(defun add-parentheses (data links par-data csfport)
; *** INPUT:
;  ---> data: the data lines (excluding parentheses)
;  ---> links: the corresponding links
;  ---> par-data: the lines included in parentheses. It is composed of lists
;       (one for each sentence). Inside each such list there is one sublist for
;	each pair of parentheses.
; *** for all sentences in 'data'
  (declare (special *TREE-FORMAT* par-level))
  (let (newdata newlinks actprev curcat curnumb external-link par-level newroot newlink
        par-result par-lines par-links par-head par-head-cat curlinumb)
   (do* ((cursent (first data) (first data))
	 (data (rest data) (rest data))
	 (cursentlinks (first links) (first links))
	 (links (rest links) (rest links))
	 (curpar (first par-data) (first par-data))
	 (par-data (rest par-data) (rest par-data)))
	((and (null cursent) (null curpar)) (list newdata newlinks))
      (setq par-result nil)
      (cond ((or (null cursent)
                 (and (eq 1 (length cursent))
                      (characterp (get-synt-word (first cursent)))
                      (not (null (rest (butlast (first curpar)))))))
; *** Special case: the outside sentence is empty *******************************
; *** or it includes just a single character (that is, by default, assumed to ***
; *** follow the parenthesis)
	       (setq *PARSE-CONTEXT* (list 'parenth (1+ (second *PARSE-CONTEXT*))))
	       (setq par-result 
                   (parse-sentences (list (rest (butlast (first curpar)))) csfport))
     ; *** curpar is a pair <lines links>. First takes the lines; rest-butlast
     ;     excludes the parentheses
     ; *** par-result is a pair <lines-inside-the-parentheses, their-links>
     ;     both of them are included in a second level of parentheses for compatiblity with 
     ;     parse-sentences, which in general works on a list of sentences
	       (setq par-lines (first (first par-result)))
	       (setq par-links (first (second par-result)))
               (setq par-head
	           (first (find-a-line 
                              '(linked-to 0 not-word #\( not-word #\))
                              par-lines par-links)))
	       (setq par-head-cat (get-synt-categ par-head))
	       (setq par-level (1- (second *PARSE-CONTEXT*)))
	       (cond ((= par-level 0)
		        (setq *PARSE-CONTEXT* '(sentence 0)))
		     (t (setq *PARSE-CONTEXT* `(parenth ,par-level))))
               (setq newroot 
                    (cond ((eq *TREE-FORMAT* 'tut)
                             (list '(0 10) '|t| 
                                  '(generic-t verb main allval allval allval) nil 'empty))
                          (t `((posit (0 10))
                               (form #\t)
                               (syn ((lemma generic-t) (cat verb) (type main) (mood allval)
                                     (tense allval) (trans allval)))
                               (sem nil)
                               (coref empty)))))
               (setq newlink (make-link 0 'TOP-VERB 'add-parentheses))
               ; *** insert into par-lines the data concerning the parentheses
	       (setq par-lines
		    (cons (first (first curpar))
			  (append1 par-lines (first (last (first curpar))))))
               ; *** insert into par-links the links concerning the parentheses
	       (setq par-links 
		    (cons (make-link '(0 10) 'OPEN+PARENTHETICAL 'add-parentheses)
			  (append1 par-links
			       (make-link '(0 10) 'CLOSE+PARENTHETICAL 'add-parentheses))))
               (setq par-links (change-parlinks par-links '(0 10)))
               (cond ((or (null cursent)
		          (index-precedes (get-synt-numb (first (first curpar)))
			                  (get-synt-numb (first cursent))))
			(setq newdata 
                           (cons (append (cons newroot par-lines) cursent) newdata))
			(setq newlinks 
                           (cons (append1 (cons newlink par-links) 
                                          (cond ((null cursentlinks) nil)
                                                (t '((0 10) END add-parenheses))))
                                 newlinks)))
		     (t (setq newdata 
                          (cons (append cursent (cons newroot par-lines))
                                newdata))
			(setq newlinks 
                          (cons (cons '((0 10) 'INITIATOR 'add-parentheses)
                                      (cons newlink par-links))
                                newlinks))))
                 ;(format t "newdata: ~a~% newlinks: ~a~%" newdata newlinks)
                 ;(break "")
                   )
; *** Standard case: the outside sentence is not empty *******************************
; *** for all lines of the next sentence
    (t (do* ((prevlines nil (cond (insert prevlines)
				   (t (cons curline prevlines))))
; *** after an insertion, curline is the line after the parenthesis. 
;     No movement ahead
	      (curline (first cursent) (cond (insert curline)
					     (t (first cursent))))
	      (cursent (rest cursent) (cond (insert cursent)
					     (t (rest cursent))))
	      (prevlinks nil (cond (insert prevlinks)
				   (t (cons curlink prevlinks))))
	      (curlink (first cursentlinks) (cond (insert curlink)
					          (t (first cursentlinks))))
	      (cursentlinks (rest cursentlinks) (cond (insert cursentlinks)
					     	      (t (rest cursentlinks))))
	      (insert nil nil))
; *** curpar includes all the parenthesized items of the current sentence
	     ((null curpar)
		(cond ((null curline)
			(setq newdata (cons (reverse prevlines) newdata))
			(setq newlinks (cons (reverse prevlinks) newlinks)))
		      (t (setq newdata 
		      	   (cons
			      (append 
				 (reverse prevlines) (list curline) cursent)
			      newdata))
			 (setq newlinks 
		      	     (cons
			       (append 
				  (reverse prevlinks) (list curlink) cursentlinks)
			       newlinks)))))
; *** if there are no more data lines, or
;     if the index of the parenthesis precedes the index of the data line, then
;     it's time to insert the data concerning the parenthesis
             (setq par-result nil)
             (setq curlinumb (get-synt-numb curline))
	     (cond ((or (null curline)
			(index-precedes (get-synt-numb (first (first curpar))) curlinumb))
; *** first, we determine the attachment point of the whole parenthesis
		     (let* (par-attach
; *** then, we remove the pair of parenthesis
			    (par-content (rest (butlast (first curpar))))
                            changes)
; *** analyse the content
			 (setq *PARSE-CONTEXT* (list 'parenth
						     (1+ (second *PARSE-CONTEXT*))))
                         (cond ((and 
                                  (eq (get-synt-categ (first par-content)) 'NOUN)
                                  (eq (get-synt-type (first par-content)) 'PROPER))
   ; *** if the parenthis begins with a name, it could be a 
   ;     citation, as [Lesmo et al. 92]
                                 (setq actprev (line-not-trace prevlines))
                                 (cond ((null actprev)
                                         (setq curnumb 0))
                                       (t (setq curcat (get-synt-categ actprev))
                                          (setq curnumb (get-synt-numb actprev))))
                                 (setq external-link
                                    (list curnumb
                                        (case curcat
                                           (verb 'VERB-OBJ)
                                           (prep 'PREP-ARG)
                                           (conj 'COORD2ND+BASE)
                                           (otherwise 'APPOSITION))
                                         'citation))
			         (setq par-result 
                                     (parse-citation par-content external-link))))
   ; *** if the parenthesis is at the beginning of the sentence and it contains just
   ;     a number, then it is taken as a numbering
                         (cond ((and (eq (get-synt-categ (first par-content)) 'NUM)
                                    (null (second par-content))
			            (eq 1 (get-synt-numb (first (first curpar)))))
                                 (setq par-result 
                                     (list
                                         (list (get-synt-numb
                                                  (first (find-a-line '(head) 
                                                             (cons curline cursent)
                                                             (cons curlink cursentlinks))))
                                               'NUM-RMOD-LISTPOS
                                               'add-parentheses)))))
   ; *** if the citation analysis fails, standard analysis
            ; (format t "Add parentheses: par-level= ~a~% par-result ~a~%" *PARSE-CONTEXT* par-result)
            ; (break "")
                         (cond ((null par-result)
			         (setq par-result 
                                      (parse-sentences (list par-content) csfport)))
                               (t (setq par-result
                                      (list (list par-content) (list par-result)))))
; *** par-result is a pair <lines-inside-the-parentheses, their-links>
;     both elements are included in a second level of parentheses for
;     compatiblity with parse-sentences, which in general works on a list of
;     sentences
; *** neither par-lines nor par-links include the data about the surrounding parentheses
            ; (format t "Add parentheses: par-level= ~a~% par-result ~a~%" *PARSE-CONTEXT* par-result)
            ; (break "")
			 (setq par-lines (first (first par-result)))
			 (setq par-links (first (second par-result)))
                         (setq par-head
			     (first (find-a-line 
                                         '(linked-to 0 not-word #\( not-word #\))
                                         par-lines par-links)))
			 (setq par-head-cat (get-synt-categ par-head))
			 (setq par-level (1- (second *PARSE-CONTEXT*)))
			 (cond ((= par-level 0)
			 	  (setq *PARSE-CONTEXT* '(sentence 0)))
			       (t (setq *PARSE-CONTEXT* `(parenth ,par-level))))
; ***********************************************
;   looking for the attachment point of the parenthesis; this depends on the root
;   of the tree inside the parenthesis (par-head)
                         (cond ((eq #\" (get-synt-word (first (first curpar))))	;"
                                  (setq par-attach (get-synt-numb (first prevlines)))
				  (setq changes 
                                        (list 1 (get-synt-numb par-head)
					        (make-link par-attach 'APPOSITION 'add-parentheses)))
                                  (setq par-links
                                     (change-parlinks par-links par-attach)))
; **** (SIC) ************************************
; *** if the par-head is the word 'sic', the parenthesis must be attached to the
;     immediately preceding word, if any, or to the subsequent word, if any
;     Notice that in double parentheses (as "((sic))")
                               ((eq (get-synt-word par-head) 'sic)
                                 (cond ((not (null prevlinks))
                                         (setq par-attach
                                              (get-synt-numb (first prevlines))))
                                       ((not (null curlink))
                                         (setq par-attach 
                                              (get-synt-numb curlink))))
                                 (cond ((not (null par-attach))
                                         (cond ((and (equal #\( (get-synt-word (first par-lines)))
                                                     (equal #\) (get-synt-word (ult par-lines))))
				                 (setq changes
                                                    (list 3 
					               (list (get-synt-numb (first par-lines)) 
                                                           (make-link par-attach
                                                                'OPEN+PARENTHETICAL 'add-parentheses))
					               (list (get-synt-numb par-head)
                                                           (make-link par-attach
                                                                'APPOSITION 'add-parentheses))
					               (list (get-synt-numb (ult par-lines)) 
                                                           (make-link par-attach
                                                                'CLOSE+PARENTHETICAL 'add-parentheses)))))
                                               (t (setq changes
                                                    (list 1 (get-synt-numb par-head)
			                                 (make-link par-attach 
                                                              'APPOSITION 'add-parentheses))))))))
   ; *** the first branch handles ' ... the words "in this case" ... '
   ;     curpar includes also the open and closed parentheses
                               ((and (equal #\( (get-synt-word (first par-lines)))
                                     (equal #\) (get-synt-word (ult par-lines))))
   ; *** this branch handles doubled parentheses: (( ... )), where they should
   ;     be handled exactly as a single one, by duplicating the parentheses
   ;     labels. Now par-lines refers to the inner parenthesis
			         (setq par-attach
				   (get-synt-numb 
				      (first (find-a-line
						 `(head)
						 (append (reverse prevlines)
                                                         (list curline)
                                                         cursent)
                                                 (append (reverse prevlinks)
                                                         (list curlink)
                                                         cursentlinks)))))
				 (setq changes 
                                    (list 1 (get-synt-numb par-head)
					    (make-link par-attach 'APPOSITION 'add-parentheses)))
                                 (setq par-links
                                     (change-parlinks par-links par-attach)))
; *** if it is a conjunction, the conjunction is supposed to be split, with the
;     first part outside the parenthesis, and the second part inside the parenthesis
; ****** COORDINATING CONJUNCTION ***************
			       ((or (and (eq par-head-cat 'CONJ)
				         (eq (get-synt-type par-head) 'COORD))
                                    (eq (get-synt-word par-head) #\&))
				 (let* ((rem 
					   (find-a-line 
					     `(position ,(get-synt-numb par-head))
					     par-lines par-links 'all))
   ; *** rem contains all the lines and links following the head of the parentheses
   ;     (head included)
					secondc conjoined-lines)
   ; *** secondc is the line of the second conjunct (if any)
				    (setq secondc (find-second-conj par-head
							 (rest (first rem)) 
							 (rest (second rem))
							 nil 0))
                                    (cond ((or (null secondc)
                                               (equal secondc '(nil)))
                                            (setq secondc 
                                               (list
                                                 (first 
                                                    (find-a-line 
                                                         `(linked-to ,(get-synt-numb par-head)) 
                                                         par-lines par-links nil))))))
                                    (cond ((or (null secondc)
                                               (equal secondc '(nil)))
                                            (setq secondc 
                                               (list
                                                 (second (first rem))))))
                                    (cond ((or (null secondc) (equal secondc '(nil)))
                                            (format t "~a; ~a~%" par-lines par-links)
                                            (exception 'parse-error "add-parentheses: null secondc")))
				    (setq conjoined-lines 
					(find-first-conj 
                                            par-head secondc prevlines prevlinks
                                            par-lines par-links))
   ; *** conjoined-lines are the two lines that are conjoined
				    (cond ((not (null conjoined-lines))
                                             (setq par-attach (get-synt-numb (first conjoined-lines)))
                                             (setq changes 
                                               (list 2
					         (list
					           (get-synt-numb par-head)
						   (make-link par-attach 
                                                     (get-coord-label 1 par-head) 'add-parentheses))
						 (list
						   (get-synt-numb (second conjoined-lines))
						   (make-link (get-synt-numb par-head)
                                                     (get-coord-label 2 par-head) 'add-parentheses))))))))
; **** PARTICIPLE *******************************
; *** if the par-head is a participle, then it should be handled as
;     a reduced relative clause
			      ((and (eq par-head-cat 'VERB)
				    (eq (get-synt-mood 
					  (find-first-aux 
					    par-head 
					    (reverse 
					     (find-prec-lines par-head par-lines))))
					'PARTICIPLE))
			         (setq par-attach
				   (get-synt-numb 
				      (first (find-a-line
						 `(categ (noun pron) not-trace
			 			   agree 
						     ((gender 
						        ,(get-synt-gender 
								par-head)) 
						      (number 
							,(get-synt-number
								par-head))))
						 prevlines prevlinks))))
				 (setq changes 
                                    (list 1 (get-synt-numb par-head)
					  (make-link par-attach 'VERB-RMOD+RELCL+REDUC 'add-parentheses))))
; **** THE WORD BEFORE THE PARENTHESIS IS A PROPER NAME ****
		     	      ((eq (get-synt-type (first prevlines)) 'PROPER)
                                 (setq par-attach (get-synt-numb (first prevlines)))
				 (setq changes (list 1 (get-synt-numb par-head)
					            (make-link par-attach 'APPOSITION 'add-parentheses))))
; **** THE HEAD OF THE PARENTHESIS IS A PROPER NAME ****
		     	      ((eq (get-synt-type par-head) 'PROPER)
			         (setq par-attach
				   (get-synt-numb 
				      (first (find-a-line
						 `(categ (noun) not-trace)
						 prevlines prevlinks))))
				 (setq changes
                                             (list 1 (get-synt-numb par-head)
					            (make-link par-attach 'APPOSITION 'add-parentheses))))
; *** otherwise, the parenthesis should be attached to the lowest common
;     ancestor of the left and right context of the parenthesis
		     	      (t (setq par-attach 
				  (int-find-up-clust
				    (dropnil (mapcar #'first prevlinks))
				    (dropnil (mapcar #'first 
						    (cons curlink cursentlinks)))
			 	    (first (first curpar)) nil
				    prevlines prevlinks
				    (cons curline cursent) 
				    (cons curlink cursentlinks)))
				  (setq changes 
                                    (list 1 (get-synt-numb par-head)
					    (make-link par-attach 'APPOSITION 'add-parentheses)))))
; *** if no reasonable attachment point found, attach to the previous line, or
;     if at the beginning, to the next line, or to the top
			 (cond ((null par-attach)
				 (cond ((null prevlines)
					 (cond ((null cursent)
						  (setq par-attach 0))
					       (t (setq par-attach
						    (get-synt-numb 
							(first cursent))))))
				       (t (setq par-attach 
					   (get-synt-numb 
                                              (first 
                                                  (find-a-line '(not-trace)
                                                     prevlines prevlinks))))))
                                 (setq changes
                                    (list 1 (get-synt-numb par-head)
					    (make-link par-attach 'APPOSITION 'add-parentheses)))))
; *** now, an attachment point has been found; carry out the changes:
;     code 1 means a single change; code 2 more than one
                         (cond ((= 1 (first changes))
                                 (setq par-links
			             (change-lab 
					 par-lines par-links 
                                         (second changes) (third changes))))
                               (t (setq par-links 
				      (mult-change-labs 
					 par-lines par-links (rest changes) 'add-parentheses t))))
; *** insert into par-lines the data concerning the parentheses
			 (setq par-lines 
			    (cons (first (first curpar))
				  (append1 par-lines
					   (first (last (first curpar))))))
; *** insert into par-links the links concerning the parentheses
			 (setq par-links 
			   (cons (make-link par-attach 'OPEN+PARENTHETICAL 'add-parentheses)
				(append1 par-links
				     (make-link par-attach 'CLOSE+PARENTHETICAL 'add-parentheses))))
; *** extend prevlines and prevlinks with the data and links of the parenthesis
			 (setq prevlines (append (reverse par-lines) prevlines))
			 (setq prevlinks (append (reverse par-links) prevlinks))
		         (setq curpar (rest curpar))
		         (setq insert t))))))))))

;**********************************************************************
; *** conj-pos is 1 or 2, depending on being on the COORD or COORD2ND label
(defun get-coord-label (conj-pos conjline)
  (cond ((= conj-pos 1)
           (cond ((eq (get-synt-word conjline) #\&) 'COORD+BASE)
                 (t (case (get-synt-semtype conjline)
                      (COORD 
                         (cond ((has-gramm-type (get-synt-word conjline) '&both-2)	; sia, che
                                  'COORD+CORRELAT)
                               (t 'COORD+BASE)))	; sometimes also COORD+RANGE, COORD+SYMMETRIC
                      (DISJ 'COORD+BASE)
                      (ADVERS 'COORD+ADVERS)
                      (CONCL 'COORD+EXPLIC)     ; quindi
                      (EXPLIC 'COORD+EXPLIC)
                      (COMPAR 'COORD+COMPAR)
                      (otherwise 'COORD)))))
        ((= conj-pos 2)
           (cond ((eq (get-synt-word conjline) #\&) 'COORD2ND+BASE)
                 (t (case (get-synt-semtype conjline)
                      (COORD 
                         (cond ((has-gramm-type (get-synt-word conjline) '&both-2)	; sia, che
                                  'COORD2ND+CORRELAT)
                               (t 'COORD2ND+BASE)))	; sometimes also COORD+RANGE, COORD+SYMMETRIC
                      (DISJ 'COORD2ND+BASE)
                      (ADVERS 'COORD2ND+ADVERS)
                      (CONCL 'COORD2ND+EXPLIC)     ; quindi
                      (EXPLIC 'COORD2ND+EXPLIC)
                      (COMPAR 'COORD2ND+COMPAR)
                      (otherwise 'COORD2ND)))))))

;**********************************************************************
; *** checks if the data (without parentheses) are a sequence of conjuncts
(defun find-item-list (data links)
 (let (final-links newdata newlinks
       (savelinks links))
  (do ((nxtsent (first data) (first data))
       (data (rest data) (rest data)))
     ((and (null nxtsent) (null data))
        (setq newdata (reverse newdata)))
    (let ((comma-lines 0) 
          (savelines nxtsent)
          lastcomma)
   ; *** it first checks if the sequence of items has the form:
   ;     ..... , x1 x2 ... xN, y1 y2 ... yM
   ; *** then, it replaces the last comma with a conjunction ("and")
   ;     and applies the standard analysis of conjunctions. This
   ;     involves also the function "find-conj-seq", that should
   ;     extract the possible sequence.
   ; *** the result is then compared with the initial links, to see
   ;     if all null links have been solved in this way. If yes,
   ;     the new links are returned, else the original ones are kept
     (do ((nxtline (first nxtsent) (first nxtsent))
          (nxtsent (rest nxtsent) (rest nxtsent)))
         ((null nxtline))
         (cond ((and (eq (get-synt-word nxtline) #\,)
                     (or (neq (get-synt-categ (first nxtsent)) 'CONJ)
                         (neq (get-synt-type (first nxtsent)) 'COORD)))
                 (setq comma-lines (1+ comma-lines))
                 (setq lastcomma (get-synt-linumb nxtline)))))
   ; *** if at least two commas, replace the last one
     (cond ((> comma-lines 1)
             (do* ((nxtline (first nxtsent) (first nxtsent))
                   (nxtsent (rest nxtsent) (rest nxtsent))
                   (linebuff (list nxtline) (cons nxtline linebuff)))
                 ((equal lastcomma (get-synt-linumb nxtline))
                    (setq newdata
                       (cons (append (reverse (rest linebuff))
                                     (cons (make-synt-line lastcomma 'dummy-conj
                                                  'dummy-conj 'conj 'coord 'coord)
     						; *** make-synt-line in MORPHO/tb-functions
;`(,lastcomma dummy-conj (dummy-conj conj coord coord))
                                           nxtsent))
                             newdata)))))
           (t (setq newdata (cons savelines newdata))))))
  ; (format t "Find item list: newdata: ~a~%" newdata)
  ; (break "")
   (setq newlinks (link-conjunctions newdata links))
   ; *** if, for any of the analysed sentences, it holds that lines are now linked, 
   ;     it is a sequence of conjuncts, so include the new links, otherwise maintain
   ;     the original links
  (do ((newsentlinks (first newlinks) (first newlinks))
       (newlinks (rest newlinks) (rest newlinks))
       (oldsentlinks (first savelinks) (first savelinks))
       (savelinks (rest savelinks) (rest savelinks))
       (nxtsent (first data) (first data))
       (data (rest data) (rest data)))
      ((null newsentlinks) (reverse final-links))
   ; *** the next to check if there are more than one occurrence of nil;
   ;     one occurrence is ok, since it is the first element
      (cond ((check-item-list-complete nxtsent newsentlinks)
               (setq final-links (cons newsentlinks final-links)))
            (t (setq final-links (cons oldsentlinks final-links)))))))
  
;**********************************************************************
; *** no more than one item can be left unattached (apart from adverbials,
;     which in some cases are linked afterwards)
(defun check-item-list-complete (lines links)
  (let ((numnil 0))
      (do ((nxtline (first lines) (first lines))
           (lines (rest lines) (rest lines))
           (nxtlink (first links) (first links))
           (links (rest links) (rest links)))
          ((null nxtline)
             (cond ((< numnil 2) t)
                   (t nil)))
          (cond ((and (null nxtlink)
                      (neq (get-synt-categ nxtline) 'ADV)
                      (not (and (eq (get-synt-categ nxtline) 'ADJ)
                                (eq (get-synt-categ (first lines)) 'ART))))
                   (setq numnil (1+ numnil)))))))
  
;**********************************************************************
; *** returns the first line in "lines" that is not a trace
(defun line-not-trace (lines)
  (cond ((null lines) nil)
        ((is-a-synt-trace? (first lines))
           (line-not-trace (rest lines)))
        (t (first lines))))
        
;**********************************************************************
(defun change-parlinks (links attach)
  ; *** the next break, because this function disregards the third item (source)
  ;     of a link, so it is probably never called
  (cond ((null links) nil)
        ((equal 0 (first (first links)))
          (cons (list attach (second (first links)) (third (first links)))
                (change-parlinks (rest links) attach)))
        (t (cons (first links) (change-parlinks (rest links) attach)))))

;**********************************************************************
; *** a citation is simply defined by an automaton:
;     name <, name>* [,] num |
;     name et al. [,] num
; *** it returns a list of links, in case the lines have been recognized as a
;     citation, NIL otherwise
(defun parse-citation (lines external-link)
  (let (links (state 'beg) first-name)
      (do* ((prevnumb nil (get-synt-numb nxtline))
            (nxtline (first lines) (first lines))
            (lines (rest lines) (rest lines)))
          ((or (eq state 'fail) (null nxtline))
            (cond ((or (eq state 'fail)
                       (neq state 'end)) nil)
                  (t links)))
          (cond ((eq state 'beg)
  ; *** no check on the first word: the fact that it is a proper name has already
  ;     been checked before calling 'parse-citation'
                   (setq first-name (get-synt-numb nxtline))
                   (setq links (list external-link))
                   (setq state 'second))
                ((eq state 'beg-2)
  ; *** beginning of the second reference inside the same parenthesis
                   (setq links (append1 links (list prevnumb 'COORD2ND 'citation)))
                   (setq first-name (get-synt-numb nxtline))
                   (setq state 'second))
  ; *** after the first name
                ((eq state 'second)
                   (cond ((eq (get-synt-word nxtline) #\,)
                           (setq links (append1 links (make-link prevnumb 'COORD+BASE 'citation)))
                           (setq state 'list-1))
                         ((has-gramm-type (get-synt-word nxtline) '&citation-and)	; et
                           (setq links (append1 links (make-link prevnumb 'COORD+BASE 'citation)))
                           (setq state 'et-al))
                         ((eq (get-synt-categ nxtline) 'NUM)
                           (setq links (append1 links (make-link first-name 'NUM-RMOD 'citation)))
                           (setq state 'end))
                         (t (setq state 'fail))))
  ; *** after a comma
                ((eq state 'list-1)
                   (cond ((and (eq (get-synt-categ nxtline) 'NOUN)
                               (eq (get-synt-type nxtline) 'PROPER))
                           (setq links (append1 links (make-link prevnumb 'COORD2ND+BASE 'citation)))
                           (setq state 'second))
  ; *** the next for forms as [Rossi, 1988]
                         ((eq (get-synt-categ nxtline) 'NUM)
                           (setq links (append1 links (make-link first-name 'NUM-RMOD 'citation)))
                           (setq state 'end))
                         (t (setq state 'fail))))
  ; *** after a conjunction
                ((eq state 'et-al)
                   (cond ((has-gramm-type (get-synt-word nxtline) '&citation-al)
                           (setq links (append1 links (make-link prevnumb 'COORD2ND+BASE 'citation)))
                           (setq state 'lastname))
                         ((and (eq (get-synt-categ nxtline) 'NOUN)
                               (eq (get-synt-type nxtline) 'PROPER))
                           (setq links (append1 links (make-link prevnumb 'COORD2ND+BASE 'citation)))
                           (setq state 'lastname))
                         (t (setq state 'fail))))
  ; *** after 'al.' or a second name after a conjunction (Black and White, 2002)
                ((eq state 'lastname)
                   (cond ((eq (get-synt-word nxtline) #\,)
                           (setq links (append1 links (make-link prevnumb 'COORD+BASE 'citation)))
                           (setq state 'beforend))
                         ((eq (get-synt-categ nxtline) 'NUM)
                           (setq links (append1 links (make-link first-name 'NUM-RMOD 'citation)))
                           (setq state 'end))
                         (t (setq state 'fail))))
  ; *** after a comma that follows 'alii'
                ((eq state 'beforend)
                   (cond ((eq (get-synt-categ nxtline) 'NUM)
                           (setq links (append1 links (make-link first-name 'NUM-RMOD 'citation)))
                           (setq state 'end))
                         (t (setq state 'fail))))
  ; *** before a second year
                ((eq state 'beforend-2)
                   (cond ((eq (get-synt-categ nxtline) 'NUM)
                           (setq links (append1 links (make-link prevnumb 'COORD2ND+BASE 'citation)))
                           (setq state 'end))
                         (t (setq state 'fail))))
  ; *** after the final number; N.B. if after the number there is nothing, the
  ;     loop has already terminated. Here, there could only be a comma, which
  ;     can be followed by a second reference (Black 99, White 2000), or by a
  ;     second year (Black 99, 2000)
                ((eq state 'end)
                   (cond ((memq (get-synt-word nxtline) '(#\, #\;))
     ; *** the next cond is a lookahead
                           (cond ((eq (get-synt-categ (first lines)) 'NUM)
                                    (setq links 
                                       (append1 links (make-link prevnumb 'COORD+BASE 'citation)))
                                    (setq state 'beforend-2))
                                 ((and (eq (get-synt-categ nxtline) 'NOUN)
                                       (eq (get-synt-type nxtline) 'PROPER))
                                   (setq links 
                                       (append1 links (make-link first-name 'COORD+BASE 'citation)))
                                   (setq state 'beg-2))
                                 (t (setq state 'fail))))
                         (t (setq state 'fail))))))))
                           
;**********************************************************************
; *** checks if a word is of a given grammatical type (KB/DICTIONARY/grammtypes)
; *** the data in *GRAMMTYPES* are encoded following the LISP scheme (currently
;     UTF-8). So "più" has the codes (80 73 195 185) [UP-P UP-I DOWN-U-GRAVE]
;     Unfortunately, the accented chars are kept lowercase.
; *** the data in word is encoded according to the dictionary format, i.e. ISO-8839-1;
; *** so "più" is (80 73 217) [UP-P UP-I UP-U-GRAVE]
;     Note that this is the "base" internal encoding of the dictionaries, so that it
;     does not depend on the chosen encoding scheme
(defun has-gramm-type (word gtype)
  (memb-or-eq word (get gtype 'grtype))
  )
                           
;**********************************************************************
(defun set-locution-links (data links)
   (do* ((prevsents nil (cons cursent prevsents)) 
         (cursent (first data) (first remsents))
	 (remsents (rest data) (rest remsents))
         (prevsentlinks nil (cons cursentlinks prevsentlinks)) 
	 (cursentlinks (first links) (first remsentlinks))
	 (remsentlinks (rest links) (rest remsentlinks))
	 (scount 0 (1+ scount)))
	((and (null cursent) (null remsents)) (reverse prevsentlinks))
; ***************** loop advancing on a single sentence *******
; *** see the comments below in "apply-loc-parserules"
	(setq cursentlinks
    	   (do* ((prevlines nil (cons curline prevlines))
	         (curline (first cursent) (first remlines))
	         (remlines (rest cursent) (rest remlines))
	         (prevlinks nil (cons curlink prevlinks))
	         (curlink (first cursentlinks) (first remlinks))
	         (remlinks (rest cursentlinks) (rest remlinks)))
; *** the possible changes to the label list (links found in this cycle) have been
;     collected in 'prevlinks'. They are put at disposal of the outer loop
	        ((null curline) (reverse prevlinks))
  		(cond ((and (not (null prevlines))
                            (same-locution curline (first prevlines)))
                        (setq curlink 
                             (make-link (get-synt-numb (first prevlines)) 
                             'CONTIN+LOCUT 'locution))))))))

;**********************************************************************
(defun apply-loc-parserules (data links)
; ***************** the external 'dolist' advances on chunks ***********
; *** it is assumed that the rules are grouped according to the governing
;     category, So, a 'chunk-level' is associated with such a category. For
;     instance, the levels can be 'ADJ', 'NOUN', 'VERB', thus forcing the
;     initial construction of AdjP, then NP, then S (or clauses). The loop on 
;     the chunk levels is currently executed just once, but it can, in principle
;     be repeated more than once
; *** notice that 'data' contains the whole file, and not a single sentence.
;     Inter-sentential references could be handled, but this is not enforced now.
; ***************** loop advancing on data ********************
; >>> prevsents are the previous sentences (already analyzed)
; >>> cursent is the sentence to parse now
; >>> remsents are the sentences remained to parse
; >>> prevsentlinks are the links assigned to the previous sentences
; >>> cursentlinks are the links assigned to the current sentence (initially a
;     list of NIL)
; >>> remsentlinks are lists of NIL (links to assign to the following sentences)
; *** 'data' is left unchanged inside the loop, so it's available at each cycle
; *** 'links' is reassigned as the result of the loop, with the new links
;     obtained after the application of the rules of a single chunk
 (declare (special *LANGUAGE*))
 (let (savelanguage language-obl)
  (dolist (nxtcat *CHUNK-LEVELS* links)
    (setq savelanguage *LANGUAGE*)
  ; *** this use of the *LANGUAGE* variable is restricted to forcing the application
  ;     of a set of rules to the NOUN-NOUN english construction. In this case, only the
  ;     specific rules must be applied, not the ones for any language, so that the
  ;     language-obl variable is set to true
  ; *** the format of the entry is: <current-language category special-language>
    (cond ((and (not (atom nxtcat))
                (eq (first nxtcat) *LANGUAGE*))
            (setq *LANGUAGE* (third nxtcat))
            (setq language-obl t)
            (setq nxtcat (second nxtcat))))
    ; (format t "nxtcat: ~a~%" nxtcat)
    ; (cond ((equal nxtcat '(adj (indef poss deitt interr demons)))
    ;             (break "apply-loc-parserules")
    ;  ))
    (setq links
      (do* ((prevsents nil (cons cursent prevsents)) 
            (cursent (car data) (car remsents))
	    (remsents (cdr data) (cdr remsents))
            (prevsentlinks nil (cons cursentlinks prevsentlinks)) 
	    (cursentlinks (car links) (car remsentlinks))
	    (remsentlinks (cdr links) (cdr remsentlinks))
	    (scount 0 (1+ scount)))
	 ((and (null cursent) (null remsents)) (reverse prevsentlinks))
; ***************** loop advancing on a single sentence *******
; *** it advances in parallel on 'cursent' (the word lines), and 'cursentlinks'
;     (the links, initially a list of nil's). When a word of the right category
;     (the one of the current chunk) is found, it applies the rules. The lines
;     already examined are accumulated in 'prevlines', and the links in 
;     'prevlinks'. As a result of the application of the parse rules, 'prevlinks'
;     and 'remlinks' can be modified inside the body of the loop. At the end of
;     the loop, 'prevlinks' does not contain the original links, but the ones
;     updates after the application of the rules.
; *** The final value of 'prevlinks' (which contains all the links, since we have
;     moved to the end of the sentence) is assigned to 'cursentlinks', which can
;     then be used in the outer loop
;	 (cond ((= 0 (rem scount 10))
;		 (format t " Sentence n.~s~%" (1+ scount))))
	 (setq cursentlinks
    	   (do* ((prevlines nil (cons curline prevlines))
	         (curline (car cursent) (car remlines))
	         (remlines (cdr cursent) (cdr remlines))
	         (prevlinks nil (cons curlink prevlinks))
	         (curlink (car cursentlinks) (car remlinks))
	         (remlinks (cdr cursentlinks) (cdr remlinks)))
; *** the possible changes to the label list (links found in this cycle) have been
;     collected in 'prevlinks'. They are put at disposal of the outer loop
	        ((null curline) (reverse prevlinks))
; *** process lines ***********
; *** the category corresponds to the next chunking level (nxtcat): apply the rules
		(cond ((or (and (atom nxtcat)
                                (eq nxtcat (get-synt-categ curline)))
                           (and (listp nxtcat)
                                (eq (first nxtcat) (get-synt-categ curline))
                                (memq (get-synt-type curline) (second nxtcat))))
			(multiple-value-setq
			   (prevlinks remlinks)
		   	   (apply-parserules 
				curline curlink 
				prevlines prevlinks 
				remlines remlinks
   ; *** the last parameter, found via 'find-a-line' is the governor of the
   ;     current word (if any)
				(cond ((null curlink) nil)
	                              (t (first (find-a-line 
                                              `(position ,(first curlink))
                                              (append 
                                                 (reverse prevlines) 
                                                 (list curline) 
                                                 remlines)
                                              (append 
                                                 (reverse prevlinks) 
                                                 (list curlink) 
                                                 remlinks)))))
                               language-obl))))))))
     (setq language-obl nil)
     (setq *LANGUAGE* savelanguage))))

; *********************************************************************
; *** it tries to determine if the word in 'line' can find one or more
;     dependents in its neighbors. Currently, nothing is done for the verbs,
;     except attaching their auxiliaries
; *** the function is evaluated just in case the current word (in curline)
;     has a category (+ type) corresponding to the current chunk. This check
;     is made outside the function.
; *** the rules are stored as the 'parserules property of the corresponding
;     syntactic category. The value of that property is a list of lists of
;     the form:
;     ((type1 position-rule1) (type2 position-rule2) ... (typeN position-ruleN))
;     - typeI is the syntactic subtype of the head word (the one in 'line')
;     - position-ruleI is a set of assignment rules associated with the 
;       position of the dependent with respect to the head
;     N.B. the same typeI can occur in more than one pair; i.e. there may
;	   exist different position-rules for the same category-type of
;	   the head.
; *** the position-rules have the structure:
;     (position rule1 rule2 ... ruleM)
;     where 'position' is one of 'before', 'after', 'follows', 'precedes',
;     'chunk-follows'
;     'before' and 'after' specify that the dependent must precede or follow
;        immediately (i.e. be adjacent to) the head
;     'follows' and 'precedes' relax the constraint of adjacency, admitting
;        more soft constraints. These are expressed as triples (cat type repeat)
;        where 'cat' and 'type' are the category and the type of words possibly
;        intervening between the head and the dependent, and 'repeat' is NIL
;        if just one occurrence of intervening word is allowed, T, if any
;        number is allowed (Kleene star)
;     'chunk-follows' requires that the dependent is the head of a chunk
;	 following the head.
;     More details are reported in the file "ALLLANG/KB-ALL/GRAMM-KB-ALL/parserules"
; *** the ruleI appearing in a position-rule have the form:
;     (dependent-category conditions label)
;     where 'dependent-category' is the category of the possible dependent,
;        'conditions' is a set of applicability conditions for the rule
;	 (e.g. syntactic subtype of the dependent, agreement), and 'label' is
;	 the label assigned to the dependency arc in case the conditions are
;	 satisfied (i.e. the dependency relation is established)
; >>> INPUT:
;	- curline: an input line corresponding to a word (or component of a
;     	  	word). This line, as well as the one appearing in the other
;   		input parameters (prev and data) are in readable format:
;		(linenenumb word syntinfo treeinfo coref comments)
;               where treeinfo, coref, and comments may be nil
;	- curlink: a two-elements list, whose first element is the index of the
;		parent, and whose second element is the dependency arc label
;	- prevlines: the lines of the previous words (in inverted order, so that
;	        the first of prevlines is the line immediately preceding the 
;		current one)
;	- prevlinks: the links of the previous lines, in the format described
;		above for curlink
;	- remlines: the lines following the current one
;	- remlinks: the links of the following lines, in the format described
;		above for curlink
;	- head-gov: the governor, if any, of the current word
(defun apply-parserules (curline curlink prevlines prevlinks remlines remlinks head-gov
                         language-obl)
; *** get-synt-syntinfo, get-synt-categ, and get-synt-type defined in 
;     /ALLLANG/PROC-ALL/MORPHO-PROC-ALL/tb-functions.lisp
 (declare (special *LANGUAGE*))
 (let* ((wordcat (get-synt-categ curline))
        (wordtype (get-synt-type curline))
	(prules (get wordcat 'chunk-rules))
        newlink go-on firsttyprul)
; *** loop on all syntactic types of the head word *******************
   (do ((nxttype-rules (first prules) (first prules))
	(prules (rest prules) (rest prules)))
      ((null nxttype-rules)
	 (values prevlinks remlinks))
; *** if the type of the current word corresponds to the one in the rule or if
;     the rule applies to any type ...
      (setq firsttyprul (first nxttype-rules))
      (cond ((or (eq firsttyprul 'any)
		 (eq firsttyprul wordtype)
                 (and (listp firsttyprul)
                      (or (and (eq (first firsttyprul) 'not)
                               (not (member wordtype (rest firsttyprul))))
                          (and (neq (first firsttyprul) 'not)
                               (member wordtype firsttyprul)))))
; *** loop on all positions where a dependent can be labelled
	      (do ((posrules (first (rest nxttype-rules)) (first rem-posrules))
		   (rem-posrules (rest (rest nxttype-rules)) (rest rem-posrules)))
		 ((null posrules))
                 (cond ((eq (first posrules) 'language)
                         (cond ((memb-or-eq *LANGUAGE* (second posrules))
                                 (setq posrules (rest (rest posrules)))
                                 (setq go-on t))
                               (t (setq go-on nil))))
                       ((eq language-obl t)
                         (setq go-on nil))
                       (t (setq go-on t)))
         ;(format t " apply rules: go-on: ~a; posrules = ~a~%" go-on posrules)
         ;(break "")
                (cond (go-on
; *** verify the conditions on the basis of the 'position' parameter
		 (case (first posrules)
; *** if position='before' ++++++++++++++++++++++++++++++++++++++++++++
		   (before
; *** if prevlines=nil, then the current word is the first of the sentence:
;     'before' rules cannot be applied
; *** if (first prevlinks) is not nil, then the previous word has already been
;     linked: 'before' rules are not useful
; *** if curline and (first prevlinks) are the same locution, then 'before' rules 
;     are not useful
		      (cond ((and (not (null prevlines))
                                  (not (same-locution curline (first prevlines))))
       ; *** find-locution-head looks for a possible locution occurring before the current
       ;     word. The general case is:
       ;     w1 w2 loc1 loc2 loc3 current ...
       ;     What happens is that in before-links are placed the links of <loc1 w2 w1>, while
       ;     in contin-links there are the links of <loc3 loc2>
       ; *** note that is there is no loocution (w1 w2 y1 current)
       ;     then in before-links we have the link of y1 and contin-links is empty
                               (let* ((befores (find-locution-head prevlines prevlinks nil nil))
                                      (before-links (second befores))
                                      (contin-links (fourth befores)))
                                  (cond ((null (first before-links))
       ; *** what follows has to be done in case the link of loc1 is empty, since this is the
       ;     word that should be attached to current if the conditions are satisfied
                                           (let ((rule-result
					             (apply-posparserules 
					                 (rest posrules) 
; *** 'curline' is the potential head
;     'first prevlines' is the potential dependent
;     'rest prevlines' are all lines preceding the dependent (in reverse order)
;     'cons curline remlines' are all the lines following the dependent (including
;       the head)
					                  curline 
					                  (first prevlines) 
					                  (rest prevlines) 
					                  (cons curline remlines)
                                                          head-gov)))
                                           (cond ((not (null rule-result))
			                            (setq prevlinks
                                                       (append contin-links
				                           (cons rule-result
				   	                          (rest before-links)))))))))))))
; *** if position='after' ++++++++++++++++++++++++++++++++++++++++++++
		   (after
; *** if remlines=nil, then the current word is the last of the sentence:
;     'after' rules cannot be applied
; *** if (first remlinks) is not nil, then the next word has already been linked:
;     'after' rules are not useful
		      (cond ((and (not (null remlines)) (null (first remlinks)))
                              (cond ((or (equal (get-synt-word curline) #\") ;"
                                         (equal (get-synt-word curline) #\'))
                                      (setq newlink
                                         (apply-posparserules
                                            (rest posrules)
                                            (first remlines)
                                            (second remlines)
                                            (cons (first remlines)
                                                  (cons curline prevlines))
                                            (rest (rest remlines))
                                            head-gov))
			              (setq remlinks (cons (first remlinks)
                                                           (cons newlink 
                                                              (rest (rest remlinks))))))
                                    (t (setq newlink
			                 (apply-posparserules 
					    (rest posrules) 
; *** 'curline' is the potential head
;     'first remlines' is the potential dependent
;     'cons curline prevlines' are all lines preceding the dependent (including
;       the head; in reverse order)
;     'rest remlines' are all the lines following the dependent
					    curline 
					    (first remlines) 
					    (cons curline prevlines)
					    (rest remlines)
                                            head-gov))
   ; *** if a link has been found, and the current line is the end of a locution,
   ;     then the pointer must be moved to the beginning of the locution
			              (setq remlinks 
                                             (cons newlink (rest remlinks))))))))
; *** if position='precedes' ++++++++++++++++++++++++++++++++++++++++++++
;     move backward while the conditions on the intervening words are verified:
;     for each word preceding the sequence of repetitions, make the usual work
;     on the preceding word
		   (precedes
; *** if prevlines=nil, then the current word is the first of the sentence:
;     'precedes' rules cannot be applied
		    (cond ((not (null prevlines))
; *** preccat and prectype are the category and type of the admissible intervening
;     words
		      (let ((preccat (first (second posrules)))
		 	    (prectype (second (second posrules)))
			    (repeat (third (second posrules)))
			    (separat (fourth (second posrules)))
			    (continue t))
; *** in case repeat=T, then the case of no intervening word has to be checked
			 (cond (repeat
; *** if (first prevlinks) is not nil, then the previous word has already been
;     linked: 'precedes' rules are not useful
				 (cond ((null (first prevlinks))
					  (setq prevlinks 
					     (cons
						(apply-posparserules 
						    (rest (rest posrules))
; *** 'curline' is the potential head
;     'first prevlines' is the potential dependent
;     'rest prevlines' are all lines preceding the dependent
;     'cons curline remlines' are all the lines following the dependent 
;	 (including the head)
						    curline
						    (first prevlines) 
						    (rest prevlines)
						    (cons curline remlines)
                                                    head-gov)
						 (rest prevlinks)))))))
; *** in any case, proceed backward via the loop (but it is interrupted after the
;     first cycle, in case repeat=NIL; see 'continue')
			 (setq prevlinks 
			     (cons
	; *** 'first prevlink' has already been evaluated outside the loop (if
	;     'repeat' was T)
				(first prevlinks)
			        (do* ((seen nil (append1 seen nxtword))
				      (nxtword (first prevlines) 
					       (first prevlines))
				      (prevlines (rest prevlines) 
						 (rest prevlines))
				      (seenlinks nil 
					     (append1 seenlinks 
						      (first prevlinks)))
				      (prevlinks (rest prevlinks)
						 (rest prevlinks)))
; *** seenlinks is the sequence of links preceding the word under analysis,
;     whose corresponding word has already been inspected
; *** so, the sequence of links is now split into three parts:
;     - prevlinks (preceding and not inspected)
;     - seenlinks (preceding and inspected)
;     - remlinks (following); 'remlinks' is not affected by the loop
; *** analogously, for the sentence (prevlines, seen, remlines)
; *** the loop is exited when no more words before, or 'continue' set to nil
;     (skipping conditions not met anymore)
				   ((or (null prevlines) (not continue))
				      (append seenlinks prevlinks))
; *** if just one intervening word is admitted, then block the loop
				  (cond ((not repeat) (setq continue nil)))
; *** if the current word satisfies the 'skipping' conditions, then try to apply
;     the rule to the previous word
; *** the skipping conditions are specified by an atom (e.g. 'qualif) or by a list not
;     beginning with 'char', in case they refer to the type of the possible intervening
;     words, or by the pair (char x), in case they refer to an intervening punctuation
;     marks. 
				  (cond ((or 
                                           (is-a-synt-trace? nxtword)
        ; *** traces are skipped
    ; *** the skipping category must correspond
                                           (and (eq preccat (get-synt-categ nxtword))
    ; *** the skipping type can be "any" ---> ok
				                (or (eq prectype 'any)
    ; *** or it can be an atom ---> ok if it the same as the category of the word
                                                    (and (atom prectype)
                                                         (eq prectype (get-synt-type nxtword)))
    ; *** or it can be an list of the form (char #\-) or (char (#\- #\' )) --->
    ;     ok if the input word is the same or is member of the charlist
                                                    (and (listp prectype)
                                                         (eq 'char (first prectype))
						         (memb-or-eq (get-synt-word nxtword)
                                                             (second prectype)))
    ; *** or it can be a list of types, as (deitt demons) ---> ok if the current
    ;     type is among them
                                                    (and (listp prectype)
                                                         (neq 'char (first prectype))
						         (member (get-synt-type nxtword)
                                                                  prectype))))
   ; *** intervening conjunctions are accepted if all other elements are
   ;     of the right type: DEP adj and adj and adj HEAD
   ; ---- removed because it applies to DEP and ...
                                 ;         (and (eq (get-synt-categ nxtword) 'conj)
                                 ;              (eq (get-synt-type nxtword) 'coord))
   ; *** the next word is skipped also in case it is an admitted intervening
   ;     separator (presumably, a comma)
                                           (and (eq 'PUNCT (get-synt-categ nxtword))
                                                (memb-or-eq 
						    (get-synt-word nxtword) separat)))
; *** if the previous word has not already been assigned a label
					  (cond ((null (first prevlinks))
						  (setq prevlinks 
						      (cons
							(apply-posparserules 
							    (rest (rest posrules))
; *** 'curline' is the potential head
;     'first prevlines' is the potential dependent
;     'rest prevlines' are all lines preceding the dependent
;     'append ...' constructs the list of words following the currently inspected
;	 dependent (i.e. (first prevlines))
							    curline
							    (first prevlines) 
							    (rest prevlines)
							    (append 
								 (list nxtword)
								 seen
								 (list curline)
								 remlines)
                                                                 head-gov)
							(rest prevlinks))))))
; *** if the current word does not satisfy the 'skipping' conditions, end of loop
					(t (setq continue nil))))))))))
; *** if position='follows' ++++++++++++++++++++++++++++++++++++++++++++
;     do the same as position=precedes, but going forward instead of backward
    ; *** The format of the rule is:
    ;      (follows SKIP-CONDITION DEP-DEF1 DEP-DEF2 ... DEP-DEFN)
       ; --- the SKIP-CONDITION has the form
       ;     (SKIP-OP (SKIP-DESC1 SKIP-DESC2 ... SKIP-DESCM) SKIP-OPTION)
       ;     ... SKIP-OP can be * or SEQ
       ;         SKIP-OP = *: from zero to inf words satisfying SKIP-DESC may
       ;                      occur between the head and the dependent
       ;         SKIP-OP = SEQ: if SKIP-DESC has N elements, exactly N words 
       ;                      satisfying the sequence of constraints in SKIP-DESC
       ;                      must occur between the head and the dependent
       ;     ... each SKIP-DESCi has the form (SKIP-CATEG FEATURE-CONSTR)
       ;             - SKIP-CATEG is any syntactic category
       ;             - FEATURE-CONSTR can be NIL, an atom (in which case it is the
       ;               required syntactic type) or a list (feature-name feature-value)
       ;               Currently, the only feature is CHAR, whose value is a 
       ;               character or a character list.
       ;             N.B. if SKIP-OP is *, just SKIP-DESC1 is used
       ;     ... SKIP-OPTION is a character or a sequence of characters that are
       ;             ignored in the matching process
       ; --- a DEP-DEF has the form
       ;     (DEP-CATEG CONDITIONS LABEL)
		   (follows
    ; *** if remlinks=nil, then the current word is the last of the sentence:
    ;     'follows' rules cannot be applied
		     (cond ((null remlinks) nil)
			   (t (let* ((skip-cond (second posrules))
                                     (skip-op (first skip-cond))
                                     (skip-desc (second skip-cond))
                                     (skip-option (third skip-cond))
                                     skip-currdesc skip-categ skip-constr (continue t))
   ; *** initialization of the loop:
   ;     SKIP-CURRDESC: is the next word that must be skipped
   ;           if SKIP-OP is *, then it always keeps the same check condition, and is not
   ;                            changed inside the cycle 
   ;           if SKIP-OP is SEQ, then it takes the first word to match from
   ;                            SKIP-DESC, and moves ahead. If SKIP-DESC is nil, then
   ;                            the word that immediately follows the head is checked
                                 (setq skip-currdesc (first skip-desc))
                                 (cond ((eq skip-op 'seq)
                                          (setq skip-desc (rest skip-desc))))
				 (setq remlinks
				    (do* ((seen nil (append1 seen nxtline))
					  (seenlinks nil (append1 seenlinks nxtlink))
					  (nxtline (first remlines) (first remlines))
					  (nxtlink (first remlinks) (first remlinks))
					  (remlines (rest remlines) (rest remlines))
					  (remlinks (rest remlinks) (rest remlinks)))
    ; *** seenlinks is the sequence of links following the word under analysis,
    ;     whose corresponding word has already been inspected
				        ((or (null nxtline) (not continue))
					    (append seenlinks (list nxtlink) remlinks))
    ; *** if just one intervening word is admitted, then block the loop
    ;     if the SKIP-OP is SEQ, SKIP-DESC becomes empty after the last word of the
    ;        sequence has been matched
                                        (setq skip-categ (first skip-currdesc))
                                        (setq skip-constr (second skip-currdesc))
     ; *** if the check condition admits repetition (including 0 of them) or the
     ;     sequence of intervening words has been completely matched (or it was empty)
     ;     carry out the check if the current word (nxtline) can be a dependent of the head
       ; (format t " apply rules 2: skip-categ: ~a; skip-constr = ~a; line: ~a~%"
       ;           skip-categ skip-constr nxtline)
       ; (break "")
					(cond ((or (eq skip-op '*)
                                                   (null skip-currdesc))
					        (cond ((null nxtlink)
         ; *** if the current word has not already been assigned a label
						        (setq nxtlink
							   (apply-posparserules 
								(rest (rest posrules))
          ; *** 'curline' is the potential head
          ;     'nxtline' is the potential dependent
          ;     'append ...' constructs the list of words preceding the currently inspected
          ;	    dependent (i.e. nxtline)
								curline
								nxtline
								(append 
								    seen
								    (list curline)
								    prevlines)
								remlines
                                                                head-gov))))
          ; *** if skip-currdesc is empty, then it was a SEQ condition, and the test has just
          ;     one candidate: stop the search
                                                (cond ((null skip-desc) (setq continue nil)))))
    ; *** in case the condition is SEQ:
    ;       if the previous test was made, it means the the SEQ was completed; now "continue"
    ;          is false, and there is nothing else to do
    ;       otherwise the previous branch was not followed, and "continue" is still T:
    ;          use the current word to advance in the match
    ; *** in case the condition is *:
    ;       the previous test was made, but skip-desc is not null (it never becomes null
    ;       with *) and so 'continue' is still T: try to use the word as a a skip word
    ;     N.B. locution continuations are always skipped (???)
       ;(cond ((and (listp skip-constr) (eq (first skip-constr) 'word-typ))
       ;  (format t " apply rules 3: skip-categ: ~a; skip-constr = ~a; line: ~a~%"
       ;            skip-categ skip-constr nxtline)
       ;  (break "")
       ;))
				         (cond ((and continue
                                                   (or
                                                     (is-a-synt-trace? nxtline)
        ; *** traces are skipped
                                                     (and 
                                                        (eq skip-categ (get-synt-categ nxtline))
        ; *** the skip category matches and
                                                        (or (null skip-constr)
            ; *** there is no further condition or
							    (and (atom skip-constr)
                                                                 (eq skip-constr
							            (get-synt-type nxtline)))
            ; *** there is a condition on the type and the type matches or
                                                            (and (memq skip-categ '(PUNCT SPECIAL))
				                                 (memb-or-eq 
							                (get-synt-word nxtline)
                                                                        (second skip-constr)))
            ; *** there is a condition on the mood of a verb; this should be generalized to
            ;     other features
                                                            (and (listp skip-constr)
                                                                 (eq (first skip-constr) 'mood)
                                                                 (eq (second skip-constr)
                                                                     (get-synt-mood nxtline)))
            ; *** there is a condition on the word grammtype
                                                            (and (listp skip-constr)
                                                                 (eq (first skip-constr) 'word-typ)
                                                                 (has-gramm-type
                                                                     (get-synt-word nxtline)
                                                                     (second skip-constr)))))
            ; *** there is a CHAR condition on a PUNCT or a SPECIAL or
            ;     e.g. (follows (punct (char #\,) nil) ...)
                                                            (and (eq 'PUNCT (get-synt-categ nxtline))
                                                                 (memb-or-eq 
							                 (get-synt-word nxtline)
                                                                         skip-option))))
            ; *** a SKIP-OPTION is satisfied
                                                 (cond ((and (eq skip-op 'seq)
                                                             (not (is-a-synt-trace? nxtline)))
       ; *** the match is ok: go ahead with the next word and, in case of SEQ, with the next element
       ;     to match
                                                          (setq skip-currdesc (first skip-desc))
                                                          (setq skip-desc (rest skip-desc)))))
       ; *** if the current word does not satisfy the 'skipping' conditions, stop the search
					       (t (setq continue nil)))))))))
; *** if position='chunk-follows' ++++++++++++++++++++++++++++++++++++++++++++
;     use the function 'apply-chunk-rules'. As the rule
;     to apply, just the second of 'posrules' is used, since chunk-follows
;     admits just one rule at a time
			  (chunk-follows
			     (setq remlinks
			     	(apply-chunk-rules 
				    'forward 
				    (second posrules)
				    curline curlink
				    remlines remlinks 
				    prevlines prevlinks head-gov)))
; *** if position='chunk-precedes' ++++++++++++++++++++++++++++++++++++++++++++
;     use the function 'apply-chunk-rules'. As the rule
;     to apply, just the second of 'posrules' is used, since chunk-precedes
;     admits just one rule at a time
			  (chunk-precedes
			     (setq prevlinks
			     	(apply-chunk-rules 
				    'backward 
				    (second posrules)
				    curline curlink
				    remlines remlinks 
				    prevlines prevlinks head-gov)))
			)))))))))

; ***********************************************************************
; *** this moves backward flushing all locution continuations
;     It returns all lines and links, backward, starting from the
;     element which is not a locution continuation
(defun find-locution-head (prevlines prevlinks continlines continlinks)
     (cond ((null prevlines) nil)
           ((eq 'CONTIN+LOCUT (second (first prevlinks)))
              (find-locution-head (rest prevlines) (rest prevlinks) 
                         (cons (first prevlines) continlines) 
                         (cons (first prevlinks) continlinks)))
           (t (list prevlines prevlinks (reverse continlines) (reverse continlinks)))))

; **********************************************************************
; *** given a line containing a locution, this finds the line where the locution
;     starts
(defun find-locution-beg (locutline datalines)
 (let ((found nil))
  ; *** the external do* moves on all lines in search for the locution line
   (do* ((prevlines nil (cons nxtline prevlines))
         (nxtline (first datalines) (first datalines))
         (datalines (rest datalines) (rest datalines)))
        ((or found (null nxtline))
          (cond (found 
  ; *** when the locution line is found, prevlines contains all the lines preceding
  ;     it
  ; *** the internal do moves (backward) on the preceding lines, checking if the
  ;     line refers to the same locution; in such a case, it returns the last line
  ;     of the same locution (which, since we go backward, is the first locution
  ;     line)
                     (do* ((precline (first prevlines) line)
                           (line (second prevlines) (first prevlines))
                           (prevlines (rest (rest prevlines)) (rest prevlines)))
                         ((or (null line) 
                              (not (same-locution precline line)))
                            precline)))
                (t (exception 'parse-error "PROC/chunk-parser: find-locution-beg"))))
        (setq found (equal locutline nxtline)))))

; ***********************************************************************
; *** loops on all rules of a 'position' packet
; ** INPUT: 
;  >>> posrules: the rules of that packet
;	the structure is (rule1 rule2 ... ruleN), where each rule is
;	<categs condition label>
;  >>> head: the possible head (the word which originated the search)
;  >>> dependent: the possible dependent (the word which on which the algorithm
;      is currently working)
;  >>> beforedep: all the words preceding 'dependent'
;  >>> afterdep: all the words following 'dependent'
(defun apply-posparserules (posrules head dependent beforedep afterdep head-gov)
  (let (found-label)
   (do ((rule (first posrules) (first posrules))
	(posrules (rest posrules) (rest posrules)))
; *** exit if a rule has been found for that position, or if no more rules
       ((or found-label (null rule)) found-label)
       (cond ((check-parsecond rule head dependent beforedep afterdep head-gov)
		(setq found-label
		      (make-link (get-synt-numb head) (third rule) 'chunkrules)))))))

; ***************************************************************************
;     This function looks for chunks (forward or backward, according to the
;     first argument), and then, if one is found, it applies to its head the
;     rules in 'rules', to determine the links of the chunk head to its parent
;     (which is in 'line')
; *** The result is a list of links: the one following or preceding the parent
;     (line), according to the direction of search, possibly updated with the
;     new label assigned to the chunk head
; *** 'rules' must have the form (chunkcats conditions label)
(defun apply-chunk-rules 
    (direction rule curline curlink succlines succlinks prevlines prevlinks head-gov)
; *** if direction=forward, then search-lines is 'succlines' (the following lines)
;	search-links is 'succlinks' (the following links), other-lines is
;      'prevlines' (the lines preceding 'line'), and other-links is 'prevlinks' (the
;	preceding links)
  (let ((allines (append (reverse prevlines) (list curline) succlines))
        (allinks (append (reverse prevlinks) (list curlink) succlinks))
        (search-lines (cond ((eq direction 'forward) succlines)
                            (t prevlines)))
        (search-links (cond ((eq direction 'forward) succlinks)
                            (t prevlinks)))
        (chunkskip (first rule))
        (chunkcats (second rule))
        (newlab (fourth rule))
        chunk-head startline startlink chunkokskip chunknotskip go-on)
; *** chunkcats are the possible categories of the chunk head
; *** chunkskip are the categories that could be skipped in the search
; *** chunknotskip are the categories that cannot occur in the search
      (dolist (ck chunkskip)
         (cond ((atom ck) (setq chunkokskip (cons ck chunkokskip)))
               ((eq (first ck) 'not) 
                  (setq chunknotskip (cons (second ck) chunknotskip)))
               (t (exception 'parse-error "PROC/chunk-parser: chunk-skip"))))
      (cond ((or (memq (get-synt-categ (first search-lines))
                       (append chunkokskip '(PUNCT MARKER SPECIAL)))
                 (and (eq (get-synt-categ (first search-lines)) 'ADV)
                      (eq (get-synt-categ (second search-lines)) 'VERB)))
      ; *** the second condition to anticipate verb-adv attachment; it will
      ;     be done afterwards, but here it is assumed that the adv is part
      ;     of the verbal chunk
               (setq startline (second search-lines))
               (setq startlink (second search-links))
               (setq search-lines (rest (rest search-lines)))
               (setq search-links (rest (rest search-links)))
               (setq go-on t))
            ((memq (get-synt-categ (first search-lines)) chunknotskip)
               (setq go-on nil))
            (t (setq startline (first search-lines))
               (setq startlink (first search-links))
               (setq search-lines (rest search-lines))
               (setq search-links (rest search-links))
               (setq go-on t)))
   ; *** prevlines, prevlinks, succlines, succlinks, are required just because,
   ;     inside chunk-head-cat, a call is made to check-parsecond, which needs
   ;     these parameters. "check-parsecond" is used to check the conditions
   ;     associated with the chunk head (e.g. ((agree) (type qualif)))
      (cond (go-on
              (setq chunk-head
                 (chunk-head-cat direction rule curline startline startlink 
                                 search-lines search-links allines allinks
                                 chunkcats head-gov))
  ; *** build the result: substitute the new link if the chunk has been found
  ;     or leave the links unchanged. This has to be done in prevlinks or
  ;     succlinks, respectively, depending on 'direction'
              (cond ((and (not (null chunk-head))
                          (not (equal chunk-head head-gov)))
                      (cond ((eq direction 'forward)
                               (subst-chunk-link chunk-head succlines succlinks 
                                       (make-link (get-synt-numb curline) newlab 'chunkrules)))
                            (t (subst-chunk-link chunk-head prevlines prevlinks 
                                       (make-link (get-synt-numb curline) newlab 'chunkrules)))))
                    (t (cond ((eq direction 'forward) succlinks)
                             (t prevlinks)))))
            (t prevlinks))))

; ***************************************************************
(defun subst-chunk-link (actline lines links newlink)
    (do* ((nxtline (first lines) (first lines))
          (lines (rest lines) (rest lines))
          (nxtlink (first links) (first links))
          (links (rest links) (rest links))
          (seenlinks (list nxtlink) (cons nxtlink seenlinks)))
        ((or (null nxtline) (equal actline nxtline))
            (cond ((null nxtline)
                     (exception 'parse-error "PROC/chunk-parser: subst-chunk-link"))
                  (t (append (reverse (rest seenlinks)) (list newlink) links))))))

; ***************************************************************
; *** finds the head of the chunk, checking if it respects the conditions
;     expressed in chunkcats (the head must belong to one of the categories
;     in chunkcat) and in "rule" (which has the form of the antecedent of a
;     rule of the chunker).
; *** headline specifies the position of the possible governor of the chunk. It
;     indicates the direction of search, and cannot be crossed. This is
;     important in cases such as "è quella DI differenziare i prodotti", where
;     'chunk-head-cat' is applied forward to look for a nominal chunk to attach 
;     to the preposition. The first word found is 'differenziare', which in fact
;     belongs to a nominal chunk (headed by 'quella'), but must anyway stop the
;     search.
; *** INPUT:
;   >>> direction: "forward" or "backward"
;   >>> rule: conditions to be checked on the chunk head
;   >>> nxtword: the next chunk member to test
;   >>> link: its link
;   >>> search: the words on which the search must be carried out
;       In case they are the preceding words, they are in inverse order
;   >>> searchlink: their links
;   >>> allines: all lines of the sentence
;   >>> allinks: all links of the sentence
;   >>> chunkcats: the possible categories of the head of the chunk looked for
;       or 'any' (in which case 'rule' is not used)
;   >>> head-gov: used in the application of 'rule'
;   >>> coord?: if it is 'no-coord, then a line linked as 'coord2nd' blocks the
;       search upward
; *** OUTPUT:
;   >>> the line of the head of the chunk, or nil
(defun chunk-head-cat 
       (direction rule headline nxtword link search searchlinks allines allinks
        chunkcats head-gov &optional coord?)
 (let (prevlines prevlinks succlines succlinks)
; *** find the parent of nxtword in the sentence; its position is the first of 'link'
; *** if no pointer upward, we are at the top of the chunk
  (cond ((or (null link) 
             (eq (first link) 0)
             (and (eq coord? 'no-coord) (lab-subsumes 'COORD2ND (second link))))
          (cond ((not (null nxtword))
                   (multiple-value-setq (prevlines prevlinks succlines succlinks)
                      (split-result-lines nxtword allines allinks))))
	  (cond ((null nxtword) nil)
                ((eq chunkcats 'any) nxtword)
                ((and (memq (get-synt-categ nxtword) chunkcats)
   ; *** check-parsecond is used to check the conditions appearing in the
   ;     chunk rule (e.g ((agree) (type qualif)))
   ; *** it would require the previous and following lines, but since
   ;     all conditions in the chunk rules are local to the chunk head, and here
   ;     we do not have these information, I simply pass empty lists
                      (check-parsecond 
                          (cons (get-synt-categ nxtword) (rest (rest rule)))
                           headline nxtword prevlines succlines head-gov))
                   nxtword)
		(t nil)))
; *** proceed recursively upward; unless the found word is already linked
;     to the current possible parent
       (t (let ((upword (find-chunk-word 
                    direction (first link) nxtword link search searchlinks)))
	 (chunk-head-cat 
		   direction rule headline
		   (first upword)   ; the new word found
		   (second upword)  ; its link to the parent
		   (third upword)   ; the remaining search words
		   (fourth upword)  ; the remaining search links
                          allines allinks
		   chunkcats head-gov coord?))))))

; ***************************************************************
;     looks for a word in the list representing a sentence
; *** It follows a subtree structure.
;     In case the tree is projective (as it should be), the direction
;     is fixed; i.e. if we are moving backward, the parent of a node
;     must precede the daughter, since we are moving up starting from
;     the rightmost leaf. Viceversa, if we are moving forward.
; *** However, the idea is that the search lines (preceding or
;     following) have already been set in the calling function, so
;     that it is only necessary to move ahead on that list.
; *** For instance, if we are looking for a chunk ahead in the
;     following situation: (P1 P2 P3 P4 P5 START N1 N2 N3 N4 N5)
;     then, the search lines are (N1 N2 N3 N4 N5), so we should start
;     from N1 and go ahead.
; *** On the contrary, if we are looking for a back chunk, the search
;     lines are (P5 P4 P3 P2 P1), and again we must start from P5 and
;     go ahead.
; *** Note, however, that in the first case the 'index' of the next
;     word to search is greater than the current position, while in the
;     second case it is smaller
; *** INPUT:
;  >>> direction: 'forward' or 'backward'
;  >>> pos: the position of the word to find
;  >>> word: the word we are currently positioned on
;  >>> curlink: the identifier (position+label) of the parent of 'word' in the tree
;  >>> search: the words of the sentence where the search must be done
;  >>> searchlinks: their links
; *** OUTPUT:
;  >>> a list of four elements including:
;	- W: the found word
;	- its link
;	- R: the remaining words (preceding or following W, according to the
;            direction of the search)
;	- the links of the words in R
; *** it is assumed that the word of position 'pos' does exist
(defun find-chunk-word (direction pos word curlink search searchlinks)
   (cond ((null word)
	    (exception 'parse-error "PROC/chunk-parser: find-chunk-word-1"))
	 ((equal (get-synt-numb word) pos)
	    (list word curlink search searchlinks))
; *** 'index-precedes' in "MORPHO/tb-functions"
	 ((eq direction 'backward)
; *** the word to find must precede the current word: the parent index
;     should be smaller than the current one
	    (cond ((index-precedes pos (get-synt-numb word))
		     (find-chunk-word direction pos (first search) 
			       (first searchlinks) 
			       (rest search)
			       (rest searchlinks)))
		  (t nil)))
	 ((eq direction 'forward)
; *** the word to find must follow the current word: the parent index
;     should be greater than the current one
	    (cond ((index-precedes (get-synt-numb word) pos)
	  	     (find-chunk-word direction pos (first search) 
			       (first searchlinks) 
			       (rest search)
			       (rest searchlinks)))
		  (t nil)))))

; ***********************************************************************
; *** verifies the conditions for the application of a rule
; ** INPUT: 
;  >>> rule: the rule to check
;  >>> head: the possible head (the word which originated the search)
;  >>> dependent: the possible dependent (the word which on which the algorithm
;      is currently working)
;  >>> prevlines: all the words preceding 'dependent'
;  >>> succlines: all the words following 'dependent'
(defun check-parsecond (rule head dependent prevlines succlines head-gov)
  (let ((categ (first rule))
	(actcond (second rule))
	(ok t))
;   (cond ((And (eq (get-synt-word head) '|Flamur|) (eq categ 'adj)) 
;            (format t "rule: ~a; actcond = ~a~%" rule actcond)
;            (break "check-parsecond")))
   (cond ((eq categ (get-synt-categ dependent))
; *** if the category corresponds, then check the actual conditions
   	   (do ((condit (first actcond) (first actcond))
		(actcond (rest actcond) (rest actcond)))
; *** exit if a condition is not verified, or if all conditions have been checked
	       ((or (not ok) (null condit)) ok)
	       (setq ok 
		   (eval-parsecond condit head dependent prevlines succlines head-gov)))))))

; **************************************************************
; *** actual check of a single condition.
;     the admissible operators are defined in the file ALLLANG/KB-ALL/GRAMM-KB-AL/parserules
(defun eval-parsecond (condit head dependent prevlines succlines head-gov)
   (declare (special *VERB-PARTICLE* *NOUN-NOUN* *LISP-CHAR-SET-ID* *CHAR-SET-ID*))
   (case (first condit)
; §§§ 'not'
	 (not (not (check-parsecond 
                      (list (get-synt-categ dependent) (rest condit))
                             head dependent prevlines succlines head-gov)))
; §§§ 'type'
	; *** the argument must be equal to the syntactic subtype of the dependent
	 (type (memb-or-eq (get-synt-type dependent) (second condit)))
; §§§ 'agree'
	; *** agreement check. This is a simple check, made just on single words
	;     (no subtleties of full verb complexes)
	; !!!!!!! agr-unif defined in ALLLANG/PROC-ALL/TAGGER-PROC-ALL/postagger 
	 (agree (and (agr-unif (list (get-synt-gender head))
			       (list (get-synt-gender dependent)))
		     (agr-unif (list (get-synt-number head))
			       (list (get-synt-number dependent)))))
; §§§ 'dep-follows'
	; *** the argument must be equal to the syntactic category of the word
	;     which precedes the dependent; the argument can be NIL to require
	;     that the element appears at the beginning of the sentence
	 (dep-follows 
                (test-next-words (inlist (second condit)) head (list (first prevlines)))
		;(or (and (null (second condit)) (null prevlines))
		;    (memb-or-eq (get-synt-categ (first prevlines))
		;	        (second condit)))
            )
; §§§ 'dep-precedes'
	; *** some conditions are set on the words which follow the dependent
	 (dep-precedes (test-next-words (second condit) head succlines))
; §§§ 'gov-follows'
	; *** the argument must be equal to the syntactic category of the word
	;     which precedes the head
	; *** 'find-preceding' in "PROC/MORPHO/tb-functions"
	 (gov-follows 
		(memb-or-eq
		    (get-synt-categ 
			(find-preceding head dependent prevlines succlines)) 
		    (second condit)))
; §§§ 'gov-follows-agree'
	; *** the word which precedes the head must agree in type, gender, and
        ;     number with the dependent
	; *** 'find-preceding' in "PROC/MORPHO/tb-functions"
	 (gov-follows-agree
	      (let ((precw (find-preceding head dependent prevlines succlines)))
		 (and 
	           (eq (get-synt-categ dependent) (get-synt-categ precw))
                   (eq (get-synt-type dependent) (get-synt-type precw))
	           (agr-unif (list (get-synt-gender dependent))
		             (list (get-synt-gender precw)))
		   (agr-unif (list (get-synt-number dependent))
		             (list (get-synt-number precw))))))
; §§§ 'gov-precedes'
	; *** some conditions are set on the words which follow the governor
	 (gov-precedes (test-next-words (second condit) head (find-successors head succlines)))
; §§§ 'gov-fully-intransitive'
	; *** some conditions are set on the words which follow the dependent
	 (gov-fully-intransitive (test-fully-intransitive (get-synt-word head)))
; §§§ 'skip-nouns'
	; *** the dependent (which should be of type noun) should not be followed
	;     by another noun (to avoid det-noun attachment in case of noun
        ;     complexes)
	 (skip-nouns
		(not (eq 'noun (get-synt-type (first succlines)))))
; §§§ 'word-typ'
	 (word-typ (has-gramm-type 
		      (get-synt-word dependent) (second condit)))
; §§§ 'nextword-typ'
	 (nextword-typ (has-gramm-type 
		   	  (get-synt-word (first succlines)) (second condit)))
; §§§ 'head-word-typ'
	 (head-word-typ (has-gramm-type 	
		   	  (get-synt-word head) (second condit)))
; §§§ 'head-gov-particle'
	 (head-gov-particle 
            (member
	       (get-synt-word dependent) 
               (first (leggi *VERB-PARTICLE* (get-synt-word head)))))
; §§§ 'head-char'
	 (head-char (equal
		      (get-synt-word head) (second condit)))
; §§§ 'head-gender'
	 (head-gender (memb-or-eq 	
		   	(get-synt-gender head) (second condit)))
; §§§ 'head-mood'
	 (head-mood (memb-or-eq 	
		   	(get-synt-mood head) (second condit)))
; §§§ 'head-tense'
	 (head-tense (memb-or-eq 	
		   	(get-synt-tense head) (second condit)))
; §§§ 'head-number'
	 (head-number (memb-or-eq 	
		   	(get-synt-number head) (second condit)))
; §§§ 'number'
	 (number (memb-or-eq 	
		   	(get-synt-number dependent) (second condit)))
; §§§ 'inpword-typ'
	 (inpword-typ 
                  ; (break "inpword-typ")
                   (has-gramm-type 
                        (cond ((eq *CHAR-SET-ID* 'iso-8859-1)
                                 (dictionary-normalize 
                                        (get-synt-inpword dependent)))
                              (t (get-synt-inpword dependent)))
                        (second condit)))
; §§§ 'nextcateg'
	 (nextcateg (memb-or-eq 
		   	(get-synt-categ (first succlines)) (second condit)))
; §§§ 'char'
	 (char (equal (get-synt-word dependent) (second condit)))
; §§§ 'governable' 
	; *** the list of prepositions governed by adjectives is given as a
	;     list inside 'adj-prep-govern' (to be made more general)
	 (governable (adj-prep-govern head dependent))
; §§§ 'n-governable' 
	; *** the list of prepositions governed by nouns is given as a
	;     list inside 'noun-prep-govern' (to be made more general)
	 (n-governable (noun-prep-govern head dependent))
; §§§ 'd-governable' 
	; *** the list of prepositions governed by adverbs is given as a
	;     list inside 'adv-prep-govern' (to be made more general)
	 (d-governable (adv-prep-govern head dependent))
; §§§ 'prec-verb' 
	; *** there is a verb before
	 (verb-before (not (null (find-a-line '(categ (verb)) prevlines nil))))
; §§§ 'same-locution' (same-locution in MORPHO/tb-functions)
	 (same-locution (same-locution head dependent))
; §§§ 'extended-mood': gets the mood of a verb or, if any, of its first auxiliary
	; *** 'find-first-aux' in "MORPHO/tb-functions"
	 (extended-mood 
		(eq (second condit)
		    (get-synt-mood (find-first-aux dependent prevlines))))
; §§§ 'mood': gets the mood of a verb (the current word)
	 (mood (memb-or-eq (get-synt-mood dependent) (second condit)))
; §§§ 'tense': gets the mood of a verb (the current word)
	 (tense (memb-or-eq (get-synt-tense dependent) (second condit)))
; §§§ 'head-semtype': checks if the head word belongs to semantic class given
	;     by the second argument; both the function 'inh-member' and the
	;	 semantic classes are defined in KB/semtypes
	 (head-semtype 
            (let ((semty (second condit)))
       ;         (cond ((eq 'UTF-8 *LISP-CHAR-SET-ID*)
       ;                 (setq semty (convert-base-atom-or-string-to-currlisp semty))))
             (or (inh-member (get-synt-word head) semty)
                 (has-ont-type head semty))))
; §§§ 'head-onttype': checks if the head word belongs to semantic class given,
	;     in the ontology, by the second argument; 
	 (head-onttype 
            (let ((semty (second condit)))
        ;        (cond ((eq 'UTF-8 *LISP-CHAR-SET-ID*)
        ;                (setq semty (convert-base-atom-or-string-to-currlisp semty))))
                (has-ont-type head semty)))
; §§§ 'dep-semtype': checks if the head word belongs to semantic class given
	;     by the second argument; both the function 'inh-member' and the
	;	 semantic classes are defined in KB/semtypes
	 (dep-semtype
            (let ((semty (second condit)))
        ;        (cond ((eq 'UTF-8 *LISP-CHAR-SET-ID*)
        ;                (setq semty (convert-base-atom-or-string-to-currlisp semty))))
                (or (inh-member (get-synt-word dependent) semty)
                    (has-ont-type dependent semty))))
; §§§ 'verbal-case': checks if a verbal case is admitted by the verb subcat frame
	 (verbal-case t)
; §§§ 'poss-hour'
	 (poss-hour (let ((val (get-condit-synt-value dependent)))
			       (and (< 0 val) (> 25 val))))
; §§§ 'poss-month'
	 (poss-month (let ((val (get-condit-synt-value dependent)))
			       (and (< 0 val) (> 13 val))))
; §§§ 'poss-year'
	 (poss-year (let ((val (get-condit-synt-value dependent)))
			(or ;(< val 6)
			    (and (< 50 val) (> 100 val))
			    (and (< 1000 val) (> 2100 val)))))
; §§§ 'poss-day'
	 (poss-day (let ((val (get-condit-synt-value dependent)))
			    (and (< 0 val) (> 32 val))))
; §§§ 'head-poss-day'
	 (head-poss-day (let ((val (get-condit-synt-value head)))
			    (and (< 0 val) (> 32 val))))
; §§§ 'head-poss-month'
	 (head-poss-month (let ((val (get-condit-synt-value head)))
			       (and (< 0 val) (> 13 val))))
; §§§ 'head-poss-year'
	 (head-poss-year (let ((val (get-condit-synt-value head)))
			(or (< val 6)
			    (and (< 50 val) (> 100 val))
			    (and (< 1000 val) (> 2010 val)))))
; §§§ 'next-poss-month'
	 (next-poss-month (let ((val (get-condit-synt-value (first succlines))))
			       (and (< 0 val) (> 13 val))))
; §§§ 'next-poss-year'
	 (next-poss-year (let ((val (get-condit-synt-value (first succlines))))
			(or (and (< 50 val) (> 100 val))
			    (and (< 1000 val) (> 2010 val)))))
; §§§ 'head-gov-agree'. The governor of the head agrees with the dependent
;     actually, this predicate is not used. It was an attempt to handle
;     'vera e propria disdetta', where the governor (vera) of the head (e)
;     should agree with the dependent (propria). Unfortunately, the ADJ
;     rules are applied after the CONJ rules, so that at this time the CONJ
;     has not yet a governor
	 (head-gov-agree
	   (and (eq (get-synt-categ dependent) (get-synt-categ head-gov))
                (eq (get-synt-type dependent) (get-synt-type head-gov))
	        (agr-unif (list (get-synt-gender dependent))
		          (list (get-synt-gender head-gov)))
		(agr-unif (list (get-synt-number dependent))
		          (list (get-synt-number head-gov)))))
; §§§ 'vderiv' 
	 (vderiv (not (null (get-synt-vderiv head))))
; §§§ 'vtrans' 
	 (vtrans (eq (get-synt-vtrans head) 'trans))
; §§§ 'head-dep-noun-noun'
         (head-dep-noun-noun (member (list (get-synt-word head) 
                                           (get-synt-word dependent)) 
                                     *NOUN-NOUN* :test #'equal))
; *** error
         (otherwise (exception 'parse-error "PROC/chunk-parser: eval-parsecond" condit))))

; ***************************************************************
; *** conditions has the form:
;	(verb		
;            --> an atomic condition on the category of the next word
;	 (pron (type relat))
;            --> a condition on the category of the next word, with an extra 
;                test
;	 (art (type def) pron (word quale)))
;            --> a condition on more than one following words
; *** this loops on all lines; if none succeeds, then the following lines are not
;     of the required type
(defun test-next-words (conditions head succlines)
  (cond ((null conditions) nil)
	((test-singl-nw (first conditions) head succlines) t)
	(t (test-next-words (rest conditions) head succlines))))

; ***************************************************************
; *** this checks if any of the verbclasses of the lemma "verb" is subsumed by
;     basic-trans. In the positive case returns nil (the verb is not fully intransitive)
(defun test-fully-intransitive (verb)
   (let ((verbclass (get verb 'verbal-class)) (fullintr t))
     (do ((nxtcl (first verbclass) (first verbclass))
          (verbclass (rest verbclass) (rest verbclass)))
         ((or (null nxtcl) (not fullintr)) fullintr)
         (cond ((or (parse-trh-subsumes 'basic-trans nxtcl)
                    (parse-trh-subsumes 'sent-obj-verbs nxtcl))
                   (setq fullintr nil))))))

; ***************************************************************
; *** conditions has the form:
;	verb 					or
;	(pron (type relat))			or
;	(art (type def) pron (word quale))
; *** this loops on all lines; if none succeeds, then the following lines are not
;     of the required type
(defun test-singl-nw (condition head succlines)
 (declare (special *LISP-CHAR-SET-ID*))
 (let ((nxtline (first succlines)))
  (cond ((null condition) t)
; *** if the argument is an atom, then the next word must be of a given category
	((atom condition) (eq (get-synt-categ nxtline) condition))
; *** the condition can have the form (CAT (condtype condval))
	((and (null nxtline) (null (first condition))) t)
        ((eq (get-synt-categ nxtline) (first condition))
          (let* ((condval (second (second condition)))
                 (firstcond
                     (case (first (second condition)) 
                        (not (not (test-singl-nw 
                                      (list (first condition) (second (second condition)))
                                      head succlines)))
                        (type (memb-or-eq (get-synt-type nxtline) condval))
                        (mood (memb-or-eq (get-synt-mood nxtline) condval))
                        (same-head-type-and-agr
                              (and (eq (get-synt-type nxtline)
                                       (get-synt-type head))
	                           (agr-unif (list (get-synt-gender head))
			                     (list (get-synt-gender nxtline)))
		                   (agr-unif (list (get-synt-number head))
			                    (list (get-synt-number nxtline)))))
                        (word-typ (has-gramm-type (get-synt-word nxtline) condval))
                        (char (eq condval (get-synt-word nxtline)))
                        (semtype 
              ;                (cond ((eq 'UTF-8 *LISP-CHAR-SET-ID*)
              ;                         (setq condval (convert-base-atom-or-string-to-currlisp condval))))
                              (or (eq condval (get-synt-semtype nxtline))
                                  (inh-member (get-synt-word nxtline) condval)))
                        (ok t)
                        (otherwise (exception 'parse-error 
                                             "PROC/chunk-parser: test-singl-nw" 
                                             (second condition))))))
	        (cond (firstcond 
                         (test-singl-nw (rest (rest condition)) head (rest succlines)))
        ; ???? (rest succlines) ??? we must advance on succlines ???
	              (t nil))))
; *** the category does not correspond
	(t nil))))

; ***************************************************************
; *** checks if a given preposition can be governed by a given adj
(defun adj-prep-govern (adj prep)
   (declare (special *ADJ-GOVERN*))
   (member (list (get-synt-word adj) (get-synt-word prep))
	   *ADJ-GOVERN*
	   :test #'equal))

; ***************************************************************
; *** checks if a given preposition can be governed by a given noun
(defun noun-prep-govern (noun prep)
   (declare (special *NOUN-GOVERN*))
   (or (member (list (get-synt-word noun) (get-synt-word prep))
	   *NOUN-GOVERN*
	   :test #'equal)
       (member (list (base-uppercase (get-synt-word noun))
                     (base-uppercase (get-synt-word prep)))
	   *NOUN-GOVERN*
	   :test #'equal)))

; ***************************************************************
; *** checks if a given preposition can be governed by a given noun
(defun adv-prep-govern (adv prep)
   (declare (special *ADV-GOVERN*))
   (member (list (get-synt-word adv) (get-synt-word prep))
	   *ADV-GOVERN*
	   :test #'equal))

; *********************************************************************
; ****** CHECK FOR PREPOSITIONAL ATTACHMENT ***************************
; *********************************************************************
; *** This set of procedures aims to check if any prepositional group
;     constituting an autonomous chunk (i.e. to be linked to a verb)
;     can instead be included in a previous chunk
; *** the same happens for some nouns (currently, only "rule number 3"
;     is handled, where "number" is attached to "rule")
; *** and for numbers, in the context "law 12 may 2007", where 12
;     must be attached to "law"
; *** This, for instance, applies to cases as "The present of Jane's
;     brother for Jim", where the chunking rules are unable to detect
;     that "for Jim" presumably is a modifier of "present", so they
;     build for it a new chunk
(defun check-missing-links (nopar-data links)
 (declare (special *PREFER-PP-ATTACH*))
 (let (final-links prep-dependent newlinkel upline uplink savesent savelinks
       startsearch startlink prepword downword)
; *** repeat for all sentences, advancing in parallel on data and links
   (do ((nxtsent (first nopar-data) (first nopar-data))
	(nopar-data (rest nopar-data) (rest nopar-data))
	(nxtsentlinks (first links) (first links))
	(links (rest links) (rest links)))
       ((and (null nxtsent) (null nopar-data)) (reverse final-links))
    ; *** repeat for all lines and links of the next sentence, advancing in parallel
       (setq savesent nxtsent)
       (setq savelinks nxtsentlinks)
       (do* ((prev nil (cons nxtline prev))
	     (prevlinks nil (cons nxtlink prevlinks))
 	     (nxtline (first nxtsent) (first nxtsent))
	     (nxtsent (rest nxtsent) (rest nxtsent))
	     (nxtlink (first nxtsentlinks) (first nxtsentlinks))
	     (nxtsentlinks (rest nxtsentlinks) (rest nxtsentlinks)))
	    ((null nxtline) 
		(setq final-links (cons (reverse prevlinks) final-links)))
; *** case of prepositions [rule of the parliament For the agriculture]
	   (cond ((and (eq 'PREP (get-synt-categ nxtline))
                       (null nxtlink))
                   (setq prep-dependent
                       (find-a-line 
                              `(linked-to ,(get-synt-numb nxtline)) 
                              nxtsent nxtsentlinks))
                   (cond ((neq 'VERB (get-synt-categ (first prep-dependent)))
      ; *** if the preposition does not govern a verb
                           (setq prepword (get-synt-word nxtline))
                           (setq downword (get-synt-word (first prep-dependent)))
           ; *** start from the preceding word and go up in the tree
                           (setq newlinkel nil)
                           (setq startsearch (first prev))
                           (setq startlink (first prevlinks))
                           (cond ((eq 'PUNCT (get-synt-categ startsearch))
                                    (setq startsearch (second prev))
                                    (setq startlink (second prevlinks))))
                           (do ((upline-link 
                                      (list startsearch startlink)
                                      (find-a-line `(position ,(first uplink)) 
                                                             savesent savelinks)))
           ; *** stop the search when the path upward is interrupted or when an
           ;     attachment point has been found
                               ((or (null (second upline-link)) newlinkel)
                                 (cond (newlinkel
		                          (setq nxtlink 
			                    (make-link (get-synt-numb newlinkel) 
                                                  'PREP-RMOD
                                                  'preposition-1))
		                          (setq savelinks 
		                              (change-lab
			                           savesent savelinks (get-synt-numb nxtline)
			                           nxtlink)))))
                               (setq upline (first upline-link))
                               (setq uplink (second upline-link))
                               (cond ((eq 'NOUN (get-synt-categ upline)) 
           ; *** if the new word in the path is a noun, check if the preposition
           ;     has a preferred attachment to it. Note that the depending word
           ;     is not used. This is made to exploit the already existing knowledge
           ;     on noun-prep preferences stored in *NOUN-GOVERN* or, a more specific link,
           ;     in *PREFER-PP-ATTACH*
                                        (cond ((or (noun-prep-govern upline nxtline)
                                                   (member (list (get-newtb-word upline) downword)
                                                           (leggi *PREFER-PP-ATTACH* prepword)))
                                                 (setq newlinkel upline)))))))))
; *** case of nouns [rule of the parliament n.127]
	         ((and (eq 'NOUN (get-synt-categ nxtline))
                       (null nxtlink)
                       (or (has-gramm-type (get-synt-word nxtline) '&number)
                           (has-gramm-type (get-synt-word nxtline) '&section)))
                   (setq newlinkel nil)
                   (setq startsearch (first prev))
                   (setq startlink (first prevlinks))
                   (cond ((eq 'PUNCT (get-synt-categ startsearch))
                            (setq startsearch (second prev))
                            (setq startlink (second prevlinks))))
                   (do ((upline-link 
                            (list startsearch startlink)
                            (find-a-line `(position ,(first uplink)) 
                                                        savesent savelinks)))
           ; *** stop the search when the path upward is interrupted or when an
           ;     attachment point has been found
                       ((or (null (second upline-link)) newlinkel)
                            (cond (newlinkel
		                     (setq nxtlink 
		                        (make-link (get-synt-numb newlinkel) 
                                                   'NOUN-RMOD 'preposition-1))
		                     (setq savelinks 
		                        (change-lab
			                    savesent savelinks (get-synt-numb nxtline) nxtlink)))))
                      (setq upline (first upline-link))
                      (setq uplink (second upline-link))
                      (cond ((eq 'NOUN (get-synt-categ upline)) 
           ; *** if the new word in the path is a noun, check if the preposition
           ;     has a preferred attachment to it. Note that the depending word
           ;     is not used. This is made to exploit the already existing knowledge
           ;     on noun-prep preferences stored in *NOUN-GOVERN*
                               (cond ((or (and (has-gramm-type (get-synt-word nxtline) '&number)
                                               (inh-member (get-synt-word upline) '£rule))
                                          (and (has-gramm-type (get-synt-word nxtline) '&section)
                                               (has-gramm-type (get-synt-word upline) '&section)))
                                       (setq newlinkel upline)))))))
; *** case of numbers [rule 25 january 2007]
	         ((and (eq 'NUM (get-synt-categ nxtline))
                       (null nxtlink))
                   (setq prep-dependent
                       (find-a-line 
                              `(linked-to ,(get-synt-numb nxtline)) 
                              nxtsent nxtsentlinks))
                   (cond ((inh-member (get-synt-word (first prep-dependent)) '£month)
      ; *** the movement is applied just in case the number governs a month name
           ; *** start from the preceding word and go up in the tree
                           (setq newlinkel nil)
                           (setq startsearch (first prev))
                           (setq startlink (first prevlinks))
                           (cond ((eq 'PUNCT (get-synt-categ startsearch))
                                    (setq startsearch (second prev))
                                    (setq startlink (second prevlinks))))
                           (do ((upline-link 
                                      (list startsearch startlink)
                                      (find-a-line `(position ,(first uplink)) 
                                                             savesent savelinks)))
           ; *** stop the search when the path upward is interrupted or when an
           ;     attachment point has been found
                               ((or (null (second upline-link)) newlinkel)
                                 (cond (newlinkel
		                          (setq nxtlink 
			                    (make-link (get-synt-numb newlinkel) 
                                                  'DATE-RMOD 'preposition-1))
		                          (setq savelinks 
		                              (change-lab
			                           savesent savelinks (get-synt-numb nxtline)
			                           nxtlink)))))
                               (setq upline (first upline-link))
                               (setq uplink (second upline-link))
                               (cond ((eq 'NOUN (get-synt-categ upline)) 
           ; *** if the new word in the path is a noun, check if the preposition
           ;     has a preferred attachment to it. Note that the depending word
           ;     is not used. This is made to exploit the already existing knowledge
           ;     on noun-prep preferences stored in *NOUN-GOVERN*
                                        (cond ((inh-member (get-synt-word upline) '£rule)
                                                 (setq newlinkel upline))))))))))))))

; *********************************************************************
; ****** ADD VERBAL TRACES ********************************************
; *********************************************************************
; *** This set of procedures cope with a special legal construct,
;     i.e. "di cui all'art.X" (of which at art.X), which has to be
;     interpreted as ""di cui si tratta all'art.X" (of which it is
;     talked at art.X").
; *** The procedures look for a sequence "di cui" "a ...", and then 
;     insert in the middle two word traces (for "si" and "tratta").
; *** Of course all of this applies just in case the language is "italian"
(defun add-verbal-traces (nopar-data links)
 (declare (special *LANGUAGE*))
 (cond ((neq *LANGUAGE* 'italian)
          (values nopar-data links))
       (t 
 (let (final-links final-data status prep-dependent prep-dep-line)
; *** repeat for all sentences, advancing in parallel on data and links
   (do ((nxtsent (first nopar-data) (first nopar-data))
	(nopar-data (rest nopar-data) (rest nopar-data))
	(nxtsentlinks (first links) (first links))
	(links (rest links) (rest links)))
       ((and (null nxtsent) (null nopar-data)) 
           (values (reverse final-data) (reverse final-links)))
    ; *** repeat for all lines and links of the next sentence, advancing in parallel
       (do* ((prev nil (cons nxtline prev))
	     (prevlinks nil (cons nxtlink prevlinks))
 	     (nxtline (first nxtsent) (first nxtsent))
	     (nxtsent (rest nxtsent) (rest nxtsent))
	     (nxtlink (first nxtsentlinks) (first nxtsentlinks))
	     (nxtsentlinks (rest nxtsentlinks) (rest nxtsentlinks)))
	    ((null nxtline) 
		(setq final-data (cons (reverse prev) final-data))
		(setq final-links (cons (reverse prevlinks) final-links)))
    ; *** If no link upward (new chunk)
            (cond ((null nxtlink)
      ; *** Check if next word is an unattached "di", governing "cui"
                     (cond ((has-gramm-type (get-synt-word nxtline) '&about)
                              (setq prep-dependent
                                (find-a-line 
                                       `(linked-to ,(get-synt-numb nxtline)) 
                                       nxtsent nxtsentlinks))
                              (cond ((has-gramm-type 
                                         (get-synt-word (first prep-dependent)) 
                                         '&mid-relat)  ; *** cui
           ; *** if ok, the variable 'status' records that we are just after "di cui"
                                       (setq prep-dep-line
                                             (get-synt-numb (first prep-dependent)))
                                       (setq status 'found-1))
           ; *** otherwise, reset the status
                                    (t (setq status nil))))
      ; *** Check if next word is an unattached "a" (a+ll'art.X)
                           ((has-gramm-type (get-synt-word nxtline) '&loc-to-prep)
                             (cond ((eq status 'found-1)
           ; *** if ok, and 'status' says that we are just after "di cui", insert the
           ;     traces
           ; *** this works only for "di cui", but not for "del quale", since the new lists
           ;     are built by inserting the traces after the first two lines of "prev" and
           ;     "prevlinks" (i.e. the ones of the pronoun "cui" and the preposition "di";
           ;     remember that prev and prevlinks are reversed)
                                      (setq prev
                                          (append 
                                             (list
                                                (list (list prep-dep-line 11)
                                                      #\t
                                                      '(SI PRON REFL-IMPERS ALLVAL ALLVAL 3 
                                                          LSUBJ+LOBJ+LIOBJ CLITIC)
                                                      NIL 'empty NIL) 
                                                (list (list prep-dep-line 10)
                                                      #\t
                                                      '(TRATTARE VERB MAIN IND PRES 3 SING)
                                                      NIL 'empty NIL))
                                             prev))
                                      (setq prevlinks
                                          (append 
                                              '(nil nil)
                                              prevlinks))
                                      (setq status nil))
           ; *** otherwise, reset the status
                                   (t (setq status nil))))
      ; *** if not attached, but neither "di" nor "a", reset the status
                           (t (setq status nil))))
      ; *** it has an upward link; do nothing
                  (t nil))))))))
 
; *********************************************************************
; ****** ANALYSIS OF CONJUNCTIONS *************************************
; *********************************************************************
; ***   When a coordinating conjunction is found, a movement is made both
;       forward and backward, trying to find two words of the same category
;	that can be used as conjucts. First, the procedure moves forward,
;	searching for an unlinked element, then it moves backward to search
;	for a corresponding first conjunct
(defun link-conjunctions (data links)
 (declare (special *PARSE-CONTEXT*))
 (let (final-links secondc conjoined-lines firstco)
; *** repeat for all sentences, advancing in parallel on data and links
   (do ((nxtsent (first data) (first data))
	(data (rest data) (rest data))
	(nxtsentlinks (first links) (first links))
	(links (rest links) (rest links)))
       ((and (null nxtsent) (null data)) (reverse final-links))
       (do* ((prev nil (cons nxtline prev))
	     (prevlinks nil (cons nxtlink prevlinks))
 	     (nxtline (first nxtsent) (first nxtsent))
	     (nxtsent (rest nxtsent) (rest nxtsent))
	     (nxtlink (first nxtsentlinks) (first nxtsentlinks))
	     (nxtsentlinks (rest nxtsentlinks) (rest nxtsentlinks)))
	    ((null nxtline) 
		(setq final-links (cons (reverse prevlinks) final-links)))
; *** if the current line is a coordinative conjunction
	   (cond ((or (and (eq 'SPECIAL (get-synt-categ nxtline))
                           (eq #\& (get-synt-word nxtline))
                           (null nxtlink))
                      (and (eq 'CONJ (get-synt-categ nxtline))
		           (eq (get-synt-type nxtline) 'COORD)
                           (null nxtlink))
                      (and (eq #\, (get-synt-word nxtline))
                           (has-gramm-type (get-synt-word (first prev)) '&all-pron)
                           (has-gramm-type (get-synt-word (second prev)) '&to-be)))	; "è tutto"
	           (cond ((and (has-gramm-type (get-synt-word nxtline) '&both)
                               (no-previous-correl-conj prev))
; *** if the conjunction is the first one of a correlation, link it to the first unlinked
;     item that follows
                            (setq firstco (first (find-a-line '(unlinked) nxtsent nxtsentlinks)))
                            (setq nxtlink (make-link (get-synt-numb firstco) 
                                                'COORDANTEC+CORRELAT 'conjunctions)))
; *** if at the beginning of the sentence, the conjunction is the root, and the
;     second conjunct is a verb
	                 ((and (or (null prev) (all-markers prev))
                               (eq (first *PARSE-CONTEXT*) 'sentence))
		           (setq secondc 
                                 (find-available-verb nxtsent nxtsentlinks 0))
		           (setq nxtlink (make-link 0 'TOP-CONJ 'conjunctions))
		           (setq nxtsentlinks 
		               (change-lab
			            nxtsent nxtsentlinks (get-synt-numb secondc)
			            (make-link (get-synt-numb nxtline) 
                                               (get-coord-label 2 nxtline)
                                               'conjunctions))))
; *** at the beginning, but inside a parenthesis
	                 ((and (or (null prev) (all-markers prev))
                               (neq (first *PARSE-CONTEXT*) 'sentence))
   ; *** secondc is a list of candidate lines; here, we take the first candidate
		           (setq secondc 
                             (first
			        (find-second-conj 
                                           nxtline nxtsent nxtsentlinks
                                           (first prev) 0)))
		           (setq nxtlink (make-link 0 'TOP-CONJ 'conjunctions))
		           (setq nxtsentlinks 
		               (change-lab
			            nxtsent nxtsentlinks (get-synt-numb secondc)
			            (make-link (get-synt-numb nxtline)
                                               (get-coord-label 2 nxtline)
                                               'conjunctions))))
; *** not at the beginning, but conjunction not of semtype COORD (e), DISJ (o) or
;     NEG (nè), EXPLIC (cioè) or next word is a relative pronoun: Preference for conjoned
;     sentences
		         (t (cond ((and (not (memq (get-synt-semtype nxtline) '(COORD DISJ NEG)))
                                        (or (neq (get-synt-semtype nxtline) 'ADVERS)
                                            (neq (get-synt-categ (first prev)) 'ADV)
                                            (neq (get-synt-categ (first nxtsent)) 'ADV))
                                        (or (neq (get-synt-semtype nxtline) 'EXPLIC)
                                            (has-gramm-type (get-synt-word nxtline)
                                                                     '&conj-adv-pref))
                                        (neq (get-synt-word nxtline) #\&))
		                     (setq secondc 
                                           (list-nonil
                                               (find-available-verb 
                                                         nxtsent nxtsentlinks 0)))
   ; *** if no verb found, try with standard second conjunct
				     (cond ((null secondc)
		                             (setq secondc 
			                        (find-second-conj 
                                                     nxtline nxtsent nxtsentlinks
                                                     (first prev)
                                                     0 t))))
                                   ;     (format t "second conjunct: ~a~%" secondc)
                                   ;     (break "conjunctions 1")
                                       )
   ; *** not at the beginning; double relative pronoun
                                  ((and (eq (get-synt-categ (first nxtsent)) 'PRON)
                                        (eq (get-synt-type (first nxtsent)) 'RELAT)
                                        (or (not (has-gramm-type
						 (get-synt-word (first nxtsent)) 
					         '&double-who))
                                            (has-gramm-type
						 (get-synt-word (first nxtsent)) 
					         '&base-relat)))
		                     (setq secondc 
                                          (list-nonil (find-available-verb 
                                               (rest nxtsent) (rest nxtsentlinks) 0)))
                                    ;   (format t "second conjunct: ~a~%" secondc)
                                    ;   (break "conjunctions 2")
                                  )
   ; *** standard search for a compatible second conjunct
   ;     secondc is a list of possible second conjuncts
		                  (t (setq secondc 
			                  (find-second-conj 
                                               nxtline nxtsent nxtsentlinks
                                              (first prev) 0))
                                       ; (format t "second conjunct: ~a~%" secondc)
                                       ; (break "conjunctions 3")
                                   ))
; *** the first conjunct should (but there are exceptions) be an element of the
;     same category; the final t requires that the first conjunct cannot be
;     found before a finite verb
			    (setq conjoined-lines 
				 (find-first-conj 
                                       nxtline secondc prev prevlinks nxtsent nxtsentlinks t))
; *** if the first conjunct has not been found, then try a sentence conjunction
;     the condition on 'che', to avoid the search for 'non occupa che una parte'
;     the condition on the semtype, since, in that case, a verb was already
;     looked for before
                         ;   (format t "conjoined lines: ~a~%" conjoined-lines)
                         ;   (break "conjunctions 4")
			    (cond ((and (null conjoined-lines)
		                        (or (memq (get-synt-semtype nxtline) '(COORD DISJ NEG))
                                            (eq (get-synt-word nxtline) #\&))
				        (not (has-gramm-type
                                                (get-synt-word nxtline) '&special-neg)))
		   	   	             (setq secondc 
                                                  (list-nonil 
                                                      (find-available-verb 
                                                         nxtsent nxtsentlinks 0)))
			 	 	     (setq conjoined-lines 
					          (find-first-conj 
				                       nxtline secondc prev prevlinks
                                                       nxtsent nxtsentlinks))))
; *** if now it's ok, change the current link (to point to the first conjunct)
; *** and the link of the second conjunct (to point to this conjunction)
                        ;   (format t "conjoined lines: ~a~%" conjoined-lines)
                        ;   (break "conjunctions 5")
			    (cond ((null conjoined-lines) nil)
			          (t (setq nxtlink
				        (make-link (get-synt-numb (first conjoined-lines))
                                                   (get-coord-label 1 nxtline)
                                                   'conjunctions))
				     (setq nxtsentlinks 
				        (change-lab
				            nxtsent nxtsentlinks 
				            (get-synt-numb (second conjoined-lines))
				            (make-link (get-synt-numb nxtline) 
                                                       (get-coord-label 2 nxtline)
                                                       'conjunctions)))
; *** now, try to establish if there can be a sequence of conjunctions, 
;     whose first elements are separated by commas
                                     (cond ((or (eq (get-synt-semtype nxtline) 'COORD)
                                                (eq (get-synt-word nxtline) #\&))
				              (let ((further-labs
					               (find-conj-seq 
                                                              (first conjoined-lines)
                                                              prev prevlinks)))
				                 (cond ((not (null further-labs))
					                 (setq prevlinks
				                          (mult-change-labs
					       	           prev prevlinks 
						          (reverse (build-lab-list
							        further-labs))
                                                          'conjunctions))))))))))))
; *** the next branch accounts for list of numbers (33, 34, 35)
	         ((and (equal #\, (get-synt-word nxtline))
		       (eq 'NUM (get-synt-categ (first prev)))
		       (eq 'NUM (get-synt-categ (first nxtsent))))
		    (setq nxtlink (make-link (get-synt-numb (first prev))
                                             (get-coord-label 1 nxtline)
                                             'conjunctions))
		    (setq nxtsentlinks 
			(cons (make-link (get-synt-numb nxtline)
                                         (get-coord-label 2 nxtline)
                                         'conjunctions)
			      (rest nxtsentlinks)))))))))

; *********************************************************************
; *** checks that in lines there is not a correlative conjuction
(defun no-previous-correl-conj (lines)
  (let (found)
   (do ((curline (first lines) (first lines))
        (lines (rest lines) (rest lines)))
       ((or (null curline) found)
         (not found))
       (cond ((and (eq (get-synt-categ curline) 'CONJ)
                   (has-gramm-type (get-synt-word curline) '&both))
                (setq found t))))))

; *********************************************************************
; *** this looks for a possible auxiliary before the first conjunct, which
;     is a line among the ones in allines
(defun my-find-aux (firstc allines)
  (cond ((neq (get-synt-categ firstc) 'VERB) 
            (exception 'parse-error "PROC/chunk-parser: my-find-aux 1" firstc))
        (t (do ((firstline (first allines) (first allines))
                (allines (rest allines) (rest allines)))
   ; *** if the first conjunct has not been found in prev, then stop
               ((or (null firstline)
                    (equal firstc firstline))
                  (cond ((null firstline) 
                           (exception 'parse-error "PROC/chunk-parser: my-find-aux 2"))
                        ((null allines) nil)
                        (t (let ((aux (find-first-aux firstc allines)))
   ; *** the next is needed, because in absence of auxiliaries find-first-aux
   ;     returns the main verb
                              (cond ((equal aux firstc) nil)
                                    (t aux))))))))))

; *********************************************************************
(defun all-markers (lines)
  (cond ((null lines) t)
        ((eq (get-synt-categ (first lines)) 'MARKER)
           (all-markers (rest lines)))
        (t nil)))

; *********************************************************************
; *** finds the head of the second conjunct
; *** it returns all unlinked lines until the next verb (included)
;     -- skip-verb-count is used after a subordinating conjunction or a
;        relative pronoun to skip the associated verb in the search
;     The optional "force-noverb" is used in case a non-coord (or disj,
;     explic, neg) conjunction is involved, but the sentence coordination
;     has already been attempted and failed. In this cases, the non-verbal
;     coordination must be attempted anyway
(defun find-second-conj (conjline nxtlines nxtlinks prevline skip-verb-count
                          &optional force-noverb)
 (let (rem actlines actlinks possconju foundline)
  (cond ((null nxtlines) nil)
        ((and (or (memq (get-synt-semtype conjline) '(COORD DISJ EXPLIC NEG))
                  (eq #\& (get-synt-word conjline))
                  force-noverb)
              (not (is-a-synt-clitic? (first nxtlines))))
; *** the second conjunct is the first unlinked element after the conjunction
;     except in case of clitics, which usually are not conjoined
   ; *** there is a case where an adjective is already linked; so it must be
   ;     used as second conjunct (una vera E Propria disdetta)
	   (cond ((and (eq (get-synt-categ (first nxtlines)) 'ADJ)
	               (equal (get-synt-numb conjline) 
                              (first (first nxtlinks))))
		    (list (first nxtlines)))
   ; *** in case of correlatives "both ... and ...", the second conjunct is the
   ;     first unlinked element after the conjunction (no recursion for searching
   ;     other possibilities)
	         ((has-gramm-type (get-synt-word conjline) '&both-2)
                    (setq possconju (find-a-line '(unlinked) nxtlines nxtlinks))
                    (cond ((null possconju) nil)
                          (t (list (first possconju)))))
; *** the next cond for cases as " ... e, come tale, xxx", where the material
;     between the two commas is ignored as a potential second conjunct
	         (t (cond ((eq #\, (get-synt-word (first nxtlines)))
		             (setq rem (find-a-line '(word #\,) 
					(rest nxtlines) (rest nxtlinks) 'all))
     ; *** rem has the form <<comma-line line2 line3 ...> <link1 link2 link3>>
     ;     where comma-line is the first comma after the current one
                             (setq actlines (rest (first rem)))
                             (setq actlinks (rest (second rem))))
                           (t (setq actlines nxtlines)
                             (setq actlinks nxtlinks)))
                    (setq possconju (find-a-line '(unlinked) actlines actlinks 'all))
     ; *** conju has the form <<unlinked-line line2 line3 ...> <nil link2 link3>>
     ;     where unlinked-line is the first unlinked line after the comma
                    (setq foundline (first (first possconju)))
     ; *** foundline is the unlinked line
	            (cond ((or (eq (get-synt-categ foundline) 'PUNCT)
                               (is-a-synt-clitic? foundline))
     ; *** punctuation marks and clitics are skipped
  	  	             (find-second-conj conjline 
                                 (rest (first possconju)) 
                                 (rest (second possconju)) 
                                 prevline skip-verb-count force-noverb))
	                  ((eq (get-synt-categ foundline) 'CONJ)
                             (cond ((memq (get-synt-type foundline) '(COORD COMPAR))
     ; *** a second coordinative conjunct cannot act as second conjunct
     ;     but here we skip also the next line, since it must presumably
     ;     be part of the second conjunct of the newly found conjunction
     ;     "compar" for English "than"
  	  	                     (find-second-conj conjline 
                                         (rest (rest (first possconju)))
                                         (rest (rest (second possconju)))
                                         prevline skip-verb-count force-noverb))
                                   ((eq (get-synt-type foundline) 'SUBORD)
                                     (cons foundline
  	  	                        (find-second-conj conjline 
                                            (rest (rest (first possconju)))
                                            (rest (rest (second possconju)))
                                            prevline (1+ skip-verb-count) force-noverb)))
                                   (t (exception-nothrow "Unknown conj type in find-second-conj"))))
                          ((eq 'VERB (get-synt-categ foundline))
                             (cond ((eq 0 skip-verb-count)
     ; *** in case it is a verb, stop the search
                                      (list foundline))
                                   (t (find-second-conj conjline 
                                         (rest (rest (first possconju)))
                                         (rest (rest (second possconju)))
                                         prevline (1- skip-verb-count) force-noverb))))
  	   	          (t (cons foundline
			         (find-second-conj conjline
                                        (rest (first possconju)) 
                                        (rest (second possconju)) 
                                        nil skip-verb-count force-noverb)))))))
; *** the next for "was slowly but constantly approaching"
        ((and (eq (get-synt-semtype conjline) 'ADVERS)
              (eq (get-synt-categ prevline) 'ADV)
              (eq (get-synt-categ (first nxtlines)) 'ADV)
              (eq (get-synt-categ (second nxtlines)) 'VERB))
          (list (first nxtlines)))
; *** but if the semantic type is not 'coord' (and) or 'disj' (or), explic (ossia),
;     then it is preferred to link two sentences, so the second conjunct is a verb
	 (t (list-nonil (find-available-verb nxtlines nxtlinks 0))))))

; *********************************************************************
; *** this looks for a verb which can act as first head; it is used, for
;     instance, for finding a second conjunct
(defun find-available-verb (lines links depth)
 (let ((lcateg (get-synt-categ (first lines))))
  (cond ((null lines) nil)
        ((eq lcateg 'VERB)
          (cond ((eq depth 0)
                  (cond ((null (first links)) (first lines))
                        ((member (get-synt-type (first lines)) '(AUX MOD))
                          (find-available-verb (rest lines) (rest links) depth))
                        (t nil)))
                ((member (get-synt-type (first lines)) '(AUX MOD))
                   (find-available-verb (rest lines) (rest links) depth))
                (t (find-available-verb (rest lines) (rest links) (1- depth)))))
        ((or (and (eq lcateg 'CONJ) (eq (get-synt-type (first lines)) 'SUBORD))
             (and (eq lcateg 'PRON) (eq (get-synt-type (first lines)) 'RELAT)))
          (find-available-verb (rest lines) (rest links) (1+ depth)))
        (t (find-available-verb (rest lines) (rest links) depth)))))
    
; *********************************************************************
; *** finds the head of the first conjunct
;     it returns the line of the first conjunct and the line of the chosen
;     second conjunct
(defun find-first-conj (conjline secondc prevlines prevlinks nextlines nextlinks
                        &optional nocrossv)
  (cond ((null secondc) nil)
        ((eq (get-synt-categ (first secondc)) 'ADV)
           (cond ((eq (get-synt-categ (first prevlines)) 'ADV)
               ; *** if the conjunction is between two advs, they are assumed to
               ;     be the conjoined elements
                    (list (first prevlines) (first secondc)))
                 (t (find-first-conj conjline (rest secondc) 
                                  prevlines prevlinks nextlines nextlinks nocrossv))))
        (t (let* ((firstconj 
                    (int-find-first-conj conjline
                               (first secondc) prevlines prevlinks nextlines nextlinks nocrossv))
    ; *** firstconj is a pair <line-of-first-conjunct conjunction-type>
    ;     See int-find-first-conj for the conjunction types
                  (bestpair 
                     (list (first firstconj) (first secondc) (second firstconj)))
                  nextpair)
               (do ((nxtsecond (second secondc) (first secondc))
                    (secondc (rest (rest secondc)) (rest secondc)))
                   ((null nxtsecond) 
                      (cond ((null (first bestpair)) nil)
                        ; *** for no second conjunct, a possible first conjunct has been found
                            (t bestpair)))
                   (setq nextpair
                      (int-find-first-conj conjline
                                 nxtsecond prevlines prevlinks nextlines nextlinks nocrossv))
                   (setq bestpair
                      (choose-best-conj conjline bestpair
                          (list (first nextpair) nxtsecond (second nextpair)))))))))

; *********************************************************************
; *** Compares two pairs of conjuncts, in order to choose the best one 
;     bestpair and nextpair are actually triples, whose first element is the
;     candidate first conjunct, the second element is the candidate second
;     conjunct, and the third element is the conjunction type (see int-find-first-conjunct)
(defun choose-best-conj (conjline bestpair nextpair)
  ; *** in case the pattern is:
  ;     ... candidate1st1 ... candidate1st2 (verb) ...  candidate2nd1 ... candidate2nd2 (verb) ...
  ;     then, if the two verbs are compatible, nextpair is chosen
  (let* ((firstbest (first bestpair))
         (secondbest (second bestpair))
         (typebest (third bestpair))
         (firstnext (first nextpair))
         (secondnext (second nextpair))
         (typenext (third nextpair))
         (firstbestcateg (get-synt-categ firstbest))
         (secondbestcateg (get-synt-categ secondbest))
         (firstnextcateg (get-synt-categ firstnext))
         (secondnextcateg (get-synt-categ secondnext))
         (firstbestlinumb (get-synt-linumb firstbest))
         (secondbestlinumb (get-synt-linumb secondbest))
         (firstnextlinumb (get-synt-linumb firstnext))
         (secondnextlinumb (get-synt-linumb secondnext)))
     (cond ((or (null firstbest) (null secondbest)) nextpair)
           ((or (null firstnext) (null secondnext)) bestpair)
   ; *** first criterium: adjacency
           ((eq (third bestpair) 'adjacent) bestpair)
           ((eq (third nextpair) 'adjacent) nextpair)
   ; *** second criterium: ordseq identifier sequences as "a-bis and b-ter"
           ((and (eq firstbestcateg 'SPECIAL)
                 (eq secondbestcateg 'SPECIAL)
                 (index-prevline (get-synt-linumb conjline) secondbestlinumb)
                 (< (- (get-synt-linumb conjline) firstbestlinumb) 4))
             bestpair)
           ((and (eq firstnextcateg 'SPECIAL)
                 (eq secondnextcateg 'SPECIAL)
                 (index-prevline (get-synt-linumb conjline) secondnextlinumb)
                 (< (- (get-synt-linumb conjline) firstnextlinumb) 4))
             nextpair)
   ; *** third criterium: NP's not too far away from each other
           ((and (memq firstbestcateg '(ART ADJ NOUN))
                 (memq secondbestcateg '(ART ADJ NOUN))
                 (< (- secondbestlinumb firstbestlinumb) 5))
             bestpair)
           ((and (memq firstnextcateg '(ART ADJ NOUN))
                 (memq secondnextcateg '(ART ADJ NOUN))
                 (< (- secondnextlinumb firstnextlinumb) 5))
             nextpair)
   ; *** fourth criterium: it refers to a situation as
   ;     art ... prep ... conj art ... prep ...
           ((and (memq firstbestcateg '(ART ADJ NOUN))
                 (memq secondbestcateg '(ART ADJ NOUN))
                 (eq (third nextpair) 'prep-prep-opt)
                 (index-precedes firstbestlinumb firstnextlinumb)
                 (index-precedes secondbestlinumb secondnextlinumb)
                 (index-prevline (get-synt-linumb conjline) secondbestlinumb))
             bestpair)
           ((and (memq firstnextcateg '(ART ADJ NOUN))
                 (memq secondnextcateg '(ART ADJ NOUN))
                 (eq (third bestpair) 'prep-prep-opt)
                 (index-precedes firstnextlinumb firstbestlinumb)
                 (index-precedes secondnextlinumb secondbestlinumb)
                 (index-prevline (get-synt-linumb conjline) secondnextlinumb))
             nextpair)
   ; *** fifth criterium: "The things you said and Those that ..."
           ((eq (third bestpair) 'art-prondem) bestpair)
           ((eq (third nextpair) 'art-prondem) nextpair)
   ; *** sixth criterium: any pair of prepositions
           ((eq (third bestpair) 'prep-prep-opt) bestpair)
           ((eq (third nextpair) 'prep-prep-opt) nextpair)
   ; *** seventh criterium: np conjunction wrt different preps
           ((and (eq (third bestpair) 'differ-prep)
                 (eq (third nextpair) 'standard))
              nextpair)
           ((and (eq (third nextpair) 'differ-prep)
                 (eq (third bestpair) 'standard))
              bestpair)
   ; *** eighth criterium: homogeneous verbs
           ((and (eq firstnextcateg 'VERB)
                 (eq secondnextcateg 'VERB)
                 (eq (get-synt-mood firstnext) (get-synt-mood secondnext))
                 (eq (get-synt-tense firstnext) (get-synt-tense secondnext))
                 (index-precedes firstbestlinumb firstnextlinumb)
                 (index-precedes firstnextlinumb secondbestlinumb)
                 (index-precedes secondbestlinumb secondnextlinumb))
              nextpair)
           (t bestpair))))

; *********************************************************************
; *** find the head of the first conjunct
; >>> INPUT:
;   - conjline is the line of the conjunction
;   - prevlines and prevlinks the lines and links before the conjunction
;   - nextlines and nextlinks the lines and links after the conjunction
; *** it returns a triple, whose first element is the line of the first conjunct,
;     the second element is the line of the second conjunct, and the third one
;     is an atom that identifies the found situation. This should help 
;     choose-best-conj
; *** The possible identifiers are:
;     adjacent: two words of the same category surrounding the conjunction
;     verb-verb-opt: two conjoined verbs in optimal situation (same mood and tense)
;     verb-verb-notopt: two conjoined verbs in suboptimal situation (different mood and tense)
;     verb-phras: a verb conjoined with a phrasal element (to go or no)
;     noun-adjdeitt: a noun conjoined with a deictic or demonstrative adjective
;                    (cars and other vehicles)
;     num-adjfollow: a num conjoined with "following" (article 137 and following)
;     noun-advbeyond: Los Angeles and beyond (a construction found in a project)
;     prep-prep-opt: same preposition governing same categories (di correre e di giocare)
;     differ-prep: different prepositions (with Mary and for John)
;     prep-diffgov: same preposition governing different categories (di Marco e dei ragazzi)
;     pron-np: a pronoun conjoined with a noun complex (he and Mary, you and the boys)
;     art-prondem: a NP conjoined with a demonstrative (quello, that)
;     standard: any other conjuncts having the same head category
(defun int-find-first-conj (conjline secondc prevlines prevlinks nextlines nextlinks
                            &optional nocrossv)
; *** the separation between prev (lines and links) and next (lines and links)
;     is the conjunction
  (declare (special *PREFER-CONJ-ATTACH*))
  (let ((allines (append (reverse prevlines) (list conjline) nextlines))
        (allinks (append (reverse prevlinks) (list nil) nextlinks))
        (secondcat (get-synt-categ secondc))
        (secondtype (get-synt-type secondc))
        (prevcat (get-synt-categ (first prevlines)))
        (conjnumb (get-synt-numb conjline))
        (secondnumb (get-synt-numb secondc))
        act-mood tempverb foundline)
; *** the first branch for "Buonasera e ...". Independently of the second
;     conjunct, the first conjunct is set to "buonasera" (goodevening)
;     "adjacent" is set, just because it is the top criterium
    (cond ((and (eq prevcat 'PHRAS)
                (has-gramm-type (get-synt-word (first prevlines)) '&greetings)
                (null (second prevlines)))
             (list (first prevlines) 'adjacent))
; *** if the second conjunct is an adverb, and the item before the conjunction is
;     not an adverb, no coordination
          ((and (eq secondcat 'ADV)
                (neq prevcat 'ADV))
            nil)
; *** if the second conjunct is adjacent to the conjunction, and the previous word
;     has the same category, then this is the best choice
          ((and (numberp conjnumb)
                (numberp secondnumb)
                (= secondnumb (1+ conjnumb))
                (eq prevcat secondcat))
       ; *** unless we are in the situation "Stato di obsolescenza e locazione", where
       ;     "locazione" is the second conjunct of "stato" and not of "obsolescenza"
              (cond ((and (eq secondcat 'NOUN)	  ; first conjuncts both nouns
                          (eq prevcat 'NOUN)
                          (eq (get-synt-categ (second prevlines)) 'PREP)
                          (eq (get-synt-categ (third prevlines)) 'NOUN)
                          (member (list (get-synt-word (third prevlines))
                                        (get-synt-word (first prevlines)))
                                  (leggi *PREFER-CONJ-ATTACH* 
                                                       (get-synt-word secondc))
                                  :test #'equal))
                      (list (third prevlines) 'adjacent))
                    (t (list (first prevlines) 'adjacent))))
; *** if the second conjunct is the head of a greetings noun group (as "buona serata")
;     and the previous line is a PHRAS (as "grazie") set the first conjunct to that
;     previous line
          ((and (eq secondcat 'NOUN)
                (check-noun-greeting secondc nextlines nextlinks)
                (eq (get-synt-categ (first prevlines)) 'PHRAS))
             (list (first prevlines) 'adjacent))
; *** in case of correlations, the first conjunct must be the first unlinked item after
;     the first conj of the correlation (both THE boys and the girls)
;     has the same category, then this is the best choice
          ((has-gramm-type (get-synt-word conjline) '&both-2)
            (do ((curline (first prevlines) (first prevlines))
                 (prevlines (rest prevlines) (rest prevlines))
                 (curlink (first prevlinks) (first prevlinks))
                 (prevlinks (rest prevlinks) (rest prevlinks)))
                ((or (null curline)
                     (and (eq (get-synt-categ curline) 'CONJ)
                          (has-gramm-type (get-synt-word curline) '&both)))
                   (cond ((null curline) nil)
                         (t (list foundline 'CORRELAT))))
                (cond ((null curlink)
                         (setq foundline curline)))))
          ((eq secondcat 'VERB)
; *** if it is a verb, it must also be of the same type (but mains and modals
;     are interchangeable) and of the same mood
      ;(break "int-find-first-1")
	    (let* ((lines-before-second (find-prec-lines secondc nextlines))
                   (foundline
	    	      (cond ((eq (get-synt-type secondc) 'AUX)
			       (find-a-line 
			          `(categ (VERB) type (AUX) mood ,(get-synt-mood secondc))
			          prevlines prevlinks t))
		            (t (setq act-mood 
                                   (get-synt-mood (find-first-aux secondc 
                                                      (reverse lines-before-second))))
			       (find-a-line 
			             `(categ (VERB) type (MAIN MOD) actmood ,act-mood)
			              prevlines prevlinks t))))
                   (relat-verb (find-prec-relpron (reverse lines-before-second)))
                   possfirst)
       ;(break "int-find-first-2")
     ; *** the first of foundline are the lines, the second the links
	 	(setq possfirst (first (first foundline)))
		(cond ((null possfirst)
; *** if no previous verb found, relax the constraints on type and mood
		 	 (setq foundline (find-a-line `(categ (verb)) prevlines prevlinks t))
		 	 (setq possfirst (first (first foundline)))))
                (cond ((null possfirst) nil)
                      (relat-verb 
                         (cond ((find-prec-relpron (rest (first foundline)))
                                 (list possfirst 'verb-verb-notopt))
                               (t nil)))
		      (t (list possfirst 'verb-verb-opt)))))
; *** for "l'opportunità di pubblicare o no"
          ((eq secondcat 'PHRAS)
	    (let ((possfirst
                    (first (find-a-line `(categ (verb) type (main mod))
			    prevlines prevlinks))))
                 (cond ((null possfirst) nil)
                       (t (list possfirst 'verb-phras)))))
; *** for "telefono o altro apparecchio equivalente"
          ((and (eq secondcat 'ADJ)
	        (or (memq (get-synt-type secondc) '(deitt demons))
                    (and (is-indef-determiner secondc)
                         (not (is-pron+np secondc (first prevlines) (second prevlines)))))
	        (not (has-gramm-type (get-synt-word secondc) '&follow)))
	    (let ((possfirst+lines (find-a-line `(categ (noun)) prevlines prevlinks t))
	           possline posslink chunk-head)
                 (cond ((null possfirst+lines) nil)
	               (t (setq possline (first (first possfirst+lines)))
	                  (setq posslink (first (second possfirst+lines)))
      ; *** even if an antecedent is found, another attempt is made to look for a 
      ;     better solution for cases as 'un temporale sul mare, qualche pioggia altrove'
      ;     where the first solution would be to link 'qualche' to 'pioggia', while it
      ;     is better to link it to 'un'
                          (setq chunk-head
                             (chunk-head-cat 'backward nil nil possline posslink
                                 (rest (first possfirst+lines))
                                 (rest (second possfirst+lines))
                                 allines allinks 'any nil 'no-coord))
                          (cond ((and (is-indef-determiner secondc)
                                      (is-indef-determiner chunk-head))
                                  (list chunk-head 'standard))
                                (t (list possline 'noun-adjdeitt)))))))
; *** for "132 e seguenti" (see the legal corpus)
          ((and (eq secondcat 'ADJ)
	        (has-gramm-type (get-synt-word secondc) '&follow)
         ; *** N.B. This applies to the lemma "seguente", not to the code "ss."
	        (eq prevcat 'num))
           ;  (break "followok")
	     (list (first prevlines) 'num-adjfoll))
; *** for "Torino e oltre" (see the HOPS corpus)
          ((and (eq secondcat 'ADV)
	        (has-gramm-type (get-synt-word secondc) '&beyond)
	        (eq prevcat 'noun))
	     (list (first prevlines) 'noun-advbeyond))
; *** in case of a preposition, look for another line with the same preposition
          ((eq secondcat 'PREP)
	    (let* ((firstres 
                      (find-a-line `(word ,(get-synt-word secondc)
                                      not-trace not-compos)
				 prevlines prevlinks t nil))
      ; *** firstline is the line of the found first conjunct
      ; *** firstlink is its link
      ; *** remlines are all lines remained after the search (i.e. the ones preceding firstline)
      ; *** remlinks are their links
                   (firstline (first (first firstres)))
                   (firstlink (first (second firstres)))
                   (remlines (rest (first firstres)))
                   (remlinks (rest (second firstres)))
      ; *** in "gov-by-first" the main dependent of the possible prepositional first conjunct
                   (gov-by-first (find-prep-dependent firstline prevlines prevlinks))
                   (gov-by-second (find-prep-dependent secondc nextlines nextlinks)))
                 ;(setq *print-level* 100)
                 ;(setq *print-length* 100)
                 (cond ((or (null firstline)
; *** if there is no occurrence of the same preposition, take any other preposition
			    (and (eq 'pron (get-synt-categ gov-by-first))
			         (eq 'relat (get-synt-type gov-by-first))))
   ; *** the second condition, since the first conjunct cannot govern a relative
   ;     pronoun
                          (setq firstres
	                      (find-a-line '(categ (prep) not-trace not-compos)
				      prevlines prevlinks nil nil))
                          (setq gov-by-first 
                               (find-prep-dependent (first firstres) prevlines prevlinks))
                          (cond ((or (null firstres)
   ; *** the second condition, since the first conjunct cannot govern a relative
   ;     pronoun
			             (and (eq 'pron (get-synt-categ gov-by-first))
			                  (eq 'relat (get-synt-type gov-by-first))))
                                  nil)
                                (t (list (first firstres) 'differ-prep))))
   ; *** if the match has been found, but it is not very good, look for something
   ;     better; this is the case of "di parlare dei ragazzi e di correre", where
   ;     in the first conjunct found the governed word is "de-i", while in the
   ;     second it is a verb ("correre")
                       (t (let ((catdepfirst (get-synt-categ gov-by-first))
                                (catdepsecond (get-synt-categ gov-by-second)))
                              (cond ((or (and (eq 'verb catdepfirst)
                                              (neq 'verb catdepsecond))
                                         (and (neq 'verb catdepfirst)
                                              (eq 'verb catdepsecond)))
                                       (setq firstres
                                            (find-a-line `(word ,(get-synt-word secondc)
                                                             not-trace not-compos)
				                    remlines remlinks nil nil))
                                       (setq gov-by-first 
                                            (find-prep-dependent (first firstres) prevlines prevlinks))
                                       (setq catdepfirst (get-synt-categ gov-by-first))
                                       (cond ((or (null gov-by-first) 
                                                  (and (eq 'verb catdepfirst)
                                                       (neq 'verb catdepsecond))
                                                  (and (neq 'verb catdepfirst)
                                                       (eq 'verb catdepsecond)))
   ; *** if the new search has not solved the problem, keep the first solution
                                                (list firstline 'prep-diffgov))
                                             (t (list (first firstres) 'prep-prep-opt))))
                                    (t (list firstline 'prep-prep-opt))))))))
; *** the next cond is an attempt to handle "she and the dog", "he and Mary"
;     and "a bottle and some glasses"
          ((or (is-indef-determiner secondc)
               (is-pron+np secondc (first prevlines) (second prevlines)))
             (let (foundconj)
                (cond ((is-indef-determiner secondc)
       ; *** this is for conjunctions as "a bottle and some glasses"
	                (setq foundconj (first (find-a-line `(indef-det nil not-trace not-compos)
				      prevlines prevlinks nil nil)))))
                 (cond ((null foundconj)
                         (cond ((is-pron+np secondc (first prevlines) (second prevlines))
       ; *** this is for "she and the dog", "he and Mary"
                                  (list (first prevlines) 'pron-np))
                               (t nil)))
                       (t (list foundconj 'standard)))))
; *** the next branch for "le disposizioni dell'articolo e quelle ...."
          ((and (eq secondcat 'PRON)
                (has-gramm-type (get-synt-word secondc) '&that))
	     (let ((foundconj 
                      (first 
                         (find-a-line 
                             `(categ (art) 
                               agree ((gender ,(get-synt-gender secondc)) 
                                      (number ,(get-synt-number secondc)))
                               not-trace not-compos)
			      prevlines prevlinks nil nil))))
                 (cond ((null foundconj) nil)
                       (t (list foundconj 'art-prondem)))))
          ((eq secondcat 'CONJ)
 ; *** if the second conjunct is a conjunction, it is assumed it is a subordinating conjunction,
 ;     so look for an element of the same type. Otherwise, exception
             (cond ((eq secondtype 'subord)
	              (let ((foundconj 
                               (first (find-a-line 
                                          `(categ (,secondcat) type (subord) not-trace not-compos)
				          prevlines prevlinks nil nil))))
                          (cond ((null foundconj) nil)
                                (t (list foundconj 'standard)))))
                   (t (exception-nothrow "Conj coord as second conjunct"))))
	  (t (let ((foundconj (first (find-a-line `(categ (,secondcat) not-trace not-compos)
				      prevlines prevlinks nil nil))))
; *** otherwise, conjunction without any special treatment between elements of same category
; *** not-compos to avoid using the article in the preposition+article as first
;     conjunct in "le case dei poveri e i palazzi"
                 (cond ((null foundconj) nil)
                       (t (list foundconj 'standard)))))
    )))

; *********************************************************************
(defun check-noun-greeting (secondc lines links)
   (let ((lemma (get-synt-word secondc)) sec-deps)
       (cond ((or (inh-member lemma '£daytime-period)
                  (inh-member lemma '£day))
               (setq sec-deps (first (find-dependents secondc lines links)))
               (and (eq (length sec-deps) 1)
                    (has-gramm-type (get-synt-word (first sec-deps)) '&greeting-adj)))
             (t nil))))

; *********************************************************************
; *** this checks if the previous line is a pronoun (or a proper name)
;     and after the conjunction there is an NP: "he and a friend"
(defun is-pron+np (secondc prevline prev2line)
  (let ((secondcat (get-synt-categ secondc))
        (prevcat (get-synt-categ prevline)))
     (and (or (eq secondcat 'ART)
              (and (eq secondcat 'NOUN)
                   (eq (get-synt-type secondc) 'PROPER)))
          (or (and (eq prevcat 'PRON)
                   (not (is-a-synt-trace? prevline))
	           (numberp (get-synt-numb prevline))) ; not an enclitic
              (and (eq prevcat 'NOUN)
                   (eq (get-synt-type prevline) 'PROPER)))
              (not (and (eq secondcat 'art)    ; to block application to "gli uni e gli altri"
                        (eq (get-synt-categ prev2line) 'ART))))))

; *********************************************************************
; *** this checks if line contains something as "a", "some", "any", ...
(defun is-indef-determiner (line)
  (or (and (eq (get-synt-categ line) 'ART)
           (eq (get-synt-type line) 'INDEF))
      (and (eq (get-synt-categ line) 'ADJ)
           (eq (get-synt-type line) 'INDEF))))

; *********************************************************************
; *** returns t if, in prevlines, there is a relative pron not followed
;     (preceded, since prevlines is reversed) by a verb
(defun find-prec-relpron (prevlines)
  (cond ((null prevlines) nil)
        ((and (eq (get-synt-categ (first prevlines)) 'PRON)
              (eq (get-synt-type (first prevlines)) 'RELAT))
           t)
        ((and (eq (get-synt-categ (first prevlines)) 'VERB)
              (eq (get-synt-type (first prevlines)) 'MAIN))
           nil)
        (t (find-prec-relpron (rest prevlines)))))

; *********************************************************************
; *** this tries to determine if, before a coordination, there is a sequence of
;     conjuncts separated by commas
; *** INPUT:
;   >>> firstconj: a word, which is the current first conjunct's head; it remains
;       the first conjunct, in case this function does not find any previous
;       conjunct; this correspond to the head of 'conjunct2' in METHOD below
;   >>> prevlines: the lines before the conjunction (which include firstconj)
;       this corresponds to the lines composing 'conjunct2' in METHOD below
;   >>> prevlinks: the links of the lines in 'prevlines'
; *** OUTPUT:
;   >>> a list <headline1 comma1 headline2 comma2 ... headlineN>
;       representing the sequence of comma-conjunctions and heads of conjuncts
;       The list should be of odd length, since the first element aims only at
;       establishing the first link, while the others represent the pairs
;       <COORD COORD2ND>. The list can be nil
; *** VARIABLES:
;   >>> possconjunct (and possconjlink): a sequence of lines (links) constituting
;       a possible previous conjunct ('conjunct1', in METHOD below)
;   >>> conjseq (and linkseq): the sequence of commas and second conjunct heads
;       that are the partial result of the search
;   >>> conjunct (and conjlink): the sequence of lines (links) between the
;       conjunction and a possible previous comma. These data are the basis for
;       looking for further conjuncts
; *** METHOD:
;   >>> if we have <conjunct1, conjunct2 and conjunct3>, the function puts in
;       'conjunct' and 'conjlink' the data about 'conjunct2' (this happens until
;       in status 'start'); then puts in 'conjstruct' a description of the
;       structure of conjunct2'; after that it passes in status 'foundlast' and
;       proceeds backward, putting in 'possconjunct' and 'possconjlink' the data
;       about 'conjunct1'. At the end, it finds out the structure of conjunct1
;       and compares it with the one of 'conjunct2'. If they are compatible,
;       'conjseq' and 'linkseq' are extended, and the last step is repeated to
;       see if there is any previous conjunct.
; *** STATES:
;   >>> start: going back from the conjunction to find out 'conjunct2' (see above)
;   >>> foundlast: going back from a comma to find out 'conjunct1' (see above),
;       and any possible previous conjuncts
;   >>> stop: end of search
(defun find-conj-seq (firstconj prevlines prevlinks)
  (let ((status 'start) conjseq linkseq conjunct conjlink possconjunct 
        possconjlink conjstruct beforeconj)
  ; *** the next cond to handle cases such as "Bill, John, and Mary", where the
  ;     comma before the conjunction must be skipped
   (cond ((and (eq 'punct (get-synt-categ (first prevlines)))
               (eq #\, (get-synt-word (first prevlines))))
           (setq prevlines (rest prevlines))
           (setq prevlinks (rest prevlinks))))
   (do ((curline (first prevlines) (first prevlines))
        (curlink (first prevlinks) (first prevlinks))
        (prevlines (rest prevlines) (rest prevlines))
        (prevlinks (rest prevlinks) (rest prevlinks)))
; *** at the end, if conjseq is empty or if the original first conjunct overlaps
;     with the sequence found, return nil (failure), otherwise return the sequence
;     of heads of the conjuncts
       ((eq status 'stop)
	  (let ((lastline (get-synt-numb (ult conjseq))))
	   (cond ((null lastline)
		    nil)
	         ((or (index-precedes (get-synt-numb firstconj) lastline)
                      (and (eq conjstruct 'attached-relcl)
                           (eq #\, (get-synt-word (ult conjseq)))))
        ; *** the second disjunct for the case of failure of match for "attached-relcl"
        ;     In this case, the last comma, associated with a possible continuation
        ;     of the sequence, must be removed
		    (butlast conjseq))
	   	 (t (append1 conjseq firstconj)))))
; %%% status=start means we are going back from the coordination to find all
;     material until the previous comma. These data are collected in conjseq
;     (and conjlink), and are the basis for all following comparisons
     (cond ((eq status 'start)
	     (cond ((null curline)
		     (setq status 'stop))
		   ((and (eq (get-synt-categ curline) 'PUNCT)
                         (not (null firstconj))
                         (index-precedes (get-synt-numb curline)
                                         (get-synt-numb firstconj)))
	     	     (cond ((eq (get-synt-word curline) #\,)
   ; *** now, we have determined all material between the conjunction and a
   ;     previous comma (which is in 'conjunct'). The comma is saved in conjseq
   ;     If 'conjunct' is empty, then nothing has been found, and stop
			      (cond ((null conjunct) (setq status 'stop))
          ; *** the next conjunct to account for the case where a comma
          ;     immediately precedes a conjunction. In this case, it must
          ;     be handled as any other item
                                    (beforeconj
                                       (setq beforeconj nil)
		                       (setq conjunct (append1 conjunct curline))
		                       (setq conjlink (append1 conjlink curlink)))
				    (t (setq conjseq (cons curline conjseq))
				       (setq linkseq (cons curlink linkseq))
          ; *** in conjseq and linkseq the sequence of commas
          ;     the next determines the structure of the material contained in
          ;     conjunct; the third argument specifies the already determined first 
          ;     conjunct of the last pair. The fourth one says if we are working on 
          ;     that conjunct or not. This is used in case of verbs
                                       (setq conjstruct
                                           (find-conj-struct conjunct conjlink firstconj t))
                                       (cond ((eq conjstruct 'attached-relcl)
		      	      	                 (setq status 'stop))
		      	      	             (t (setq status 'foundlast))))))
   ; *** punctuation marks other than comma stop the search
			   (t (setq status 'stop))))
   ; *** if it is not a punctuation, extend the conjunct
   ;     but if it is a conjunction, set beforeconj, in order to ignore a possible
   ;     preceding comma
		   ((and (eq 'CONJ (get-synt-categ curline))
                         (eq 'COORD (get-synt-type curline)))
                      (setq beforeconj t)
                      (setq conjunct (append1 conjunct curline))
		      (setq conjlink (append1 conjlink curlink)))
   ; *** if it is not a punctuation, extend the conjunct
		   (t (setq beforeconj nil)
                      (setq conjunct (append1 conjunct curline))
		      (setq conjlink (append1 conjlink curlink)))))
           ((eq status 'foundlast)
; %%% status=foundlast means we already have in 'conjunct' the (assumed) words
;     of the first conjunct and we are moving back to find another comma
	     (cond ((or (null curline)
			(and (eq (get-synt-categ curline) 'PUNCT)
                             (not (and beforeconj
                                       (eq (get-synt-word curline) #\,)))))
   ; *** now, we have determined all material between the previous comma, and
   ;     another preceding punct (which is in 'possconjunct'). We have to verify
   ;     if it is compatible with 'conjunct'
   ;     The third argument (firstconj) specifies if we are looking for verbal
   ;     conjuncts (as determined by the analysis of the actual - ending -
   ;     conjunction ('z' in 'x, y, z and w')); the fourth one says that we are not
   ;     working now on it (i.e., we are on x or y and not on z)
                     (setq beforeconj nil)
		     (let ((newconjstruct
                              (find-conj-struct possconjunct possconjlink firstconj nil))
                           match-result)
                        (cond ((eq newconjstruct 'attached-relcl)
                                 (setq match-result nil))
                              (t (setq match-result 
                                    (synt-compatible-conjuncts conjstruct newconjstruct))))
   ; *** match-result is nil (no match) or a triple whose first element is 'full'
   ;     or 'partial'. In case, it is 'full', then the match is perfect, and the
   ;     process can continue. Otherwise, the match was of the type yyyxxxx (new
   ; 	 material) against xxxx (conjunct). This means that the match is ok, but
   ;     it is useless to continue; the third element is the link sequence.
	     	        (cond ((and (or (null curline)
                                        (eq (get-synt-word curline) #\,))
 				    (eq (first match-result) 'full)) 
   ; *** full match
                                (cond ((null curline)
   ; *** if at end of sentence, update conjseq, linkseq, and then stop
		      		         (setq conjseq
				             (cons (second match-result) conjseq))
		      		         (setq linkseq
				             (cons (third match-result) linkseq))
                                         (setq status 'stop))
   ; *** otherwise update conjseq and linkseq, reset possconjunct, and go on
		      		     (t (setq conjseq
				           (append
                                              (list curline (second match-result))
					      conjseq))
		      		        (setq linkseq
				           (append 
                                              (list curlink (third match-result))
					      linkseq))
				        (setq possconjunct nil)
				        (setq possconjlink nil)
                                        (setq conjstruct newconjstruct))))
   ; *** partial match: update conjseq, and stop
 			      ((eq (first match-result) 'partial) 
		      		(setq conjseq
				    (cons (second newconjstruct) conjseq))
		      		(setq linkseq
				    (cons (third newconjstruct) linkseq))
				(setq status 'stop))
   ; *** no match: stop; during the loop, conjseq includes a trailing comma,
   ; 	 used to open a new conjunct; if it is not found, the comma must be
   ;	 canceled 
			      (t (setq conjseq (rest conjseq))
			         (setq linkseq (rest linkseq))
			         (setq status 'stop)))))
   ; *** if it is not a punctuation, extend the conjunct
		   ((and (eq 'conj (get-synt-categ curline))
                         (eq 'coord (get-synt-type curline)))
                      (setq beforeconj t)
		      (setq possconjunct (append1 possconjunct curline))
		      (setq possconjlink (append1 possconjlink curlink)))
		   (t (setq beforeconj nil)
                      (setq possconjunct (append1 possconjunct curline))
		      (setq possconjlink (append1 possconjlink curlink)))))))))

; *********************************************************************
; *** checks if two potential conjuncts are compatible. 
;     We already have available the type of structure (see find-conj-struct
;     below) and the list of dependents of the head (and thir links)
;     This only checks compatibility
;     'full' or 'partial' and whose second element is the head of the newly found
;     conjunct
; *** INPUT:
;   >>> two lists describing the structure of the conjuncts to compare.
;       each list has the form of the output of find-conj-struct below, i.e.
;       1. the head line of the structure
;       2. the type of structure (NG: nominal, cat: the category of any other
;          head (ex. ADJ, VERB, etc.))
;       3. the dependents found (according to the criteria above)
;       4. their links
(defun synt-compatible-conjuncts (conjstruct newconjstruct)
   (let ((conjhead (first conjstruct))
         (newconjhead (first newconjstruct))
         (conjtype (second conjstruct))
         (newconjtype (second newconjstruct)))
      (cond ((neq conjtype newconjtype)
               (let ((depcats (mapcar #'get-synt-categ (third conjstruct)))
                     (newdepcats (mapcar #'get-synt-categ (third newconjstruct))))
      ; *** the next cond to handle the case "manufatturiere, di servizi e di produzione"
                  (cond ((or (and (eq 'adj newconjtype)
                                  (eq 'prep conjtype)
                                  (has-gramm-type (get-synt-word conjhead) '&neutral-prep)
                                  (not (member 'verb depcats)))
                             (and (eq 'adj conjtype)
                                  (eq 'prep newconjtype)
                                  (has-gramm-type (get-synt-word newconjhead) '&neutral-prep)
                                  (not (member 'verb newdepcats))))
                           `(full ,newconjhead))
  ; *** the type of structure is not the same: nil
                        (t nil))))
            ((eq 'verb conjtype)
  ; *** compatibility for verbal structure is defined as: having the same mood
  ;     and tense and both having or both not having an auxiliary
               (let ((newmood (get-synt-mood newconjhead))
                     (newtense (get-synt-tense newconjhead))
                     (oldmood (get-synt-mood conjhead))
                     (oldtense (get-synt-tense conjhead))
                     (newaux (third newconjstruct))
                     (oldaux (third conjstruct)))
                  (cond ((and (eq newmood oldmood)
                              (eq newtense oldtense)
	; *** the third element of conjstruct specifies (for verbs) the possible
	;     auxiliary
                              (or (and (null oldaux)
                                       (null newaux))
                                  (and (not (null oldaux))
                                       (not (null newaux)))))
                           `(full ,newconjhead))
                        (t nil))))
            ((compat-conj-deps (second conjstruct)
  ; *** if type ok, then compare the dependents
                              (third conjstruct) (third newconjstruct))
               `(full ,(first newconjstruct)))
            (t nil))))

; *********************************************************************
; *** this finds the topmost element of a fragment of sentence
; *** INPUT:
;  >>> conjlines: the lines among which the head must be searched (in reverse order)
;  >>> conjlinks: their links (in reverse order)
;  >>> firstconjhead: the line of the head of the first conjunct
;         This is used just to check if we are looking for verbal conjuncts
;  >>> is-firstconj?: true, if we are working on the first conjunct of the last pair
;                     false, otherwise
; *** OUTPUT:
;   >>> a list including:
;       1. the head line of the structure
;       2. the type of structure (NG: nominal, cat: the category of any other
;          head (ex. ADJ, VERB, etc.))
;       3. the dependents found (according to the criteria above)
;       4. their links
;   *** In case of verbs, the third element is a possible auxiliary or nil, while 
;       the fourth one is nil.
; *** this function finds the head of the fragment and calls on it 'find-conj-deps'
(defun find-conj-struct (conjlines conjlinks firstconjhead is-firstconj?)
 (let (found (allines conjlines))
  (cond ((eq (get-synt-categ firstconjhead) 'verb)
  ; *** in case of conjoined sentences, special processing. Usually, the function
  ;     climbs the tree from the right corner, but here it is not possible, since
  ;     the arguments are not yet linked to the verb
  ; *** for verbs, the required information concerns the mood and tense of the
  ;     verb, and the possible presence of an auxiliary.
	   (do ((nextword (first conjlines) (first conjlines))
	        (conjlines (rest conjlines) (rest conjlines))
	        (nextlink (first conjlinks) (first conjlinks))
	        (conjlinks (rest conjlinks) (rest conjlinks)))
               ((or found (null nextword))
                   found)
	     (cond ((or (and (not is-firstconj?)
		             (eq (get-synt-categ nextword) 'verb))
     ; *** we are looking for the head of a possible conjunct before the first. 
     ;     It is the first verb found going back on the lines
		        (and is-firstconj? (equal nextword firstconjhead)))
     ; *** we are looking for the 'first' conjunct of the last pair; we already have 
     ;     the correct verb, so we stop when we find it.
                      (cond ((eq (second nextlink) 'VERB-RMOD+RELCL+REDUC)
                               (setq found 'attached-relcl))
                            (t (setq found (list nextword 'verb 
                                              (list (my-find-aux nextword allines))))))))))
     ;     in both cases, we must look for a possible auxiliary
  ; *** otherwise, standard processing
        (t (int-find-conj-struct conjlines conjlinks 
                       (first conjlines) (first conjlinks) nil)))))

; *** internal function for finding the structure of a possible conjunct in 
;     non-verbal cases
;  >>> seen: the lines already inspected (in order to avoid problems due to
;      loops, which should not exist)
(defun int-find-conj-struct (newlines newlinks startline startlink seen)
  (cond ((or (null startlink)
             (member startlink seen :test #'equal)
             (equal 0 (first startlink)))
          (cons startline (find-conj-deps startline newlines newlinks)))
        (t (let ((nextup 
                    (find-a-line `(position ,(first startlink)) newlines newlinks)))
               (cond ((null nextup) 
			(cons startline (find-conj-deps startline newlines newlinks)))
                     (t (int-find-conj-struct 
                            newlines newlinks
                            (first nextup) (second nextup) 
                            (cons startlink seen))))))))

; *********************************************************************
; *** this finds all the direct dependents of the topmost element of a fragment
;     of sentence, but, if the head is of a nominal compound, then looks for
;     the dependents of the noun
; *** INPUT:
;   >>> conjhead: the element whose dependents are sought
;   >>> newlines: the lines among which the search must be carried on
;   >>> newlinks: their links
; *** OUTPUT:
;   >>> a list including:
;       1. the type of structure (NG: nominal, cat: the category of any other
;          head (ex. ADJ, VERB, etc.))
;       2. the dependents found (according to the criteria above)
;       3. their links
(defun find-conj-deps (conjhead newlines newlinks)
 (let* ((headcat (get-synt-categ conjhead))
        (deps-and-links (find-dependents conjhead newlines newlinks))
        (depcats (mapcar #'get-synt-categ (first deps-and-links))))
  ; *** if the structure refers to Nominal Compound, but its head is not a 
  ;     noun, take as useful dependents the corresponding noun
  ;     This accounts for ... Marco, suo zio, e il loro vicino di casa
  (cond ((and (memq headcat '(ADJ ART NUM))
              (memq 'NOUN depcats))
          (let ((newheadcat (take-noun-dep (first deps-and-links) depcats)))
              (cons 'ng (rest (find-conj-deps newheadcat newlines newlinks)))))
        ((and (eq headcat 'NOUN)
              (eq (get-synt-type conjhead) 'proper))
           (cons 'ng deps-and-links))
        (t (cons headcat deps-and-links)))))

; *********************************************************************
; *** this finds the main element governed by a Preposition
(defun find-prep-dependent (prepline lines links)
  (cond ((null prepline) nil)
        (t (let* ((deps-and-links 
                        (find-dependents prepline lines links)
                        ;(find-dependents-nohead prepline lines links)
                           )
                  (deps (first deps-and-links))
                  (deplinks (second deps-and-links))
                  found)
              (do ((nxtdep (first deps) (first deps))
                   (deps (rest deps) (rest deps))
                   (nxtlink (first deplinks) (first deplinks))
                   (deplinks (rest deplinks) (rest deplinks)))
                ((or (null nxtdep) found) found)
                (cond ((memq (get-synt-categ nxtdep) '(ADV PUNCT)) nil)
     ; *** adverbials and punctuation are skipped
                      ((eq (get-synt-categ nxtdep) 'PREP)
     ; *** if the label is "contin+prep", then we are in the case "fino a domani"
     ;     where the actual argument is "domani", which depends on "a", which is
     ;     governed by "fino", so repeat the search under "a"
                        (cond ((eq (second nxtlink) 'CONTIN+PREP)
                                 (setq found (find-prep-dependent nxtdep deps deplinks)))
     ; *** contin+locut is for multi-word prepositions; here, the dependents are
     ;     linked to the first word of the locution, i.e. the present one
     ;     "nil" means that the loop goes on 
                              ((memq (second nxtlink) 
                                  '(CONTIN+LOCUT PREP-ARG PREP-ARG-RANGE)) nil)
                               ; anziche' da, come in, ...
                              (t (exception 'parse-error 
                                       "PROC/chunk-parser: unknown prep-prep structure"))))
                      (t (setq found nxtdep))))))))

; *********************************************************************
; *** this finds the dependent of category 'noun'
(defun take-noun-dep (deps depcats)
  (cond ((null depcats) (exception 'parse-error "PROC/chunk-parser: take-noun-dep"))
        ((eq (first depcats) 'NOUN) (first deps))
        (t (take-noun-dep (rest deps) (rest depcats)))))

; *********************************************************************
; *** this determines the compatibility of the two structures
(defun compat-conj-deps (struct-type deps newdeps)
   (let* ((depcats (mapcar #'get-synt-categ deps))
          (deptypes (mapcar #'get-synt-type deps))
          (newdepcats (mapcar #'get-synt-categ newdeps))
          (newdeptypes (mapcar #'get-synt-type newdeps)))
       (cond ((eq struct-type 'ng)
  ; *** the only constraint on noun groups if that if one includes a
  ;     relative clause, both must do
               (cond ((memq 'VERB depcats) (memq 'VERB newdepcats))
                     (t (not (memq 'VERB newdepcats)))))
             ((eq struct-type 'prep)
               (cond ((and (memq 'PRON depcats)
                           (memq 'RELAT deptypes))
                        (and (memq 'PRON newdepcats)
                             (memq 'RELAT newdeptypes)))
                     (t (not (and (memq 'PRON newdepcats)
                                  (memq 'RELAT newdeptypes))))))
             (t t))))

; *********************************************************************
; *** this takes as input the result of find-conj-seq, and converts it into
;     a list of pairs <linenumber, newlink>
;     ((13 asini ...) (14 , ...) (15 cavalli ...) (16 , ...) (17 muli ...) ) --->
;       ((14 (13 COORD)) (15 (14 COORD2ND)) (16 (15 COORD)) )
; *** the input must be of odd length, since the first element serves just as
;     initial attachment point
(defun build-lab-list (prevconjs)
  (cond ((null prevconjs) 
           (exception 'parse-error "PROC/chunk-parser: Argument of even length in lab list"))
	((null (rest prevconjs)) nil)
	(t (append (list 
		      (list (get-synt-numb (second prevconjs))
			    (make-link (get-synt-numb (first prevconjs)) 'COORD 'conjunctions))
		      (list (get-synt-numb (third prevconjs))
			    (make-link (get-synt-numb (second prevconjs)) 'COORD2ND 'conjunctions)))
		   (build-lab-list (rest (rest prevconjs)))))))

; *********************************************************************
; ****** ANALYSIS OF VERBAL DEPENDENTS ********************************
; *********************************************************************
; ***   In this section, I assume that all standard rules have already been
;	applied. Hopefully, most of the unattached elements (especially the
;	unmarked ones) are verbal dependents. Here, I'm trying to devise some
;	heuristic methods to determine the labels.
; ***	One of my goals is to exploit, so far as possible, the knowledge about
;	subcategorization frames, previously developed to 'check' labels already
;	assigned. However, the present task obviously is much more complex. So,
;	the introduction of heuristics.
; ***	I recall that the available subcat frames include a set of transformations,
;	in the sense that transformation have already be applied to 'deep' subcat
;	frames, in order to obtain all possible 'surface' subcat frames. This is
;	perfect, were it not that the number of surface frames is large, and that
;	here we do not know exactly which are the dependents.
; ***	In the other task, all possible permutations of the foreseen complements are
;	generated, and each of them is matched against the (known) set of actual
;	complements. Note that I'm saying 'complements', and not 'dependents',
;	since in checking existing assignments the adjuncts were already taken
;	apart: another piece of information that, here, we lack.
; ***	My approach, here, will be as follows:
;	Step 1a: If the verb is not in an infinitival form, then find any unmarked
;		 Nominal chunk immediately preceding the verb (skipping punctuation
;		 adverbs, and clitics). If any, assume it is the subject (in case
;		 the verb is active) or the object (if it is passive).
;	Step 1b: If the verb is in an infinitival form, then insert a subject
;		 trace just after it
;	Step 2: Analyse any clitic immediately preceding (proclitics) or included
;		in (enclitics) the verb.
;	Step 3a: If the verb belongs to one of the subclasses of 'transitives',
;		 it is not passive, and the previous step has not interpreted an
;		 enclitic as the direct object, do the same for a following chunk
;		 and assume it is the direct object.
;	Step 3b: If the verb belongs to one of the subclasses of verbs governing
;		 a clause, look for a 'di' or 'che'-headed clausal chunk following
;		 the verb and use it as the direct object.
;	Step 4: Use the knowledge about the subcat frames of the given verb to
;		assess other dependents surrounding the verb. I'm not sure about
;		how this can be done, yet.
;	N.B. Somewhere, the process must take into account relative clauses
(defun find-v-complements (data links csfport)
;   ---> 'data' are the input lines
;   ---> 'links' are the links, some of which have already been assigned, while
;	 others are NIL; the two input lists are assumed to be parallel!
; *** the result is a pair <data, links> where 'links' includes the newly
;     assigned links. Data is returned because it could have been changed by
;     insertion of traces
      (let (final-links final-data)
; *** repeat for all sentences, advancing in parallel on data and links
	(do ((nxtsent (first data) (first data))
	     (data (rest data) (rest data))
	     (nxtsentlab (first links) (first links))
	     (links (rest links) (rest links)))
	    ((and (null nxtsent) (null data))
               (values (reverse final-data) (reverse final-links)))
; *** repeat for all lines of a sentence, advancing in parallel on data and links
	    (do* ((prev nil (cons nxtline prev))
		  (nxtline (first nxtsent) (first nxtlocsent))
		  (nxtlocsent (rest nxtsent) (rest nxtlocsent))
		  (prevlabs nil (cons nxtlab prevlabs))
		  (nxtlab (first nxtsentlab) (first nxtlocsentlab))
		  (nxtlocsentlab (rest nxtsentlab) (rest nxtlocsentlab)))
; *** at end of sentence, exit the loop. 'final-links' and 'final-data' are the
;     updated infos. 'final-data' is required because of the possible insertion of
;     traces, which alters the previous list of data lines
	    ((null nxtline) 
		(setq final-links (cons (reverse prevlabs) final-links))
		(setq final-data (cons (reverse prev) final-data)))
	    (cond ((and (eq (get-synt-categ nxtline) 'VERB)
			(memq (get-synt-type nxtline) '(MAIN MOD)))
; *** 'find-first-aux' in "MORPHO/tb-functions"
		     (multiple-value-setq (prev prevlabs nxtlocsent nxtlocsentlab nxtline nxtlab)
				(apply-verbal-rules
					nxtline nxtlab prev
					prevlabs nxtlocsent nxtlocsentlab))
   ; *** the next line for printing the caseframe infos
                     (print-dependents nxtline csfport
                          (append (reverse prev) (list nxtline) nxtlocsent)
                          (append (reverse prevlabs) (list nxtlab) nxtlocsentlab))))))))

; ***************************************************************************
; *** prints on the csf file the data about the dependents of the verb
(defun print-dependents (verbline csfport allines allinks)
  (let ((alldeps (find-dependents verbline allines allinks)) alldeplines alldeplinks
        (verbpos (get-synt-linumb verbline)) (firstafter t))
     (setq alldeplines (first alldeps))
     (setq alldeplinks (second alldeps))
     (format csfport "***** Verb: ~a ~a (~a ~a ~a)~%" 
               (get-synt-numb verbline)
               (get-synt-inpword verbline)
               (get-synt-word verbline)
               (get-synt-mood verbline)
               (get-synt-tense verbline))
    ; *** if the first of alldeps is #\# there are no dependents before the verb
     (cond ((and (not (null alldeplines))
                 (not (> (get-synt-linumb (first alldeplines)) verbpos)))
              (format csfport " Before: ~%")))
     (do ((nxtdep (first alldeplines) (first alldeplines))
          (alldeplines (rest alldeplines) (rest alldeplines)))
        ((null nxtdep))
        (cond ((> (get-synt-linumb nxtdep) verbpos)
                 (cond (firstafter	; *** this is the first line after the head
                         (format csfport " After: ~%")
                         (setq firstafter nil)))))
        (print-csf-case csfport (get-csf-info nxtdep allines allinks)))))

; ***************************************************************************
(defun print-csf-case (csfport depdata)
   (format csfport "               ")
   (print-csf-singw csfport (first depdata))
   (cond ((not (null (second depdata)))
            (format csfport "                   ")
            (print-csf-singw csfport (second depdata))
            (cond ((not (null (third depdata)))
                     (format csfport "                       ")
                     (print-csf-singw csfport (third depdata)))))))

; ***************************************************************************
(defun print-csf-singw (csfport wrdata)
   (format csfport "~a: ~a (~a ~a ~a)~%" 
       (get-synt-numb wrdata)
       (get-synt-inpword wrdata)
       (get-synt-word wrdata)
       (get-synt-categ wrdata)
       (get-synt-type wrdata)))

; ***************************************************************************
; *** this extracts, for each dependent of the verb, the information to print in the .csf file
(defun get-csf-info (line allines allinks)
   (let (downline downdownline
         (linecat (get-synt-categ line))
         (linetype (get-synt-type line)))
      (cond ((eq 'PREP linecat)
               (setq downline (find-prep-dependent line allines allinks))
               (setq downdownline 
                    (rest (get-csf-info downline allines allinks))))
            ((and (eq 'CONJ linecat) (eq 'SUBORD linetype))
               (setq downline (find-case (find-dependents line allines allinks) 'CONJ-ARG)))
            (t (setq downline (is-a-synt-noun-complex line allines allinks))))
      (cond ((null downline) (list line))
            ((null downdownline) (list line downline))
            (t (list line downline downdownline)))))

; ***************************************************************************
; *** MAIN FUNCTION for assigning labels to arguments of finite verbs
; *** INPUT
;  >>> curline: the verbal line
;  >>> curlink: the label of the arc linking the verb to its governor (possibly,
;	but not necessarily, nil); it is a pair <pointer-to-parent, label>
;  >>> prevlines: all lines preceding the verb (in inverse order)
;  >>> prevlinks: all labels preceding the verb (in inverse order)
;  >>> nxtlines: all lines following the verb 
;  >>> nxtlinks: all labels following the verb
; *** OUTPUT
;  >>> a pair <newprevlinks, newnxtlinks> of updated link lists
(defun apply-verbal-rules
		 (curline curlink prevlines prevlinks nxtlines nxtlinks)
   (let (found-deps prevclit aftercompl beforecompl possconjs other-bef-compl
	 (allines (append (reverse prevlines) (list curline) nxtlines))
	 (allinks (append (reverse prevlinks) (list curlink) nxtlinks))
         (verb-class (cond ((eq (get-synt-type curline) 'MOD) '(modal))
                           (t (get-cf-verbclass curline)))))
; !!! find-cf-deps+labs in "PARSER/checkcond"
; *** find-cf-deps+labs finds just the already attached dependents (which
;     should be just the auxiliaries)
	(setq found-deps (find-cf-deps+labs (get-synt-numb curline) allines allinks))
; *** the next do loads prevclit (which contains the proclitics)
	(do ((nxtp (first prevlines) (first prl))
             (prl (rest prevlines) (rest prl))
	     (nxtplk (first prevlinks) (first prlk))
             (prlk (rest prevlinks) (rest prlk)))
            ((and (null nxtplk)
                  (or (null prl) 
                      (and (not (is-a-synt-trace? nxtp))
                           (not (memq (get-synt-word nxtp) '(#\" #\')))
                           (not (eq (get-synt-categ nxtp) 'ADV))
                           (not (eq (get-synt-type nxtp) 'AUX))
                           (not (eq (get-synt-type nxtp) 'RELAT)))
                      (is-a-synt-clitic? nxtp)))
              (cond ((is-a-synt-clitic? nxtp) 
		      (cond ((is-a-synt-clitic? (first prl))
		 	       (setq prevclit (list (first prl) nxtp)))
		            (t (setq prevclit (list nxtp))))))))
     ; (format t "prevclits: ~a~%" prevclit)
     ; (break "Apply verbal rules 2")
; *** looks for all following unattached possible verbal dependents
;     conjoin-equal-deps handles sequences of possible conjuncts (ex. 'John saw
;     the girls, the boys, the parents') by taking just the first as a 
;     dependent, and the others as conjuncts (returned in possconjs)
	(multiple-value-setq (aftercompl possconjs)
	   (conjoin-equal-deps
   ; *** look for any unlinked word until the next verb (excluding punctuation)
	          (find-aft-compl curline curlink nxtlines nxtlinks prevlines prevlinks 
                                 allines allinks
		                 (or (member 'trans-dir-disc verb-class)
		                     (member 'trans-dir-disc-indobj verb-class))
                                 nil)
		  allines allinks))
   ; *** if a sequence of conjunctions has been identified, insert their labels
     ; (format t "aftercompl: ~a~%" aftercompl)
     ; (break "Apply verbal rules 3")
	(cond ((not (null possconjs))
		 (setq nxtlinks
		       (mult-change-labs nxtlines nxtlinks 
					 (build-conj-labs possconjs)
                                         'conj-in-verbal))
	         (setq allinks 
		       (append (reverse prevlinks) (list curlink) nxtlinks))))
; *** then, all possible preceding dependents are searched for
	(multiple-value-setq (beforecompl possconjs)
	   (conjoin-equal-deps
               (reverse
		(find-bef-compl curline curlink prevlines prevlinks 
                                (or (member 'trans-dir-disc verb-class)
                                    (member 'trans-dir-disc-indobj verb-class))
                                allines allinks))
	       allines allinks))
      ;(format t "beforecompl: ~a~%" beforecompl)
      ;(break "Apply verbal rules 4")
   ; *** if a sequence of conjunctions has been identified, insert their labels
	(cond ((not (null possconjs))
		 (setq prevlinks
                     (reverse
		       (mult-change-labs (reverse prevlines) (reverse prevlinks)
					 (build-conj-labs possconjs)
                                         'conj-in-verbal)))
	         (setq allinks 
		       (append (reverse prevlinks) (list curlink) nxtlinks))))
; *** the next does the actual match. The resulting value of apply-verbal-rules
;     is the six-tuple returned by match-caseframes (with possible traces added):
;     <prevlines prevlinks nxtlines nxtlinks verbline verblink>
    (setq other-bef-compl (append beforecompl prevclit))
  ;(format t "Before: ~a~%After: ~a~%" other-bef-compl aftercompl)
  ;(break "")
	(match-caseframes
		found-deps
		curline curlink
		prevlines prevlinks
		nxtlines nxtlinks
		allines allinks
       		(append other-bef-compl aftercompl)
		verb-class)))

; ***************************************************************************
; *** this checks if the previous complex is marked and includes a relative
;     pronoun
; *** rightchunkw is the right foot of the chunk
(defun prev-marked-relpron (rightchunkw allines allinks)
 (let (headlinumb pronrmod)
  (cond ((and (eq (get-synt-categ rightchunkw) 'PRON)
              (eq (get-synt-type rightchunkw) 'RELAT)
              (has-gramm-type (get-synt-word rightchunkw) '&art-mid-relat))
   ; *** the right foot of the chunk is a possibly marked relpron (it. "cui" or "quale")
           (setq headlinumb (ult (climb-tree (get-synt-numb rightchunkw) allines allinks)))
   ; *** headlinumb is the line number of the head of the chunk
           (cond ((not (eq headlinumb 0))
   ; *** if it is 0, then the structure has already been chosen as the head of the sentence
                    (eq 'PREP (get-synt-categ 
                                 (first (find-a-line 
                                          `(position ,headlinumb) allines allinks)))))
                 (t nil)))
        ((eq (get-synt-categ rightchunkw) 'NOUN)
   ; *** if the right foot of the chunk is a noun, the structure could be "del cui fratello"
   ;    (of whose brother) so. we must check if, among the dependents of "fratello" 
   ;    there is "cui"
           (setq pronrmod (find-case (find-dependents rightchunkw allines allinks) 'PRON-RMOD))
           (cond ((null pronrmod) nil)
   ; *** if a "pron-rmod" of the noun has been found, then check if it is a relative
   ;     pronoun and if the head of its chunk is a preposition. This is made by recursively
   ;     calling prev-marked-relpron. Only one recursion step is possible, since the call
   ;     below satisfies the first condition of the cond above.
                 (t (prev-marked-relpron pronrmod allines allinks))))
        (t nil))))
   ; *** otherwise, return NIL. Notice that the case of "del cui fratello simpatico mi hai 
   ;     parlato" (about whose sympathetic brother you told me), since in this case the
   ;     right foot is an adjective

; ***************************************************************************
; *** a verbal line is a reduced relative in case it is a participle and
;     it has no previous auxiliary (i.e. find-first-aux returns the line itself)
(defun is-reduced-relative (verbline prevlines)
  (and (eq (get-synt-categ verbline) 'VERB)
       (eq (get-synt-mood verbline) 'PARTICIPLE)
       (equal verbline (find-first-aux verbline prevlines))))

; ***************************************************************************
; *** it moves backward from a verb, and collects all unlinked elements.
;     Special treatment is deserved for relatives and subordinates (but is
;     could be improved). For relative clauses, a verb that cannot be linked
;     increases a counter (relpr-count), which is decremented when the relative
;     pronoun is found. When the counter is 0, we are outside the relative clause
;     so we can go on with the standard search. Unfortunately, this cannot be done
;     when the pronoun is found, but it must be delayed to the beginning of the
;     relative pronoun complex. So, in
;     "The book about which you told me", the counter should be reset at "about"
;     not at "which". In Italian, it is possible to say "Il libro, l'introduzione del
;     quale hai letto, ..." and the "free" dependent is "il libro", because the
;     pronoun complex is "l'introduzione del quale". This is accomplished via 
;     relprbeg: 
;     Il libro       l'introduzione del quale hai letto certamente contiene capitoli fantastici
;          ^         ^                   ^          ^                 ^
;          |         |                   |          |             |   |                
;          |relpr-   | relpr-count = 0   |relpr-    |relpr-       |verbline
;          |  count=0| relprbeg not nil  |  count=1 |  count=0    |   
;    
;     In the example above, "certamente" (certainly) is included among the dependents
;     of "contiene" (includes), then all material is skipped until relpr-count=0
;     and relprbeg=nil. Consequently, the next dependent is "il libro".
;     A different case is the one of doubly attached relative pronouns (as in "To whom you
;     told me, I gave the box"), where "whom" is  such a pronoun: it has a double role, and
;     in the TUT annotation, "to whom" (in this example) must be attached to "gave", while
;     a trace of "whom" is inserted under "told". In these cases, the beginner of the
;     pronoun complex must be included in the result.
(defun find-bef-compl (verbline verblink prevlines prevlinks dir-disc? allines allinks)
 (declare (special *LANGUAGE* *TRACE-UNL*))
 (let (stop verbaux vb-mood vb-previtem founddeps w-lemma w-categ w-type w-line w-linked? 
       w-linkup w-linklab w-p-categ w-p-type relpbeg-line relpbeg-linked w-prec-by-conj 
       w-auxil w-act-mood w-aux-precline w-p-lemma w-past-part-noaux w-prec-double-relpr 
       w-prec-by-relpron w-clitic? w-trace? w-double-relpr? relprbeg (relpr-count 0))
   (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
             (format t "*** Entering Find-before-compl. Verbline: ~a~%" verbline)
     ;        (format t "prevlines: ~%~a~%" prevlines)
     ;        (format t "prevlinks: ~%~a~%" prevlinks)
             (break "")))
   (cond 
; *** reduced relatives cannot have previous complement
     ((is-reduced-relative verbline prevlines) nil)
; *** basic infos about the verb in focus and on the sentence
     (t (setq verbaux (find-first-aux verbline prevlines))
      (setq vb-mood (get-synt-mood verbaux))
      (setq vb-previtem 
         (cond ((eq verbaux verbline) (first prevlines))
               (t (find-line-before verbaux prevlines))))
; *** basic loop on all the items preceding the verb in focus *********************
    (do ((nxtline (first prevlines) (first prevlines))
         (prevlines (rest prevlines) (rest prevlines))
         (nxtlink (first prevlinks) (first prevlinks))
         (prevlinks (rest prevlinks) (rest prevlinks)))
       ((or (null nxtline) stop)
          (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
              (format t "*** Exiting Find-before-compl. Dependents: ~a~%" founddeps)
              (break "§§§§ END §§§§")))
          founddeps)
   ; *** some variables are reset ***
     (setq w-clitic? nil)
     (cond ((null relprbeg)
   ; *** w-double-relpr? is not reset if we are inside a relpron complex 
              (setq w-double-relpr? nil)))
   ; *** extract features related to the current word
     (setq w-lemma (get-synt-word nxtline))
     (setq w-categ (get-synt-categ nxtline))
     (setq w-type (get-synt-type nxtline))
     (setq w-line (get-synt-numb nxtline))
     (setq w-linked? (not (null nxtlink)))
     (setq w-linkup (first nxtlink))
     (setq w-linklab (second nxtlink))
     (setq w-trace? (is-a-synt-trace? nxtline))
     (setq w-p-categ (get-synt-categ (first prevlines)))
     (setq w-p-type (get-synt-type (first prevlines)))
     (cond ((eq w-categ 'PRON)
              (setq w-clitic? (is-a-synt-clitic? nxtline))
              (cond ((eq w-type 'RELAT)
                      (setq w-double-relpr?
                          (and (has-gramm-type w-lemma '&double-who)
                               (not (has-gramm-type w-lemma '&base-relat))))
                      (setq relprbeg (find-relpr-beg (cons nxtline prevlines) 
                                                    (cons nxtlink prevlinks)))
                      (cond ((null relprbeg) (exception-nothrow "Find bef compl"))
                            (t (setq relpbeg-line (first relprbeg))
                               (setq relpbeg-linked (not (null (second relprbeg))))))))))
     (cond ((and (eq w-categ 'VERB) (memq w-type '(MAIN MOD)))
              (setq w-auxil (find-first-aux nxtline prevlines))
              (setq w-act-mood (get-synt-mood w-auxil))
              (setq w-aux-precline (find-line-before w-auxil (cons nxtline prevlines)))
              (setq w-past-part-noaux (is-reduced-relative nxtline prevlines))
              (cond (w-past-part-noaux 
                      (setq w-prec-double-relpr 
                         (has-gramm-type (get-synt-word w-aux-precline) '&double-who))))
              (setq w-p-lemma (get-synt-word w-aux-precline))
              (setq w-p-categ (get-synt-categ w-aux-precline))
              (setq w-prec-by-conj (preceded-by-conj-subord nxtline prevlines prevlinks))
              (setq w-prec-by-relpron (preceded-by-relpron nxtline prevlines prevlinks 0))))
; &&&& START of the analysis of the current word &&&&&&&&&&&&&&&
     (cond ((< relpr-count 0)
              (exception 'parse-error "Too many relative pronouns in int-find-bef-compl"))
           ((or 
; %%% conditions for terminating the search (no more possible dependents) %%%%%%%%%%%%%%%%%%%
              (and (memq vb-mood '(INFINITE GERUND))
                   (not (prev-marked-relpron vb-previtem allines allinks)))
         ; *** In general, infinites cannot have previous dependents, but the second
         ;     sub-condition is needed for phrases as "il libro di cui parlare", 
         ;     "dal quale prendere spunto", "alla cui introduzione ispirarsi"
              (and (dependent? (first verblink) nxtline allines allinks))
         ; *** if the verb already depends on the word, the word cannot depend on the
         ;     verb (loops), and no preceding word can (projectivity)
              (and (eq w-categ 'PUNCT) (eq w-lemma #\-)
                   (eq w-p-categ 'NOUN) (eq w-p-type 'PROPER)
                   (null (rest prevlines)))
         ; *** the one above introduced for EVALITA: If there is an initial sequence as
         ;     "MILANO - ...", MILANO is assumed to be a kind of heading, so it cannot be 
         ;     the subject
              (and (eq w-categ 'PUNCT) (eq w-lemma #\.))
         ; *** periods should occur only as separators in initial sequences, as in 
         ;     directive names
	      (and (= relpr-count 0)
                   (eq w-categ 'CONJ) (eq w-type 'SUBORD) (not w-prec-by-conj))
         ; *** a subordinate conjuction stops the search if it is not associated with
         ;     another verb
              (and (= relpr-count 0)
	           (eq w-categ 'PRON) (eq w-type 'RELAT) w-double-relpr? (not w-trace?))
         ; *** a double rel pron stops the search (remember we are in the case
         ;     of relpr-count = 0; this means that the verb should be the head of the
         ;     relative clause, and the double relpron must not be attached to it)
              (and (= relpr-count 0)
                   (eq w-categ 'ADV) (eq w-type 'INTERR) w-linked?)
         ; *** an already linked interrogative adverb stops the search
              (and (= relpr-count 0) (not w-linked?)
	           (eq w-categ 'VERB) (memq w-type '(MAIN MOD))
                   (not (memq w-act-mood '(INFINITE GERUND)))
                   (not dir-disc? )
                   (not w-prec-by-conj)
                   (not w-prec-by-relpron))
         ; *** a finite verb, which is not preceded by a subordinating conjunction or by a relative
         ;     pronoun, and the verb under analysis does not admit direct discourse
              ;(and (= relpr-count 0) 
              ;      (equal relpbeg-line nxtline)
                   ; (equal nxtline poss-subj))
                    )
         ; *** relpr-count is equal to 0. We are looking for the dependents of the verb of a
         ;     relative clause, and we have found the (head) of a relative pronoun.
         ;     However, the line was already found as poss-subj, so the search is suspended
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: One")))
            (setq stop t))
           ((or 
; %%% conditions for terminating the search after adding the current element %%%%%%%%%%%%%%%%
              (and (= relpr-count 0) (not w-linked?)
                   (eq w-categ 'ADV) (eq w-type 'INTERR))
         ; *** an interrogative adverb is included in the result, and stops the search
         ;     This does not work for "La sedia dove la metto", but for the moment
         ;     this seems safer to me
              (and (= relpr-count 0) (not w-linked?)
	           (eq w-categ 'VERB) (memq w-type '(MAIN MOD))
                   dir-disc? 
                   (not (memq w-act-mood '(INFINITE GERUND)))
                   (eq 'fail (belong-to-chunk verbline verblink w-line nil allines allinks nil))
                   (not w-prec-by-conj)
                   (not w-prec-by-relpron))
         ; *** the governing verb admits direct discourse, and the current item is a verb
         ;     analysis (relpr-count=0)
              (and (= relpr-count 0); (not w-linked?)
                   (not w-trace?)
                   (equal relpbeg-line nxtline)
                  ; (not (equal nxtline poss-subj))
                   ))
         ; *** relpr-count is equal to 0. We are looking for the dependents of the verb of a
         ;     relative clause, and we have found the (head) of a relative pronoun.
         ;     The line is included in the result, and the search is suspended
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: Two")))
            (setq founddeps (append1 founddeps nxtline))
            (setq stop t))
; %%% conditions for skipping the current item and going ahead %%%%%%%%%%%%%%%%%%%%%%%%%%%%
           ((or 
              (and (= relpr-count 0)
	           (eq w-categ 'PRON) (eq w-type 'RELAT) w-double-relpr? w-trace?)
         ; *** if this is a trace, then it is the trace already inserted for the double rel:
         ;     spesso a chi ama lo sport, piace correre -->
         ;        spesso a chi [ama [chi] lo sport], piace correre
         ;     it is ignored, and the search continues
              (and (= relpr-count 0) 
	           (eq w-categ 'VERB) (memq w-type '(MAIN MOD))
                   (eq w-act-mood 'GERUND) w-linked?
                   (lab-subsumes 'DET-ARG w-linklab))
         ; *** gerunds used as nouns as in "the encountering of" 
              (and (= relpr-count 0) 
	           (eq w-categ 'VERB) (memq w-type '(MAIN MOD))
                   w-past-part-noaux (or w-linked? w-prec-double-relpr))
         ; *** reduced relatives; they are already linked, except in cases they are
         ;     preceded by a double rel pron
              (and (= relpr-count 0) 
	           (eq w-categ 'VERB) (memq w-type '(MAIN MOD))
                   w-linked?
                   (lab-subsumes 'COORD2ND w-linklab))
          ; *** verbs already linked as a second conjunct 
              (and (= relpr-count 0) 
	           (eq w-categ 'VERB) (memq w-type '(MAIN MOD))
                   w-linked?
                   (eq w-linklab 'VERB+MODAL-INDCOMPL))
          ; *** verbs governed by a modal
              (and (= relpr-count 0) (not w-linked?)
	           (eq w-categ 'VERB) (memq w-type '(MAIN MOD))
                   (neq w-act-mood 'INFINITE) (neq w-act-mood 'GERUND)
                   w-prec-by-conj)
          ; *** verbs preceded by a subordinating conjunction
              (and (= relpr-count 0) (eq w-categ 'VERB) (memq w-type '(MAIN MOD))
                   (not w-prec-by-relpron))
          ; *** standard verbal case
              (and (> relpr-count 0) 
                   (not (equal relpbeg-line nxtline))
                   (or (neq w-categ 'VERB) (not (memq w-type '(MAIN MOD)))
                       (memq w-act-mood '(INFINITE GERUND)))))
          ; *** we are skipping the material inside a relative clause; we have not yet reached
          ;     the beginning of the relative pronoun complex; note that if relprbeg is nil,
          ;     we have not reached the relative pronoun; if it is not nil, but it is different
          ;     from the current line, we are inside, but we have not reached, the beginning of
          ;     the relative pronoun complex
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: Three")))
             nil)
; %%% conditions for adding the current item and going ahead %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	  ((or (and (= relpr-count 0) (not w-linked?) (not w-clitic?)
                    (memq w-categ '(ADJ ART NOUN PREP PRON NUM))
                    (not (equal nxtline relpbeg-line)))
         ; *** adjectives are questionable, but they should already be linked now,
         ;     unless head of a NG
	       (and (= relpr-count 0) (eq w-categ 'VERB) 
                    w-past-part-noaux (not w-linked?) (not w-clitic?) (not w-prec-double-relpr)))
         ; *** "viste le condizioni, ho pensato"
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: Four")))
            (setq founddeps (append1 founddeps nxtline)))
; %%% some special conditions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	  ((and (= relpr-count 0)
                (eq w-categ 'CONJ) (eq w-type 'SUBORD) w-prec-by-conj
                w-linked?)
         ; *** an expected conj subord has been found: reset the flag
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: Five")))
            (setq w-prec-by-conj nil))
	  ((and (= relpr-count 0)
                (eq w-categ 'CONJ) (eq w-type 'SUBORD) w-prec-by-conj
                (not w-linked?))
         ; *** an expected unlinked conj subord has been found: reset the flag
         ;     and add the conjunctcion to the result
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: Five")))
            (setq founddeps (append1 founddeps nxtline))
            (setq w-prec-by-conj nil))
	  ((and (= relpr-count 0)
                (eq w-categ 'VERB) (memq w-type '(MAIN MOD))
                (memq w-act-mood '(INFINITE GERUND))
                (eq 'CONJ (get-synt-categ w-aux-precline))
                (eq 'SUBORD (get-synt-type w-aux-precline))
                (has-gramm-type w-p-lemma '&conj-sub-g-inf))
                          ; &conj-sub-g-inf dopo, anzichè, se
         ; *** a verb is preceded by a subordinative conjunction
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: Six")))
            (setq w-prec-by-conj t))
         ; *** a verb is preceded by a subordinative conjunction
          ((and (= relpr-count 0) (not w-linked?)
                (eq w-categ 'VERB) (memq w-type '(MAIN MOD))
                dir-disc? 
                (neq w-act-mood 'INFINITE) (neq w-act-mood 'GERUND)
                (eq 'fail (belong-to-chunk verbline verblink w-line nil allines allinks nil))
                w-prec-by-conj)
         ; *** the governing verb admits direct discourse, but the verb is preceded by a
         ;     subordinating conjunction
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: Seven")))
            (setq w-prec-by-conj nil))
          ((and (eq w-categ 'VERB) (memq w-type '(MAIN MOD))
                (not (memq w-act-mood '(INFINITE GERUND))))
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: Eight")))
            (setq relpr-count (1+ relpr-count)))
         ; *** standard verbal case: in case of finite verbs, a verb can belong to a 
         ;     relative clause; so, increment the number of expected relative pronouns. 
         ;     This is used to continue the search before the corresponding relative pronoun
          ((and (> relpr-count 0) 
                (not w-double-relpr?)
                (equal relpbeg-line nxtline))
          ; *** relpr-count is greater than 0. Now, we have found the (head) of a relative pronoun,
          ;     so that the counter is decremented
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: Nine")))
            (setq relpr-count (1- relpr-count)))
          ((and (= relpr-count 1) 
                w-double-relpr?
                (not w-trace?)
                (equal relpbeg-line nxtline))
          ; *** relpr-count is greater than 0. In this case, every material is skipped until the 
          ;     relative pronoun is found: in this case the counter is decremented
          ; *** in this case, we have found the first "chi" (not the trace) of the example
          ;     spesso a chi ama lo sport, piace correre -->
          ;        spesso a chi [ama [chi] lo sport], piace correre
          ;     His head ("a") is included in the result, and the search continues
          ;     from the previous word ("spesso")
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: Ten")))
            (setq relpr-count 0)
            (setq w-double-relpr? nil)
            (setq relprbeg nil)
            (setq relpbeg-line nil)
            (setq relpbeg-linked nil)
            (setq founddeps (cons nxtline founddeps)))
          ((and (> relpr-count 1) 
                w-double-relpr?
                (equal relpbeg-line nxtline))
          ; *** relpr-count is greater than 1. In this case, we are skipping any material
          ;     inside a rel clause, but we are reached its beginning, so the counter is
          ; *** decremented
          ;     An example is "Lo sport [che [a chi piace correre] interessa] è l'atletica]
          ;     We are now on "a" and relpr-count is 2. We are now in the outer rel clause
          ;     (relpr-count=1), and when we reach the rel pron (che) we can re-start the
          ;     collection of the dependents of "è"
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: Eleven")))
            (setq relpr-count (1- relpr-count)))
          (t
            (cond ((and (not (null *TRACE-UNL*)) (< *TRACE-UNL* 3))
                               (format t "-- Nxtline: ~a~%" nxtline)
                               (break "Before: Twelve"))))
              ))))))

; ***************************************************************************
; *** this returns a four-tuple <relprbeg-line, relprbeg-link, prevlines, prevlinks>, 
;     where relprbeg-line is the line where the relpron complex begins, 
;     relprbeg-link is the corresponding link, prevlines and prevlinks are all items 
;     preceding the beginning of relpron complex
(defun find-relpr-beg (lines links)
  (cond ((and (has-gramm-type (get-synt-word (first lines)) '&mid-relat)   ; 'cui'
              (memq (get-synt-categ (second lines)) '(ART PREP)))
   ; *** "a cui", "il cui" (what about "al cui" ???)
          (list (second lines) (second links)
                (nthcdr 2 lines) (nthcdr 2 links)))
        ((and (has-gramm-type (get-synt-word (first lines)) '&double-who)   ; 'chi'
              (eq (get-synt-categ (second lines)) 'PREP))
   ; *** "a chi"
          (list (second lines) (second links)
                (nthcdr 2 lines) (nthcdr 2 links)))
        ((and (has-gramm-type (get-synt-word (first lines)) '&art-relat)   ; 'quale'
              (eq (get-synt-categ (second lines)) 'ART))
            (cond ((eq (get-synt-categ (third lines)) 'PREP)
   ; *** "per il quale": returns "per" and all preceding lines
                    (list (third lines) (third links)
                          (nthcdr 3 lines) (nthcdr 3 links)))
   ; *** "not-prep il quale": returns "il" and all preceding lines
                  (t (list (second lines) (second links)
                          (nthcdr 2 lines) (nthcdr 2 links)))))
   ; *** otherwise ("che", or "chi" without prep) returns the current line 
        (t (list (first lines) (first links)
                 (rest lines) (rest links)))))

; *****************************************************************************
; *** this checks if a verb is preceded by a conj subord
;     (without intervening verbs or relative pronouns)
(defun preceded-by-conj-subord (verbline prevlines prevlinks)
  (let ((categ (get-synt-categ (first prevlines)))
        (type (get-synt-type (first prevlines))))
   (cond ((null prevlines) nil)
         ((and (eq categ 'VERB)
               (not (equal (get-synt-numb verbline) (first (first prevlinks))))) nil)
     ; *** we have found another verb not linked to the one under analysy (not an aux?)
     ;     stop the search
         ((and (eq categ 'CONJ) (eq type 'SUBORD)) t)
         ((and (eq categ 'PRON) (eq type 'RELAT)) nil)
         (t (preceded-by-conj-subord verbline (rest prevlines) (rest prevlinks))))))

; *****************************************************************************
; *** this checks if a verb is preceded by a relative pronoun
;     (without intervening verbs or subordinative conjunctions)
(defun preceded-by-relpron (verbline prevlines prevlinks numverbs)
  (let ((categ (get-synt-categ (first prevlines)))
        (type (get-synt-type (first prevlines))))
   (cond ((null prevlines) nil)
         ((and (eq categ 'VERB) (eq type 'MAIN)
               (not (is-reduced-relative (first prevlines) (rest prevlines))))
            (preceded-by-relpron verbline (rest prevlines) (rest prevlinks) (1+ numverbs)))
     ; *** we have found another verb; auxiliaries are not relevant and modals are not
     ;     counted because they are paired with a main
         ((and (eq categ 'CONJ) (eq type 'SUBORD))
             (cond ((eq numverbs 0) nil)
                   (t (preceded-by-relpron verbline (rest prevlines) (rest prevlinks) (1- numverbs)))))
         ((and (eq categ 'PRON) (eq type 'RELAT)) 
             (cond ((eq numverbs 0) t)
                   (t (preceded-by-relpron verbline (rest prevlines) (rest prevlinks) (1- numverbs)))))
         (t (preceded-by-relpron verbline (rest prevlines) (rest prevlinks) numverbs)))))

; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; &&&&& END OF THE SECTION CONCERNING THE COLLECTION OF THE VERBAL DEPENDENTS &&&&&
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; &&&&& START OF THE SECTION CONCERNING THE EVALUATION OF VERBAL CASEFRAMES &&&&&&&
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

; ***************************************************************************
; >>> INPUT:
;   found-deps: the lines of the dependents already found (e.g. auxiliaries)
;   curline:    the line of the verb
;   curlab:     the possible link up of the verb
;   prevlines prevlabs, nxtline nxtlabs, allines allabs: obvious
;   unlinked:   The lines of the assumed dependents of the verb
;   verbcl:     The subcategorization class of the verb
; *** OUTPUT: 6 values. the lines are returned, since there could be some trace
;     added.
;   >>> prevlines prevlinks nxtlines nxtlinks verbline verblink
(defun match-caseframes 
	(found-deps curline curlab prevlines prevlabs nxtlines nxtlabs 
	 allines allabs unlinked verbcl)
  (declare (special *LANGUAGE* *VERB-CF-TRANSFORM*))
  (let (transfs notransfs derived-cl caseframes applconds cf-match best-match
        passive-obl unmroles appltransf temp-best-m
        (assignedlabels (mapcar #'(lambda (x) (second (second x))) found-deps))
        )
   ; *** collects all obligatory and forbidden transformations
         (multiple-value-setq (transfs notransfs passive-obl)
             (get-transformations curline curlab allines allabs))
   ; *** starting from each of the verbal classes, find all derived classes
   ; 	 to which all the obligatory transformations have been applied, and no
   ;     excluded transformation (notransfs) has been applied
   ; *** transfs is a list of lists, each of which represents a combination
   ;     that is required. This is due to the fact that in some cases, there are
   ;     two or more transfs that are required, but are mutually exclusive. This
   ;     is intended to mean that "one of them" is needed. So, if we have
   ;     that the obligatory transfs are 
   ;     (infinitivization causal-infinitivization-trans null-obj)
   ;     then, we get 
   ;     ((infinitivization null-obj) (causal-infinitivization-trans null-obj))
   ; *** derived-cl is a list of pairs <class <list of applied transformations>>
	 (setq derived-cl 
 	    (remove-dummies
	       (flatten 
	          (mapcar 
		       #'(lambda (x)
 			    (find-derived-cl x transfs notransfs passive-obl))
		      verbcl))))
   ; *** the caseframes of the derived classes are extracted. 
   ;     caseframe is a list of triples <surface-cf class transfs>
   ; !!! get-inthn-cf-deps in hier-funct.lisp
	    (setq caseframes 
		  (mapcar #'(lambda (x)
			       (list (get-inthn-cf-def (first x))
				     (first x) (second x)))
			  derived-cl))
;	    (setq caseframes (cancel-found-cases caseframes found-deps assignedlabels))
   ; *** merge-equal-cfs does the following:
   ;     caseframes: 
   ;      ((cf1 base1 transfs1) (cf2 base2 transfs2) (cf1 base3 transfs3))
   ;             --->
   ;      ((cf1 (base1 transfs1) (base3 transf3)) (cf2 (base2 transfs2)))
   ;     i.e. equal surface realizations are unified (in order to avoid to
   ;     check it twice), but their origin is retained
	    (setq caseframes (merge-equal-cfs caseframes))
   ; *** now, we must check the correspondence between the actual (potential)
   ;     caseframe in the input sentence, and all the found derived classes
   ; *** cf-match is a list of lists (see check-caseframes)
	    (setq cf-match 
		(check-caseframes 
		    unlinked caseframes curline curlab allines allabs 'obj-subj))
	    (setq best-match 
                   (choose-best-assign curline unlinked (elimdup cf-match) allines allabs))
            (setq unmroles (first (first best-match)))
            (setq appltransf (second best-match))
          ;    (format t "BEST-MATCH: ~a~%" best-match)
          ;    (break "")
   ; *** best-match has the form ((n (label1 .. labelN)) transf), where N is the number of 
   ;     unassigned labels. Just the labels have to be passed to merge-cf-assign
   ; *** new-best-match tries to find something better than the RMOD labels, on the
   ;     basis of semantic knowledge
	 ;   (setq new-best-match 
         ;        (refine-verbal-rmod-labels unlinked best-match allines allabs))
   ; *** in case a "ne" dependent (a pronoun) is present, and it has not been recognized
   ;     as an argument of the verb, then it must become a VISITOR, and a trace of its
   ;     must be attached below the VERB-OBJ
            (multiple-value-setq (temp-best-m prevlines prevlabs nxtlines nxtlabs allines allabs)
                 (find-ne-pron curline (second (first best-match)) unlinked allines allabs))
             (setq best-match (list (list unmroles temp-best-m) appltransf))
         ;    (format t "BEST-MATCH AFTER NE PRON: ~a~%" best-match)
         ;    (format t "             ****~% prevlines: ~a~% prevlabs: ~a~% nxtlines: ~a~% nxtlabs: ~a~%"
         ;            prevlines prevlabs nxtlines nxtlabs)
         ;    (break "")
   ; *** the next is an attempt to handle appositions
            (multiple-value-setq (best-match unlinked prevlines prevlabs nxtlines nxtlabs)
                 (find-appositions curline (second (first best-match)) unlinked allines allabs))
         ;    (format t "BEST-MATCH AFTER APPOSITIONS: ~a~%" best-match)
         ;    (break "")
   ; *** merge-cf-assigns carries out the actual update of the lines and labs
	    (merge-cf-assign 
			best-match unlinked prevlines prevlabs
			nxtlines nxtlabs curline curlab 'verbal)
;]
   ; *** if unlinked is empty, return nxtlabs unchanged
;	[t [values prevlabs nxtlabs]]]
))

; *****************************************************************************
; *** this tries to replace RMOD labels by more specific RMODs, according to the
;     (semantic?) features of the governed subtree
;(defun refine-verbal-rmod-labels (unlinked best-match allines allabs)
; (let (newmatch)
;   (do ((nxtlab (first best-match) (first best-match))
;        (best-match (rest best-match) (rest best-match))
;        (nxtunl (first unlinked) (first unlinked))
;        (unlinked (rest unlinked) (rest unlinked)))
;       ((null nxtlab) newmatch)
;       (cond ((eq nxtlab 'RMOD)
;                (setq newmatch (append1 newmatch (find-verbal-rmod-lab nxtunl allines allabs))))
;             (t (setq newmatch (append1 newmatch nxtlab)))))))

; *****************************************************************************
; *** this is an anticipation of semantic analysis
;(defun find-verbal-rmod-lab (verbal-dep allines allabs)
;  (let ((headcat (get-synt-categ verbal-dep)))
;      (cond ((eq headcat 'PREP)
;               (setq meaning 
;                  (inlist (get-word-meaning (get-synt-word verbal-dep) headcat 
;                                            (get-synt-type verbal-dep) nil)))
;        ; *** meaning is supposed to have either the form "meaning1" or the form "(m1 m2 ...)"
;               (do ((nxtmean (first meaning) (first meaning))
;                    (meaning (rest meaning) (rest meaning)))
;                   ((or (null nxtmean) found)
;                      (cond (found found) (t 'RMOD)))
;                   (setq prep-mean (leggi *PREP-TEMPLATES* nxtmean))
;                   (setq act-prep-mean (select-prep-mean prep-mean '££situation 'XXX)))))))


; *****************************************************************************
; *** if in match-labels there is a RMOD associated with a "ne" pronoun and
;     a VERB-OBJ, change the label from RMOD to VISITOR and insert a trace
;     to "ne" below the VERB-OBJ
;        1 Sandro (SANDRO NOUN PROPER M SING ££NAME) [4;VERB-SUBJ]
;        2 ne (NE PRON PERS ALLVAL ALLVAL PARTIT CLITIC) [4;RMOD]
;        3 ha (AVERE VERB AUX IND PRES TRANS 3 SING) [4;AUX+TENSE]
;        4 visto (VEDERE VERB MAIN PARTICIPLE PAST TRANS SING M) [0;TOP-VERB]
;        5 la (IL ART DEF F SING) [4;VERB-OBJ]
;        6 fine (FINE NOUN COMMON F SING FINIRE INTRANS) [5;DET+DEF-ARG]
;        7 . (#\. PUNCT) [4;END]
;                     ---->
;        1 Sandro (SANDRO NOUN PROPER M SING ££NAME) [4;VERB-SUBJ]
;        2 ne (NE PRON PERS ALLVAL ALLVAL PARTIT CLITIC) [4;VISITOR]          <<<<<<<
;        3 ha (AVERE VERB AUX IND PRES TRANS 3 SING) [4;AUX+TENSE]
;        4 visto (VEDERE VERB MAIN PARTICIPLE PAST TRANS SING M) [0;TOP-VERB]
;        5 la (IL ART DEF F SING) [4;VERB-OBJ]
;        6 fine (FINE NOUN COMMON F SING FINIRE INTRANS) [5;DET+DEF-ARG]
;        6.10 t [2f] (NE PRON PERS ALLVAL ALLVAL PARTIT CLITIC) [6;PRON-RMOD] <<<<<<<
;        7 . (#\. PUNCT) [4;END]
(defun find-ne-pron (verbline match-labels unlinked allines allinks)
   (let (obj-found ne-found found resultlines resultlinks
         prevlines prevlinks nxtlines nxtlinks)
       (do* ((nxtlab (first match-labels) (first match-labels))
             (match-labels (rest match-labels) (rest match-labels))
             (nxtunlink (first unlinked) (first unlinked))
             (unlinked (rest unlinked) (rest unlinked))
             (resultlabels (list nxtlab) 
                 (cond ((null nxtlab) resultlabels)
                       (t (append1 resultlabels nxtlab)))))
   ; *** nxtlab and nxtunlink are the new case label and the associated line
   ;     resultlabels are the new match, that can be different from the original
   ;     one in case the RMOD "ne" has been changed into VISITOR
           ((or (null nxtlab) found)
              (cond (found 
                      (multiple-value-setq (prevlines prevlinks nxtlines nxtlinks)
                               (split-result-lines verbline resultlines resultlinks))
                      (values (append resultlabels match-labels)
                              prevlines prevlinks nxtlines nxtlinks resultlines resultlinks))
                    (t (multiple-value-setq (prevlines prevlinks nxtlines nxtlinks)
                               (split-result-lines verbline allines allinks))
                       (values resultlabels 
                              prevlines prevlinks nxtlines nxtlinks allines allinks))))
   ; *** BODY *******************************************
   ; *** if we find a "ne" RMOD 
           (cond ((and (eq nxtlab 'RMOD)
                       (eq (get-synt-word nxtunlink) 'ne)
                   (cond (obj-found
        ; ***  if we have already found the VERB-OBJ, make all required changes
                           (setq found t)
                           (multiple-value-setq (resultlines resultlinks)
                                     (attach-ne-visitor obj-found nxtunlink allines allinks)))
        ; ***  otherwise, save the info about the "ne" line
                         (t (setq resultlabels (append1 (butlast resultlabels) 'VISITOR))
                            (setq ne-found nxtunlink)))))
   ; *** if we find a VERB-OBJ
                 ((eq nxtlab 'VERB-OBJ)
                   (cond (ne-found
        ; ***  if we have already found a "ne" RMOD, make all required changes
                           (setq found t)
                           (multiple-value-setq (resultlines resultlinks)
                                     (attach-ne-visitor nxtunlink ne-found allines allinks)))
        ; ***  otherwise, save the info about the VERB-OBJ line
                         (t (setq obj-found nxtunlink))))))))

; *****************************************************************************
; *** this assumes that the verb-obj-line is a determiner or a noun. It looks for the
;     head noun of the VERB-OBJ, and assumes it has no dependents. Then it attaches
;     to it as a dependent a trace of ne-line, linked via PREP-RMOD
; *** so, it works for "ne ha visto l'inizio" ("of it" [he] has seen the beginning,
;     whose meaning is "he saw its beginning") but not for "ne ha visto l'inizio
;     del primo atto" ("of it" [he] has seen the beginning of the first act)
(defun attach-ne-visitor (verb-obj-line ne-line allines allinks)
  (let (act-governors (obj-categ (get-synt-categ verb-obj-line)))
  ; *** the next cond sets the actual governor of the trace as the head noun of the
  ;     VERB-OBJ
    (cond ((eq 'NOUN obj-categ)
             (setq act-governors (list verb-obj-line)))
          ((is-a-synt-noun-complex verb-obj-line allines allinks)
             (setq act-governors (find-ne-noun-dep verb-obj-line allines allinks))))
  ; *** act-governor could be null in more complex cases as "ne ha visto partire il figlio"
  ;     ("of it" [he] has seen to leave the son --> he saw his son leaving), where the
  ;     verb-obj is a sentence
    (cond ((null act-governors)
             (values allines allinks))
          ((eq act-governors 'no-dep)
             (cond ((eq obj-categ 'NUM)
     ; *** for "ne ho visti due"
                      (add-ne-trace (list verb-obj-line) ne-line allines allinks))
                   (t (exception 'parse-error 
                              "PROC/chunk-parser: more than one dependent in attach-ne-visitor"))))
          (t (add-ne-trace act-governors ne-line allines allinks)))))
 
; *****************************************************************************
; *** looks for the (unique) dependent of verb-obj-line
;    otherfound is used to handle conjunctions: if more than one dependent is
;    found, but there are two of them and one of them is a conjunction
;    duplicate the "ne" trace also below the conjunction
(defun find-ne-noun-dep (verb-obj-line allines allinks)
   (let ((savallines allines) (savallinks allinks)
         found otherfound downdep downcateg other-governor
         (obj-linumb (get-synt-numb verb-obj-line)))
     (do ((nxtline (first allines) (first allines))
          (allines (rest allines) (rest allines))
          (nxtlink (first allinks) (first allinks))
          (allinks (rest allinks) (rest allinks)))
         ((null nxtline)
           (cond (found 
                   (cond ((null otherfound) (list found))
         ; *** since the only accepted case is the one of coordinating conjunction, it is assumed that
         ;     the conjunctions (that follows the first conjunct) is in otherfond (not in found)
                         ((eq (length otherfound) 1)
                           (cond ((and (eq (get-synt-categ (first otherfound)) 'CONJ)
                                       (eq (get-synt-type (first otherfound)) 'COORD))
                                    (setq downdep (find-ne-noun-dep (first otherfound)
                                                       savallines savallinks))
          ; *** downdep is the line governed by the conjunction
                                    (setq downcateg (get-synt-categ downdep))
                                    (cond ((eq 'NOUN downcateg)
                                             (setq other-governor verb-obj-line))
                                          ((is-a-synt-noun-complex downdep savallines savallinks)
                                             (setq other-governor 
                                                  (find-ne-noun-dep downdep savallines savallinks))))
                                    (cond ((null other-governor) (list found))
                                          (t (list found other-governor))))
                     ; *** the other dependent is not a conjunction
                                 (t 
               ;(exception 'parse-error "PROC/chunk-parser: more than one dependent in find-ne-noun-dep")
                                     (list found))))
                         (t (exception 'parse-error 
                                "PROC/chunk-parser: more than one dependent in find-ne-noun-dep"))))
                 (t 'no-dep)))
   ; *** BODY
  ;(format t "nxtline: ~a~%" nxtline)
  ;(break "chunk-parser: find-ne-noun-dep")
         (cond ((and (equal (first nxtlink) obj-linumb)
                     (not (memq (get-synt-categ nxtline) '(PREDET ADV))))
         ; *** predeterminers and adverbs (anche quei ...) are skipped
                  (cond ((null found)
                           (setq found nxtline))
                        (t (setq otherfound (append1 otherfound nxtline)))))))))

; *****************************************************************************
; *** this looks for the line after which the trace must be inserted (act-governor)
;     and introduces the trace line and link
(defun add-ne-trace (act-governors ne-line allines allinks)
   (cond ((null act-governors)
            (values allines allinks))
         (t (let ((savlines allines) (savlinks allinks) newlines newlinks done
                  trace-and-foll (firstgov (first act-governors)))
              (do ((nxtline (first allines) (first allines))
                   (allines (rest allines) (rest allines))
                   (nxtlink (first allinks) (first allinks))
                   (allinks (rest allinks) (rest allinks)))
   ; *** either the sentence is finished (error) or we have found the
   ;     line of the governor. In the body, the trace line has been
   ;     built and put in newlines and newlinks, together with all following lines
                  ((or done (null nxtline))
                     (cond (done
                             (add-ne-trace 
                                   (rest act-governors)
                                   (first trace-and-foll)
                                    newlines newlinks))
                           (t (exception 'parse-error "PROC/chunk-parser: add-ne-trace")
                             (values savlines savlinks))))
   ; ******* BODY **************+
                     (cond ((equal nxtline firstgov)
   ; *** we are on the governor of the trace. The trace is built and put in front
   ;     of the remaining lines and links (allines and allinks)
                              (setq done t)
                              (setq trace-and-foll 
                                 (ins-trace (get-synt-linumb firstgov)
                                            ne-line 'PRON-RMOD '|f| allines allinks 10))
                              (setq newlines (append (reverse (cons nxtline newlines)) 
                                                     (first trace-and-foll)))
                              (setq newlinks (append (reverse (cons nxtlink newlinks))
                                                     (second trace-and-foll))))
                           (t (setq newlines (cons nxtline newlines))
                              (setq newlinks (cons nxtlink newlinks)))))))))

; *****************************************************************************
; *** this looks for unmarked RMOD dependents, that could be interpreted as
;     appositions of a preceding noun
(defun find-appositions (verbline match-labels unlinked allines allinks)
   (let (rem-match rem-unlink app-link prevlines prevlinks nxtlines nxtlinks)
       (do ((nxtlab (first match-labels) (first match-labels))
            (match-labels (rest match-labels) (rest match-labels))
            (nxtunlink (first unlinked) (first unlinked))
            (unlinked (rest unlinked) (rest unlinked)))
           ((null nxtlab)
               (multiple-value-setq (prevlines prevlinks nxtlines nxtlinks)
                    (split-result-lines verbline allines allinks))
               (values rem-match rem-unlink prevlines prevlinks nxtlines nxtlinks))
           (cond ((and (eq nxtlab 'RMOD)
                       (is-appos-unmarked-case nxtunlink allines allinks))
    ; *** this appears as a possible apposition
                    (setq app-link (possible-apposition nxtunlink allines allinks))
                    (cond ((not (null app-link))
    ; *** and in fact there is a possible attachment point
                            (setq allinks
                                 (merge-sing-cf-assign 'APPOSITION nxtunlink 
                                              allines allinks (get-synt-numb app-link)
                                              'appositions)))
    ; *** but it cannot be attached anywhere
                          (t (setq rem-match (append1 rem-match nxtlab))
                             (setq rem-unlink (append3 rem-unlink nxtunlink)))))
    ; *** this is not a possible apposition
                 (t (setq rem-match (append1 rem-match nxtlab))
                    (setq rem-unlink (append3 rem-unlink nxtunlink)))))))

; *****************************************************************************
; *** simply splits the lines and the links, into the ones preceding and following
;     verbline
(defun split-result-lines (verbline allines allinks)
    (do* ((prevlines nil (cons nxtline prevlines))
          (prevlinks nil (cons nxtlink prevlinks))
          (nxtline (first allines) (first tmplines))
          (nxtlink (first allinks) (first tmplinks))
          (tmplines (rest allines) (rest tmplines))
          (tmplinks (rest allinks) (rest tmplinks)))
       ((or (null nxtline)
            (equal verbline nxtline))
          (cond ((null nxtline) 
                   (exception 'parse-error "PROC/chunk-parser: split-result-lines"))
                (t (values prevlines prevlinks tmplines tmplinks))))))

; *****************************************************************************
; *** returns true if the dependent "line" is the head of an unmarked case
(defun is-appos-unmarked-case (line allines allinks)
    (or (eq 'NOUN (get-synt-categ line))
        (is-a-synt-noun-complex line allines allinks)))

; *****************************************************************************
; *** returns true if the dependent "line" is preceded by a comma and by a possible
;     attachment for an apposition
(defun possible-apposition (line allines allinks)
  (let (found)
     (do ((nxtline (first allines) (first templines))
          (templines (rest allines) (rest templines))
          (prevlines nil (cons nxtline prevlines))
          (nxtlink (first allinks) (first templinks))
          (templinks (rest allinks) (rest templinks))
          (prevlinks nil (cons nxtlink prevlinks)))
         ((or (null nxtline) found)
     ; *** found is non-nil when the "possible-apposition" line is found.
     ;     it is 'fail if that line has no possible attachment point, the
     ;     attachment line, otherwise
           (cond ((eq found 'fail) nil)
                 ((not (null found)) found)
                 ((null nxtline)
                    (exception 'parse-error "PROC/chunk-parser: possible-apposition"))))
         (cond ((equal nxtline line)         
                 (cond ((eq (get-synt-word (first prevlines)) #\,)
                          (do ((prevline (second prevlines) (first prevlines))
                               (prevlines (rest (rest prevlines)) (rest prevlines)))
                              ((or (null prevline) 
                                   (not (or (eq (get-synt-categ prevline) 'adj)
                                            (memq (get-synt-word prevline) '(#\' #\")))))  ;"
     ; *** currently, only adjectives can appear between the comma and the attachment
     ;     of the apposition
                                 (cond ((or (null prevline)
                                            (not (memq (get-synt-categ prevline) '(noun pron))))
                                          (setq found 'fail))
                                       (t (setq found prevline))))))
     ; *** if the possible apposition is not preceded by a comma, fail
                       (t (setq found 'fail))))))))

; *****************************************************************************
; *** this selects all transformations that are obligatory or forbidden in a
;     given context
; *** it returns a triple:
;     1. A list of lists of obligatory transformations; see comments in match-caseframes
;     2. A list of forbidden transformation
;     3. A passivization marker, that specifies if passivization is truly obligatory, or
;        if it can be overcome for intransitives
(defun get-transformations (curline curlink allines allinks)
   (declare (special *VERB-CF-TRANSFORM*))
   (let (transf-name transf-type transf-cond cond-val
         pos-result neg-result
         (passive? (is-passive curline allines allinks))
         (inf-form? (is-inf-form curline allines allinks))
         (lobj-clitic? (has-a-cf-lobj-clitic curline allines allinks)))
      (dolist (transf *VERB-CF-TRANSFORM*)
  ; *** get-transf-name, get-transf-type and get-transf-cond in PARSER-PROC-ALL/hier-funct
         (setq transf-name (get-transf-name transf))
         (setq transf-type (get-transf-type transf))
         (setq transf-cond (get-transf-cond transf))
         (setq cond-val (eval-transf-cond transf-cond
                                passive? inf-form? lobj-clitic? curline curlink allines allinks))
  ; *** if the transformation is replacing and the condition is satisfied, then the
  ;     transformation is obligatory;
  ; *** if the transformation is extending and the condition is satisfied, then the
  ;     transformation is optional (so it is neither added in neg nor in pos)
  ; *** if the condition is not satisfied, then the transformation cannot be applied
  ; ??? Da-infinitivization should be checked
         (cond ((and cond-val (eq transf-type 'replacing))
                  (setq pos-result (cons transf-name pos-result)))
               ((not cond-val)
                  (setq neg-result (cons transf-name neg-result)))))
      (setq pos-result (split-oblig-transfs pos-result))
      (values pos-result neg-result (cond ((eq passive? 'obl) t) (t nil)))))
                 
; *****************************************************************************
; *** in case incompatible tranfs are obligatory, this is intended to mean that 
;     at least one of them must be applied
; *** the function looks for subsets of transfs that are mutually incompatible
;     and then produces sets of obligatory transfs, that do not include any
;     incompatible pair
; *** Ex. (t1 t2 t3 t4 t5 t6)
;     If t1, t2, and t3 are incompatible, and the same for t4 and t5, the result is
;     ((t1 t4 t6) (t2 t4 t6) (t3 t4 t6) (t1 t5 t6) (t2 t5 t6) (t3 t5 t6))
(defun split-oblig-transfs (transfs)
  (declare (special *INCOMPATIBLE-TRANSF*))
    (let (incompset nxtincomp nxtactincomp)
    (do ((nxttr (first transfs) (first transftocheck))
         (transftocheck (rest transfs) (rest transftocheck)))
 ; *** nxttr is t1
 ; *** transftocheck is first (t2 t3 t4 t5 t6), but inside the loop
 ;     it is changed to (t4 t5 t6)
        ((null nxttr))
        (setq nxtincomp (first (leggi *INCOMPATIBLE-TRANSF* nxttr)))
 ; *** nxtincomp is (t2 t3 t7 ...)
        (setq nxtactincomp nil)
        (do ((nxtcheck (first transftocheck) (first tocheck))
             (tocheck (rest transftocheck) (rest tocheck)))
            ((null nxtcheck) 
               (setq incompset (cons (cons nxttr nxtactincomp) incompset))
               (setq transftocheck (subtrl transftocheck nxtactincomp)))
 ; *** incompset becomes ((t1 t2 t3))
            (cond ((memq nxtcheck nxtincomp)
                     (setq nxtactincomp (cons nxtcheck nxtactincomp))))))
 ; *** at the end of the loop, incompset is ((t1 t2 t3) (t4 t5) (t6))
 ;     now, we only need a cartesian product
    (list-cartes incompset)))

; *****************************************************************************
; *** this evaluates the condition of a transformation
; *** is-passive, is-inf-form and has-a-lobj-clitic are evaluated just once outside
;     and cached in passive?, inf-form? and lobj-clitic?
(defun eval-transf-cond (transf-cond passive? inf-form? lobj-clitic? 
                                         curline curlink allines allinks)
  (declare (special *LANGUAGE*))
  (cond ((eq transf-cond t) t)
        (t (case (first transf-cond)
               (language (cond ((memb-or-eq *LANGUAGE* (second transf-cond)) t)
                               (t nil)))
               (and (cond ((eval-transf-cond 
                                   (second transf-cond) passive? inf-form? lobj-clitic?
                                   curline curlink allines allinks)
                             (cond ((null (rest (rest transf-cond))) t)
                                   (t (eval-transf-cond 
                                          (cons 'and (rest (rest transf-cond)))
                                          passive? inf-form? lobj-clitic? 
                                          curline curlink allines allinks))))
                          (t nil)))
               (or (cond ((eval-transf-cond 
                                   (second transf-cond) passive? inf-form? lobj-clitic?
                                   curline curlink allines allinks)
                            t)
                         (t (cond ((null (rest (rest transf-cond))) nil)
                                  (t (eval-transf-cond 
                                          (cons 'or (rest (rest transf-cond)))
                                          passive? inf-form? lobj-clitic? 
                                          curline curlink allines allinks))))))
               (not (not (eval-transf-cond (second transf-cond)
                                     passive? inf-form? lobj-clitic?
                                     curline curlink allines allinks)))
               (is-passive passive?)
               (is-inf-form inf-form?)
               (governed-as (eq (second curlink) (second transf-cond)))
                  ; *** the verb is linked to its governor via a link with a given label
               (gov-by-modal-with-lobj-visitor 
                    (gov-by-modal-with-visitor curline curlink allines allinks 'lobj))
               (gov-by-modal-with-liobj-visitor 
                    (gov-by-modal-with-visitor curline curlink allines allinks 'liobj))
               (gov-by-modal-with-lobj-and-liobj-visitor 
                    (gov-by-modal-with-visitor curline curlink allines allinks 'lobj+liobj))
               (has-a-lobj-clitic lobj-clitic?)
               (has-gramm-type
                    (let ((elemcode (second transf-cond))
                          (grammtype (third transf-cond)))
                       (cond ((eq elemcode 'governor)
                                (has-gramm-type 
                                   (get-synt-word (get-parser-governor curlink allines allinks))
                                   grammtype))
                             ((eq elemcode 'gov-gov)
                                (has-gramm-type 
                                   (get-synt-word
                                       (get-parser-governor 
                                             (get-parser-governor curlink allines allinks) 
                                             allines allinks))
                                   grammtype))
                             (t (exception 'parse-error "PROC/chunk-parser: eval-transf-cond")))))
               (up-up-cat
                  (eq (get-synt-categ  
                          (get-parser-governor (get-parser-governor curlink allines allinks)
                                               allines allinks))
                      (second transf-cond)))
               (!overcomes (eval-transf-cond (second transf-cond)
                                     passive? inf-form? lobj-clitic?
                                     curline curlink allines allinks))
               (otherwise (eval transf-cond))))))

; *****************************************************************************
; *** it inspects all caseframes; if two caseframes have the same surface
;     realization, they are merged
; *** caseframe is a list of triples <surface-cf class transfs>
; *** the result is a list of lists <surface-cf (cl1 tr1) (cl2 tr2) ...>
(defun merge-equal-cfs (caseframes)
  (cond ((null caseframes) nil)
	(t (let (eqcasefr diffcasefr (firstcf (first caseframes)))
		(multiple-value-setq (eqcasefr diffcasefr)
		      (find-eq-casefr 
				(first firstcf) 
				(rest caseframes)
				(list (list (second firstcf) (third firstcf)))
				nil))
   ; *** eqcasefr is the info about the caseframes having the same surface
   ;     realization of the first (including the first)
		(cons eqcasefr
                      (merge-equal-cfs diffcasefr))))))

; *****************************************************************************
; *** it inspects othercfs to find a caseframe having the surface realization
;     as the one given in refsurf
; *** returns the equals in eqcf, the different remaining ones in diffcf
; >>> refsurf is the cf triple <surfrealiz surfcases deepcases> to look for
; >>> othercfs is a list of <cf-triple class transfs>, where each cf-triple
;     has the structure shown above for refsurf
; >>> eqcf is an accumulator of pairs <class transfs> having the realization
;     equal to the one in refsurf
; >>> diffcf is an accumulator collecting all caseframes having realizations
;     other than the one in refsurf
(defun find-eq-casefr (refsurf othercfs eqcf diffcf)
  (cond ((null othercfs) (values (cons refsurf eqcf) diffcf))
	(t (let ((firstoth (first othercfs)))
              (cond ((equal refsurf (first firstoth))
   ; *** if they are equal, the recursion goes on with an extended list of
   ;     equal caseframes
	   	       (find-eq-casefr
				refsurf 
				(rest othercfs) 
				(append1 eqcf 
				      (list (second firstoth) (third firstoth)))
			 	diffcf))
   ; *** otherwise, it goes on with an extended list of different caseframes
	   	    (t (find-eq-casefr
				refsurf 
				(rest othercfs) 
		   		eqcf 
		   		(append1 diffcf firstoth))))))))

; *****************************************************************************
(defun get-cf-verbclass (verbline)
  (let ((verbcl (get (get-synt-word verbline) 'verbal-class)))
      (cond ((null verbcl) 
              (setq verbcl (list (get-synt-subcat verbline)))
              (cond ((null verbcl) 
                      '(intrans))   ; *** the default (for English verbs in
                                    ;     dictionary-funct-eng)
                    (t verbcl)))
	    (t verbcl))))

; *****************************************************************************
; *** removes from a list of trasformed verbal classes, the dummy classes (which
;     appear in the hierarchy, but cannot have real verbs as members)
; *** recall that cllist actually is a list of pairs <class, applied transfs>
(defun remove-dummies (cllist)
  (cond ((null cllist) nil)
	((get (first (first cllist)) 'dummy) (remove-dummies (rest cllist)))
	(t (cons (first cllist) (remove-dummies (rest cllist))))))
  
; *****************************************************************************
; *** it inserts a trace after govline; it skips any further component of the
;     parent. The function returns a pair <newsucc, newsucclab>
; *** INPUT:
;  ---> govline: the data of the line governing the trace
;  ---> referent: the line of the referent co-indexed with the trace; if referent=nil, this
;			 is a generic trace
;  ---> tracelab: the label of the arc connecting the trace to the parent (govline)
;  ---> tracetype: the type of trace (F, P, W)
;  ---> succ: the lines of the sentence that follow govline (they possibly include
;  			one or more enclitics)
;  ---> succlab: the labels of the arcs to parents of the lines in succ
;  ---> traceindex: the incremental index of the trace
(defun ins-trace (govlinumb referent tracelab tracetype
                              succ succlab traceindex &optional person number)
  (declare (special *TREE-FORMAT*))
  (cond ((or (null referent) (eq referent 'PRON))
  ; *** the referent can be PRON in case we are adding a VERB-SUBJ trace
           (cond ((null person) (setq person 'ALLVAL)))
           (cond ((null number) (setq number 'ALLVAL)))))
  (cond ((not (atom govlinumb))
          (format t "Trace dependent on another trace in chunk-parser:~% Governor: ~a~%"
                    govlinumb)
          (setq govlinumb (first govlinumb))
          (setq traceindex (1+ traceindex))))
  (cond ((eq *TREE-FORMAT* 'tut)
           (ins-tut-trace govlinumb referent tracelab tracetype 
                              succ succlab traceindex person number))
        (t (ins-flatavm-trace govlinumb referent tracelab tracetype 
                              succ succlab traceindex person number))))

; *****************************************************************************
; *** this builds the trace in tut format
(defun ins-tut-trace (govlinumb referent tracelab tracetype
                                   succ succlab traceindex &optional person number)
  (cond ((null referent)
; *** empty trace
	   (int-ins-trace
		(list
	      	    (list govlinumb traceindex)				; line number
		    #\t
		    '(generic-t pron pers allval allval allval)		; syntinfo
                    nil							; treeinfo
		    'empty)						; trace refernce
	 	(make-link govlinumb tracelab 'traces)			; link
	 	succ
	 	succlab))
        ((eq referent 'pron)
; *** pronominal trace (inserted as a subject of a verb)
           (int-ins-trace
                (list
                    (list govlinumb traceindex)
                    #\t
                    `(generic-t pron pers allval ,number ,person)
                    nil
                    'empty)
	 	(make-link govlinumb tracelab 'traces)
	 	succ
	 	succlab))
; *** otherwise, it is an anaphoric trace
   	(t (int-ins-trace 
		(list 
	      	    (list govlinumb traceindex)				; line number
	      	     #\t
	      	    (get-synt-syntinfo referent)			; syntinfo
                    nil							; treeinfo
	      	    (list (first referent) tracetype))			; trace refernce
	 	(make-link govlinumb tracelab 'traces)			; link
	 	succ
	 	succlab))))

; *****************************************************************************
; *** this builds the trace in (flat) avm format
(defun ins-flatavm-trace (govlinumb referent tracelab tracetype 
                                    succ succlab traceindex &optional person number)
  (cond ((null referent)
; *** empty trace
           (int-ins-trace
                `((posit (,govlinumb ,traceindex))			; new line number
                  (form #\t)
                  (syn ((lemma generic-t) (cat pron) (type pers)	; syntinfo
                        (gender allval) (number allval) (person allval)))
                  (sem nil)						; seminfo
                  (coref empty))				        ; trace reference
                (make-link govlinumb tracelab 'traces)			; link
                succ
                succlab))
        ((eq referent 'pron)
; *** pronominal trace
           (int-ins-trace
                `((posit (,govlinumb ,traceindex))			; new line number
                  (form #\t)
                  (syn ((lemma generic-t) (cat pron) (type pers)	; syntinfo
                        (gender allval) (number ,number) (person ,person)))
                  (sem nil)						; seminfo
                  (coref empty))				        ; trace reference
                (make-link govlinumb tracelab 'traces)			; link
                succ
                succlab))
; *** an anaphoric trace
        (t (int-ins-trace
                `((posit (,govlinumb ,traceindex))
                  (form #\t)						; syntinfo
                  (syn ,(get-flatavm-syntinfo referent))
                  (coref ((line ,(get-flatavm-numb referent)) (ctype ,tracetype))))
                       ; *** the trace reference; assumed to be a partial trace
                (make-link govlinumb tracelab 'traces)			; link
                succ
                succlab))))

; *****************************************************************************
; *** this just skips components of the word after which the trace has to be
;     attached (e.g. clitics)
(defun int-ins-trace (traceline tracelink succ succlab)
   (cond ((null succlab)
; *** end of recursion if trace at end of sentence
	   (list (list traceline) (list tracelink)))
	 ((atom (get-synt-numb (first succ)))
; *** end of recursion if next label atomic: it is not a further component, so
;     insert the label here
	   (list (cons traceline succ) (cons tracelink succlab)))
; *** recursion step
	 (t (let ((res 
		   (int-ins-trace traceline tracelink
                                    (rest succ) (rest succlab))))
	      (list (cons (first succ) (first res))
		    (cons (first succlab) (second res)))))))

; *****************************************************************************
; *** finds lines which have not already been linked; they are potential
;     complements of a preceding verb; the search is made only on the lines
;     following the verb
; *** it tests the next line and:
;     - if it is a barrier stops the search
;       Barriers are defined either in the 'test-barrier' function, or 
;       explicitly in the body of this function.
;       There are three types of barriers:
;       - exclusive: the line blocks the search and is NOT included in the
;	  result. All barriers defined in 'test-barriers' belong to this
;   	  category. They are:
;	  . The end of the sentence
;	  . Relative pronouns
;	  . Conjunctions whose first conjunct is the verb whose dependents are
;	    being sought.
;	  Other conditions blocking the search are:
;	  . Another finite verb
;	  . A clitic which is not an enclitic of the current verb (since
;	    proclitics precede the verb, this must depend on a following verb)
;	  . The 'non' adverb (which usually precedes the verb)
;	  . The 'da' preposition, if followed by a pronoun and a past participle,
;	    which is assumed to be the agent complement of the reduced relative
; 	    (da lui visto)
;	  . A comma followed by an unmarked NP (assumed to be the subject of a
;	    following verb)
;	  . An unmarked NP, if we are looking for complements of a reduced
;	    relative clause
;  	- inclusive: the line blocks the search, but it is included in the result
;	  as last found dependent:
;	  . A gerund or infinite verb (but not past participles)
;	  . A subordinative conjunction
;	  . The doubly attached pronoun 'chi'
;       - suspending: the line blocks the search, it is not included in the result
;         but the next verb is included
;         . A question adverb (as "dove" "quando": Dimmi dove Vai)
;     - if it is a possible complement, it is appended to the result and the
;       function calls itself recursively on the rest of the sentence
;     - otherwise, it calls itself on the rest of the sentence
(defun find-aft-compl (verbline verblink nxtlines nxtlinks prevlines prevlinks
                      allines allinks dir-disc? conjunction-line)
 (declare (special *LANGUAGE* *TRACE-UNL*))
 (let (founddeps stop w-categ w-type w-mood w-tense w-lemma w-linked? interr-sent
       w-double-relpr? w-linkup is-there-a-verb-ahead unlinked-verb-ahead mainverb
       mainverb-linked same-v-chunk w-cases w-nominative? verb-ahead w-linklab w-line
       w-n-categ w-n-type w-n-lemma w-n-mood w-n-clitic? w-n2-categ w-n2-type w-clitic? 
       in-reduced-past-part vb-linumb vb-mood vb-type vb-linklab vb-tense
       non-projective-attach past-part-noaux)
; *** basic infos about the verb in focus and on the sentence
   (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
             (format t "*** Entering Find-after-compl. Verbline: ~a~%" verbline)
             (break "")))
   (setq vb-linumb (get-synt-linumb verbline))
   (setq vb-type (get-synt-type verbline))
   (setq vb-mood (get-synt-mood verbline))
   (setq vb-tense (get-synt-tense verbline))
   (setq vb-linklab (second verblink))
   (setq interr-sent 
        (or (eq #\? (get-synt-word (ult nxtlines)))
            (member 'INTERR (mapcar #'get-synt-type allines))))
   (setq past-part-noaux (is-reduced-relative verbline prevlines))
; *** basic loop on all the items following the verb in focus *********************
   (do* ((prvline nil nxtline)
         (nxtline (first nxtlines) (first nxtlines))
         (nxtlines (rest nxtlines) (rest nxtlines))
         (nxtlink (first nxtlinks) (first nxtlinks))
         (nxtlinks (rest nxtlinks) (rest nxtlinks)))
      ((or (null nxtline) stop) 
         (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                   (format t "*** Exiting Find-after-compl. Dependents: ~a~%" founddeps)
                   (break "§§§§ END §§§§")))
         founddeps)
; *** basic infos about the next word **********
      (setq w-linked? (not (null nxtlink)))
      (setq w-linkup (first nxtlink))
      (setq w-linklab (second nxtlink))
      (setq w-categ (get-synt-categ nxtline))
      (setq w-type (get-synt-type nxtline))
      (setq w-lemma (get-synt-word nxtline))
      (setq w-line (get-synt-numb nxtline))
      (setq non-projective-attach (test-not-projective w-line nxtlinks vb-linumb))
; *** basic infos about the following words ****
      (setq w-n-categ (get-synt-categ (first nxtlines)))
      (setq w-n-type (get-synt-type (first nxtlines)))
      (setq w-n-lemma (get-synt-word (first nxtlines)))
      (setq w-n-mood (cond ((eq w-n-categ 'VERB)
                              (get-synt-mood (first nxtlines)))
                           (t nil)))
      (setq w-n2-categ (get-synt-categ (second nxtlines)))
      (setq w-n2-type (get-synt-type (second nxtlines)))
      (setq is-there-a-verb-ahead
        (member 'VERB (mapcar #'get-synt-categ (rest (rest nxtlines)))))
      (setq unlinked-verb-ahead
        (find-next-unlinked-mainv nxtlines nxtlinks))
; *** further useful infos in case the next word is a pronoun
      (cond ((eq w-categ 'PRON) 
              (setq w-clitic? (is-a-synt-clitic? nxtline))
              (setq w-cases (get-all-cases nxtline))
              (setq w-nominative? (and (member 'lsubj w-cases) (not (member 'lobj w-cases))))
              (cond ((eq w-type 'RELAT) 
                      (setq w-double-relpr?
                           (and (has-gramm-type w-lemma '&double-who)
                                (not (has-gramm-type w-lemma '&base-relat))))))))
; *** further useful infos in case the following word is a pronoun
      (cond ((eq w-n-categ 'PRON) 
              (setq w-n-clitic? (is-a-synt-clitic? (first nxtlines)))))
; *** further useful infos in case the next word is a verb
      (cond ((eq w-categ 'VERB)
               (setq w-mood (get-synt-mood nxtline))
               (setq w-tense (get-synt-tense nxtline))
               (setq same-v-chunk
                   (belong-to-chunk verbline verblink w-line nil allines allinks nil))
               (cond ((memq w-mood '(GERUND INFINITE))
                        (cond ((eq w-type 'AUX)
                                 (cond ((null w-linkup)
                                          (setq mainverb nil))
                                       (t (setq mainverb (find-a-line `(position ,w-linkup)
                                                                 nxtlines nxtlinks)))))
                              (t (setq mainverb (list nxtline nxtlink))))
                        (setq mainverb-linked (not (null (second mainverb))))))))
; *** further useful infos in case the next word is an adverb
      (cond ((eq w-categ 'ADV)
               (setq verb-ahead 
                  (first (find-a-line '(categ (verb) type (main mod)) nxtlines nxtlinks)))))
      (cond
; %%% conditions for terminating the search (no more possible dependents) %%%%%%%%%%%%%%%%%%%
            ((or (eq vb-linklab 'ADJC+QUALIF-RMOD)					;[01]
               ; *** the verb is already linked (presumably to a following noun) as an 
               ;     adjectival modifier (as in English participles: "the presumed advantage")
                 (and (eq w-categ 'ADV) (has-gramm-type w-lemma '&neg-adv) 		;[02]
                      (or w-n-clitic? (eq w-n-categ 'VERB))
                      (neq *LANGUAGE* 'english))
               ; *** it is a negative adverb, the next line is a clitic or a verb,
               ;     and the language is not English
                 (and (eq w-categ 'ART) (has-gramm-type w-n-lemma '&art-mid-relat))	;[03]
               ; *** it is an article followed by a mid relative pronoun ("cui", "quale")
                 (and (eq w-categ 'CONJ) (eq w-type 'SUBORD) w-linked?)			;[04]
               ; *** the item is an already linked subordinating conjunction
                 (and (eq w-categ 'CONJ) (eq w-type 'COORD) 				;[05]
                      (eq w-linkup vb-linumb) (neq w-n-categ 'PHRAS))
               ; *** conjunctions linked to the verb in focus
               ;       ex. John saw Mary AND BILL met Harry
               ; *** the last conjunct excludes from barriers cases as 
               ;     'pubblicare o no l'opera'
                 (and (eq w-categ 'PREP) (foll-relpron nxtlines))			;[06]
               ; *** it is a PP governing a relative pronoun
                 (and (eq w-categ 'PRON) (eq *LANGUAGE* 'italian) w-clitic? 		;[07]
                      (not (same-linumb vb-linumb w-line)))
               ; *** the item is a clitic pronoun not included in the verb (enclitic). 
               ;     Other clitics (proclitics) must precede the verb, so they cannot be
               ;     attached to a preceding verb
                 (and (eq w-categ 'PRON) (eq w-type 'RELAT)				;[08]
                      (or (and w-linked? w-double-relpr?)
                          (and (not w-double-relpr?) (not (is-a-synt-trace? nxtline)))))
               ; *** (unambiguous) double rel pron already linked
                 (and (eq w-categ 'PRON) w-nominative?					;[09]
                      (or (neq *LANGUAGE* 'english) 
                          (and (not interr-sent) 
                               (not (has-loc-pron prevlines)))))
               ; *** it is a nominative pron, and we are not in case of English inversion
                 (and (eq w-categ 'PUNCT) (eq w-lemma #\,)				;[10]
                      (or (and (eq w-n-categ 'ADV) (eq w-n2-categ 'VERB))
                          (and (eq w-n-categ 'PRON) w-nominative? (eq w-n2-categ 'VERB))
                          (and (or (eq w-n-categ 'NOUN)
                                   (is-a-synt-noun-complex (first nxtlines) allines allinks))
                               is-there-a-verb-ahead)))
               ; *** the item is a comma that, presumably, separates two clauses
                 (and (eq w-categ 'VERB) 						;[11]
                      (memq w-mood '(GERUND INFINITE)) 
                      (or (null mainverb) mainverb-linked))
               ; *** it is a gerund or infinite verb and it (if it is main) or its
               ;     governor (if it's an auxiliary) is already linked
                 (and (eq w-categ 'VERB) 						;[12]
                      (eq w-mood 'PARTICIPLE) (eq w-tense 'PAST) 
                      (or w-linked? past-part-noaux))
               ; *** it is an already linked past participle, or a past participle
               ;     without an auxiliary: this is for "VISTE le premesse, partirò",
               ;     where "partirò" stops the search, assuming that "viste" is a
               ;     dependent of it
                 (and (eq w-categ 'VERB) 						;[13]
                      (or (not (memq w-mood '(GERUND INFINITE))) mainverb-linked)
                      (or (not (eq w-mood 'PARTICIPLE)) (not (eq w-tense 'PAST)) w-linked?)
                      (or (not dir-disc?) w-linked?
                          (and (eq 1 vb-linumb) (eq 'PARTICIPLE vb-mood))
                          same-v-chunk))
               ; *** it is a verb, but not a gerund or infinite with an unlinked main and is
               ;     not an unlinked past participle and is not in the same chunk of the verb
               ;     in focus
                 (and (eq w-categ 'VERB) conjunction-line 				;[14]
                      (eq w-linkup conjunction-line))
               ; *** in case a coordinating conjunction is found, no element can be
               ;     linked to a preceding verb, until the second conjunct is found
               ; *** the second conjunct is identified by having the link up
               ;     pointing to the conjunction line number (which is in 'conjunction-line')
                 (and (eq w-categ 'PHRAS)
                      (eq (get-synt-word prvline) #\,))
                 (and (memq w-categ '(ART NOUN PRON)) in-reduced-past-part))		;[15]
               ; *** see the comments for ADJ not QUALIF
               (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                                  (format t "-- Nxtline: ~a~%" nxtline)
                                  (break "After: One")))
               (setq stop t))
; %%% conditions for terminating the search after adding the current element %%%%%%%%%%%%%%%%
            ((and (not w-linked?)
                  (not non-projective-attach)
               (or (and (eq w-categ 'CONJ) (eq w-type 'SUBORD))			;[01]
               ; *** the item is an unlinked subordinating conjunction
                   (and (eq w-categ 'PRON) (eq w-type 'RELAT)				;[02] 
                      (and (or (not w-linked?) (not w-double-relpr?))
                               (or w-double-relpr? (is-a-synt-trace? nxtline))))
               ; *** (unambiguous) double rel pron not already linked
                   (and (eq w-categ 'VERB) dir-disc?					;[03]
                      (not (and (eq 1 vb-linumb) (eq 'PARTICIPLE vb-mood)))
                      (not same-v-chunk))
               ; *** an unlinked verb, but not a participle at the beginning of the
               ;     sentence, and not with the verb in focus already linked to it
               ;     if the verb in focus admits direct discourse
                   (and (eq w-categ 'PRON) (eq w-type 'RELAT)				;[05] 
                        (or w-double-relpr? (not (is-a-synt-trace? nxtline))))
               ; *** (unambiguous) double rel pron not linked
                   (and (eq w-categ 'ADV) (has-gramm-type w-lemma '&neg-adv) 		;[10]
                        (or (is-a-synt-clitic? (first nxtlines)) (eq w-n-categ 'VERB))
                        (eq *LANGUAGE* 'english)
                        (or (not (null (first nxtlinks)))
                            (neq w-n-mood 'INFINITE)
                            (neq vb-type 'MOD)))))
               ; *** it is a negative adverb, the next line is a clitic or a verb,
               ;     the language is English and the next item is not an
               ;     unlinked infinite, with a modal verb in focus
               (setq founddeps (append1 founddeps nxtline))
               (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                                  (format t "-- Nxtline: ~a~%" nxtline)
                                  (break "After: Two")))
               (setq stop t))
; %%% conditions for terminating the search after adding some other element %%%%%%%%%%%%%%%%
            ((or (and (eq w-categ 'VERB) 						;[13]
                      (memq w-mood '(GERUND INFINITE)) 
                      (not (null mainverb)) (not mainverb-linked))
               ; *** it is a gerund or infinite verb and it (if it is main) or its
               ;     governor (if it's an auxiliary) is not already linked
                 (and (memq w-categ '(ADJ PRON)) (eq w-type 'INTERR)		;[06] 
                      (not unlinked-verb-ahead)))
               ; *** a question element, but without an unlinked verb ahead
               (setq founddeps (append1 founddeps (first mainverb)))
               (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                                  (format t "-- Nxtline: ~a~%" nxtline)
                                  (break "After: Three")))
               (setq stop t))
             ((and (eq w-categ 'ADV)
                   (has-gramm-type w-lemma '&neg-adv)
                   (or (is-a-synt-clitic? (second nxtlines))
                       (eq 'VERB (get-synt-categ (second nxtlines))))
                   (eq *LANGUAGE* 'english)
                   (eq w-n-mood 'INFINITE)
                   (eq vb-type 'MOD)
                   (not w-linked?)
                   (null (second nxtlinks)))
               (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                                  (format t "-- Nxtline: ~a~%" nxtline)
                                  (break "After: Four")))
               (setq founddeps (append founddeps (list nxtline (first nxtlines))))
               (setq stop t))
             ((and (eq w-categ 'ADV)
                   (has-gramm-type w-lemma '&neg-adv)
                   (or (is-a-synt-clitic? (second nxtlines))
                       (eq 'VERB (get-synt-categ (second nxtlines))))
                   (eq *LANGUAGE* 'english)
                   (eq w-n-mood 'INFINITE)
                   (eq vb-type 'MOD)
                   w-linked?
                   (null (second nxtlinks)))
               (setq founddeps (append1 founddeps (first nxtlines)))
               (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                                  (format t "-- Nxtline: ~a~%" nxtline)
                                  (break "After: Five")))
               (setq stop t))
             ((or (and (eq w-categ 'PREP) 
                       (or (eq w-n-type 'INTERR) (eq w-n2-type 'INTERR))
                       unlinked-verb-ahead)
               ; *** it is a question PP, and there is a verb ahead
                  (and (eq w-categ 'ADV) (eq w-type 'INTERR)
                       unlinked-verb-ahead)
               ; *** it is a question adverb, and there is a verb ahead
                  (and (memq w-categ '(ADJ PRON)) (eq w-type 'INTERR)		;[06] 
                       unlinked-verb-ahead))
               ; *** a question element, with an unlinked verb ahead
               (setq founddeps (append1 founddeps unlinked-verb-ahead))
               (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                                  (format t "-- Nxtline: ~a~%" nxtline)
                                  (break "After: Six")))
               (setq stop t))
; %%% some special conditions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
             ((and (eq w-categ 'VERB)
                   (eq w-mood 'PARTICIPLE) (eq w-tense 'PAST)
                   (not w-linked?))
               ; *** an unlinked past participle
               (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                                  (format t "-- Nxtline: ~a~%" nxtline)
                                  (break "After: Seven")))
               (setq in-reduced-past-part 'skip))
             ((or (and (eq w-categ 'CONJ) (eq w-type 'COORD))
                       (eq (get-synt-word nxtline) #\&)
                       (eq w-linklab 'COORD))
                ; *** a coordinative conjuction produces a skip until the second conjunct is
                ;     found
               (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                                  (format t "-- Nxtline: ~a~%" nxtline)
                                  (break "After: Eight")))
               (setq conjunction-line w-line))
             ((and (eq w-categ 'PREP) (has-gramm-type w-lemma '&agent-compl-prep)
                   (eq w-n-categ 'PRON) (eq w-n-type 'RELAT)
                   (eq w-n2-categ 'VERB) 
                   (eq (get-synt-mood (second nxtlines)) 'PARTICIPLE)
                   (eq (get-synt-tense (second nxtlines)) 'PAST))
               (setq nxtlines (rest (rest nxtlines)))
               (setq nxtlinks (rest (rest nxtlinks)))
               (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                                  (format t "-- Nxtline: ~a~%" nxtline)
                                  (break "After: Nine")))
               (setq in-reduced-past-part t))
            ((or (and (memq w-categ '(ADJ PRON)) (eq w-type 'INTERR)		;[06] 
                      (not unlinked-verb-ahead))
               ; *** a question element, but without an unlinked verb ahead
                 (and (eq w-categ 'PREP) 						;[07]
                      (or (eq w-n-type 'INTERR) (eq w-n2-type 'INTERR))
                      (null unlinked-verb-ahead))
               ; *** it is a question PP, but there is no verb ahead
                 (and (eq w-categ 'PRON) w-nominative?					;[11]
                      (eq *LANGUAGE* 'english) 
                      (or interr-sent (has-loc-pron prevlines)))
               ; *** it is a not nominative pron, and we are in case of English inversion
                 (and (memq w-categ '(ART NOUN PRON)) 				;[09]
                      in-reduced-past-part))
               ; *** see the comments for ADJ not QUALIF
               (setq in-reduced-past-part nil)
               (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                                  (format t "-- Nxtline: ~a~%" nxtline)
                                  (break "After: Ten")))
               (setq founddeps (append1 founddeps nxtline)))
; %%% conditions for adding the current item and going ahead %%%%%%%%%%%%%%%%
            ((and (not w-linked?)
                  (not non-projective-attach)
               (or (and (eq w-categ 'ADJ) (eq w-type 'QUALIF)				;[07]
                        (not in-reduced-past-part))
               ; *** an unlinked qualificative adjective, looking for complements
               ;     of a reduced relative
                   (and (eq w-categ 'ADJ) (neq w-type 'QUALIF)			;[08]
                        in-reduced-past-part)
               ; *** if the adjective is not qualif, this is a noun complex, and
               ;     we are not looking for the complements of a reduced relative
                   (and (not in-reduced-past-part)
                        (neq w-categ 'PUNCT)
                        (neq w-categ 'MARKER)
                        (neq w-categ 'SPECIAL))))
               (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                                  (format t "-- Nxtline: ~a~%" nxtline)
                                  (break "After: Eleven")))
               (setq founddeps (append1 founddeps nxtline)))
; %%% conditions for skipping the current item and going ahead %%%%%%%%%%%%%%%%
            ((or w-linked?
               ; *** the current item is already linked
                 non-projective-attach
                 in-reduced-past-part
                 (eq w-categ 'PUNCT)
                 (eq w-categ 'MARKER)
                 (eq w-categ 'SPECIAL)
                 (and (eq w-categ 'ADJ) (eq w-type 'QUALIF) (not in-reduced-past-part))
                 (and conjunction-line
                      (neq w-linkup conjunction-line)))
               ; *** in case a coordinating conjunction is found, no element can be
               ;     linked to a preceding verb, until the second conjunct is found
               ; *** the second conjunct is identified by having the link up
               ;     pointing to the conjunction line number (which is in 'conjunction-line')
               (cond ((and (not (null *TRACE-UNL*)) (> *TRACE-UNL* 1))
                                  (format t "-- Nxtline: ~a~%" nxtline)
                                  (break "After: Twelve")))
               nil)
         )	; end of the body fo the do
       )      ; end of the do
     ))     ; end of let and defun

; *****************************************************************************
; *** if the unattached word is followed by a word already attached to another
;     word occurring between the verb and the word to attach, the attachement is
;     not possible (projectivity)
;               --------------------
;               V                  |
;      VERB ... W1 ... UNATT  .... W2 ...
(defun test-not-projective (linumb nxtlinks verblinumb)
    ; (format t "enter not projective; linumb= ~a~%" linumb)
   (let (found)
      (do ((nxtlink (first nxtlinks) (first nxtlinks))
           (nxtlinks (rest nxtlinks) (rest nxtlinks)))
         ((or (and (null nxtlink) (null nxtlinks))
              found)
            found)
     ;(format t "nxtlink= ~a~%" nxtlink)
     ;(break "")
         (cond ((and (not (null nxtlink))
                     (index-precedes (first nxtlink) linumb)
                     (index-precedes verblinumb (first nxtlink)))
                  (setq found t))))))

; *****************************************************************************
; *** there is in lines an item of the grammtype &loc-pron ("ci", "vi" for Italian;
;     "there" for English)
(defun has-loc-pron (lines)
   (cond ((null lines) nil)
         ((has-gramm-type (get-synt-word (first lines)) '&loc-pron) t)
         (t (has-loc-pron (rest lines)))))

; *****************************************************************************
; *** looks for an unlinked main verb after a given line (in nextlines)
(defun find-next-unlinked-mainv (nxtlines nxtlinks)
  (cond ((null nxtlines) nil)
        ((and (eq (get-synt-categ (first nxtlines)) 'VERB) 
              (neq (get-synt-type (first nxtlines)) 'AUX))
          (cond ((null (first nxtlinks))
                   (first nxtlines))
                (t nil)))
        (t (find-next-unlinked-mainv (rest nxtlines) (rest nxtlinks)))))

; *****************************************************************************
; *** this looks for sequences of dependents (found by find-aft-compl), which are
;     similar in structure and (from the second onward) are preceded by commas.
;     If such a sequence (of length greater than 3) is found, then all elements
;     after the first are considered as conjuncts, and returned separately in the
;     form <word0, comma1, word1, ..., comman, wordn>
(defun conjoin-equal-deps (words allines allinks)
  (let (found start (count 0) conjseq oldcateg)
     (do ((nxtword (first words) (first words))
          (words (rest words) (rest words))
	  (pos 1 (1+ pos)))
	 ((or found (null nxtword)))
; *** if we find a new line satisfying the requirements for continuing the sequence
;     (same category of the previous item, i.e. oldcateg)
   	(cond ((and (not (null oldcateg))
		    (eq oldcateg (get-synt-categ nxtword)))
		(let ((precl (find-prec-line nxtword allines allinks)))
		    (cond ((equal #\, (get-synt-word precl))
                        ; *** if the new item is preceded by a comma add it to the 
                        ;     sequence of possible conjuncts
		 	    (setq count (1+ count))
			    (setq conjseq (append conjseq (list precl nxtword))))
	      		  ((> count 2)
			; *** if the element is not preceded by a comma, but we
			;     have already found a complete sequence (count > 2),
			;     finish the search
			    (setq found t))
		        ; *** if we do not have a previous sequence, 
			;     initialize a sequence
	      		  (t ;(setq oldelem (get-synt-word nxtword))
	      		     (setq oldcateg (get-synt-categ nxtword))
	   		     (setq start pos)
			     (setq conjseq (list nxtword))
	   		     (setq count 1)))))
; *** if it is not possible to continue a sequence, but we have already found
;     a complete sequence (count > 2), finish the search
	      ((> count 2)
		(setq found t))
; *** if we do not have a previous sequence, but we find new preposition,
;     initialize a sequence
	      (t	;(eq 'prep (get-synt-categ nxtword))
	   	        ;(setq oldelem (get-synt-word nxtword))
	   	(setq oldcateg (get-synt-categ nxtword))
	   	(setq start pos)
		(setq conjseq (list nxtword))
	   	(setq count 1))
		))
; *** the search for a sequence (do) is finished
;     Now, if count is greater than 2, conjseq consists in 
;     <word0, comma1, word1, ..., comman, wordn>
     (cond ((> count 2) 
	     ; (break "conjoin-equal-deps")
	      (values (rem-from-list words (1+ start) (1- count)) conjseq))
	   (t (values words nil)))))

; *****************************************************************************
; *** given a sequence
;     [<line1, elem1> <line2, comma2> <line3, elem3> ... <linek, elemk>]
;       which contains an odd number of elements, builds the sequence
;     [<line2, <line1, coord>>, <line3, <line2, coord-2nd> ... 
;		<linek, <linek-1, coord-2nd>>
; *** the input is the result of the detection of a sequence of elements preceded
;     by commas (in conjoin-equal-deps, during the search for verb dependents)
; *** the second is the input to the function 'mult-change-labs', which will
;     change the links of the commas and the elements, assumed to be a sequence
;     of coordinated elements
(defun build-conj-labs (possconjs)
   (cond ((null (rest possconjs)) nil)
	 (t (cons (list (get-synt-numb (second possconjs))
			(make-link (get-synt-numb (first possconjs)) 'COORD 'conj-in-verbal))
		  (cons (list (get-synt-numb (third possconjs))
			      (make-link (get-synt-numb (second possconjs)) 
				    'COORD2ND 'conj-in-verbal))
			(build-conj-labs (rest (rest possconjs))))))))

; *****************************************************************************
(defun test-barriers (nxtlines nxtlinks verblinumb)
  (let ((nxtcateg (get-synt-categ (first nxtlines))))
; *** end of sentence
    (or (null nxtlines)
; *** relative pronouns
	(and (eq nxtcateg 'PRON)
	     (eq (get-synt-type (first nxtlines)) 'RELAT)
	     (or (not (has-gramm-type (get-synt-word (first nxtlines)) '&double-who))
	         (has-gramm-type (get-synt-word (first nxtlines)) '&base-relat))
	     (not (is-a-synt-trace? (first nxtlines))))
; *** 'cui', 'quale' relative pronouns
	(and (eq nxtcateg 'ART)
	     (has-gramm-type (get-synt-word (second nxtlines)) '&art-mid-relat))
; *** prepositions governing (followed by) a relative pronoun or an article and
;     a relative pronoun ("per il quale")
;     or governing a question element "Vorrei sapere in quale albergo posso ..."
	(and (eq nxtcateg 'PREP)
	     (foll-relpron (rest nxtlines)))
; *** conjunctions linked to the verb whose complements are sought stop the search
;	ex. John saw Mary AND BILL met Harry
; *** the last conjunct excludes from barriers cases as 'pubblicare o no l'opera'
	(and (eq nxtcateg 'CONJ)
	     (eq (get-synt-type (first nxtlines)) 'COORD)
	     (equal (first (first nxtlinks)) verblinumb)
	     (neq (get-synt-categ (second nxtlines)) 'PHRAS)))))

; *****************************************************************************
; *** checks if a line (a prep) is followed by a relpron
(defun foll-relpron (nxtlines)
  (or (and (eq (get-synt-categ (first nxtlines)) 'PRON)
	   (eq (get-synt-type (first nxtlines)) 'RELAT)
           (not (has-gramm-type (get-synt-word (first nxtlines)) '&double-who)))
      (and (eq (get-synt-categ (first nxtlines)) 'ART)
	   (eq (get-synt-categ (second nxtlines)) 'PRON)
	   (eq (get-synt-type (second nxtlines)) 'RELAT))))

; *****************************************************************************
; *** finds all classes derived from vclass to which all transformations in
;     'transfs' and no transformations in 'notransfs' have been applied
; *** returns a list of pairs <class, list-of-applied-trasformations>
(defun find-derived-cl (vclass transfs notransfs passive-obl)
; *** transfs is a list of lists; see the comments above
  (declare (special *LANGUAGE* *VERB-CF-TRANSFORM*))
   ; *** if "passivization" is among the transformations to apply, but the class is
   ;     not passivizable, then remove 'passivization', unless passive-obl is t. 
   ;     This can happen, because the
   ;     auxiliary 'to be' is assumed to mark passivization, which should not happen
   ;     for intransitives. Note, in fact, that in Italian the auxiliary is "to be"
   ;     for the past of the intransitives: "Marco e' andato": "Marco is gone"
   ;     However, when the auxiliary is checked, it is not known
   ;     if the verb is intransitive; at that time, its actual class is unknown, so
   ;     that it could be either transitive or intransitive.
   ;     But if passive-obl is t, the auxiliary is "andare" or "venire", so that 
   ;     the construction is necessarily passive
   (cond ((and (neq *LANGUAGE* 'english)
      ; *** in English, passive is always marked by "to be"
               (not passive-obl)
               (not (cf-hier-dep (list vclass)
                             (third (assoc 'passivization *VERB-CF-TRANSFORM*))
                             nil)))
            (mult-find-deriv-cl vclass 
                  (mapcar #'(lambda (x) (remove 'passivization x)) transfs) notransfs))
         (t (mult-find-deriv-cl vclass transfs notransfs))))

; *****************************************************************************
(defun mult-find-deriv-cl (vclass transfs notransfs)
  (let (result)
     (cond ((null transfs)
              (find-int-deriv-cl vclass nil notransfs nil))
           (t (dolist (nxttrgroup transfs result)
                  (setq result (append result 
                                   (find-int-deriv-cl vclass nxttrgroup notransfs nil))))))))

; *****************************************************************************
(defun find-int-deriv-cl (vclass transfs notransfs appliedtransfs)
  (let ((dercl (get-inthn-tr-sons vclass))
	(result (cond ((null transfs) (list (list vclass appliedtransfs)))
		      (t nil))))
; *** the dolist moves horizontally in the tree of derived patterns
;     while the recursive call below moves downward
      (dolist (nxtcl dercl result)
	 (let ((appliedtr (first (get-inthn-tr-source nxtcl))))
	   (cond ((not (member appliedtr notransfs))
; *** if nxtcl is obtained via one of the forbidden transformations, the search
;     from nxtcl is blocked
; *** otherwise, the local result is extended via the recursive call
		      (setq result
			(append result
				(find-int-deriv-cl
					nxtcl
					(remove appliedtr transfs)
					notransfs
					(cons appliedtr appliedtransfs))))))))))

; *****************************************************************************
; *** it tries to find the best correspondence between a set of verbal dependents
;     found in the sentences, and one of the caseframes associated with the
;     verb (in 'caseframes')
; *** INPUT:
;  >>> unlinked: a list of data lines which can be possibly linked to the verb
;  >>> caseframes: the list of potential caseframes (already transformed and
;	filtered) which could describe the verb occurrence. They have the form
;      ((cf1 (base11 transfs11) (base12 transf12)) (cf2 (base21 transfs21)) ...)
;	where cfi is the actual surface caseframe to check, basein is the 
;       basic class from which it originates, and transfin are the applied
;       transformations
;  >>> verbline: the line of the head verb
;  >>> verblink: the arc to the governor of the verb (possibly nil)
;  >>> allines: the lines of the sentence
;  >>> allinks: the arcs to the parents
;  >>> preferences: since, in this context, the subject has usually been already
;	assigned, I introduced the optional parameter "preferences" to enable
;	a reordering of the caseframes, so that the object appears (and is
;	matched before) the subject. To obtain this, the parameter must have the
;	value 'obj-subj', which is the only value which has an effect. Actually,
;	this is done in two steps, since a possible locution object must precede
;    	the direct object
; *** OUTPUT:
;  >>> a list of lists; the first element of each list is a pair <n foundroles>,
;      where n is the number of kb roles which remained unmatched (complements
;      not found in the sentence) and foundroles are the labels to be assigned
;      to the cases. The rest of the list are the original pairs base+transfs
(defun check-caseframes 
      (unlinked caseframes verbline verblink allines allinks &optional preferences)
  (cond ((null caseframes) nil)
; *** 'first' is the first caseframe to check
; *** 'first first' is the surface realization
	(t (let* ((curdefs (first (first (first caseframes))))
		  (curcases (second (first (first caseframes))))
		  (curroles (third (first (first caseframes)))))
; *** the next moves emptycompl in front of case lists
	  ;   (cond ((member 'EMPTYCOMPL curroles)
	;	      (setq curcases 
	;		   (move-kb-first 'EMPTYCOMPL curcases curroles nil))
	;	      (setq curdefs 
	;		   (move-kb-first 'EMPTYCOMPL curdefs curroles nil))
	;	      (setq curroles 
	;		   (move-kb-first 'EMPTYCOMPL curroles curroles nil))))
; *** check on the first caseframe, and recursive call
; *** firstres is a list of lists of labels; each list of labels for a possible
;     match
; *** singres is a list of pairs <number of unmatched complements, list of labels>
	       (let (firstres singres)
	            (setq firstres 
                         (cond ((null unlinked) (list nil))
                               (t (check-sing-casefr 
		     			unlinked curdefs curcases curroles
		     			verbline verblink allines allinks))))
		    (setq singres 
                           (add-empty-cases curroles curcases firstres))
; *** a verbal locution cannot be accepted without its locution case
		    (append 
                      (build-final-cf-repr
			   (filter-not-locut (locut-member curroles) singres)
			   (rest (first caseframes)))
		      (check-caseframes 
		  	   unlinked (rest caseframes) verbline
			   verblink allines allinks preferences)))))))

; *****************************************************************************
; *** if there are locutionary cases, they must be present
(defun filter-not-locut (locutcases singres)
  (cond ((null locutcases) singres)
        ((null singres) nil)
        ((subset locutcases (second (first singres)))
	  (cons (first singres) (filter-not-locut locutcases (rest singres))))
	(t (filter-not-locut locutcases (rest singres)))))

; *****************************************************************************
; *** it attaches to all solutions found the infos on the applied transformations
(defun build-final-cf-repr (singres transfs)
  (mapcar #'(lambda (x) (cons x transfs)) singres))
 
; *****************************************************************************
(defun move-kb-first (lab target lablist acc)
   (cond ((null lablist) (exception 'parse-error "PROC/chunk-parser: move-kb-first"))
	 ((eq lab (first lablist))
	    (cons (first target) (append acc (rest target))))
	 (t (move-kb-first lab 
		(rest target) (rest lablist) (append1 acc (first target))))))

; *****************************************************************************
; *** this function checks if there are any moved elements left in curroles;
;     it simply calls int-add-e-cases on all matches in firstres
; *** if there was no match found, firstres is set to (nil), so that int-add-e-cases
;     is applied once
(defun add-empty-cases (curroles curcases firstres)
 (cond ((null firstres) nil)
       (t (cons (int-add-e-cases curroles curcases (first firstres))
		(add-empty-cases curroles curcases (rest firstres))))))

; *****************************************************************************
; *** this function checks if there are any moved elements left in curroles;
;     they have not been found (so that they do not appear in firstres) and 
;     their corresponding surface case must end with nil (ex. s-verb-obj-nil)
; *** it returns the number of roles not found at the end and a list of (possibly
;     extended with traces) found roles
(defun int-add-e-cases (curroles curcases firstres)
 (let (foundrole found-s-case remroles remcases traces)
     (multiple-value-setq (foundrole found-s-case remroles remcases)
              (find-s-nil curroles curcases))
  ; *** initialization of the loop: if a cancelled case is found, then the
  ;     corresponding deep case is put in foundrole, the surface case in 
  ;     found-s-case, and remroles and remcases contain the roles and cases
  ;     remained unmatched; this search loop is necessary, since more than one
  ;     trace may exist
       (do ()
           ((null foundrole)
 	      (let ((unmatch (- (length curroles)
                                (+ (length traces) 
                                   (- (length firstres) 
                                      (count-adjunct firstres 0))))))
  ; *** too many things may have been matched, in case a role was taken as an
  ;     adjunct, and many traces are needed
                    (cond ((< unmatch 0) 'fail)
                          (t (list unmatch (append firstres traces))))))
           (setq traces 
               (cons (concatl 
                        (put-separator #\/ (list foundrole found-s-case nil)))
                     traces))
           (multiple-value-setq (foundrole found-s-case remroles remcases)
                             (find-s-nil remroles remcases)))))

; *****************************************************************************
; *** this looks for cancelled cases;
;     they are identified by means of nil as the last element of the surface
;     name, as s-verb-subj-nil, or s-verb-indcompl-agent-nil
; *** when the first of them is found (if any) a four-tuple is returned,
;     including:
;     1. The deep label of the found cancelled case (e.g. VERB-SUBJ)
;     2. Its surface label before  the cancellation (e.g. VERB-INDCOMPL-AGENT)
;     3. The remaining (unmatched) deep roles
;     4. The remaining (unmatched) surface cases
(defun find-s-nil (roles cases)
   (cond ((null cases) (values nil nil nil nil))
         ((is-a-cancelled-case (first cases))
            (values (first roles) 
                    (remove-cancel-marker (rem-surf-marker (first cases)))
                    (rest roles) (rest cases)))
         (t (find-s-nil (rest roles) (rest cases)))))

; *****************************************************************************
; *** this matches the (unlinked) possible dependents of a verb with a stored
;     subcat pattern, the latter in a fixed order
; *** it returns a list of labels (one for each case in 'unlinked')
(defun check-sing-casefr
	(unlinked kbvp kbcases kbroles verbline verblab allines allabs)
  (cond ((null unlinked) nil)
; *** try the match on first unlinked element
	(t (let (cf-m-rest match-first)
               (setq match-first 
	 	   (match-in-casefr
		       kbvp kbcases kbroles verbline verblab 
		       (first unlinked) allines allabs nil nil nil))
; *** match-first has the form of a list of six-tuples
;     <found-kbvp, found-case, found-role, rem-kbvp, rem-cases, rem-roles>,
;     Each element of the list represents one match found between the first
;     actual dependent and one of the arguments in the KB. 
	      (cond (match-first
; *** if it succeeds, then match the rest (it actually succeeds always, since
;     any actual dependent may be an adjunct)
; *** since match-first is a list of matches, the match of the rest must be 
;     repeated for each different solution found for the first
                    (setq cf-m-rest nil)
                    (dolist (next-match-first match-first cf-m-rest)
		     (let ((match-rest
		  	     (check-sing-casefr
		    		 (rest unlinked) 
				 (fourth next-match-first)
				 (fifth next-match-first)
				 (sixth next-match-first)
				 verbline verblab allines allabs)))
; *** if the rest does not match, then fail
			(cond ((eq 'fail match-rest)
                                  'fail)
; *** otherwise, return the solution(s) found
			      (t (setq cf-m-rest
                                   (append cf-m-rest 
				    (compose-casefr next-match-first match-rest))))))))
; *** failure in match-first
                    (t 'fail))))))

; *****************************************************************************
; *** this builds a larger set of matches on the basis of the result of
;     the match for a single actual dependent and the result of the match for 
;     the remaining elements.
;   INPUT:
;     - match-first: a six-tuple of the form
;       <found-kbvp, found-case, found-role, rem-kbvp, rem-cases, rem-roles>,
;	(the result of match-in-casefr below)
;     - match-rest: a complete solution for the remaining actual dependents
;       its form is simply a list of list of labels, one for each match
;       found
;   OUTPUT:
;     - a list of lists of labels
(defun compose-casefr (match-first match-rest)
  (cond ((null match-rest)
           (list (list (build-double-labels (third match-first) (second match-first)))))
	(t (mult-cons (build-double-labels (third match-first) (second match-first))
                      match-rest))))
	; *** mult-cons in PROC/utilities

; *****************************************************************************
(defun build-double-labels (dcase scase)
  (let ((actscase (rem-surf-marker scase)))
       (cond ((eq dcase actscase) actscase)
             (t (concatl (put-separator '/ (list dcase actscase)))))))

; *****************************************************************************
; *** it looks for a match of depline with any of the kb cases appearing in
;     kbvp; it returns nil (no match) or a list of six-tuples
;     <found-kbvp, found-case, found-role, rem-kbvp, rem-cases, rem-roles>,
;     one for each match that has been found
; $$$ INPUT:
;     - kbvp: the list of (remaining) surface realizations
;     - kbcases: the list of (remaining) surface cases
;     - kbroles: the list of (remaining) deep roles
;     - verbline: the data line of the verb
;     - depline: the data line of the dependent whose role has to be found
;     - allines: all the lines of the sentence
;     - allabs: all the upper links of the sentence
;     - seenkbvp: accumulator of all the kbvp that have been already examined
;     - seenkbcases: accumulator of all the kbcases that have been already examined
;     - seenkbroles: accumulator of all the kbroles that have been already examined
(defun match-in-casefr 
	(kbvp kbcases kbroles verbline verblab depline allines allabs
         seenkbvp seenkbcases seenkbroles)
  ; *** end of recursion; all matches have been found, but add the hypothesis that
  ;     depline refers to an adjunct (the only useful information is the label in
  ;     the third element)
  (declare (special *LANGUAGE*))
  (cond ((null kbvp) 
          (list
           (list 'dummy1 's-rmod
	         (apply-adjunct-rules depline verbline allines allabs)
                 seenkbvp seenkbcases seenkbroles)))
  ; *** the first condition of the next 'and' to avoid that an extraobj follows
  ;     the verb (PERHAPS in the definition of EXTRAOBJ???)
        ((and (not (and (eq (first kbroles) 'VERB-EXTRAOBJ)
                        (index-precedes (get-synt-numb verbline)
                                        (get-synt-numb depline))))
  ; *** the second condition to avoid that, in English, a subject follows the verb
  ;     when the sentence is not interrogative
  ; !!!! N.B. This does not work for non-main verbs (NOW MADE VIA CHOOSE-BEST-ASSIGN)
  ;            (not (and (eq *LANGUAGE* 'english)
  ;                      (neq #\? (get-synt-word (ult allines)))
  ;                      (not (member 'interr (mapcar #'get-synt-type allines)))
  ;                      (eq (first kbcases) 'S-VERB-SUBJ)
  ;                      (index-precedes (get-synt-numb verbline)
  ;                                      (get-synt-numb depline))))
  ; *** the first of the surface realization (kbvp) is nil in case it refers
  ;     to a role cancelled by a transformation; this cannot be useful to 
  ;     match an existing case
              (not (null (first kbvp)))
	      (test-cf-condit (first kbvp) verbline verblab depline allines allabs))
           (cons 
  ; *** the next list is one result of the match: the three matched kb elements,
  ;     followed by the original lists from which the matched one has been cancelled
  ;     (the preceding ones were accumulated, the following ones are the rest)
	      (list (first kbvp) (first kbcases) (first kbroles)
		          (append seenkbvp (rest kbvp))
		          (append seenkbcases (rest kbcases))
		          (append seenkbroles (rest kbroles)))
  ; *** but other matches can be found: recursion
	      (match-in-casefr 
			(rest kbvp) (rest kbcases) (rest kbroles) 
			verbline verblab depline allines allabs
		        (append1 seenkbvp (first kbvp))
		        (append1 seenkbcases (first kbcases))
		        (append1 seenkbroles (first kbroles)))))
  ; *** the first match is not ok, but other matches can be found: recursion
        (t (match-in-casefr 
			(rest kbvp) (rest kbcases) (rest kbroles) 
			verbline verblab depline allines allabs
		        (append1 seenkbvp (first kbvp))
		        (append1 seenkbcases (first kbcases))
		        (append1 seenkbroles (first kbroles))))))

; *****************************************************************************
(defun test-cf-condit (cf-condit verbline verblab depline allines allabs)
 (cond ((null cf-condit) nil)
       (t (let ((mod-condit
	   	  (cond ((atom (first cf-condit))
		    	  (list (list 'cat (first cf-condit))))
		 	((<= 2 (length (first cf-condit)))
		    	  (cons 'and 
			  	  (cons (list 'cat (first (first cf-condit)))
			 		(rest (first cf-condit))))))))
	     (cond ((gramrul-cf-condit 
		        mod-condit verbline verblab depline allines allabs) t)
	      	   (t (test-cf-condit 
		      	(rest cf-condit)
			verbline verblab depline allines allabs)))))))

; *****************************************************************************
; *** this should apply the gramrul rules to determine the correct adjunct
;     label.
(defun apply-adjunct-rules (depline verbline allines allabs)
     'RMOD)

; ***************************************************************************
(defun get-deep-case (label) (first (expl+cats (explode label) #\/)))

; ***************************************************************************
(defun get-surface-case (label) (ult (expl+cats (explode label) #\/)))

; ***************************************************************************
; *** chooses the best assignment, according to the criteria listed in the
;     function below. The comparison is made on the surface labels, so
;     expl+cats is used to extract them
; *** INPUT:
;  >>> verbline: obvious
;  >>> unlinked: the lines of the assumed dependents; this is parallel
;                to the "foundroles" component of cf-match
;  >>> cf-match: a list of tuples <match cl1 cl2 ... clN>, 
;         where 'match': <n foundroles>, where n is the number of kb roles
;               which remained unmatched (complements not found in the
;               sentence)
;         and 'clI' <classI transfsI>, where classI is a subcat class, and
;               transfsI is the list of transformations from which it
;               was produced
;  >>> allines, allabs: obvious
(defun choose-best-assign (verbline unlinked cf-match allines allinks)
 (let (semchecked)
  (declare (special semchecked))
   ; *** semchecked is a variable used and expanded in is-verb-sem-compatible
   ;     it holds 4-tuples of the form (pos label role)
   ;     where each triple specifies if the dependent in 'pos' (1, 2, ...)
   ;     can be an acceptable filler for the syntactic label 'label'
   ;     (verb-subj, verb-indcompl-from, ...). It is acceptable if 'role' si
   ;     non-nil: in such a case it is the associated semantic role (agent,
   ;     come-origin, ...)
  (let* ((sorted-cf-match (basic-sort-cf cf-match))
          (best nil) (dbest nil))
   ; *** basic-sort-cf makes a firts level sorting of the caseframes, wher the
   ;     ones with less rmod come first. Currently, this is done just to simplify the
   ;     debug, but parhaps, this ordering can be used, in case of a perfect match
   ;     to avoid further tests
   ; *** 'best' is the currently best match (including all infos)
   ;     'dbest' is the same but with the surface label in place of the full ones 
   ;  (format t "sorted matches: ~a ~%" sorted-cf-match)
   ;  (format t "Initial assignment: ~a ~%" dbest)
   ;  (break "")
     (dolist (next sorted-cf-match best)
     	  (let ((dnext 
                 (cons
		  (list (first (first next))
		        (mapcar #'get-surface-case (second (first next))))
                  (rest next)))
     	        (semnext 
                 (cons
		  (list (first (first next))
		        (mapcar #'get-deep-case (second (first next))))
                  (rest next))))
   ;     'semnext' is the same as next but with the deep label in place of the full ones 
   ;(format t "New candidate: ~a ~%" dnext)
   ;(break "")
     	      (cond ((and (is-verb-sem-compatible verbline unlinked semnext allines allinks)
                          (or (null best)
     	                      (is-best-assign verbline unlinked dnext dbest allines allinks)))
   ;(format t "The new candidate is better!!!!~%")
   ;(format t "New candidate: ~a ~%" dnext)
   ;(break "")
			 (setq best next)
			 (setq dbest dnext))))))))


; **************************************************************************
(defun basic-sort-cf (cf-match)
   (sort cf-match #'less-rmod))

; **************************************************************************
(defun less-rmod (cf1 cf2)
   (< (count-rmod (second (first cf1))) (count-rmod (second (first cf2)))))

; **************************************************************************
(defun count-rmod (lablist)
   (cond ((null lablist) 0)
         ((lab-subsumes 'RMOD (first lablist))
            (1+ (count-rmod (rest lablist))))
         (t (count-rmod (rest lablist)))))

; **************************************************************************
; *** checks if there is semantic compatibility between the dependents of a verb
;     and the expected semantic grid. The latter is stored in the
;     "SEMANT-KB-lang/word-mean-lang.dat" file, in the record associated
;     with the meaning of the verb
; *** INPUT: (example "Le temperature si sentono alte")
;     >>> verbline is the line of the verb
;         e.g. (4 |sentono| (SENTIRE VERB MAIN IND PRES TRANS 3 PL) NIL NIL NIL)
;     >>> unlinked are the candidate nodes to be attached to the verb
;         e.g. ((1 |Le| (IL ART DEF F PL) NIL NIL NIL)
;               (3 |si| (SI PRON REFL-IMPERS ALLVAL ALLVAL 3 LSUBJ+LOBJ+LIOBJ CLITIC) NIL NIL NIL)
;               (5 |alte| (ALTO ADJ QUALIF F PL) NIL NIL NIL))
;     >>> semnext is the candidate assignment
;         e.g. ((1 (VERB-OBJ RMOD VERB-PREDCOMPL+OBJ)) (BTRANS-PRED-SINF NIL)) 
;         N.B. Only the deep labels appear in the list
;     >>> allines and allinks are all the lines and links of the sentence
; *** the semantic meaning (vsem below) is a list <mean1 mean2 ... meanN>
;     each meanK can be an atom (e.g. ££to-eat), if no information about the
;     thematic grid is available, or a list 
;     <concept <THEMATIC-GRID (case1 role1) ... (caseM roleM)>>
(defun is-verb-sem-compatible (verbline unlinked semnext allines allinks)
  (declare (special *SYSTEM-CONTEXT*))
  (let ((vsem (get-word-meaning (get-synt-word verbline) 'VERB nil nil))
        (cases (second (first semnext))) (foundok nil)
        nheads nmarks noungrouphead fail sel-restr dep-mean pronref)
    ;(format t "Checking thematic grid~%    Verb: ~a~%    Verb meaning: ~a~%  " 
    ;           (get-synt-word verbline) vsem)
    ;(break "")
    (cond ((null vsem) t)
  ; *** in case of absence of semantic infos about the verb, the assignment is accepted
  ;     in any case, the thematic infos are used just for ATLAS
          ((or (neq *SYSTEM-CONTEXT* 'atlas)
               (atom (first vsem))
               (neq (first (second (first vsem))) 'thematic-grid)) t)
  ; *** in case of absence of the thematic grid, the assignment is accepted
  ;     more than one thematic grid can be present. At this stage, it is enough
  ;     that one of them is matched
          (t 
            (do ((nxtgrid (rest (second (first vsem))) (rest (second (first remgrid))))
                 (remgrid (rest vsem) (rest remgrid)))
      ; *** repeat for all thematic grids
                ((or (null nxtgrid) foundok) 
          ;(format t "Exiting the outer loop on thematic grids; result: ~a~%" foundok)
          ;(break "")
                  foundok)
      ; *** the body of the outer loop is composed of two loops; the first one collects
      ;     all the complements (their heads: in nheads) and all the restrictions
      ;     present in the thematic grid; the second loop checks that complements
      ;     satisfy the restrictions
             (setq nheads nil)
             (setq nmarks nil)
             (setq fail nil)
        ; *********** FIRST INTERNAL LOOP ********************************
    ;  (format t "Entering the outer loop on thematic grids;~% nxtgrid: ~a~% cases: ~a~%" 
    ;           nxtgrid cases)
    ;  (break "")
             (do ((nxtdep (first unlinked) (first unlinked))
                  (unlinked (rest unlinked) (rest unlinked))
                  (nxtcase (first cases) (first cases))
                  (cases (rest cases) (rest cases)))
                 ((null nxtdep))
     ; *** if the next case assignment concerns a true verbal argument
               (cond ((and (lab-subsumes 'VERB-ARG nxtcase)
                           (not (lab-subsumes 'VERB-ARG*LOCUT nxtcase))
                           (neq nxtcase 'VERB-EXTRAOBJ)
                           (neq nxtcase 'VERB-EXTRAINDOBJ))
                       (setq nmarks (append1 nmarks nxtcase))
        ; *** if the governor of the dependent is a preposition, take its argument
        ;     Note that in this case, the preposition is no more relevant, since its function
        ;     was to select the specific verb argument, and this has already been accomplished
        ;     in the previous match phase
                       (cond ((eq (get-synt-categ nxtdep) 'PREP)
                                (setq nxtdep (find-prep-dependent nxtdep allines allinks))))
        ; *** if the argument is a noun group govened by a determiner, find its head word
                       (setq noungrouphead (is-a-synt-noun-complex nxtdep allines allinks))
    ;(format t "Checking thematic grid~%    nxtdep: ~a~%    noungrouphead: ~a~%  " 
    ;          nxtdep noungrouphead)
    ;(break "")
                       (cond ((or (eq (get-synt-categ nxtdep) 'NOUN)
                 ; *** a bare NP
                                  (and (null noungrouphead)
                                       (eq (get-synt-categ nxtdep) 'ADJ))
                 ; *** a non-determiner adjective (e.g. for predcompls)
                                  (eq (get-synt-categ nxtdep) 'ADV))
                 ; *** an adverbial argument (e.g. "non mi sento BENE")
                                (setq nheads (append1 nheads nxtdep)))
                             ((and (null noungrouphead)
                 ; *** relative pronouns are not noun group heads
                                   (eq (get-synt-categ nxtdep) 'PRON))
                                (cond ((eq (get-synt-type nxtdep) 'RELAT)
                                         (setq pronref 
                                             (get-linear-relpron-ref nxtdep allines))
                          ; *** pronref is a list of possible referents. Currently I use 
                          ;     only the first one, i.e. the closest
                                         (setq nheads (append1 nheads (first pronref))))
                                      ((eq (get-synt-type nxtdep) 'PERS)
                                         (setq nheads (append1 nheads nxtdep)))))
                             ((not (null noungrouphead))
                                (setq nheads (append1 nheads noungrouphead)))
                             (t (setq nheads (append1 nheads nil)))))))
        ; *** at the end of the loop, we have collected in nheads all heads associated with a
        ;     semantic content and in nmarks all arc labels
        ; *********** SECOND INTERNAL LOOP ********************************
      ;(format t "Checking thematic grid~%    Nheads: ~a~%    Nmarks: ~a~%  " nheads nmarks)
      ;(break "")
             (do ((nxthead (first nheads) (first nheads))
                  (nheads (rest nheads) (rest nheads))
                  (nxtmark (first nmarks) (first nmarks))
                  (nmarks (rest nmarks) (rest nmarks)))
                ((or (null nxthead) fail)
                ;   (format t "end of check on the single thematic grid: ~a ~%" fail)
                ;   (break "")
                   (setq foundok (not fail)))
                (setq sel-restr (first (leggi nxtgrid nxtmark)))
                (setq dep-mean 
                         (cond ((and (eq (get-synt-categ nxthead) 'PRON)
                                         (get-synt-type nxthead) 'PERS)
                                  (cond ((memq (get-synt-person nxthead) '(1 2))
                                           '(££PERSON))
                                        (t nil)))
                               (t (get-word-meaning (get-synt-word nxthead) 
                                                 (get-synt-categ nxthead) 
                                                 (get-synt-type nxthead) 
                                                 nil
                                                 `((gender ,(get-synt-gender nxthead))
                                                   (number ,(get-synt-number nxthead)))))))
                (cond ((and (listp dep-mean)
                            (not (atom (first dep-mean))))
                        (cond ((eq 'thematic-grid (first (second (first dep-mean))))
      ; *** this can happen when the filler of the case is a verb or a nominalization with an
      ;     associated thematic grid. The assignment produces:
      ;     ((mean (THEMATIC-GRID ...))) --> (mean)
                                 (setq dep-mean (list (first (first dep-mean)))))
                              (t (break "chunk-parser: non atomic meaning, but not a them-grid")))))
        ;(format t "Next selection restriction: ~a~%Next word meaning: ~a~%  " sel-restr dep-mean)
        ;(break "")
                (cond ((and (listp dep-mean) (null (second dep-mean)))
                         (setq dep-mean (first dep-mean))))
                (cond ((or (null dep-mean) 
                           (null sel-restr)
                           (eq sel-restr '@null)
         ; *** sel-restr can be nil if the dependent has been assigned a case label not foreseen
         ;     in the thematic-grid (ex. verb-obj for an intransitive)
         ; *** its value is @null in some special cases (as "there" in "there is")
                           (not (is-compatible-with-semantic-restr dep-mean sel-restr)))
                         (setq fail t)))))))))

; **************************************************************************
; *** the function checks that "meaning" satisfies the selection restriction
;     given by sel-restr
; *** it assumes that the syntactic role is put in correspondance, via the thematic
;     grid, with an ontologic relation
;     There is compatibility if the filler of the role is in the range of that
;     relation or if the relation has as range a type of "description", such that
;     the filler is in the range of it. This second condition accounts for the fact
;     that the "ontologic" filler is a real entity, while in sentences often occurs
;     their descriptions
(defun is-compatible-with-semantic-restr (meaning sel-restr)
  (let ((restr-range (first (get sel-restr 'range))) range-descriptor)
   (cond ((is-instance-or-subclass-of meaning restr-range) t)
         (t (setq range-descriptor (mult-find-entity-descr restr-range))
   ;(format t "Semantic compatible: ~a~%" range-descriptor)
   ;(break "")
            (and (not (null range-descriptor))
                 (is-instance-or-subclass-of meaning range-descriptor))))))
 
; **************************************************************************
(defun mult-find-entity-descr (conc)
 (let (tempres)
  (cond ((atom conc) (find-entity-descr conc))
        ((eq (first conc) 'union)
           (setq tempres (mapcar #'find-entity-descr (rest conc)))
           (cond ((all-null tempres) nil)
                 (t (cons 'union tempres))))
        (t nil))))

; **************************************************************************
; *** given a concept, it finds the class associated with it via a subclass of
;     ££description, i.e. the class the acts as a description of it (conc)
;     Ex. them-rel = ££property-value
;         domains  = (&propval-has-descr)
;         --> relrange = ££property-value-description
(defun find-entity-descr (conc)
  (let ((domains (get conc 'domain-of)) found)
      (do ((nxtdom (first domains) (first domains))
           (domains (rest domains) (rest domains)))
         ((or found (null nxtdom)) found)
         (setq relrange (first (get nxtdom 'range)))
         (cond ((is-subclass-of relrange '££description)
                  (setq found relrange))))))
   
; **************************************************************************
; *** given the line of a relpron, finds it referent (the noun line to which the verb
;     governing the relpron is attached)
;     This is just an approximate solution, since the links are not yet available (here, we
;     are checking the sem for building the links)
;     So, we move backward on the lines, and we take the nouns that precede the relpron
; *** the function returns a list of noun lines; the first line is the one of the noun
;     closest to the relpron:
; *** The boy saw the girl with the hat that he met on sunday
;     ((8 |hat| (...)) (5 |girl| (...)))
(defun get-linear-relpron-ref (nxtdep allines)
  (let ((revlines (reverse allines)) (foundrelpron nil) possrefs)
     (do ((nxtline (first revlines) (first revlines)) 
          (revlines (rest revlines) (rest revlines)))
         ((or (null nxtline)
              (and foundrelpron
                   (not (memq (get-synt-categ nxtline) '(PREP ART NOUN)))))
           (reverse possrefs))
         (cond (foundrelpron 
                  (cond ((eq (get-synt-categ nxtline) 'NOUN)
                           (setq possrefs (cons nxtline possrefs)))))
               ((equal nxtdep nxtline)
                  (setq foundrelpron t))))))

; **************************************************************************
; *** the criteria for being a best match are:
;	1. A 'verbal-locution' label is assigned
;       2. There is no unmarked non-temporal RMOD
;	3. The 'emptycompl' label is assigned (pseudo-reflexives)
;	4. The subject is not assigned to a temporal expression
;	5. The 'verb+indcompl-modal' label is assigned (modals)
;	6. ITALIAN: The 'verb+caus-indcompl' label is assigned (causal verbs) to an
;          adjacent verb (this is useful when the governing verb has
;          an enclitic). 
;       7. ENGLISH: The subject does not follow the verb, unless the sentence
;          is interrogative or the pron-rmod-loc+metaph is assigned
;          or the verb-expletive is assigned
;	8. The 'pron-rmod-loc+metaph' label is assigned
;       9. The 'subj-impers' label is not assigned to an enclitic
;       10. The 'subj-impers' label is assigned, and the verb has preference
;           for impersonals wrt reflexives
;	11. There are less adjuncts
;	12. The verb is imperative, and the SUBJ is not assigned
;	13. There are less complements not found in the sentence
;       14. A verb-predcompl case is assigned to an adj 
;       15. Less transformations have been applied
;	16. There is no competition between SUBJ and OBJ, but OBJ is assigned
;	17. There is competition between SUBJ and OBJ, but and SUBJ is 
;           assigned to a case before the verb or OBJ is assigned to a case
;           after the verb
;       18. There is competition between INDOBJ and OBJ, and the verb is a
;           £predic3 verb (dire, spiegare, ...)
;       19. In both matches there are a VERB-OBJ and a VERB-PREDCOMPL-OBJ:
;           Choose the one where the VERB-OBJ comes before the other
; *** criteria 11 and 13 are different: if cf1 includes just 2 complements, and
;     cf2 includes three complements (perhaps cf1 was obtained by cf2 via
;     'null-subj'), and if two unlinked words appear in the sentence, then
;     both matches are ok (without adjuncts), but the first is better, since
;     it is a 'complete' match
; >>> INPUT:
;   --- verbline: the data line of the verb
;   --- unlinked: all the lines assumed to depend on the verb
;   --- m1, m2: the two match results to compare; each of them is a pair
;               <n foundroles>, where n is the number of kb roles
;               which remained unmatched (complements not found in the
;               sentence), and foundroles is the list of found ones
; >>> OUTPUT:
;   --- true, if m1 (the next match) is better than m2 (the currently best)
;       false, otherwise
(defun is-best-assign (verbline unlinked m1 m2 allines allabs)
 (declare (special m1 m2))
 (cond ((null m1) t)
  ; *** if m1 is still empty, m2 is certainly better
       (t (let* ((match1 (first m1))
                 (match2 (first m2))
                 (numcriteria 19)
                 (res 'fair))
            (do ((ind 1 (1+ ind)))
                ((or (> ind numcriteria) (neq res 'fair))
                   (cond ((eq res 'fair) nil)
                         (t res)))
              ; *** goes on applying criteria until the result is t or nil or there are
              ;     no other to apply
               (setq res (apply-criterium verbline match1 match2 ind unlinked allines allabs))
;      (cond ((and res (neq res 'fair))
;              (format t "New assignment: ~a because of criterium ~a~%" match1 ind)))
               )))))

; ****************************************************************************
; *** OUTPUT:
;     t: match1 is better than match2
;     nil: match2 is better than match1
;     fair: the criterium does not help
; *** match1 is new assignment; match2 is the currently best
(defun apply-criterium (verbline match1 match2 ind unlinked allines allabs)
 (declare (special *LANGUAGE* *PREF-IMPERSONAL* m1 m2))
 (case ind
  (1 (let ((locut1 (locut-member (second match1)))
           (locut2 (locut-member (second match2))))
       ; *** obj-locut only in match1 (CRITERIUM 1) ***************************************
        (cond ((and locut1 (not locut2)) t)
       ; *** obj-locut  only in match2 (inverse of criterium1)
              ((and (not locut1) locut2) nil)
       ; *** obj-locut or in both or in none (parity wrt criterium 1)
             (t 'fair))))
  (2 (let ((unm-rmod1 (with-unmarked-rmod (second match1) unlinked allines allabs))
           (unm-rmod2 (with-unmarked-rmod (second match2) unlinked allines allabs)))
       ; *** unmarked rmod only in match1 (CRITERIUM 2) **************************************
	(cond ((and unm-rmod1 (not unm-rmod2)) nil)
       ; ***  unmarked rmod only in match2 (inverse of criterium 2)
	      ((and (not unm-rmod1) unm-rmod2) t)
       ; *** parity wrt to criterium 2
              (t 'fair))))
  (3 (let ((emptym1 (member 'EMPTYCOMPL (second match1)))
           (emptym2 (member 'EMPTYCOMPL (second match2))))
       ; *** emptycompl only in match1 (CRITERIUM 3) **************************************
	(cond ((and emptym1 (not emptym2)) t)
       ; *** emptycompl only in match2 (inverse of criterium 3)
	      ((and (not emptym1) emptym2) nil)
       ; *** parity wrt to criterium 3
              (t 'fair))))
  (4 (let* ((subj1 (find-bestassign-case 'VERB-SUBJ (second match1) unlinked))
            (subj2 (find-bestassign-case 'VERB-SUBJ (second match2) unlinked))
            (obj1 (find-bestassign-case 'VERB-OBJ (second match1) unlinked))
            (obj2 (find-bestassign-case 'VERB-OBJ (second match2) unlinked))
            (extraobj1 (find-bestassign-case 'VERB-EXTRAOBJ (second match1) unlinked))
            (extraobj2 (find-bestassign-case 'VERB-EXTRAOBJ (second match2) unlinked))
            (temporal-subj1 (is-temporal-expression subj1 allines allabs))
            (temporal-subj2 (is-temporal-expression subj2 allines allabs))
            (temporal-obj1 (is-temporal-expression obj1 allines allabs))
            (temporal-obj2 (is-temporal-expression obj2 allines allabs))
            (temporal-extraobj1 (is-temporal-expression extraobj1 allines allabs))
            (temporal-extraobj2 (is-temporal-expression extraobj2 allines allabs)))
       ; *** only in match2 the subj or the obj is a temporal expression (CRITERIUM 4) ************
        (cond ((and (or temporal-subj2 temporal-obj2 temporal-extraobj2) 
                    (not temporal-subj1) (not temporal-obj1) (not temporal-extraobj1)) t)
       ; *** only in match1 the subj or the obj is a temporal expression (inverse of CRITERIUM 4) **
              ((and (or temporal-subj1 temporal-obj1 temporal-extraobj1) 
                    (not temporal-subj2) (not temporal-obj2) (not temporal-extraobj1)) nil)
       ; *** parity wrt to criterium 4
              (t 'fair))))
  (5 (let ((cmodal1 (member 'VERB+MODAL-INDCOMPL (second match1)))
           (cmodal2 (member 'VERB+MODAL-INDCOMPL (second match2))))
       ; *** verb+modal-indcompl only in match1 (CRITERIUM 5) **************************************
	(cond ((and cmodal1 (not cmodal2)) t)
       ; *** verb+modal-indcompl only in match2 (inverse of criterium 5)
	      ((and (not cmodal1) cmodal2) nil)
       ; *** parity wrt to criterium 5
              (t 'fair))))
  (6 (let* ((ccaus1 (find-bestassign-case 'VERB+CAUS-INDCOMPL (second match1) unlinked))
            (ccaus2 (find-bestassign-case 'VERB+CAUS-INDCOMPL (second match2) unlinked))
            (caus-adjacent-1
                   (and (not (null ccaus1)) 
                        (eq (1+ (get-synt-linumb verbline)) (get-synt-linumb ccaus1))))
            (caus-adjacent-2
                   (and (not (null ccaus2)) 
                        (eq (1+ (get-synt-linumb verbline)) (get-synt-linumb ccaus2)))))
       ; *** verb+caus-indcompl only in match1 (CRITERIUM 6) **************************************
	(cond ((neq *LANGUAGE* 'italian) 'fair)
              ((and caus-adjacent-1 (not caus-adjacent-2)) t)
       ; *** verb+caus-indcompl only in match2 (inverse of criterium 6)
	      ((and (not caus-adjacent-1) caus-adjacent-2) nil)
       ; *** parity wrt to criterium 6
              (t 'fair))))
  (7 (cond ((or (neq *LANGUAGE* 'english) 
                (eq #\? (get-synt-word (ult allines)))
                (member 'interr (mapcar #'get-synt-type allines)))
              'fair)
           (t (let* ((subj1 (find-bestassign-case 'VERB-SUBJ (second match1) unlinked))
                     (subj2 (find-bestassign-case 'VERB-SUBJ (second match2) unlinked))
                     (expl1 (find-bestassign-case 'VERB-EXPLETIVE (second match1) unlinked))
                     (expl2 (find-bestassign-case 'VERB-EXPLETIVE (second match2) unlinked))
                     (subj-after-v-1 
                         (cond ((null subj1) nil)
                               (t (index-precedes (get-synt-numb verbline) (get-synt-numb subj1)))))
                     (subj-after-v-2
                         (cond ((null subj2) nil)
                               (t (index-precedes (get-synt-numb verbline) (get-synt-numb subj2))))))
                 (cond ((and (null subj1) (null subj2)) 'fair)
                       ((and (null subj1) (not (null subj2))) nil)
                       ((and (not (null subj1)) (null subj1)) t)
       ; *** the SUBJ follows the verb only in match2 (CRITERIUM 7) *************************
                       ((and subj-after-v-2 (null expl2) (not subj-after-v-1)) t)
       ; *** the SUBJ follows the verb only in match1 (inverse of criterium 7)
                       ((and (not subj-after-v-2) subj-after-v-1 (null expl1)) nil)
                       (t 'fair))))))
  (8 (let ((pronrlm1 (member 'PRON-RMOD-LOC+METAPH (second match1)))
           (pronrlm2 (member 'PRON-RMOD-LOC+METAPH (second match2))))
       ; *** pron-rmod-loc+metaph only in match1 (CRITERIUM 8) ******************************
        (cond ((and pronrlm1 (not pronrlm2)) t)
       ; *** pron-rmod-loc+metaph only in match2 (inverse of criterium 8)
	      ((and (not pronrlm1) pronrlm2) nil)
       ; *** pron-rmod-loc+metaph or in both or in none (parity wrt to criterium 8)
              (t 'fair))))
  (9 (let* ((subjimp1 (find-bestassign-case 'VERB-SUBJ+IMPERS (second match1) unlinked))
	    (subjpred1 (and (not (null subjimp1))
	   	            (= (get-synt-linumb subjimp1) (get-synt-linumb verbline))))
  	    (subjimp2 (find-bestassign-case 'VERB-SUBJ+IMPERS (second match2) unlinked))
	    (subjpred2 (and (not (null subjimp2))
		            (= (get-synt-linumb subjimp2) (get-synt-linumb verbline)))))
       ; *** 'verb-subj+impers' in match2 for an enclitic, but not in match1 (CRITERIUM 9) *
        (cond ((and subjpred1 (not subjpred2)) t)
       ; *** 'verb-subj+impers' occurs in match1 for an enclitic, but not in match2 
       ;     (inverse of criterium 9)
	      ((and (not subjpred1) subjpred2) nil)
       ; *** it is not the case that 'verb-subj+impers' occurs in match1 for an enclitic,
       ;     but not in match2 (parity for criterium 9)
              (t 'fair))))
  (10 (let ((verbprefimp (member (get-synt-word verbline) *PREF-IMPERSONAL*))
           (subjimp1 (find-bestassign-case 'VERB-SUBJ+IMPERS (second match1) unlinked))
  	   (subjimp2 (find-bestassign-case 'VERB-SUBJ+IMPERS (second match2) unlinked)))
       ; *** there is an impersonal subject in match1 and not in match2, and the verb 
       ;     prefers impersonal readings (assumption that the alternative to an impersonal 
       ;     is always a reflexive) (CRITERIUM 10) ******************************************
        (cond ((and subjimp1 (not subjimp2) verbprefimp) t)
       ; *** inverse of criterium 10
	      ((and subjimp2 (not subjimp1) verbprefimp) nil)
       ; *** parity wrt criterium 10
  	      (t 'fair))))
  (11 (let ((adj1 (count-adjunct (second match1) 0))
            (adj2 (count-adjunct (second match2) 0)))
       ; *** less adjuncts in match1 (CRITERIUM 11) *****************************************
        (cond ((< adj1 adj2) t)
       ; *** less adjuncts in match2 (inverse of criterium 10)
	      ((> adj1 adj2) nil)
       ; *** even adjuncts
  	      (t 'fair))))
       ; *** verb imperative and SUBJ not assigned in match1 (CRITERIUM 12) *****************
  (12 (cond ((and (eq 'imper (get-synt-mood verbline))
                 (not (member 'VERB-SUBJ (second match1)))
                 (member 'VERB-SUBJ (second match2))) t)
       ; *** verb imperative and SUBJ not assigned in match2 (inverse of criterium 12)
           ((and (eq 'imper (get-synt-mood verbline))
                 (member 'VERB-SUBJ (second match1))
                 (not (member 'VERB-SUBJ (second match2)))) nil)
       ; *** (parity wrt criterium 12)
  	   (t 'fair)))
       ; *** less unassigned in match1 (CRITERIUM 13) *******************************
  (13 (cond ((< (first match1) (first match2)) t)
       ; *** less unassigned in match2 (inverse of criterium 13)
           ((> (first match1) (first match2)) nil)
       ; *** also even unassigned (parity wrt criterium 13)
  	   (t 'fair)))
  (14 (let ((predcompl1 (find-bestassign-case 'VERB-PREDCOMPL+SUBJ (second match1) unlinked))
	    (predcompl2 (find-bestassign-case 'VERB-PREDCOMPL+SUBJ (second match2) unlinked)))
        (cond ((null predcompl1)
                 (setq predcompl1 
                     (find-bestassign-case 'VERB-PREDCOMPL+OBJ (second match1) unlinked))))
        (cond ((null predcompl2)
                 (setq predcompl2 
                     (find-bestassign-case 'VERB-PREDCOMPL+OBJ (second match2) unlinked))))
       ; *** a verb-predcompl case is assigned to an adj in match1, but not in match2 **
       ;     (CRITERIUM 14) ******************
        (cond ((and (eq 'adj (get-synt-categ predcompl1))
                    (eq 'qualif (get-synt-type predcompl1))
                    (not (and (eq 'adj (get-synt-categ predcompl2))
                              (eq 'qualif (get-synt-type predcompl2))))) t)
       ; *** a verb-predcompl case is assigned to an adj in match2, but not in match1 **
       ;     inverse of (CRITERIUM 14) ******************
              ((and (eq 'adj (get-synt-categ predcompl2))
                    (eq 'qualif (get-synt-type predcompl2))
                    (not (and (eq 'adj (get-synt-categ predcompl1))
                              (eq 'qualif (get-synt-type predcompl1))))) nil)
       ; *** (parity wrt criterium 13)
  	      (t 'fair))))
  (15 (let ((transf1 (second (second m1)))
	    (transf2 (second (second m2))))
       ; *** less transformations applied in match1 (CRITERIUM 15) ******************
        (cond ((< (length transf1) (length transf2)) t)
       ; *** less transformations in match2 (inverse of criterium 14)
              ((> (length transf1) (length transf2)) nil)
       ; *** also even number of transformations (parity wrt criterium 14)
  	      (t 'fair))))
  (16 (let ((v-o1 (member 'VERB-OBJ (second match1)))
            (v-p-o1 (member 'VERB-PREDCOMPL+OBJ (second match1)))
            (v-o2 (member 'VERB-OBJ (second match2)))
            (v-p-o2 (member 'VERB-PREDCOMPL+OBJ (second match2))))
	(cond ((and v-o1 v-p-o1 v-o2 v-p-o2)
       ; *** VERB-OBJ and VERB-PREDCOMPL-OBJ appear in both casefranes
                (let ((pos-vo1 
                        (get-unlinked-pos 'VERB-OBJ (second match1) unlinked))
                      (pos-vpo1 
                        (get-unlinked-pos 'VERB-PREDCOMPL+OBJ (second match1) unlinked))
                      (pos-vo2 
                        (get-unlinked-pos 'VERB-OBJ (second match2) unlinked))
                      (pos-vpo2 
                        (get-unlinked-pos 'VERB-PREDCOMPL+OBJ (second match2) unlinked)))
	        (cond ((and (index-precedes pos-vo1 pos-vpo1)
	                    (index-precedes pos-vpo2 pos-vo2))
			 t)
	              ((and (index-precedes pos-vo2 pos-vpo2)
	                    (index-precedes pos-vpo1 pos-vo1))
			 nil)
		      (t 'fair))))
              (t 'fair))))
  (17 (let ((obj-subj (altern-obj-subj (second match1) (second match2) unlinked)))
       ; *** obj and subj not in contrast, but 'obj' only in match1  OR
       ;     'subj' in both, with m1 undergone to passivization (so that m1's
       ;     subj actually is a deep obj) (CRITERIUM 17) *****************************
        (cond ((and (null obj-subj) 
	            (or (and (member 'VERB-OBJ (second match1))
		             (not (member 'VERB-OBJ (second match2))))
                        (and (member 'VERB-SUBJ (second match1))
			     (member 'VERB-SUBJ (second match2))
                             (member 'passivization (second (second m2)))
                             (not (member 'passivization (second (second m1)))))))
		 t)
       ; *** obj and subj not in contrast, and the previous condition not verified
  	      (t 'fair))))
  (18 (let ((obj-subj (altern-obj-subj (second match1) (second match2) unlinked)))
       ; *** obj and subj in contrast (same position)
	(cond ((eq 'VERB-SUBJ (first obj-subj))
       ; *** subj in match1 with the word preceding the verb (CRITERIUM 18) ********
	        (cond ((index-precedes
			   (get-synt-numb (second obj-subj))
			   (get-synt-numb verbline)) t)
		      (t nil)))
	      ((eq 'VERB-OBJ (first obj-subj))
       ; *** obj in match1 with the verb preceding the word
		(cond ((index-precedes
			   (get-synt-numb verbline)
			   (get-synt-numb (second obj-subj))) t)
		      (t nil)))
              (t 'fair))))
  (19 (let ((obj-indobj (altern-obj-indobj (second match1) (second match2) unlinked)))
       ; *** obj and indbj in contrast (same position)
	(cond ((eq 'VERB-INDOBJ (first obj-indobj))
       ; *** indobj in match1 filled by a pronou in first or second person
       ;     with a "£predic3" verb (dim-mi racconta-mi) (CRITERIUM 19) ********
	        (cond ((and (eq 'pron (get-synt-categ (second obj-indobj)))
			    (eq (get-synt-person (second obj-indobj)) 1)
                            (inh-member (get-synt-word verbline) '£predic3))
			 t)
		      (t nil)))
              (t 'fair))))))

; *****************************************************************************
; *** actually, subjline can be the subj, the obj, or the extraobj
(defun is-temporal-expression (subjline allines allinks)
   (let* ((subjhead (find-np-head subjline allines allinks))
          (subjword (get-synt-word subjhead)))
       (or (inh-member subjword '£season)
           (inh-member subjword '£month)
           (inh-member subjword '£weekday)
           (inh-member subjword '£daytime-period)
           (inh-member subjword '£volta)
           (inh-member subjword '£timemeasure)
           (and (inh-member subjword '£day)
                (has-number-dependent subjhead allines allinks)))))

; *****************************************************************************
; *** checks if any of the dependents of curline has the category NUM
(defun has-number-dependent (curline allines allabs)
   (let ((deps (first (find-dependents curline allines allabs))))
      (member 'NUM (mapcar #'get-synt-categ deps))))

; *****************************************************************************
(defun get-unlinked-pos (label labels unlinked)
  (cond ((null labels) (exception 'parse-error "PROC/chunk-parser: get-unlinked-pos"))
        ((eq label (first labels)) (get-synt-numb (first unlinked)))
        (t (get-unlinked-pos label (rest labels) (rest unlinked)))))

; *****************************************************************************
; *** searches the label 'lab' in 'match', and returns the corresponding element
;     in 'unlinked'
(defun find-bestassign-case (lab match unlinked)
  (cond ((null match) nil)
	((eq lab (first match)) (first unlinked))
	(t (find-bestassign-case lab (rest match) (rest unlinked)))))

; *****************************************************************************
(defun with-unmarked-rmod (labels cases allines allinks)
   (cond ((null labels) nil)
         ((null (first labels))
            (with-unmarked-rmod (rest labels) (rest cases) allines allinks))
         ((lab-subsumes 'RMOD (first labels))
            (cond ((and (is-a-synt-noun-complex (first cases) allines allinks)
                        (not (is-temporal-expression (first cases) allines allinks)))
                     t)
                  (t (with-unmarked-rmod (rest labels) (rest cases) allines allinks))))
         (t (with-unmarked-rmod (rest labels) (rest cases) allines allinks))))

; *****************************************************************************
; *** compares lablist1 and lablist2 to see if they have 'subj' and 'obj' in the
;     same position, or viceversa. If it is true, it returns <subj, elem> or
;     <obj, elem>, where 'elem' is the element in the list 'unlinked' in the
;     same position of the contrasting label; 'subj' is returned if 'subj'
;     occurs in the first list and 'obj' in the second, and 'obj' if it is the
;     other way around. It returns NIL if no contrast is found.
(defun altern-obj-subj (lablist1 lablist2 unlinked)
  (cond ((or (null lablist1) (null lablist2)) nil)
	((and (eq 'VERB-SUBJ (first lablist1))
	      (eq 'VERB-OBJ (first lablist2)))
	  (list 'VERB-SUBJ (first unlinked)))
	((and (eq 'VERB-OBJ (first lablist1))
	      (eq 'VERB-SUBJ (first lablist2)))
	  (list 'VERB-OBJ (first unlinked)))
	(t (altern-obj-subj (rest lablist1) (rest lablist2) (rest unlinked)))))
	
; *****************************************************************************
; *** exactly as above, but for comparing VERB-OBJ and VERB-INDOBJ
;     (for cases as dim-mi)
(defun altern-obj-indobj (lablist1 lablist2 unlinked)
  (cond ((or (null lablist1) (null lablist2)) nil)
	((and (eq 'VERB-INDOBJ (first lablist1))
	      (eq 'VERB-OBJ (first lablist2)))
	  (list 'VERB-INDOBJ (first unlinked)))
	((and (eq 'VERB-OBJ (first lablist1))
	      (eq 'VERB-INDOBJ (first lablist2)))
	  (list 'VERB-OBJ (first unlinked)))
	(t (altern-obj-indobj (rest lablist1) (rest lablist2) (rest unlinked)))))
	
; *****************************************************************************
; *** counts the adjuncts present in labellist; currently, since all adjuncts
;     are labelled as 'rmod', the test is simply 'eq'. In the future it should
;     exploit the label hierarchy
(defun count-adjunct (labellist count)
   (cond ((null labellist) count)
	 ((eq (first labellist) 'RMOD)
	      (count-adjunct (rest labellist) (1+ count)))
	 (t (count-adjunct (rest labellist) count))))

; *****************************************************************************
; *** checks if any of the labels in labellist is a verbal locution
(defun locut-member (labellist)
  (cond ((null labellist) nil)
	((member 'VERB-ARG*LOCUT (get-all-lab-ancestors (first labellist)))
	   (cons (first labellist) (locut-member (rest labellist))))
	(t (locut-member (rest labellist)))))

; *****************************************************************************
; *** updates the link lists to insert the found caseframe links
; *** INPUT:
;   >>> best-match: a list of labels; 
;   >>> unlinked: a list of lines corresponding to unlinked dependents; 
;	if there are n unlinked elements, then it is assumed that best-match
;       includes n 'true' labels, but they can be followed by m 'trace' labels;
;       the true labels have the form DeepLab/SurfLab, while the trace labels
;       have the form DeepLab/NIL
;   >>> prevlines, prevlinks, nxtlines, nxtlinks: as usual (with respect to the
;	head verb)
;   >>> verblinumb: the number of the line of the head verb
;   >>> the higher level procedure which is performing the link assignment
; *** OUTPUT: 6 values. the lines are returned, since there could be some trace
;     added.
;   >>> prevlines prevlinks nxtlines nxtlinks verbline verblink
(defun merge-cf-assign 
	(best-match unlinked prevlines prevlinks nxtlines nxtlinks verbline verblink source)
 (declare (special *LANGUAGE*))
 (let ((verblinumb (get-synt-numb verbline))
       (deep-cases (mapcar #'get-deep-case best-match)))
  (cond ((null best-match) (values prevlines prevlinks nxtlines nxtlinks verbline verblink))
     ; *** the next branch is a first attempt of modifying the structure during parsing
     ;     in order to adjust a "wrong" structure
     ; *** it applies in cases as "... know about which events occur tonight"
     ;     In such a case, the first hypothesis is to have subtree for "about which events"
     ;     as in "... know about which events I have talked to him". In other words,
     ;     it is a dependent of the verb of the subordinate (have talked). However, in
     ;     the other case, we must have that the argument of "about" is the verb "occur",
     ;     and "which events" is its subject
        ((and (eq *LANGUAGE* 'english)
              (not (member 'VERB-SUBJ deep-cases))
     ; *** the involved element must be the first of unlinked, since the question case
     ;     must be the first of the dependents
              (has-gramm-type (get-synt-word (first unlinked)) '&about)
              (eq (get-synt-type  
                     (first 
                         (find-a-line 
                              `(linked-to ,(get-synt-numb (first unlinked)))
                               prevlines prevlinks)))
                  'interr))
     ; *** "about" should have been hypothesised as an "rmod"; it must be replaced by
     ;     "which events", which should act as "verb-subj"
           (setq best-match (cons 'VERB-SUBJ (rest best-match)))
     ; *** the current verb becomes the argument of "about"
           (setq verblink (list (get-synt-numb (first unlinked)) 'VERB-OBJ source))
     ; *** the argument of "about" is put in place of "about" in front of "unlinked"
           (setq unlinked
               (cons 
                  (first 
                     (find-a-line 
                         `(linked-to ,(get-synt-numb (first unlinked))) prevlines prevlinks))
                  (rest unlinked)))
     ; *** then, the procedure is applied to the new data
	   (merge-cf-assign 
	        best-match unlinked prevlines prevlinks nxtlines nxtlinks verbline verblink source))
        ((null unlinked)
; *** here, unlinked is empty, but some elements remain in best-match; the remaining
;     elements must refer to traces, which are introduced in the representation
          (let (newnxtlines newnxtlinks
                (traces (check-traces-and-sort best-match))
  ; *** find-controlling-element returns a triple <line gramfun type>, where 'gramfun' 
  ;     specifies the grammatical function of the trace in the governed verb,
  ;     'line' is the "traced" element, and type is either #\f or #\p.
  ; *** Usually, 'case' is 'SUBJ', as in modals (Lucia vuole correre, Lucia
  ;     wants to run, where 'Lucia' is the traced SUBJ of 'correre' - to run -)
  ; *** But in some cases also the OBJ could be traced, as in 'una cosa da
  ;     fare' (a thing to do), where 'cosa' plays the role of OBJ of 'fare'
  ;     (to do). Of course, this is not a case of verbal control, but
  ;     find-controlling-element could be used in many cases
                (control (find-controlling-element 
	                     prevlines prevlinks nxtlines nxtlinks
                             verbline verblink))
    ; *** in this second call, find-controlling-element returns just the lines
    ;     of the referent, so that "visitors" is zero or more lines
                (visitors (find-controlling-element
                             prevlines prevlinks nxtlines nxtlinks
                             verbline verblink 'RMOD)))
  ; *** if traces is null, then some non-trace element is present in 
  ;     best-match. This should not happen if unlinked is NIL
            ; (setq visitors (subtrl visitors control))
             (cond ((null traces)
                      (values prevlines prevlinks nxtlines nxtlinks verbline verblink))
		   ((not (numberp verblinumb))
  ; *** to avoid inserting a trace under another trace
                      (values prevlines prevlinks nxtlines nxtlinks verbline verblink))
                   (t (multiple-value-setq (prevlines prevlinks newnxtlines newnxtlinks)
                          (add-traces traces control visitors
                              verbline verblink prevlines prevlinks nxtlines nxtlinks 10))
                      (values prevlines prevlinks newnxtlines newnxtlinks verbline verblink)))))
	((or (null nxtlines)
	     (index-precedes (get-synt-numb (first unlinked))
			     (get-synt-numb (first nxtlines))))
; *** if the first unlinked element precedes the first element to the right
;     of the verb, the update must be done to 'prevlinks' (usually, this concerns
;     the subject)
	   (let ((templinks 
		   (merge-sing-cf-assign 
		      (first best-match) (first unlinked)
		      prevlines prevlinks verblinumb source)))
	       (merge-cf-assign 
		    (rest best-match)
		    (rest unlinked)
		    prevlines templinks nxtlines nxtlinks verbline verblink source)))
; *** otherwise, update 'nxtlinks'
	(t (let ((templinks 
		    (merge-sing-cf-assign 
			(first best-match) (first unlinked)
			nxtlines nxtlinks verblinumb source)))
	       (merge-cf-assign 
		    (rest best-match)
		    (rest unlinked)
		    prevlines prevlinks nxtlines templinks verbline verblink source))))))

; *****************************************************************************
; *** inserts into the links list, in the position associated with the first
;     element of unlinked, the new label (together with the parent line number)
(defun merge-sing-cf-assign (label unlink lines links verblinumb source)
   (cond ((null lines) 
            (exception 'parse-error "PROC/chunk-parser: merge-sing-cf-assign"))
	 ((equal (first lines) unlink)
	    (cons (make-link verblinumb label source) (rest links)))
	 (t (cons (first links)
		  (merge-sing-cf-assign 
			label unlink (rest lines) (rest links) verblinumb source)))))

; *****************************************************************************
; *** this function aims at determining which is the referent of a trace.
;     in case it finds one, then it returns a pair <referent gramfun>,
;     where 'referent' is the line of the referent, and <gramfun> is the
;     grammatical function (e.g. SUBJ), the trace plays
; *** the optional "other-compl" is used to pick-up "rmods" of modals, which
;     are assumed to be visitors of the modal; in this case, the function does
;     not return a triple, but only the line of the referent
(defun find-controlling-element 
	   (prevlines prevlinks nxtlines nxtlinks verbline verblink
              &optional other-compl)
  (let* ((allines (append (reverse prevlines) (list verbline) nxtlines))
         (allinks (append (reverse prevlinks) (list verblink) nxtlinks))
         (governor (get-parser-governor verblink allines allinks))
         (govline (first governor))
         (govlink (second governor))
         (govcateg (get-synt-categ govline))
         (aux (find-first-aux verbline prevlines))
         (auxmood (get-synt-mood aux))
  ; **** N.B. In case there is no aux, aux is the main verb!
         gov-gov)
  ; *** govline is the line of the governor of verbline, govlink is its link up
     (cond ((null govline)
             (cond ((null other-compl)
                      (let ((double-pron
                               (find-a-line '(word-typ &double-who) prevlines prevlinks)))
                             (cond ((null double-pron) nil)
                                   (t (list (first double-pron) 'double-who #\p)))))
                   (t nil)))
           ((and (null other-compl)
                 (eq auxmood 'PARTICIPLE)
                 (eq (get-synt-tense aux) 'PAST)
                 (memq govcateg '(NOUN PRON)))
    ; *** if the verb is the past participle, and its governor is a noun,
    ;     then the controller has been found (reduced relative clause)
    ; *** "Il film visto da ..." (the movie seen by ...)
    ;     ("the movie" verb-obj ...)
    ; *** N.B. The second element of the result is VERB-SUBJ, since it refers
    ;          to the surface label of the governed element
             (list govline 'VERB-SUBJ #\p))
           ((and (null other-compl)
                 (eq auxmood 'PARTICIPLE)
                 (eq (get-synt-tense aux) 'PRES)
                 (eq govcateg 'NOUN))
    ; *** analogously, if the verb is the present participle, and its governor
    ;     is a noun, but in this case the traced case is SUBJ
             (list govline 'VERB-SUBJ #\p))
           ((and (eq auxmood 'INFINITE)
                 (eq govcateg 'VERB))
             (cond ((null other-compl)
                      (cond ((eq (second verblink) 'VERB+MODAL-INDCOMPL)
                               (list (find-control-case govline allines allinks) 'VERB-SUBJ #\f))
;                            ((eq (second verblink) 'VERB+CAUS-INDCOMPL)
;                               (list (find-control-case govline allines allinks) 'VERB-OBJ #\f))
                            (t '(NIL NIL NIL))))
       ; *** this second branch aims at finding a possible visitor in the governing
       ;     modal (if the infinite verb is governed by a modal)
                   ((memq (second verblink) 
                          '(VERB+MODAL-INDCOMPL VERB+CAUS-INDCOMPL))
                       (let ((found-mod-compl
                               (remove-prec-subord
                                 (remove-neg-adv
                                   (mult-find-case
                                      (find-dependents govline allines allinks)
                                      other-compl))
                                 govline)))
                          (cond ((null found-mod-compl) nil)
                                (t (mapcar #'(lambda (x) (list x nil nil)) found-mod-compl)))))
                   (t nil)))
    ; *** if the verb is the infinite, and its governor is a verb, then the
    ;     trace controller has to be found inspecting the cases of that verb
           ((and (null other-compl)
                 (or (and (eq govcateg 'CONJ)
                          (eq (get-synt-type govline) 'SUBORD))
                     (eq govcateg 'prep)))
    ; *** if the governor of the verb is a subordinating
    ;     conjunction, or a preposition, then the trace controller has to be
    ;     found going up another level, to the governor of the conjunction or
    ;     the preposition
             (setq gov-gov
                  (cond ((null govlink) nil)
                        (t (first (find-a-line `(position ,(first govlink)) 
                                               allines allinks)))))
             (cond ((null gov-gov) nil)
                   ((and (memq (get-synt-categ gov-gov) '(noun pron))
                         (has-gramm-type (get-synt-word govline) '&vgov-prep-2))
            ; *** this branch for "una cosa da fare" (a thing to do)
                      (list gov-gov 'VERB-OBJ #\f))
                   (t (list (find-control-case gov-gov allines allinks)
                            'VERB-SUBJ #\f))))
           ((or (and (null other-compl)
                     (eq govcateg 'CONJ)
                     (eq (get-synt-type govline) 'COORD))
                (eq (get-synt-word govline) #\&))
    ; *** if the governor of the verb is a coordinating conjunction, then
    ;     we assume that the trace controller is the SUBJ of the verb which
    ;     is the first conjunct; this does not apply to visitors (null other-compl)
             (setq gov-gov
                  (cond ((null govlink) nil)
                        (t (first (find-a-line `(position ,(first govlink)) 
                                               allines allinks)))))
             (cond ((null gov-gov) nil)
                   ((neq (get-synt-categ gov-gov) 'VERB)
                       (cond ((eq (get-synt-categ gov-gov) 'PHRAS)
                                '(NIL NIL NIL))		; this is for "buonasera e ben tornati"
                             (t ;(exception 'parse-error "PROC/chunk-parser: Non-verbal conjunction")
                                nil)))
                   ((verb-agreement verbline gov-gov allines)
    ; *** if the two verbs agree, then the subject of the governing verb is shared with the
    ;     governed one
                      (list (find-case (find-dependents gov-gov allines allinks) 'VERB-SUBJ)
                            'VERB-SUBJ #\f))
                   (t nil))))))

; *****************************************************************************
(defun verb-agreement (verbline1 verbline2 allines)
  (let ((v1 (find-first-aux verbline1 allines))
        (v2 (find-first-aux verbline2 allines)))
     (and (agr-unif (list (get-synt-number v1))
	            (list (get-synt-number v2)))
          (agr-unif (list (get-synt-person v1))
	            (list (get-synt-person v2))))))

; *****************************************************************************
; *** this checks if the verb in verbline is a control verb of any type
(defun find-control-case (verbline allines allinks)
   (let ((gov-deps (find-dependents verbline allines allinks)))
    ; *** gov-deps (see 'find-dependents' in MORPHO/tb-functions) is a pair
    ;     <lines links>, of elements depending on 'governor'
       (cond ((subj-control verbline)
                (let ((result (find-case gov-deps 'VERB-SUBJ)))
                    (cond ((null result)
                            (find-case gov-deps 'VERB-SUBJ+IMPERS))
                          (t result))))
             ((obj-control verbline)
                (find-case gov-deps 'VERB-OBJ))
             ((indobj-control verbline)
                (find-case gov-deps 'VERB-INDOBJ))
             (t nil))))

; *****************************************************************************
; *** removes negative adverbials from a list of dependents
(defun remove-neg-adv (compls)
  (let (result)
     (dolist (nxtcompl compls result)
        (cond ((not (and (eq (get-synt-categ nxtcompl) 'ADV)
                         (eq (get-synt-type nxtcompl) 'NEG)))
                 (setq result (append1 result nxtcompl)))))))

; *****************************************************************************
; *** removes subordinates whose head (conjunction) precedes the line whose
;     number is in govline from a list of dependents
; *** this is used to avoid that in a sentence as:
;       "If you kindly ask, you can go"
;     The conditional is moved from the modal (can) to the governed verb (go)
(defun remove-prec-subord (compls govline)
  (let (result)
     (dolist (nxtcompl compls result)
        (cond ((not (and (eq (get-synt-categ nxtcompl) 'CONJ)
                         (eq (get-synt-type nxtcompl) 'SUBORD)
                         (index-precedes (get-synt-numb nxtcompl) 
                                         (get-synt-numb govline))))
                 (setq result (append1 result nxtcompl)))))))

; *****************************************************************************
; *** this looks for a dependent linked as "case" among deplist
(defun mult-find-case (deplist case)
   (let ((lines (first deplist))
         (links (second deplist))
         result)
    (do ((nxtline (first lines) (first lines))
         (lines (rest lines) (rest lines))
         (nxtlink (first links) (first links))
         (links (rest links) (rest links)))
       ((null nxtline) result)
       (cond ((eq case (get-surface-case (second nxtlink)))
                (setq result (append1 result nxtline)))))))

; *****************************************************************************
; *** this looks for a dependent linked as "case" among deplist
(defun find-case (deplist case)
   (let ((lines (first deplist))
         (links (second deplist))
         found)
    (do ((nxtline (first lines) (first lines))
         (lines (rest lines) (rest lines))
         (nxtlink (first links) (first links))
         (links (rest links) (rest links)))
       ((or (null nxtline) found)
          found)
       (cond ((eq case (get-surface-case (second nxtlink)))
                (setq found nxtline))))))

; *****************************************************************************
; *** *SUBJ-CONTROL-VERBS*, *OBJ-CONTROL-VERBS* and *INDOBJ-CONTROL-VERBS*
;     defined in KB/SUBCAT/verbal-control.dat
(defun subj-control (verbline)
  (declare (special *SUBJ-CONTROL-VERBS*))
   (or (eq 'MOD (get-synt-type verbline))
       (member (get-synt-word verbline) *SUBJ-CONTROL-VERBS*)))

(defun obj-control (verbline)
  (declare (special *OBJ-CONTROL-VERBS*))
   (member (get-synt-word verbline) *OBJ-CONTROL-VERBS*))

(defun indobj-control (verbline)
  (declare (special *INDOBJ-CONTROL-VERBS*))
   (member (get-synt-word verbline) *INDOBJ-CONTROL-VERBS*))

; *****************************************************************************
; *** checks if all labels in best-match refer to traces (i.e. their last
;     part is nil: verb-obj/verb-subj/nil) and in such a case sort the traces
;     according to a default ordering 
(defun check-traces-and-sort (best-match)
  (let ((all-traces t) result)
  ; *** the dolist loops on all the labels; if any of them does not end with nil
  ;     then all-traces is set to nil
  ; *** in the meantime, all labels are cleaned of the trailing nil, and sorted
  ;     into result
     (cond ((dolist (nxtlab best-match all-traces)
               (let ((expl-lab (expl+cats (explode nxtlab) #\/)))
                   (cond ((null (ult expl-lab))
                           (setq result 
                              (trace-sort-insert 
                                 result 
    ; *** the new label is simple if the length of expl-lab is 2 (SUBJ NIL) or
    ;     if the first and the second coincide (SUBJ SUBJ NIL). Maybe the first
    ;     case never arises
                                   (cond ((or (= 2 (length expl-lab))
                                              (eq (first expl-lab) (second expl-lab)))
                                           (first expl-lab))
    ; *** otherwise it is formed diregarding the final nil:
    ;     (SUBJ AGTCOMPL NIL) ---> SUBJ/AGTCOMPL
                                         (t (concatl 
                                              (put-separator #\/ 
                                                   (butlast expl-lab))))))))
                         (t (setq all-traces nil)))))
  ; *** if all traces, return result
               result)
  ; *** otherwise, return nil
           (t nil))))

; *********************************************************************
; *** inserts a new trace label in the proper position in a list of trace labels
;     the order is defined in trace-precedes
(defun trace-sort-insert (lablist newlab)
   (cond ((null lablist) (list newlab))
         ((trace-precedes newlab (first lablist))
            (cons newlab lablist))
         (t (cons (first lablist) (trace-sort-insert (rest lablist) newlab)))))

; *** returns true is the label tlab1 must appear before the label tlab2
(defun trace-precedes (tlab1 tlab2)
   (cond ((memq tlab1 '(VERB-SUBJ VERB-OBJ/VERB-SUBJ)) t)
         ((and (eq tlab1 'VERB-SUBJ/VERB-INDCOMPL-AGENT)
               (not (memq tlab2 '(VERB-SUBJ VERB-OBJ/VERB-SUBJ)))) t)
         ((and (eq tlab1 'VERB-OBJ)
               (neq tlab2 'VERB-SUBJ)) t)
         (t nil)))

; *********************************************************************
; *** this adds one or more traces in front of nxtlines, linking them
;     to the verb in verblinumb via arcs labelled according to the contents
;     of "traces"
; >>> INPUT:
;   *** traces: a list of case labels
;   *** control: a triple <line case-label tracetype>
;   *** tvisitors: zero or more lines (non nil in case the verb is governer by a modal
;       which has an extra rmod)
;   *** verbline, verblink, nxtlines, nxtlinks: as usual
;   *** traceindex: a value starting from 10, incremented by 1 for each trace
(defun add-traces (traces control tvisitors verbline verblink
                         prevlines prevlinks nxtlines nxtlinks traceindex)
 (let ((verblinumb (get-synt-numb verbline)) 
       (newprevlines prevlines) (newprevlinks prevlinks))
  (cond ((null traces) (values newprevlines newprevlinks nxtlines nxtlinks))
        (t (let* ((surf-lab (get-surface-case (first traces))) insres auxline)
               (cond ((eq surf-lab (second control))
   ; *** if the label in 'control' is equal to the surface label of the current
   ;     "traces" element, then this is the traced element
                        (setq insres
                            (ins-trace verblinumb
                                   (first control)
                                   (first traces)
                                   (third control)
                                   nxtlines nxtlinks traceindex)))
                     ((or (eq surf-lab 'VERB-OBJ)
                          (and (eq surf-lab 'VERB-SUBJ)
                               (eq (second verblink) 'VERB+CAUS-INDCOMPL)))
   ; *** this second branch applies to:
   ;     - lo puoi prendere 
   ;     - me lo fai prendere 
   ;       where "lo" is a lobj pronoun and must be used as a trace for the object
   ;       of "prendere", but also for
   ;     - lo fai correre
   ;       where "lo" is lobj, but must act as the subject of "correre"
   ; *** in these cases, in matched-vis we find the (possible) lobj pronoun ("lo")
                       (let (matched-vis other-vis)
                           (multiple-value-setq (matched-vis other-vis)
                                (find-lobj-visitor tvisitors))
                           (cond ((not (null matched-vis))
                                    (setq insres
                                        (ins-trace verblinumb
                                               (first matched-vis)
                                               (first traces)
                                               #\f
                                               nxtlines nxtlinks traceindex))
                                    (setq newprevlinks 
                                        (set-visitor prevlines prevlinks (first matched-vis)))
                                    (setq tvisitors other-vis))
                                 (t (setq insres
                                          (ins-trace verblinumb
   ; *** otherwise this is a generic label, and NIL is passed to ins-trace
                                                 nil
                                                 (first traces)
                                                 #\f
                                                 nxtlines nxtlinks traceindex))))))
                     ((eq surf-lab 'VERB-INDOBJ)
                       (let (matched-vis other-vis)
                           (multiple-value-setq (matched-vis other-vis)
                                (find-liobj-visitor tvisitors))
                           (cond ((not (null matched-vis))
                                    (setq insres
                                        (ins-trace verblinumb
                                               (first matched-vis)
                                               (first traces)
                                               #\f
                                               nxtlines nxtlinks traceindex))
                                    (setq newprevlinks 
                                        (set-visitor prevlines prevlinks (first matched-vis)))
                                    (setq tvisitors other-vis))
                                 (t (setq insres
                                          (ins-trace verblinumb
   ; *** otherwise this is a generic label, and NIL is passed to ins-trace
                                                 nil
                                                 (first traces)
                                                 #\f
                                                 nxtlines nxtlinks traceindex))))))
                     ((and (eq surf-lab 'VERB-SUBJ)
                           (not (null (second verblink)))
                           (lab-subsumes 'COORD2ND (second verblink)))
                         (let ((first-conjv 
                        ; *** the first conjunct of the verbal coordination
                                   (first 
		                      (find-a-line 
                                        '(position
                                          (second
                        ; *** looks for the line of the conjunction
		                              (find-a-line 'position (first verblink)
			                                   prevlines prevlinks)))
			                  prevlines prevlinks))) up-subj)
                               (setq up-subj
                                    (find-case
                                       (find-dependents first-conjv prevlines prevlinks)
                                       'VERB-SUBJ))
                               (cond ((not (null up-subj))
                                        (setq insres
                                          (ins-trace verblinumb
          ; *** insert a trace to the upper subj
                                                 up-subj
                                                 (first traces)
                                                 #\f
                                                 nxtlines nxtlinks traceindex)))
                                      (t (setq insres
                                             (ins-trace verblinumb
          ; *** otherwise this is a generic label, and NIL is passed to ins-trace
                                                    nil
                                                    (first traces)
                                                    #\f
                                                    nxtlines nxtlinks traceindex))))))
                     ((eq (second control) 'double-who)
   ; *** in case the current element of traces is double-who, then it is used to fill the
   ;     required trace
                        (setq insres
                            (ins-trace verblinumb
                                   (first control)
                                   (first traces)
                                   #\p
                                   nxtlines nxtlinks traceindex)))
                     ((eq surf-lab 'VERB-SUBJ)
   ; *** standard case of subjects: try to force a pronominal interpretation for the subject,
   ;     getting infos for gender and number from the governing verb (Hyp: language with
   ;     verb-subject agreement)
                        (setq auxline (find-first-aux verbline prevlines))
                        (setq insres
                            (ins-trace verblinumb
                                   'pron
                                   (first traces)
                                   #\f
                                   nxtlines nxtlinks traceindex
                                   (get-synt-person auxline)
                                   (get-synt-number auxline))))
                     (t
                        (setq insres
                            (ins-trace verblinumb
   ; *** otherwise this is a generic label, and NIL is passed to ins-trace
                                   nil
                                   (first traces)
                                   #\f
                                   nxtlines nxtlinks traceindex))))
              (add-traces (rest traces) control tvisitors verbline verblink
                          newprevlines newprevlinks (first insres) (second insres)
                          (1+ traceindex)))))))

; *********************************************************************
; *** this returns a visitor in the lobj case (if any) and the list of the
;     remaining visitors. If it is not a pron, it must be unmarked
(defun find-lobj-visitor (visit)
  (let ((savevisit visit) found visitdata visitcateg)
     (do ((nxtvis (first visit) (first visit))
          (visit (rest visit) (rest visit)))
         ((or found (null nxtvis))
            (values found (subtrl savevisit found)))
         (setq visitdata (first nxtvis))
         (setq visitcateg (get-synt-categ visitdata))
         (cond ((and (neq visitcateg 'PREP)
                     (or (neq visitcateg 'PRON)
	                 (member 'lobj (get-all-cases visitdata))))
   ; *** the (single) unmarked visitor, must not be governed by a prep and either
   ;     is not a pronoun or has the 'lobj case
                  (setq found nxtvis))))))

; *********************************************************************
; *** this returns a visitor in the liobj case (if any) and the list of the
;     remaining visitors. If it is not a pron, it must be governed by a 
;     INDOBJ preposition 
(defun find-liobj-visitor (visit)
  (let ((savevisit visit) found visitcateg visitdata)
     (do ((nxtvis (first visit) (first visit))
          (visit (rest visit) (rest visit)))
         ((or found (null nxtvis))
            (values found (subtrl savevisit found)))
         (setq visitdata (first nxtvis))
         (setq visitcateg (get-synt-categ visitdata))
         (cond ((or (and (eq visitcateg 'PREP)
                         (has-gramm-type (get-synt-word visitdata) '&indobj-prep))
                    (and (eq visitcateg 'PRON)
	                 (member 'liobj (get-all-cases visitdata))))
   ; *** the (single) unmarked visitor, must not be governed by a prep and either
   ;     is not a pronoun or has the 'lobj case
                  (setq found nxtvis))))))

; *********************************************************************
; *** This looks for "visitor-line" in prevlines and replaces the arc label
(defun set-visitor (prevlines prevlinks visitor-line)
   (cond ((null prevlines)
            (exception 'parse-error "Visitor line not found"))
         ((equal (first prevlines) visitor-line)
            (cons (make-link (first (first prevlinks)) 'VISITOR 'control) (rest prevlinks)))
         (t (cons (first prevlinks) 
                  (set-visitor (rest prevlines) (rest prevlinks) visitor-line)))))

; *********************************************************************
; *** prepositions are sometimes attached in the wrong place. This 
;     functions tries to solve some of these problems. In particular,
;     in a sequence N1 - P1 - N2 - P2 - N3, if N3 is originally attached
;     to N2 (via P2, and possibly via a governing determiner),
;     but this is semantically awkward, then this functions checks
;     if the attachment to N1 is better (this is actually done at various
;     levels up).
; *** there are two possible reasons for moving:
;     1. The sequence N1 - P1 - N2 is a kind of pseudo-locution (as
;        "parte di ricambio" - spare part), so that any attachment to
;        "parte" must be preferred to an attachment to "ricambio"
;        (ex. "parte di ricambio con difetti")
;     2. The attachment N1 - P2 - N3 is semantically "relevant" (as in
;        "direttiva del consiglio" - directive of the council), so that
;        attachments to intervening PP's should be avoided (as in
;        "directive of may 15 of the council")
;     3. The attachment N1 - P2 - N3 is such that P2 (in the sentence) is
;        preceded by a comma, and there is another preposition (P4) equal (i.e.
;        same lemma) to P2 as an ancestor of N1. Then attach P2 to the 
;        parent of P4 (i.e. the two equal prepositions become sisters)
; *** the knowledge needed for evaluating the attachment should be found
;     in an ontology, but, as usual, I adopt an extra file, to speed up
;     the implementation (see the variable *PREFER-PP-ATTACH*). Actually,
;     this choice is also motivated by the fact the here, in the parsing
;     stage, semantics is rarely used
(defun move-prepositions (data labels)
 (declare (special *PREFER-PP-ATTACH*))
 (let (final-links nxtcateg)
; *** repeat for all sentences, advancing in parallel on data and labels
    ; (format t "In Move-propositions")
    ; (break "")
    (do ((nxtsent (first data) (first locdata))
	 (locdata (rest data) (rest locdata))
	 (nxtsentlab (first labels) (first labels))
	 (labels (rest labels) (rest labels)))
        ((and (null nxtsent) (null locdata))
            (reverse final-links))
; *** work on all lines, advancing in parallel on data and labels
       (setq *PRINT-LEVEL* nil)
       (setq *PRINT-LENGTH* nil)
       (setq nxtsentlab
	  (do* ((prev nil (cons nxtline prev))
	        (prevlabs nil (cons nxtlab prevlabs))
	        (nxtline (first nxtsent) (first nxtlocsent))
	        (nxtlocsent (rest nxtsent) (rest nxtlocsent))
	        (nxtlab (first nxtsentlab) (first nxtlocsentlab))
	        (nxtlocsentlab (rest nxtsentlab) (rest nxtlocsentlab)))
	       ((null nxtline) (reverse prevlabs))
; *** when a noun is found, check its upward attachment
               (setq nxtcateg (get-synt-categ nxtline))
               (cond ((eq nxtcateg 'NOUN)
       ; *** case 1: "parte DI ricambio CON difetti"
                       (let (parentline parentlab actprep prep-info parentparline parentparlab
                             prevlines prevlinks)
                           (multiple-value-setq (parentline parentlab)
                               (find-noun-group-governor nxtline nxtlab prev prevlabs))
                           (cond ((eq (get-synt-categ parentline) 'PREP)
                 ; *** if the noun or its governing determiner is attached to a preposition, 
                 ;     then activate the search for a possible better attachment
                                   (setq actprep (get-synt-word parentline))
                                   (setq prep-info (leggi *PREFER-PP-ATTACH* actprep))
                                   (multiple-value-setq (parentparline parentparlab)
                                        (find-a-line-split `(position ,(first parentlab)) prev prevlabs))
                 ; *** nxtline="(DIFETTI)"; parentline="(CON)"
                 ;     actprep= "con"; parentparline="(RICAMBIO)"
                                   (cond ((eq (get-synt-categ parentparline) 'NOUN)
                                            (cond ((not (member 
                                                          (list (get-synt-word parentparline)
                                                                (get-synt-word nxtline))
                                                          prep-info :test #'equal))
                    ; *** if the current attachment is to a noun and the current one is not a strong 
                    ;     attachment (quasi-multiword), try the movement
                                                     (setq prevlabs
                                                           (find-new-up-attach
                                                                parentline parentparline nxtline
                                                                parentparlab prev prevlabs 
                                                                nxtsent nxtsentlab
                                                                'prep-move))
                                           ;        (format t "Prevlabs: ~a~%" prevlabs)
                                           ;        (break "move1")
                                                   )))
                                         (t nil))))))
    ; *** when a verb which is the head of a relative clause is found, check its upward attachment
                     ((and (eq nxtcateg 'VERB)
                           (lab-subsumes 'VERB-RMOD+RELCL (second nxtlab)))
       ; *** "tassi di umidità che fanno si che" nxtline="fanno"; parentline="umidità"
                       (let* (parentline parentlab parentparline parentparlab
                              actprep prep-info parparparent parparline parparlab
                              newupnounline newupnounlab)
                           (multiple-value-setq (parentline parentlab)
                                  (find-a-line-split `(position ,(first nxtlab)) prev prevlabs))
                     ; *** now: parentline "(UMIDITA')" (in "tassi di UMIDITA' che fanno sì che")
                           (cond ((eq (get-synt-categ parentline) 'NOUN)
                                    (multiple-value-setq (parentparline parentparlab)
                                           (find-noun-group-governor parentline parentlab prev prevlabs))
                     ; *** now: parentparline "(DI)" (in "tassi DI umidità che fanno sì che")
                                    (setq actprep (get-synt-word parentparline))
                                    (setq prep-info (leggi *PREFER-PP-ATTACH* actprep))
                                    (multiple-value-setq (newupnounline newupnounlab)
                                           (find-a-line-split `(position ,(first parentparlab)) prev prevlabs))
                     ; *** now: newupnounline "(TASSO)" (in "TASSI di umidità che fanno sì che")
                                    (cond ((member 
                                             (list (get-synt-word newupnounline) (get-synt-word parentline))
                                             prep-info :test #'equal)
                           ; *** the test is ok: set the relcl verb link to the upper noun
                                            (setq nxtlab 
                                                  (make-link (get-synt-numb newupnounline) 
                                                             (second nxtlab) 'prep-move)))))
                           ; *** otherwise, leave the link unchanged
                                          (t nil))))
    ; *** it is neither a PP nor a RELCL: leave the link unchanged
                     (t nil))))
        ;(break "move3")
; *** end of the loop on a sentence: save the new links
       (setq final-links (cons nxtsentlab final-links)))))

; *********************************************************************
; *** this compares possible attachments and chooses the best one
; *** the structure of *PREFER-PP-ATTACH* is as follows:
;     (prep ((NOUNUP1 NOUNDOWN1)
;                        .........
;            (NOUNUPN NOUNDOWNN)))
; *** every pair (NOUNUP NOUNDOWN) encodes a "strong attachment" (quasi-multiword)
;     any PP currently attached to NOUNDOWN, should instead be attached to NOUNUP
; *** INPUT:
;      given a sequence  NOUNUP -> PREP -> NOUNDOWN -> PP-PREP -> PP-NOUN
;   >>> el-to-move: the line of the involved prep (PP-PREP in the example)
;   >>> currattach: the line of the mid noun (NOUNDOWN in the example)
;   >>> noundown: the line of the down noun (PP-NOUN in the example)
;   >>> upuplab: the link associated with the upper prep (from PREP to NOUNUP)
;   >>> prevlines: all lines preceding PP-NOUN, i.e. including the preposition
;       that can possibly be moved
;   >>> prevlabs: all links preceding PP-NOUN
; *** The function starts from the current attachment of PP-PREP (i.e. NOUNDOWN)
;     and checks if something better can be found by moving up. This may happen
;     in case PREP actually is another prep (possibly skipping determiners), and
;     NOUNUP is a noun such that <NOUNUP NOUNDOWN> is stored in *PREFER-PP-ATTACH*
;     in association with the preposition PREP.
;     In this case, in fact, the sequence <NOUNUP PREP NOUNDOWN> is a
;     quasi-multiword (as "topic under discussion"), so that any further PP (as
;     "around this table") should be attached to "topic" instead of to "discussion"
; *** the second case of movement (see the comments in "move-prepositions") is when
;     the sequence "NOUNUP PP-PREP PP-NOUN" is strongly preferred (as in "the
;     topic of today under discussion"). Also in this case, the change is made.
; *** we will use (as an example of multiple movement):
;     N-ok <-l1- P1 <-l2- N-temp <-l3- P2 <-l4- N-old <-l5- PREP <-l6- N-down
;     If both (N-ok P1 N-temp) and (N-temp P2 N-old) are strong links, then (PREP N-down)
;     should be attached to N-ok
; *** Example: "Parti di ricambio con difetti" or 
;              "giornata con velori di temperatura superiori alla media, con tassi"
;        currattach: "(RICAMBIO ...)" "(MEDIA ...)"
;        el-to-move: "(CON ...)"      "(CON ...)"
;        noundown:   "(DIFETTO ...)"  "(TASSI ...)"
;        upuplab:    the up link of "DI" or the up link of "A" (alla media)
(defun find-new-up-attach (el-to-move currattach noundown upuplab 
                           prevlines prevlabs allines allabs context)
  (declare (special *PREFER-PP-ATTACH*))
  (let* ((bestparent currattach)
         (newstart currattach)
         (newstartlink upuplab)
         (poss-parallel-prep nil)
         (no-standard-move nil)
         (prep-to-move (get-synt-word el-to-move))
         prep-info-down prep-info-up upline uplink upupdata upupline upuplink)
    ; *** newstart is the current attachment line of PREP+NOUNDOWN (i.e. NOUNUP)
    ;     el-to-move = PREP
    ;     noundown = N-down
    ;     newstart = currattach = N-old 
    ;     newstartlink = upuplab = <-l4- (link from N-old to P2)
      (cond ((eq (get-synt-word (find-prec-line el-to-move allines allabs)) #\,)
              (setq poss-parallel-prep t)))
     ;(format t "In find-new-up-attach; el-to-move: ~a~% currattach: ~a~% noundown: ~a~%"
     ;        el-to-move currattach noundown)
     ;(break "")
      (do* ()
           ((null newstart)
         ; *** the loop ends when it is not possible to move up to find a better
         ;     attachment, or with the attachment has been found
     ;(format t "In find-new-up-attach: exit from the loop")
     ;(break "")
             (cond ((eq context 'verb) bestparent)
            ; *** this function was originally written for moving prepositions, where
            ;     the intended output was the whole set of new labels;
            ;     then, it was used for moving relative clauses, where the needed
            ;     output is the attachment line; so, the "context" variable was added
            ;     to keep apart the two situations
                   ((null bestparent) prevlabs)
                   ((equal currattach bestparent)
                      (cond ((eq context 'prep-down) nil)
                            (t prevlabs)))
            ; *** if not in case of relative clauses and the best attachment is the
            ;     original one, no change
                   ((eq context 'prep-down)
            ; *** if the context is prep-down, we are looking for the attachment of a
            ;     previously unattached preposition; the required output here is just the
            ;     new link
                      (make-link (get-newtb-numb bestparent) 'PREP-RMOD 'prep-move))
                   (t (mult-prep-change-lab prevlines prevlabs el-to-move bestparent))))
         (multiple-value-setq (upline uplink)
              (find-a-line-split `(position ,(first newstartlink)) prevlines prevlabs))
            ; *** newstartlink = upuplab: the up link of "DI" or the up link of "A" (alla media)
            ; *** upline:   "(PARTI ...)"  "(SUPERIORI ...)"
            ; *** uplink:   "the link from PARTI up"  "(VALORI ...)"
     ;(format t "find-new-up-attach: upline: ~a~%" upline)
     ;(break "")
         (cond ((null upline)
                  (setq newstart nil))
               ((or (eq (get-synt-categ upline) 'NOUN)
                    (is-a-synt-noun-complex upline allines allabs))
    ; *** if the upper line refers to a possible determiner, then simply move up one level
                  (setq newstartlink uplink))
               ((and (eq (get-synt-categ upline) 'ADJ)
                     poss-parallel-prep)
    ; *** if the upper line refers to an adjective, move to the upper noun
                  (multiple-value-setq (upline newstartlink)
                       (find-a-line-split `(position ,(first uplink)) prevlines prevlabs)))
               ((eq (get-synt-categ upline) 'PREP)
                 (cond ((and poss-parallel-prep
    ; *** the search goes on just in case the node above (i.e. POSSATTACH) is a NOUN
                             (eq (get-synt-word upline) prep-to-move))
                          (setq bestparent 
                                (first (find-a-line `(position ,(first uplink)) prevlines prevlabs)))
                          (setq newstart nil))
                       (t (setq prep-info-down (leggi *PREFER-PP-ATTACH* prep-to-move))
                          (setq prep-info-up (leggi *PREFER-PP-ATTACH* (get-synt-word upline)))
    ; *** and move up to the governor of the preposition
                          (multiple-value-setq (upupline upuplink) 
                                (find-a-line-split `(position ,(first uplink)) prevlines prevlabs))
    ;     upupline = N-temp (PARTI di ricambio con difetti)
                          (cond ((null upupline)
                                   (setq newstart nil))
                                ((and (eq (get-synt-categ upupline) 'NOUN)
                                      (not no-standard-move))
    ; *** the search goes on just in case the node above (i.e. POSSATTACH) is a NOUN
           ; *** this means that now POSSATTACH is upupline
           ;  (format t "PREP-UP: ~a~%PREP-DOWN: ~a~%UPUPLINE: ~a~%CURRATTACH: ~a~%NOUNDOWN~a~%"
           ;            prep-info-up prep-info-down upupline currattach noundown)
           ;  (break "")
           ; *** the list is (N-temp N-old) or (N-temp N-down)
                                  (cond ((or (member (list (get-synt-word upupline)
                                                           (get-synt-word newstart))
                                                     prep-info-up :test #'equal)
    ; *** <POSSATTACH UPELEMENT NOUNUP> is a strong link, so the attachment to
    ; *** POSSATTACH is better than the previous one (to NOUNUP)
                                             (member (list (get-synt-word upupline)
                                                           (get-synt-word noundown))
                                                     prep-info-down :test #'equal))
    ; *** <POSSATTACH PREP NOUNDOWN> is a strong link, so the attachment to
    ; *** POSSATTACH is better than the previous one (to NOUNUP)
                                           (setq bestparent upupline)
                                           (setq newstart upupline)
                                           (setq newstartlink upuplink))
                                        (t (setq newstart nil))))
                                ((and (eq (get-synt-categ upupline) 'ADJ)
                                      poss-parallel-prep)
                                   (setq newstart upupline)
                                   (setq newstartlink upuplink)
                                   (setq no-standard-move t))
                                (t (setq newstart nil))))))
               (t (setq newstart nil))))))

; *********************************************************************
; *** this function takes into account the transitivity of nouns.
;     If a PP is moved up to a new noun, then the latter could
;     include a transitivity property. Since the moved item has a
;     "strong" relations to that noun, it must act as a complement
;     of its.
; *** Moreover, if the moved prep is preceded by a comma, also the
;     comma is moved to the same place
(defun mult-prep-change-lab (prevlines prevlabs el-to-move newattach)
  (let (upderiv uptrans
        (prevword (second prevlines)))
    (cond ((eq 'NOUN (get-synt-categ newattach))    
            (setq upderiv (get-synt-vderiv newattach))
            (setq uptrans (get-synt-vtrans newattach))))
    (cond ((eq (get-synt-word prevword) #\,)
            (setq prevlabs
             (change-lab prevlines prevlabs 
                     (get-synt-numb prevword)
                     (make-link (get-synt-linumb newattach) 
                           (second (second prevlabs)) 'prep-move)))))
    (cond ((and upderiv (eq uptrans 'trans))
             (check-arg-and-change-lab 'NOUN-OBJ 
                     prevlines prevlabs el-to-move newattach))
          ((and upderiv (memq uptrans '(intrans refl)))
             (check-arg-and-change-lab 'NOUN-SUBJ 
                     prevlines prevlabs el-to-move newattach))
          (t (change-lab prevlines prevlabs 
                     (get-synt-linumb el-to-move)
                     (make-link (get-synt-linumb newattach) 'PREP-RMOD 'prep-move))))))

; *********************************************************************
; *** this modifies a possible previous argument of a noun, by assigning
;     it the PREP-RMOD label, and giving to "line" the argument role:
;        descrizione
;          |-- di (NOUN-OBJ) ---> ogni documento
;     is modified into
;        descrizione
;          |-- di (PREP-RMOD) ---> ogni documento
;          |-- di (NOUN-OBJ) ---> item
(defun check-arg-and-change-lab (arglabel prevlines prevlinks el-to-move newattach)
  (let ((prevarg (find-case
                     (find-dependents newattach prevlines prevlinks)
                     arglabel))
        (attachlinumb (get-synt-numb newattach)))
    (cond ((null prevarg)
             (change-lab prevlines prevlinks
                     (get-synt-numb el-to-move)
                     (make-link attachlinumb arglabel 'prep-move)))
          (t (mult-change-labs 
                 prevlines prevlinks
                 (list (list (get-synt-numb el-to-move) (list attachlinumb arglabel))
                       (list (get-synt-numb prevarg) (list attachlinumb 'PREP-RMOD)))
                 'prep-move
                 t)))))
    
; *********************************************************************
; *** usually, at the end of the main work, some items remain unattached.
;     This post-processor determines, via some low-level heuristics, an
;     attachement for all lines of the sentence
(defun attach-unlinked (data labels)
 (declare (special *PREFER-PP-ATTACH*))
 (let (final-data final-links newlineslinks attach tempres
       unattached-words possconjs newres nxttype nxtcateg nxt2categ)
; *** repeat for all sentences, advancing in parallel on data and labels
    (do ((nxtsent (first data) (first locdata))
	 (locdata (rest data) (rest locdata))
	 (nxtsentlab (first labels) (first labels))
	 (labels (rest labels) (rest labels)))
        ((and (null nxtsent) (null locdata))
           (values (reverse final-data) (reverse final-links)))
; *** first, do the work on unlinked verbs
     ;(format t "Attach unlinked 1; sentence: ~a~%           links: ~a~%" nxtsent nxtsentlab)
     ;(break "")
       (setq newlineslinks
           (attach-unlinked-verbs nxtsent nxtsentlab 
                          (first (find-a-line '(head) nxtsent nxtsentlab))))
       (setq nxtsent (first newlineslinks))
       (setq nxtsentlab (second newlineslinks))
; *** then, look for possible sequences of conjuncts
       (setq unattached-words
            (find-unattached-words nxtsent nxtsentlab))
       (multiple-value-setq (unattached-words possconjs)
	   (conjoin-equal-deps unattached-words nxtsent nxtsentlab))
   ; *** if a sequence of conjunctions has been identified, insert their labels
       (cond ((not (null possconjs))
	        (setq nxtsentlab
	              (mult-change-labs nxtsent nxtsentlab
				 (build-conj-labs possconjs)
                                 'conj-in-attach-unl))))
; *** then, work on all other unlinked elements, advancing in parallel on data
;     and labels
       (setq nxtsentlab
	  (do* ((prev nil (cons nxtline prev))
	     (prevlabs nil (cons nxtlab prevlabs))
	     (nxtline (first nxtsent) (first nxtlocsent))
	     (nxtlocsent (rest nxtsent) (rest nxtlocsent))
	     (nxtlab (first nxtsentlab) (first nxtlocsentlab))
	     (nxtlocsentlab (rest nxtsentlab) (rest nxtlocsentlab)))
	    ((null nxtline) (reverse prevlabs))
	   (declare (special prev prevlabs nxtline nxtlab nxtlocsent nxtlocsentlab))
   ;(format t "attach-unlinked loop: prevlabs: ~a~%" prevlabs)
   ;(break "")
            (setq nxtcateg (get-synt-categ nxtline))
            (setq nxt2categ (get-synt-categ (first nxtlocsent)))
	    (cond ((null nxtlab)
             ; (format t "attach-unlinked; nextline: ~a~%" nxtline)
             ; (break "One")
                    (setq nxttype (get-synt-type nxtline))
		    (setq nxtlab 
; *** depending on the category of the unattached word, different actions are
;     taken. The standard function for doing that is 'find-attachment', which
;     moves leftward and rightward to find an attachment point if the specified
;     category.
		       (case nxtcateg
			   (ADJ 
   ; *** the adj case is a bit more complex, since it aims at handling
   ;     cases as 'vera e propria disdetta', where in case the unlinked
   ;     ADJ (vera) is followed by a CONJ linked to it, then an attachment
   ;     to a following noun is preferred
                             (cond ((and (eq 'CONJ nxt2categ)
                                         (equal (get-synt-numb nxtline)
                                            (first (first nxtlocsentlab))))
                                     (find-attachment 
				        'ADJ nxttype '(nil (NOUN)) nxtsent nxtsentlab))
                                   ((eq nxttype 'qualif)
        ; *** in the standard case of attachment of adjectives, first look for the
        ;     closest attachment point, then, if it concerns a noun to the right,
        ;     try to see if agreement is ok, otherwise look for a better attachment
                                      (setq tempres
                                          (find-attachment 
				              'ADJ 'QUALIF '((NOUN VERB) (NOUN))
                                              nxtsent nxtsentlab))
                                      (cond ((null (first tempres)) tempres)
                                            ((index-precedes (first tempres) 
                                                             (get-synt-numb nxtline))
               ; *** the attachment is to a previous word
                                              (let* ((attachment-line-link
                                                      (find-a-line
	                                                   `(position ,(first tempres))
                                                           prev prevlabs))
                                                     (attach-line (first attachment-line-link))
                                                     (attach-link (second attachment-line-link)))
                                                 (cond ((and (eq (get-synt-categ attach-line) 'NOUN)
                                                             (not (int-ch-agree attach-line
                                                                    (list (get-synt-gender nxtline)
                                                                          (get-synt-number nxtline))
                                                                    '(gender number))))
               ; *** the link is to a noun, and agreement is not respected
                                                         (setq newres
					                    (move-adj-up attach-link 
                                                                 nxtline
                                                                 prev prevlabs))
                                                         (cond ((null newres) tempres)
                                                               (t newres)))
               ; *** the attachment to the noun is ok
                                                       (t tempres))))
                                            (t tempres)))
                                   (t (find-attachment 
				           'ADJ nxttype '((VERB) nil)
                                           nxtsent nxtsentlab))))
			   (ADV 
                              (cond ((and (eq nxttype 'TIME)
                                          (not (null prev))
                                          (eq (get-synt-numb (first prev))
                                              (second (first nxtsentlab))))
                    ;(format t "ADV; (first prev): ~a; (first nxtsentlab): ~a~%"
                    ;                (first prev) (first nxtsentlab))
                    ;(break "")
                                       (list (get-synt-numb (first nxtsent)) 'ADVB-RMOD-TIME))
                                    (t (find-attachment 
                                            'ADV nxttype '((VERB) (VERB)) nxtsent nxtsentlab))))
			   (NOUN 
       ; *** if the previous item is a noun, then noun-noun attachment
                              (cond ((eq 'NOUN (get-synt-categ (first prev)))
				       (find-attachment 
				          'NOUN nxttype '((NOUN) nil)
                                           nxtsent nxtsentlab))
       ; *** otherwise, look for a verb (before or after)
				    (t (find-attachment 
				          'NOUN nxttype '((VERB) (VERB))
                                          nxtsent nxtsentlab))))
			   (PRON (cond ((or (not (has-gramm-type 
                                              (get-synt-word nxtline) '&double-who))
                                            (has-gramm-type
                                              (get-synt-word nxtline) '&base-relat))
					 (find-attachment 
				  	    'PRON nxttype '((VERB) (VERB))
						   nxtsent nxtsentlab))
; *** a first exception are the 'doubly linked' relative pronoun, whose most
;     common case is 'chi'. Here, the requirement is to skip all material
;     belonging to the relatve clause after the pronoun, and to link it to the
;     next available verb
				       (t (attach-double-relpron 
						nxtsent nxtsentlab))))
; *** also for prepositions, it has to be kept apart the case where the prep
;     governs a relative pronoun ("di cui", "per il quale"). In these cases,
;     the head verb must follow the preposition (this does not apply to "double relpron")
			   (PREP (cond ((foll-relpron nxtlocsent)
					  (find-attachment
					      'PREP nxttype '(() (VERB))
					      nxtsent nxtsentlab))
     ; *** we first try to ascertain if the preposition can be attached to any preceding
     ;     noun. This preference over the verb exists just in case a "strong" relation
     ;     exists between previous noun, the preposition, and the governed noun, and
     ;     this preference is encoded in *PREFER-PP-ATTACH*
                                       (t (let ((preptab (leggi *PREFER-PP-ATTACH*
                                                              (get-synt-word nxtline)))
                                                (prepdep (first 
                                                           (find-a-line 
                                                             `(linked-to ,(get-synt-numb nxtline))
                                                              nxtsent nxtsentlab)))
                                                upnoun upnounlink posslab)
               ; *** if the preposition governs an adj, art or num, move one level down to find the
               ;     noun
                                             (cond ((memq (get-synt-categ prepdep) '(ART ADJ NUM))
                                                      (setq prepdep (first 
                                                           (find-a-line 
                                                             `(linked-to ,(get-synt-numb prepdep))
                                                               nxtsent nxtsentlab)))))
               ; *** before making this complex search check if at least the pair PREP - GOVERNED-NOUN
               ;     is in the table
                                             (cond ((member (get-synt-word prepdep) 
                                                            (mapcar #'second preptab))
               ; *** if it is ok, check if there is a possible attachment to a previous noun, in order to
               ;     initialize the search
                                                     (multiple-value-setq 
                                                           (upnoun upnounlink)
                                                           (find-prev-noun prev prevlabs))
                                                     (cond ((null upnoun)
                                                              (find-attachment 
					                           'PREP nxttype '((VERB) (VERB)) 
						                    nxtsent nxtsentlab))
                                                           (t (setq posslab
                                                                 (find-new-up-attach
                                                                   nxtline upnoun prepdep
                                                                   upnounlink prev prevlabs 
                                                                   nxtsent nxtsentlab
                                                                   'prep-down))
                                                              (cond ((null posslab)
                                                                       (find-attachment 
					                                   'prep nxttype '((VERB) (VERB)) 
						                                 nxtsent nxtsentlab))
                                                                    (t posslab)))))
                                                   (t (find-attachment 
					                   'prep nxttype '((VERB) (VERB)) 
						           nxtsent nxtsentlab)))))))
			   (CONJ (find-attachment 'CONJ nxttype '((VERB) nil)
						    nxtsent nxtsentlab))
			   (PHRAS (find-attachment 'PHRAS nxttype '((VERB) (VERB))
						    nxtsent nxtsentlab))
			   (SPECIAL 
                               (cond ((eq (get-synt-word nxtline) #\%)
				        (make-link (get-synt-numb (first prev)) 
                                              'NUM-ARG-PERCENT 'attach-unlinked))))
			   (MARKER nil)))
             ;  (format t "attach-unlinked; nextline: ~a~%    nextlab:~a~%" nxtline nxtlab)
             ;  (break "Two")
                   ))
; *** second level attempt; if the previous attachment failed
	    (cond ((null nxtlab)
           ;   (format t "attach-unlinked; nextline: ~a~%" nxtline)
           ;   (break "Three")
		    (cond ((eq nxtcateg 'PREP)
			     (setq nxtlab (find-attachment 
					  'PREP nxttype '((noun) ())
						    nxtsent nxtsentlab)))
		          ((eq nxtcateg 'ADV)
			     (setq nxtlab (find-attachment 
					  'ADV nxttype '((conj) (adv prep art noun adj pron num))
						    nxtsent nxtsentlab)))
		          ((eq nxtcateg 'CONJ)
			     (setq nxtlab (find-gov-verb (get-synt-numb nxtline) 
                                                  nxtsent nxtsentlab))))
           ;   (format t "attach-unlinked; nextline: ~a~%    nextlab:~a~%" nxtline nxtlab)
           ;   (break "Four")
                ))))
        ; (format t "Links: ~a~%" nxtsentlab)
        ; (break "sette-2")
       (setq nxtsentlab (assign-root nxtsent nxtsentlab))
  ; *** now, attach the punctuation marks
    ;  (format t "Links: ~a~%" nxtsentlab)
    ;  (break "after assign-root")
       (setq nxtsentlab (attach-punct-and-others nxtsent nxtsentlab))
    ;  (format t "Links: ~a~%" nxtsentlab)
    ;  (break "after attach-punct-and-others")
  ; *** At this point, the sentence should have a single root
  ;     N.B. Probably, this check is redundant, since it has already been made inside assign-root
       (setq nxtsentlab (check-single-root nxtsent nxtsentlab))
  ; *** at the very end, attach a possible sentence number
       (setq nxtsentlab (check-listpos nxtsent nxtsentlab))
       (setq final-data (cons nxtsent final-data))
       (setq final-links (cons nxtsentlab final-links)))))

; *********************************************************************
; *** starts from the attachment of an adj to a noun (for which ther is no agreement)
;     and moves up to find an agreeing attachment
; *** the only admitted sequence to go upwards is NOUN-PREP-NOUN...
;     it stops if:
;     - it finds a null link: failure (nil)
;     - it finds a line which is neither a PREP nor a NOUN: failure (nil)
;     - it finds a NOUN line for which agreement is ok: returns a pair (# of that line, label)
(defun move-adj-up (attach-link adjline prev prevlinks)
  (let (newlinelink newline newlink newcat adjtype)
   (cond ((null attach-link) nil)
         (t (setq newlinelink
                (find-a-line `(position ,(first attach-link)) prev prevlinks))
            (setq newline (first newlinelink))
            (setq newlink (second newlinelink))
            (setq newcat (get-synt-categ newline))
            (setq adjtype (get-synt-type adjline))
            (cond ((eq newcat 'NOUN)
                     (cond ((int-ch-agree newline 
                                (list (get-synt-gender adjline)
                                      (get-synt-number adjline))
                                '(gender number))
                              (make-link (get-synt-numb newline) 
                                     (choose-arc-label 'NOUN 'ADJ adjtype) 'move-adj))
                           (t (move-adj-up newlink adjline prev prevlinks))))
                  ((eq newcat 'PREP)
                     (move-adj-up newlink adjline prev prevlinks))
                  (t nil))))))

; *********************************************************************
; *** looks for a noun immediately preceding  prepositions
;     it skips adjs and advs (un regalo "molto bello" per Lucia)
(defun find-prev-noun (prevlines prevlinks)
  (cond ((null prevlines) nil)
        (t (let ((categ (get-synt-categ (first prevlines))))
               (cond ((eq categ 'NOUN) 
                            (values (first prevlines) (first prevlinks)))
                     ((memq categ '(ADJ ADV)) 
                        (find-prev-noun (rest prevlines) (rest prevlinks)))
                     (t nil))))))

; *********************************************************************
; *** this returns all words whose link is nil, excluding punctuation
(defun find-unattached-words (lines links)
  (let (result)
   (do ((nxtline (first lines) (first lines))
        (lines (rest lines) (rest lines))
        (nxtlink (first links) (first links))
        (links (rest links) (rest links)))
       ((null nxtline) (reverse result))
       (cond ((and (null nxtlink)
                   (neq (get-synt-categ nxtline) 'PUNCT))
                (setq result (cons nxtline result)))))))

; *********************************************************************
(defun dual-par (par)
  (cond ((char= par #\() #\))
	((char= par #\[) #\])
	((char= par #\<) #\>)
	((char= par #\)) #\()
	((char= par #\]) #\[)
	((char= par #\>) #\<)
	((char= par #\") #\")
	(t par)))

; *********************************************************************
; *** this checks for an initial sequence of a number or a letter
;     followed by a dot or a closed parenthesis. In this case, the
;     element is linked via the label 'num-rmod-listpos to the head
;     of the chunk immediately following the separator (climb-tree)
(defun check-listpos (allines allinks)
   (let ((firstline (first allines))
         (secondline (second allines))
         (remlinks (rest (rest allinks))))
   (cond ((and (null (first allinks))
               (null (second allinks))
               (is-listpos firstline secondline))
           (let ((path-up (reverse (climb-tree
                                       (get-synt-numb (third allines)) allines allinks)))
                 uplink1 uplab1 uplink2 uplab2)
             (cond ((or (null path-up)          ; the sequence is standing alone
                        (equal path-up '(nil)))
          ; *** this is the case of "1." If standing alone, this is not taken as a
          ;     list number, so it's a number followed by a period.
                      (setq uplink1 0)
                      (setq uplab1 'TOP-NUMBER)
                      (setq uplink2 1)
                      (setq uplab2 'END))
                    ((eq (first path-up) 0)  ; this means that we have reached the top
                      (setq uplink1 (second path-up))
                      (setq uplab1 'NUM-RMOD-LISTPOS)
                      (setq uplink2 (second path-up))
                      (setq uplab2 'SEPARATOR))
                    (t (setq uplink1 (first path-up))
                      (setq uplab1 'NUM-RMOD-LISTPOS)
                      (setq uplink2 (first path-up))
                      (setq uplab2 'SEPARATOR)))
             (append (cons (make-link uplink1 uplab1 'listpos)
                           (cons (make-link uplink2 uplab2 'listpos)
                                 remlinks)))))
   ; *** this second branch for a paragraph number (recognized by the tokenizer, i.e.
   ;     including two or more dots (3.4.2 is ok, 3.4 is not)) not followed by a separator
         ((and (null (first allinks))
               (eq 'paragraph-n (get-synt-categ firstline)))
           (let ((path-up (reverse (climb-tree
                                       (get-synt-numb (second allines)) allines allinks)))
                 uplink uplab)
             (cond ((null path-up)              ; the paragraph is standing alone
                      (setq uplink 0)
                      (setq uplab 'TOP-PARAGRAPH-N))
                   ((eq (first path-up) 0)      ; this means that we have reached the top
                      (setq uplink (second path-up))
                      (setq uplab 'NUM-RMOD-LISTPOS))
                   (t (setq uplink (first path-up))
                      (setq uplab 'NUM-RMOD-LISTPOS)))
             (cons (maek-link uplink uplab 'listpos) (rest allinks))))
         (t allinks))))

; *********************************************************************
; *** this takes as input two lines, and tells if they can be a pair
;     acting as paragraph number (as "1.", "a)", ...)    ;(
; *** It does not check if the two lines are at the beginning of a
;     paragraph
(defun is-listpos (firstline secondline)
   (or (eq 'paragraph-n (get-synt-categ firstline))
       (and (or (and (eq 'SPECIAL (get-synt-categ firstline))
                     (characterp (get-synt-word firstline))
                     (member (get-tule-char-name-from-lisp-char (get-synt-word firstline))
                             (union (get-charset-value 'minlet)
                                    (get-charset-value 'caplet))))
                (eq 'NUM (get-synt-categ firstline)))
            (characterp (get-synt-word secondline))
            (member (get-tule-char-name-from-lisp-char (get-synt-word secondline))
                    '(period closed-par)))))

; *********************************************************************
; *** this finds the first verb able to be the head of a subordinating
;     conjunction (and following it)
; *** It skips pairs conj-subord+verb and pron-relat+verb, as well as reduced
;     relative. To do this, it maintains a counter (vcount); it is initialized
;     to 0; all data inside the chunk governed by the conjucntion are skipped.
; *** inaux is used to keep apart reduced relatives from participles associated
;     with its auxiliary
(defun find-gov-verb (conjlinumb nxtlines nxtlinks)
   (let ((vcount 1) inaux categ type verbline verblink mainv)
     (do ((line (first nxtlines) (first remlines))
	  (remlines (rest nxtlines) (rest remlines))
	  (link (first nxtlinks) (first remlinks))
	  (remlinks (rest nxtlinks) (rest remlinks)))
	((or (null line)
	     (and (= vcount 0)
	          (eq (get-synt-categ verbline) 'VERB)
                  mainv
	          (memq (get-synt-type verbline) '(main mod))
		  (neq verblink 'COORD2ND)))
    ; *** if inaux is true, then the verb is preceded by auxiliaries, so it
    ;     cannot be a reduced relative, and is a possible governing verb
	   (cond ((null line) nil)
		 (t (list (get-synt-numb verbline) 'RMOD 'unlinked-verbs))))
	(setq categ (get-synt-categ line))
	(setq type (get-synt-type line))
   ; *** if climbing the tree from the current line, we are still inside
   ;     the subordinate, so go ahead
	(cond ((equal conjlinumb 
                  (ult (climb-tree 
                          (get-synt-numb line) nxtlines nxtlinks)))
                 nil)
	      ((eq categ 'VERB)
	      	 (cond ((eq type 'AUX)
		          (setq inaux t)
                          (setq mainv nil))
   ; *** if the verb is a second conjunct, it does not count to change the
   ;     current status
		       ((eq (second link) 'COORD2ND)
			  (setq inaux nil))
   ; *** if the verb is not a reduced relative, then it is assumed to satisfy
   ;     one of the expectations (vcount)
	               ((or inaux
	      	            (neq (get-synt-mood line) 'PARTICIPLE))
                          (setq verbline line)
                          (setq verblink (second link))
		          (setq inaux nil)
		          (setq mainv t)
		          (setq vcount (1- vcount)))))
	      ((or (and (eq categ 'PRON) (eq type 'RELAT))
		   (and (eq categ 'CONJ) (eq type 'SUBORD)))
		 (setq vcount (1+ vcount)))))))

; *********************************************************************
; *** For punctuation, different things are done here.
;     1. The attachment point is looked for via 'find-up-cluster'
;     2. Pairs of parenthetical and quotes are looked for
; *** All work is made by the function 'find-up-cluster', which is called when
;     an open parenthesis or open double quote is found
(defun attach-punct-and-others (nxtsent nxtsentlab)
; *** par-stack stores open parentheses still to be closed
; *** openmin stores a possible « symbol, in order to match it with the
;     corresponding closed »
  (declare (special *LANGUAGE*))
  (let (first-quote char attach failed-quotes templabs openmin)
       (declare (special par-stack))
   (setq templabs 
    (do* ((prev nil (cons nxtline prev))
	  (prevlabs nil (cons nxtlab prevlabs))
	  (nxtline (car nxtsent) (car nxtlocsent))
	  (nxtlocsent (cdr nxtsent) (cdr nxtlocsent))
	  (nxtlab (car nxtsentlab) (car nxtlocsentlab))
	  (nxtlocsentlab (cdr nxtsentlab) (cdr nxtlocsentlab)))
	 ((null nxtline) (reverse prevlabs))
	 (declare (special prev prevlabs nxtline nxtlab nxtlocsent nxtlocsentlab))
       (cond ((and (eq 'PUNCT (get-synt-categ nxtline)) (null nxtlab))
	       (setq char (get-synt-word nxtline))
     	       (cond ((or (char= char #\")		; "
     	                  (and (char= char #\') (neq *LANGUAGE* 'english)))
; *** the punctuation mark is " or ' (!!! in the current version they are
;     not taken apart, so they can be paired freely (my "friend'))
   ; *** if 'first-quote' is nil, then either it is the first " of the pair, or the
   ;     search for attachment of the first " of the pair failed. In the first case,
   ;     we must search for the attachment point, else, we must repeat the failure
   ;     also for the second element
		       (cond ((null first-quote)
			       (cond (failed-quotes
   ; *** failed-quotes is true if no attachment point was found either with a
   ;     'first' ", or with a «; but NIL must be left also in case a » is found
   ;     without the corresponding open.
					(setq nxtlab nil)
					(setq failed-quotes nil))
   ; *** now, we are sure that first-quote is null just because it was never
   ;     looked for, so do it
				     (t (setq first-quote (find-up-cluster char t))
   ; *** and if it isn't found, set failed-quotes to t
			       	       (cond ((null first-quote)
				       		(setq nxtlab nil)
				       		(setq failed-quotes t))
   ; *** conversely, if it is found, build the "open" link
			             	     (t (setq nxtlab 
					           (make-link first-quote 
							'OPEN+QUOTES 'attach-punct)))))))
   ; *** first-quote is not null
			     (t (setq nxtlab (make-link first-quote 'CLOSE+QUOTES 'attach-punct))
			       (setq first-quote nil))))
; *** the punctuation mark is «
     	             ((= (char-code char) 171)                           ; #\«	UTF-8 !!!!!
   ; *** an attachment point must be looked for and, if found, openmin must be set
   ;     N.B. Nested « and » are not accounted for
   		        (setq openmin (find-up-cluster char t))
          	        (cond ((null openmin) (setq nxtlab nil))
   ; *** if it is found, build the "open" link
   	                      (t (setq nxtlab 
   			              (make-link openmin 'OPEN+QUOTES 'attach-punct)))))
; *** the punctuation mark is »
                        ((= (char-code char) 187)                       ; #\»	UTF-8 !!!!!
   	       	        (cond ((null openmin) (setq nxtlab nil))
   ; *** if the attachment point for the corresponding 'open' was found,
   ;     use it to build the 'close' link
   	                      (t (setq nxtlab 
   			              (make-link openmin 'CLOSE+QUOTES 'attach-punct)))))
     ; *** if an unattached comma is followed by a coordinating conjunction,
     ;     assume same attachment point
                     ((and (char= char #\,)
                           (eq (get-synt-categ (first nxtlocsent)) 'CONJ)
                           (eq (get-synt-type (first nxtlocsent)) 'COORD))
		        (setq attach (first (first nxtlocsentlab)))
                        (cond ((or (null attach) (eq attach 0))
            ; *** this may happen in case the following conjunction has not yet found an attachhment
            ;     I assume that sentences do not start with a comma (unfortunately not true)
            ; *** if the attachment point of the next item is 0, it cannot be used as the
            ; attachment of the comma, otherwise, we could have two roots
		                 (setq attach (get-synt-numb (first prev)))
                                 (cond ((null attach)
                      ; *** in this case, attach to the following conj
                                          (setq nxtlab (make-link (get-synt-numb (first nxtlocsent))
                                                                  'SEPARATOR 'attach-punct)))
                                       (t (setq nxtlab (make-link attach 'SEPARATOR 'attach-punct)))))
                              (t (setq nxtlab (make-link attach 'SEPARATOR 'attach-punct)))))
; *** nothing is done if this a list numbering
     ; *** this is not the first element of a list numbering (NUM-RMOD-LISTPOS)
                     ((and (not (and (null prev)
                                     (is-listpos nxtline (first nxtlocsent))))
     ; *** neither the second element of a list numbering
                           (not (and (null (rest prev))
                             	     (is-listpos (first prev) nxtline))))
		        (setq attach (find-up-cluster char))
		        (cond ((not (null attach))
			        (setq nxtlab (make-link attach 'SEPARATOR 'attach-punct))))))))))
; *** if any punct remains unlinked, then, if also the following element is
;     unlinked, then assume it is a coordination, otherwise link it as a separator
;     to the previous element
         (solve-rem-puncts-and-others nxtsent templabs)))

; *********************************************************************
; *** try a final solution for commas
(defun solve-rem-puncts-and-others (allines allinks)
  (let (first-conj conjoined-lines
        (savallines allines) (savallinks allinks))
    (do* ((prevlines nil (cons nxtline prevlines))
	  (prevlinks nil (cons nxtlink prevlinks))
	  (nxtline (car allines) (car allines))
	  (allines (cdr allines) (cdr allines))
	  (nxtlink (car allinks) (car allinks))
	  (allinks (cdr allinks) (cdr allinks)))
	 ((null nxtline) (reverse prevlinks))
       (let ((nxtcateg (get-synt-categ nxtline))
             (nxtword (get-synt-word nxtline))
             (nxtnumb (get-synt-numb nxtline))
             (prevnumb (get-synt-numb (first prevlines)))
             (prevcateg (get-synt-categ (first prevlines))))
       (cond ((not (null nxtlink)) nil)
; *** if an unlinked comma is followed by another unlinked element, assume a
;     coordination
             ((and (eq 'PUNCT nxtcateg)
	           (eq #\, nxtword))
		(cond ((null (first allinks))
			(setq conjoined-lines 
			     (find-first-conj nxtline (list (first allines)) 
                                        prevlines prevlinks allines allinks))
   ; (break "conjoined lines")
			(cond ((not (null conjoined-lines))
			        (setq first-conj (first conjoined-lines))
				(setq nxtlink (make-link (get-synt-numb first-conj) 
                                                         'COORD 'final-punct))
				(setq allinks (cons (make-link nxtnumb 'COORD2ND 'final-punct)
                                                    (rest allinks))))
  ; *** but if no first conjunct can be found, opt for a separator
		      	      (t (setq nxtlink 
				       (make-link prevnumb 'SEPARATOR 'final-punct)))))
; *** otherwise, assume that the comma is a separator
		      (t (setq nxtlink (make-link prevnumb 'SEPARATOR 'final-punct)))))
; *** if it is an article, and it is preceded by a comma and a noun, assume it is
;     an apposition
             ((and (eq 'ART nxtcateg)
                   (eq #\, (get-synt-word (first prevlines)))
                   (eq 'NOUN (get-synt-categ (second prevlines))))
	       (setq nxtlink (make-link (get-synt-numb (second prevlines)) 'APPOSITION 'final-punct)))
; *** nothing is done if this a list numbering
     ; *** this is not the first element of a list numbering (NUM-RMOD-LISTPOS)
             ((and (not (and (null prevlines)
                             (is-listpos nxtline (first allines))))
     ; *** neither the second element of a list numbering
                   (not (and (null (rest prevlines))
                             (is-listpos (first prevlines) nxtline))))
; *** solve others: attach to previous line
                (cond ((or (and (eq prevcateg 'CONJ)
                                (eq (get-synt-type (first prevlines)) 'COORD))
                           (eq (get-synt-word (first prevlines)) #\&))
                         (setq nxtlink (make-link prevnumb 'COORD2ND 'final-punct)))
                      ((and (eq 'PUNCT nxtcateg)
                            (eq #\- nxtword))
                         (setq nxtlink (make-link prevnumb 'SEPARATOR 'final-punct)))
                      ((and (eq 'PUNCT nxtcateg)
	                    (eq #\. nxtword)
                            (null allines))
                         (setq nxtlink (make-link 0 'END 'final-punct)))
                      ((memq nxtcateg '(VERB PRON ART NOUN))
                         (let ((attach (get-synt-numb
                                            (first (find-a-line '(categ (conj) type (coord))
                                                                  prevlines prevlinks nil)))))
                             (cond ((null attach)
                                     (setq attach
                                        (get-synt-numb
                                            (first (find-a-line '(word #\:) prevlines prevlinks nil))))))
                             (cond ((null attach)
                                     (setq attach
                                        (get-synt-numb
                                            (first (find-a-line '(word #\,) prevlines prevlinks nil))))))
                             (cond ((null attach)
                                     (setq attach
                                        (get-synt-numb
                                            (first (find-a-line '(categ (verb)) prevlines prevlinks nil))))))
                             (cond ((null attach)
                                      (setq attach 
                                        (get-synt-numb (get-tree-single-root savallines savallinks)))))
                             (cond ((null attach) (setq attach 0)))
                             (setq nxtlink (make-link attach 'UNKNOWN 'final-punct))))
                      ((not (null prevnumb))
                         (setq nxtlink (make-link prevnumb 'UNKNOWN 'final-punct)))
                      (t (setq prevnumb (get-synt-numb (get-tree-single-root savallines savallinks)))
                         (cond ((null prevnumb)
                                ; (format t "in solve-rem-punct; savallines: ~a~%; savallinks ~a~%"
                                ;    savallines savallinks)
                                ; (break "")
                                 (setq nxtlink (make-link 0 'UNKNOWN 'final-punct)))
                               (t (setq nxtlink (make-link prevnumb 'UNKNOWN 'final-punct))))))))))))

; *********************************************************************
; *** looks for a word whose category is one of 'categs'. It moves in parallel
;     leftward and rightward, until the word is found or the sentence ends (or
;     'starts', for leftward search)
; *** INPUT:
;   >>> categs: a pair, whose first element are the left categories, and the
;         	second element are the right categories; these are the categories
;		to which the current word could be attached
;   >>> wrdcat: the category of the word to be attached
; *** OUTPUT:
;   >>> a pair with the line number of the found parent and the label of the link
;		currently, the arc label is determined on the unique basis of the
;		category of the dependent (adv --> advbmod)
(defun find-attachment (wrdcat wrdtype categs allines allabs)
; *** currently, no preference is enforced, so it moves one step leftward and
;     one step rightward, until one of the required categories is found
; *** prev, nxtline and nextlocsent are global and contain the left context (in
;     reverse order), the current line (the one to attach), and the right context,
;     respectively; prevlabs and nxtlocsentlabs are the labels
  (declare 
      (special prev prevlabs nxtlocsent nxtlocsentlab nxtline))
  (let ((res (int-find-attach 
			wrdcat wrdtype nxtline (first categs) prev prevlabs 
			       (second categs) nxtlocsent nxtlocsentlab
			       allines allabs)))
      (cond ((null res) nil)
	    ((eq (get-synt-type (first res)) 'AUX)
	       (make-link (get-synt-numb (find-main-v (second res) allines allabs))
		     (choose-arc-label 'VERB wrdcat wrdtype)
                     'attach-unlinked))
	    (t (make-link (get-synt-numb (first res)) 
		     (choose-arc-label (get-synt-categ (first res)) wrdcat wrdtype)
                     'attach-unlinked)))))

; *********************************************************************
; *** given the link of an auxiliary to its governor, finds the main verb
;     it is assumed that going up one or two levels, the main verb is necessarily
;     found
; *** it returns the line of the governing main
(defun find-main-v (link-up allines allinks)
  (let ((possmain 
	    (find-a-line (list 'position (first link-up)) allines allinks)))
      (cond ((eq (get-synt-type (first possmain)) 'AUX)
	      (first (find-a-line (list 'position (first (second possmain))) 
				  allines allinks)))
	    (t (first possmain)))))

; *********************************************************************
; *** finds a possible attachment point; see the comments of the previous function
;     it returns the line of the attachment point and its link up
; *** INPUT:
;  >>> wrdcat: the category of the word to be attached
;  >>> curline: the line of the word to be attached
;  >>> leftcat: the possible attachments to the left (a list of categories)
;  >>> leftlines: the lines preceding the word (in reverse order)
;  >>> leftlabs: the links preceding the word (in reverse order)
;  >>> rightcat: the possible attachments to the right (a list of categories)
;  >>> rightlines: the lines following the word
;  >>> allines: all the lines in the sentence
;  >>> allabs: all the links in the sentence
(defun int-find-attach 
	(wrdcat wrdtype curline leftcat leftlines leftlabs 
		        rightcat rightlines rightlabs allines allabs)
  (cond ((and (null leftlines) (null rightlines)) nil)
        (t (let ((firstleft (first leftlines))
                 (firstright (first rightlines)))
              (cond ((and (not (null leftlines))
	                  (memq (get-synt-categ firstleft) leftcat)
      ; *** the next test to avoid attachment to a participle already attached as
      ;     an adjectival modifier to a (presumably following) noun
                          (not (eq 'ADJC+QUALIF-RMOD (second (first leftlabs))))
      ; *** the next to avoid loops
	                  (eq 'fail (belong-to-chunk 
                                         firstleft (first leftlabs)
			                 (get-synt-numb curline) 
                                         nil allines allabs nil)))
; *** if found in the left, return the result
	               (make-link firstleft (first leftlabs) 'attach-unlinked))
	            ((and (not (null rightlines))
	                  (memq (get-synt-categ firstright) rightcat)
     ; *** infinites that are prepositional arguments cannot take dependents on their left
     ;     gerunds for English ("on combating")
                          (not (and (equal rightcat '(VERB))
                                    (eq (second (first rightlabs)) 'PREP-ARG)
                                    (memq (get-synt-mood firstright) '(INFINITE GERUND))))
     ; *** reduced relatives cannot take items from the left
			  (not (is-reduced-relative firstright 
                                    (reverse (find-prec-lines firstright allines))))
      ; *** the next to avoid loops
	                  (eq 'fail (belong-to-chunk 
                                            firstright (first rightlabs)
				            (get-synt-numb curline)             
                                            nil allines allabs nil)))
; *** if found in the right, return the result
	               (make-link firstright (first rightlabs) 'attach-unlinked))
; *** if not found, but either the left or right category is a barrier, and 
;     what is looked for is a verb, continue only on the other side
                    ((or (and (eq (get-synt-categ firstleft) 'CONJ)
                              (eq (get-synt-type firstleft) 'SUBORD))
                         (and (eq (get-synt-categ firstleft) 'PRON)
                              (eq (get-synt-type firstleft) 'RELAT)))
	              (int-find-attach 
		          wrdcat wrdtype curline
                          (remove 'verb leftcat) (rest leftlines) (rest leftlabs) 
		          rightcat (rest rightlines) (rest rightlabs)
                          allines allabs))
; *** if the word at the left is a coord such that the second conjunct is at the
;     right of the word to attach, then no attachment to the left of the conjunction
;     is admitted (projectivity)
                    ((and (or (and (eq (get-synt-categ firstleft) 'CONJ)
                                   (eq (get-synt-type firstleft) 'COORD))
                              (eq (get-synt-word firstleft) #\&))
                          (member (get-synt-numb firstleft) rightlabs
                                  :test #'(lambda (x y) (eq x (first y)))))
   ; *** the test succeeds if one of the words following the word to attach (rightlabs)
   ;     points to (first) the conj found at the left of the word to attach
	              (int-find-attach 
		          wrdcat wrdtype curline
                          nil (rest leftlines) (rest leftlabs) 
		          rightcat (rest rightlines) (rest rightlabs)
                          allines allabs))
; *** if the next word on the right is a conj subord or a pron relat, then the search is blocked
;     in that direction, unless the relative pronoun is a double relative (see next branch). 
                    ((or (and (eq (get-synt-categ firstright) 'CONJ)
                              (eq (get-synt-type firstright) 'SUBORD))
                         (and (eq (get-synt-categ firstright) 'PRON)
                              (eq (get-synt-type firstright) 'RELAT)
  	                      (not (has-gramm-type (get-synt-word firstright) '&double-who))))
	              (int-find-attach 
		          wrdcat wrdtype curline
                          leftcat (rest leftlines) (rest leftlabs) 
		          (remove 'verb rightcat) (rest rightlines) (rest rightlabs)
                          allines allabs))
; *** In case of double relatives, the
;     search must continue after the next verb (which is the one of the relative clause):
;     "Da chi era colpevole non posso essere giudicato" (By whom was guilty I cannot be judged)
;     In this case "was" must be skipped, in order to find "be judged"
                    ((and (eq (get-synt-categ firstright) 'PRON)
                          (eq (get-synt-type firstright) 'RELAT)
  	                  (has-gramm-type (get-synt-word firstright) '&double-who))
                      (multiple-value-setq (rightlines rightlabs)
                         (jump-after-verb rightlines rightlabs))
	              (int-find-attach 
		          wrdcat wrdtype curline
                          leftcat (rest leftlines) (rest leftlabs) 
		          rightcat rightlines rightlabs
                          allines allabs))
; *** standard recursion: both leftward and rightward
	            (t (int-find-attach 
		          wrdcat wrdtype curline leftcat (rest leftlines) (rest leftlabs) 
		          rightcat (rest rightlines) (rest rightlabs)
                          allines allabs)))))))

; *********************************************************************
; *** determines the arc name on the basis of the linked categories
(defun choose-arc-label (headcat depcat deptype)
  (cond ((eq headcat 'VERB) 
            (cond ((eq depcat 'ADV)
                    (cond ((eq deptype 'TIME) 'ADVB-RMOD-TIME)
                          ((eq deptype 'LOC) 'ADVB-RMOD-LOC)
                          (t 'ADVB-RMOD)))
                  (t 'RMOD)))
  	(t (case depcat
		(adj 
                   (cond ((eq deptype 'QUALIF) 'ADJC+QUALIF-RMOD)
                         (t 'ADJC-RMOD)))
		(adv
                   (cond ((eq deptype 'TIME) 'ADVB-RMOD-TIME)
                         ((eq deptype 'LOC) 'ADVB-RMOD-LOC)
                         (t 'ADVB-RMOD)))
		(prep 'PREP-RMOD)
		(noun 'NOUN-RMOD)
		(pron 'PRON-RMOD)
		(num 'NUM-RMOD)
		(phras 'PHRAS)
		(verb 'VERB-RMOD)))))

; *********************************************************************
; *** moves ahead on lines and links until a main verb is found
;     returns all lines and links after that verb
(defun jump-after-verb (lines labs)
   (let (found)
     (do ((nxtline (first lines) (first lines))
          (lines (rest lines) (rest lines))
          (nxtlab (first labs) (first labs))
          (labs (rest labs) (rest labs)))
        ((or (null nxtline) found)
           (cond ((null nxtline) nil)
		 ((is-a-synt-trace? nxtline)
          ; *** it should be the trace of the double relpron
                    (values lines labs))
                 (t (values (cons nxtline lines) (cons nxtlab labs)))))
        (cond ((and (eq (get-synt-categ nxtline) 'VERB)
                    (eq (get-synt-type nxtline) 'MAIN))
                 (setq found t))))))

; *********************************************************************
; *** this aims at finding an attachment point for a comma, or a pair of
;     parenthesis
; *** the idea is very simple: the attachment point is the least common
;     ancestor of the lines preceding the separator and the lines following 
;     the separator
; *** barrier is used when there is a closing element that cannot be crossed.
;     for instance, in ' the "administrative" work ', the head of the cluster
;     is 'work', but the closing quotes should not be crossed, so the line to
;     return is the one of 'administrative'
(defun find-up-cluster (char &optional barrier)
; *** prev, prevlabs, nxtline, nxtlab, nxtlocsent, nxtlocsentlab are special
 (declare (special prev prevlabs nxtlocsent nxtlocsentlab nxtline nxtlab))
 (let ((linumb (get-synt-numb nxtline))
       up-path head-numb find-res newnxtlines newnxtlinks barrier-line)
  (cond ((or (char= char #\")
             (char= char #\')
             (= (char-code char) 171)			; !!! UTF-8 «
                              )
; *** in case of quotations, what is looked for is the head of the cluster
;     inside the quotation: e.g. for "Repubblica indipendente di Valona"
;     (independent Republic of Valona), it is "repubblica"
; *** up-path is the sequence of line numbers representing the path from
;     the line following the (first) double quotes, to the top of the chunk
	   (setq up-path
		(climb-tree 
			(get-synt-numb (first nxtlocsent))
			(append (reverse prev) (list nxtline) nxtlocsent)
			(append (reverse prevlabs) 
				(list nxtlab) nxtlocsentlab)))
           (cond (barrier
                   (setq barrier-line
                    (find-a-line `(word ,(closing-quote char)) nxtlocsent nxtlocsentlab))))
; *** the head is assumed to be the last line in the path (the highest)
;     following the quote, but in case there is a barrier, it must not be crossed
	   (cond (barrier
                    (setq head-numb
		       (last-greater up-path linumb (get-synt-numb barrier-line))))
                 (t (setq head-numb (last-greater up-path linumb))))
	   (setq find-res
	      (find-a-line 
		 `(position ,head-numb) nxtlocsent nxtlocsentlab))
; *** find-res should be the head of the cluster following the double quote
; *** then, return its line number
	   (get-synt-numb (first find-res)))
; *** otherwise (e.g. for commas) the link should be to the least common ancestor of
;     what precedes and what follows the comma
	 (t (setq newnxtlines nxtlocsent)
	    (setq newnxtlinks nxtlocsentlab)
  	    (int-find-up-clust (dropnil (mapcar #'first prevlabs))
 		     	       (dropnil (mapcar #'first newnxtlinks))
		     nxtline nxtlab prev prevlabs newnxtlines newnxtlinks)))))

; *********************************************************************
(defun closing-quote (char)
  (cond ((= (char-code char) 171) (code-char 187))    ;  #\« #\»     UTF-8
        (t char)))

; *********************************************************************
; *** returns the line number which precedes (in the list linumbs)
;     the first line number referring to a line which precedes val
; *** the situation is as follows:
;     w1 w2 ... val w3 w4 ... barrier-linumb w5 w6 ...
;     where barrier-linumb could be absent
;     the procedure looks for the first line in "linumbs" whose follower
;     is absent (i.e. it is the last line) or is outside the boundaries
; *** linumbs is assumed to contain at least one index, and that
;     the first index follows val
(defun last-greater (linumbs val &optional barrier-linumb)
   (cond ((null (second linumbs))
            (first linumbs))
  ; *** index-precedes defined in *HOME-DIR*/PROC/MORPHO/tb-functions
	 ((index-precedes val (second linumbs))
            (cond ((or (null barrier-linumb)
                       (index-precedes (second linumbs) barrier-linumb))
                ; *** the second boundary does not exist, or it is respected
	            (last-greater (rest linumbs) val))
                  (t (first linumbs))))
	 (t (first linumbs))))

; *********************************************************************
; *** looks for an attachment point for commas; prevpoint is the line
;     preceding the comma; nxtpoint is the line following the comma;
;     curline is the line of the comma
(defun int-find-up-clust
	(prevpoint nxtpoint curline curlink prevlines prevlinks nxtlines nxtlinks)
 (let ((allines (append (reverse prevlines) (list curline) nxtlines))
       (allinks (append (reverse prevlinks) (list curlink) nxtlinks)))
; *** if the left or right context is empty, the attachment is made to the
;     root of the other side; it is assumed that the root is the closest unattached
;     element to the left (or right) of the comma
  (cond ((null prevpoint)
	  (second (reverse (climb-tree 
			    (get-synt-numb 
				 (first (find-a-line '(not-unlinked)
						 nxtlines nxtlinks)))
			     allines allinks))))
        ((null nxtpoint)
	  (second (reverse (climb-tree 
			    (get-synt-numb 
				 (first (find-a-line '(not-unlinked)
						 prevlines prevlinks)))
			     allines allinks))))
; *** otherwise, to the lowest common ancestor (which can be at the left or
;     at the right)
	(t (find-lowest 
		(get-synt-numb 
		    (first (find-a-line '(not-unlinked) prevlines prevlinks)))
		(get-synt-numb 
		    (first (find-a-line '(not-unlinked) nxtlines nxtlinks)))
		allines allinks)))))

; *********************************************************************
; *** starting from the nearest element (which is the first of pointers)
;     go up until a nil pointer or a 0 (root) pointer is found
; *** returns the whole path (ordered from lowest to highest), as a list
;     of line numbers
(defun climb-tree (start allines allinks)
  (do* ((prev start (first (second nxtup)))
	(path (list prev) (cons prev path))
    	(nxtup (find-a-line `(position ,prev) allines allinks) 
	       (find-a-line `(position ,prev) allines allinks)))
       ((or (null nxtup)
            (null (second nxtup))
	    (equal 0 (first (second nxtup)))
; *** the last exit condition in case there are (wrong) loops in the tree
	    (member (first (second nxtup)) path))
	 (cond ((equal 0 (first (second nxtup)))
; *** the top, if any, must be inserted in the path
		  (reverse (cons 0 path)))
	       (t (reverse path))))))
  
; *********************************************************************
; *** checks if a line occurs in the subtree of the other
; *** INPUT:
;  ## dep: the line number of the direct governor link of the possible dependent
;  ## gov: the line of the possible governor
(defun dependent? (dep gov allines allinks)
  (do* ((prev dep (first (second nxtup)))
	(path (list prev) (cons prev path))
    	(nxtup (find-a-line `(position ,prev) allines allinks) 
	       (find-a-line `(position ,prev) allines allinks)))
       ((or (equal (first nxtup) gov)
            (null nxtup)
            (null (second nxtup))
	    (equal 0 (first (second nxtup)))
; *** the last exit condition in case there are (wrong) loops in the tree
	    (member (first (second nxtup)) path))
	 (cond ((equal (first nxtup) gov) t)
	       (t nil)))))
  
; *********************************************************************
; *********************************************************************
; *** find the two paths upward, and then determine the first common element
(defun find-lowest (left right allines allabs)
   (let ((leftpath (climb-tree left allines allabs))
	 (rightpath (climb-tree right allines allabs)))
   (do ((leftn (first leftpath) (first leftpath))
	(leftpath (rest leftpath) (rest leftpath)))
       ((or (null leftn)
	    (member leftn rightpath :test #'equal))
	   leftn))))
 
; *********************************************************************
(defun attach-unlinked-verbs (allines allabs head)
; *** this handles unattached verbs;
;     - if the verb is preceded by a subordinating conjunction, then it is
;	attached to it as CONJ-ARG
;     - if the verb is preceded by a relative pronoun, then it is attached to
;	the noun/pronoun preceding the relpron as VERB-RMOD+RELCL
;     - if the verb is preceded by a Modal verb, then it is attached to it as
;	VERB+MODAL-INDCOMPL
;     - if the verb is a gerund, then it is attached to the nearest verb as 
;       adjunct
;     In all the cases above, the search for a preceding element is interrupted
;     by any intervening verb. If no case applies, the verb is taken as the main
;     verb. If more than one candidate main verb remains, then one of them is
;     chosen, while the others remain are attached to the preceding verb, if any,
;     or to the next verb (if any) or to the preceding word (low-low
;     level heuristics)
; *** in case the verb is chosen as the main verb, but the processing occurs
;     inside a parenthesis, then the verb is interpreted as an apposition
 (declare (special *PARSE-CONTEXT*))
 (let (context still-unlinked modal-dependence newlineslabs status found)
 ; %%%%%%%%%%%%%%%%%%% A PRE-STEP: try attach reduced relative clauses %%%%%%%%%%%%%%%
 ; %%%%%%%%%%%%%%%%%%%             or predicative complements %%%%%%%%%%%%%%%%%%%%%%%%
; *** N.B. Rel clauses directly preceded by the governing noun have already been
;          attached by the chunking parse rules
;     This is useful for cases as "la riunione del consiglio convocata" (the 
;     meeting of the council summoned), where, in Italian, "riunione" (meeting)
;     is feminine, "consiglio" (council) is masculine, and "convocata"
;     (summoned) is feminine
; *** The case of predicative complements is exemplified by "seems often reached by"
;     The loop checks for nouns and predicative verbs. The status is initially "search-all"
;     If something that is not an adj, article or preposition is found, the search for nouns
;     is blocked, while the one for predicative verbs is continued (status = "searrch-verb").
;     This is blocked when a non-predicative verb is found
  (setq status 'search-all)
  ;(format t " Attach-unlinked-verbs: allabs= ~a~%" allabs)
  ;(break "")
  (setq allabs
    (do* ((prlines nil (cons nxtline prlines))
	  (prlabs nil (cons nxtlab prlabs))
	  (nxtline (first allines) (first remlines))
	  (nxtlab (first allabs) (first remlabs))
	  (remlines (rest allines) (rest remlines))
	  (remlabs (rest allabs) (rest remlabs)))
	 ((null nxtline) (reverse prlabs))
        (cond ((and (null nxtlab)
                    (eq (get-synt-categ nxtline) 'VERB)
                    (eq (get-synt-mood nxtline) 'PARTICIPLE))
     ; *** a verb which is a participle
                 (do ((nxtprev (first prlines) (first tempprev))
                      (tempprev (rest prlines) (rest tempprev)))
                    ((or (eq status 'fail) (null nxtprev))
                       (setq nxtlab found))
                    (cond ((eq status 'search-all)
     ; *** if the next possible attachment point is a noun
                            (cond ((eq (get-synt-categ nxtprev) 'NOUN)
     ; *** if it agrees in gender and number 
     ; !!!!!!! agr-unif defined in ALLLANG/PROC-ALL/TAGGER-PROC-ALL/postagger 
                                     (cond ((and (agr-unif (list (get-synt-gender nxtline))
			                                   (list (get-synt-gender nxtprev)))
		                                 (agr-unif (list (get-synt-number nxtline))
			                                   (list (get-synt-number nxtprev))))
                                              (setq found
                                                  (make-link (get-synt-numb nxtprev)
                                                        'VERB-RMOD+RELCL+REDUC 'unlinked-verbs)))))
     ; *** if the next possible attachment point is a verb
                                  ((eq (get-synt-categ nxtprev) 'VERB)
                                     (cond ((and (has-gramm-type (get-synt-word nxtprev) '&predverb)
                                                 (neq (get-synt-type nxtprev) 'AUX))
                                             (setq found
                                                 (make-link (get-synt-numb nxtprev)
                                                         'VERB-PREDCOMPL 'unlinked-verbs)))
                                           (t (setq status 'fail))))
     ; *** if it is not an art, adj, or prep (simple pp-structure) stop the search
                                  ((not (memq (get-synt-categ nxtprev) '(ADJ ART PREP)))
                                     (setq status 'search-verb))))
                           ((eq status 'search-verb)
                              (cond ((eq (get-synt-categ nxtprev) 'VERB)
                                       (cond ((and (has-gramm-type (get-synt-word nxtprev) '&predverb)
                                                   (neq (get-synt-type nxtprev) 'AUX))
                                                (setq found
                                                    (make-link (get-synt-numb nxtprev)
                                                            'VERB-PREDCOMPL 'unlinked-verbs)))
                                             (t (setq status 'fail))))))))))))
 ; %%%%%%%%%%%%%%%%%%% FIRST MAIN STEP: attach unlinked verbs %%%%%%%%%%%%%%%
 ; %%%%%%%% depending on CONTEXT; context is set up by CONJ, PRON RELAT %%%%%
 ; %%%%%%%% or MODAL
  ;(format t " Attach-unlinked-verbs 2: allabs= ~a~%" allabs)
  ;(break "")
  (setq allabs
    (do* ((prlines nil (cons nxtline prlines))
	  (prlabs nil (cons nxtlab prlabs))
	  (nxtline (first allines) (first remlines))
	  (nxtlab (first allabs) (first remlabs))
	  (remlines (rest allines) (rest remlines))
	  (remlabs (rest allabs) (rest remlabs)))
	 ((null nxtline) (reverse prlabs))
; *** the function advances on the lines and labels. When a CONJ, PRON RELAT, or
;     MODAL are found, the 'context' variable is set. When a verb is encountered,
;     in case it is unlinked, it is attached to the contextual element. In any
;     case, a verb resets the context variable
; *** Of course, modals deserve special treatment, since they must both exploit
;     any previous context and set up a new one.
	 (let ((nxtcat (get-synt-categ nxtline))
	       (nxttype (get-synt-type nxtline)))
; *** if the next word is a greeting at the beginning of the sentence, set the 
;     context to "greeting-start"
	     (cond ((and (eq nxtcat 'PHRAS) (null prlines))
		     (setq context (list (make-link (get-synt-numb nxtline) 
                                             'GREETING-START 'unlinked-verbs))))
; *** if the next word is a subordinative conjunction, set the context to 'conj-arg'
  ; *** in this case the context is a four-tuple: the fourth item saves the pointer to the
  ;     parent of the conjunction; this is checked after to avoid loops
	           ((and (eq nxtcat 'CONJ) (eq nxttype 'SUBORD)
                         (neq (second nxtlab) 'CONTIN+LOCUT))
		     (setq context (cons (append1
                                             (make-link (get-synt-numb nxtline) 'CONJ-ARG 'unlinked-verbs)
                                             (first nxtlab))
                                         context)))
; *** if the next word is a relative pronoun, set the context to 'relcl'
		   ((and (eq nxtcat 'PRON) 
			 (eq nxttype 'RELAT) 
			 (not (is-a-synt-trace? nxtline)))
		     (setq context (cons (make-link (get-synt-numb nxtline)
                                               'VERB-RMOD+RELCL 'unlinked-verbs) context)))
		   ((eq nxtcat 'PHRAS)
		      (cond ((null nxtlab)
			      (setq still-unlinked 
				     (append1 still-unlinked nxtline)))))
; *** if the next word is an unattached verb, use the context to establish the
;     attachment
		   ((eq nxtcat 'VERB)
                                     ;  (format t "context = ~a; nxtlab = ~a~% nxtline = ~a~%"
                                     ;      context nxtlab nxtline)
				     ;  (format t " prlines = ~a; prlabs = ~a~%" prlines prlabs)
				     ;  (format t " remlines = ~a~% remlabs = ~a~%" remlines remlabs)
                                     ;  (break "")
		      (cond ((null nxtlab)
; *** if the context is nil, the verb remains unlinked
			      (cond ((null context)
				      (setq still-unlinked 
					     (append1 still-unlinked nxtline)))
; *** if the context is GREETING-START, the sentence began with "buonasera", and the first
;     verb we find after the comma is the second conjunct. Note that, at this stage,
;     the comma should have been already linked to the greeting
				    ((eq (second (first context)) 'GREETING-START)
                                       (setq nxtlab (make-link (first (first context)) 
                                                          'COORD2ND+BASE 'unlinked-verbs))
                                       (setq context nil))
; *** if the context concerns a relative clause, then the referent of the relpron
;     must be determined.
				    ((eq (second (first context)) 'VERB-RMOD+RELCL)
   ; ** first, go back to the relative pronoun
				      (let* ((pronline (find-a-line 
							`(position ,(first (first context)))
							 prlines prlabs 'all))
			      	             (remlines (rest (first pronline)))
			      	             (remlabs (rest (second pronline)))
   ; *** then search for a referent; nxtline is the verb line, and the first of the
   ;	 second of pronline is the link up of the relative pronoun. If the pron
   ;	 is the subject of the verb, then agreement with the referent is required
					     (ref (find-relcl-ref nxtline
							(first (first pronline))
							(first (second pronline))
							remlines remlabs allines allabs)))
					 (cond ((null ref)
					          (cond ((eq (first *PARSE-CONTEXT*) 'sentence)
						           (setq nxtlab (first context)))
				                        (t (setq still-unlinked 
					                        (append1 still-unlinked nxtline)))))
			      	      	       (t (setq nxtlab 
						     (make-link (get-synt-numb ref)
							   'VERB-RMOD+RELCL 'unlinked-verbs)))))
			      	      (setq context (rest context)))
; *** if the context concerns a modal, then it is ready to act as the link for the verb,
;     Moreover, any modifier of the modal is taken as a visitor, and a corresponding
;     trace is added to the depending verb
                                    ((eq (second (first context)) 'VERB+MODAL-INDCOMPL)
                                       (cond ((eq (get-synt-mood (find-first-aux nxtline prlines))
                                              'INFINITE)
                                                (setq nxtlab (first context))
    ; *** the first of context is the governing verb, while the current line is the
    ;     one of the governed verb
                                                (setq modal-dependence
                                                   (cons
                                                      (list (first (first context))
                                                            (get-synt-numb nxtline))
                                                      modal-dependence))
                                                (setq context (rest context)))))
; *** otherwise,  the context concerns a subordinate clause, and the context
;     act as the link for the verb; If the subodinating conjunction is not dependent on 
;     the verb, set the verb as its argument
                                    ((eq (second (first context)) 'CONJ-ARG)
                                       (cond ((not (equal (fourth (first context)) 
                                                          (get-synt-numb nxtline)))
                                                (setq nxtlab (butlast (first context)))
				                (setq context (rest context)))
                                             (t (setq still-unlinked 
					              (append1 still-unlinked nxtline)))))
				    (t (break "unknown context in Attach unlinked verbs")))))
	              (cond ((eq nxttype 'MOD)
		      	      (setq context (cons (make-link (get-synt-numb nxtline) 
						        'VERB+MODAL-INDCOMPL 'unlinked-verbs)
                                                  context)))))
     ; *** if we are in an initial "greetings" context, and we find a comma, we
     ;     assume the comma act as a conjunction, and link it to the greetings
     ;     the context remains GREETING-START, in order to use it to attach a 
     ;     possibly following verb as a second conjunct
		   ((and (eq nxtcat 'PUNCT)
                         (eq (get-synt-word nxtline) #\,)
                         (eq (second (first context)) 'GREETING-START))
                      (setq nxtlab (make-link (first (first context)) 'COORD+BASE 'unlinked-verbs))
		      (setq context (list (make-link (get-synt-numb nxtline) 
                                                  'GREETING-START 'unlinked-verbs))))))))
 ; *** if modal-dependence is non null, then it must be checked if any rmod
 ;     depends on the modal; in such a case, it is lowered to the depending verb
  ;(format t " Attach-unlinked-verbs 3: head: ~a~% still-unlinked: ~a~% allabs= ~a~%" 
  ;           head still-unlinked allabs)
  ;(break "")
 ; %%%%%%%%%%%%%END OF FIRST MAIN STEP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; *** if no unlinked verb remains, perhaps the head was of another category
  (cond ((null still-unlinked) (list allines allabs))
; *** if the head exists already, default attachment for all unlinked verbs
        ((not (null head))
          (cond ((and (eq 'CONJ (get-synt-categ head))
                      (not (has-2nd-conj head allabs)))
                  (setq allabs
                     (change-lab allines allabs 
	                (get-synt-numb (first still-unlinked))
                        (make-link (get-synt-numb head) 'COORD2ND 'unlinked-verbs)))
                  (cond ((not (null (rest still-unlinked)))
                          (list allines
                           (default-attach-verbs allines allabs (rest still-unlinked))))
                        (t (list allines allabs))))
                (t (list allines (default-attach-verbs allines allabs still-unlinked)))))
; *** otherwise, if just one unlinked verb, then it is the head,
	((= (length still-unlinked) 1)
          (list allines
              (change-lab allines allabs
	             (get-synt-numb (first still-unlinked))
	             (cond ((eq (first *PARSE-CONTEXT*) 'sentence)
		                (make-link 0 'TOP-VERB 'unlinked-verbs))
	                   (t (make-link 0 'APPOSITION 'unlinked-verbs))))))
  ; *** if more than one unlinked verb or phrasal element, first check if 
  ;     there is any phrasal element
        (t (let ((vcandidate (find-phrasal-el still-unlinked))
                 (top-lab 'TOP-VERB))
  ;  (format t "Attach-unlinked; ~a~%" vcandidate)
  ;  (break "AU-1")
               (cond ((null vcandidate)
  ; *** otherwise, check if any of them is a
  ;     declarative verb; in such a case, take it as the top (vcandidate)
                       (setq vcandidate 
                           (find-d-v still-unlinked)))
                     (t (setq top-lab 'TOP-PHRAS)))
  ;  (format t "Attach-unlinked; ~a~%" vcandidate)
  ;  (break "AU-2")
               (cond ((or (null vcandidate)
       ; The next two lines to avoid taking "Detto" as the head in "Detto questo vediamo ..."
                          (and (eq 1 (get-synt-linumb vcandidate))
                               (eq 'VERB (get-synt-categ vcandidate))
                               (eq 'PARTICIPLE (get-synt-mood vcandidate))))
  ; *** otherwise, if any of the unlinked verbs is finite, take the
  ;     first of them as top
                       (setq vcandidate 
                           (find-first-finite still-unlinked allines))
  ;  (format t "Attach-unlinked; ~a~%" vcandidate)
  ;  (break "AU-3")
  ; *** if no finite verb found, take the first infinite as the top
                       (cond ((null vcandidate)
                               (setq vcandidate (first still-unlinked))))))
  ;  (format t "Attach-unlinked; ~a~%" vcandidate)
  ;  (break "AU-4")
  ; *** adjust the label of the chosen top verb
               (setq allabs
	          (change-lab allines allabs 
                             (get-synt-numb vcandidate)
		             (cond ((eq (first *PARSE-CONTEXT*) 'sentence)
			             (make-link 0 top-lab 'unlinked-verbs))
		      	           (t (make-link 0 'APPOSITION 'unlinked-verbs)))))
  ; *** now, the top verb has been assessed: link the remaining verbs
               (list allines (default-attach-verbs allines allabs
                                (subtrl still-unlinked (list vcandidate)))))))))

; *********************************************************************
; *** This function checks that the tree has exacty one root
;     Returns a list of links
(defun check-single-root (lines links)
  (let ((savelinks links) (savelines lines) foundroots rootnumb)
    (do ((nxtline (first lines) (first lines))
         (lines (rest lines) (rest lines))
         (nxtlink (first links) (first links))
         (links (rest links) (rest links)))
        ((null nxtline)
           (cond ((eq (length foundroots) 1) savelinks)
                 ((eq (length foundroots) 0)
                    (multiple-value-setq (rootnumb savelinks)
                              (force-a-root savelines savelinks))
                    savelinks)
                 (t (break "More than one root after attach-unlinked-verbs"))))
        (cond ((eq (first nxtlink) 0)
                 (setq foundroots (cons nxtline foundroots)))))))

; *********************************************************************
; *** This selects as root a verb linked as a second conjunct of a coordination
;     or the first verb found in the sentence
;     Returns a pair: the line number of the found root and the list of new links
(defun force-a-root (lines links)
 (cond ((eq (length lines) 1)
           (values (get-synt-numb (first lines))
                   (put-root-arc lines links (get-synt-numb (first lines)))))
     (t (let ((savelinks links) (savelines lines) coord found rootnumb verbline possroot)
       (do ((nxtline (first lines) (first lines))
            (lines (rest lines) (rest lines))
            (nxtlink (first links) (first links))
            (links (rest links) (rest links)))
          ((or (null nxtline) found)
             (cond (found (values rootnumb found))
                   ((not (null verbline))
                      (values possroot (change-lab savelines savelinks possroot 
                                               (make-link 0 'TOP-VERB 'force-root))))
                   (t (exception-nothrow "No root found in force-a-root"))))
          (cond ((eq (get-synt-categ nxtline) 'VERB)
                   (cond ((eq (second nxtlink) 'COORD2ND)
                            (setq coord (find-a-line `(position ,(first nxtlink)) savelines savelinks))
                            (cond ((null coord) (exception-nothrow "In force-a-root 2"))
                                  (t (setq rootnumb (get-synt-numb nxtline))
                                     (setq found
                                        (mult-change-labs savelines savelinks 
                                           (list (list (get-synt-numb (first coord))	; line to change
                                                     (list (first (second coord)) 'SEPARATOR))   ; new link
                                                 (list rootnumb				; line to change
                                                     (list 0 'TOP-VERB)))       ; new link
                                           'force-root t)))))
                         ((null verbline)
                            (setq verbline nxtline)
                            (setq possroot (get-synt-numb verbline)))))))))))

; *********************************************************************
; *** modal-dependence is a list of pairs
;     <governing-modal-line-number governed-verb-line-number>
;     possibly empty
; *** the result is a pair <newallines newallabs>
(defun move-raised-adjuncts (modal-dependence allines allabs)
  (let (newlineslabs)
    (cond ((null modal-dependence)
             (list allines allabs))
          (t (setq newlineslabs
                  (sing-mov-rais-adjunct
                               (first modal-dependence) allines allabs))
             (move-raised-adjuncts (rest modal-dependence)
                         (first newlineslabs) (second newlineslabs))))))

; *********************************************************************
; *** for each modal dependence (i.e. a verb depending on a modal), look
;     if the governor has any rmod dependent, markit as visitor, and add
;     corresponding traces below the dependent verb
; *** modal-dep is a non-empty list of pairs
;     <governing-modal-line-number governed-verb-line-number>
; *** the result is a pair <newallines newallabs>
(defun sing-mov-rais-adjunct (modal-dep allines allabs)
  (declare (special *TREE-FORMAT*))
  (let ((governing-line (first modal-dep))
        (governed-line (second modal-dep))
        found-rmods done newtracelabs newtracelines temptrindex result
        actgovlinumb)
  ; *** first look for rmods depending on the governing modal
  ;     Puts the lines of the rmods in found-rmods and, as a side effect,
  ;     changes the link of those lines setting the label "visitor"
    (do* ((prlines nil (cons nxtline prlines))
          (prlabs nil (cons nxtlab prlabs))
          (nxtline (first allines) (first remlines))
          (nxtlab (first allabs) (first remlabs))
          (remlines (rest allines) (rest remlines))
          (remlabs (rest allabs) (rest remlabs)))
         ((null nxtline)
           (setq allabs (reverse prlabs)))
         (cond ((and (equal governing-line (first nxtlab))
                     (eq (second nxtlab) 'RMOD)
                     ; *** negative adverbs should not be taken as raised
                     (not (and (eq (get-synt-categ nxtline) 'ADV)
                               (eq (get-synt-type nxtline) 'NEG))))
  ; *** the label is changed from rmod to visitor, and the rmod line is
  ;     saved in found-rmods
                  (setq nxtlab (make-link governing-line 'VISITOR 'unlinked-v-raising))
                  (setq found-rmods (cons nxtline found-rmods)))))
  ; *** now, for all rmods found (if any), a trace is added after the governed
  ;     verb
   (cond ((not (null found-rmods))
   ; *** the next do looks for the insertion place: first looks for the
   ;     governed verb
           (do* ((prlines nil (cons nxtline prlines))
                 (prlabs nil (cons nxtlab prlabs))
                 (nxtline (first allines) (first remlines))
                 (nxtlinumb (get-synt-numb nxtline) (get-synt-numb nxtline))
                 (nxtlab (first allabs) (first remlabs))
                 (remlines (rest allines) (rest remlines))
                 (remlabs (rest allabs) (rest remlabs)))
                ((or (null nxtline) done)
   ; *** inside the body of the loop, when the governed verb is found, the
   ;     traces are inserted in the proper place; so that, at the end, what
   ;     is needed is just to return the result (new lines and new labs)
                   (cond (done result)
                         (t (exception 'parse-error
                                      "PROC/chunk-parser: Governed verb not found"))))
    ; *** here we have found the governed verb
    ;     The second conjunct is for not inserting the trace between the verb and a
    ;     possible enclitic
                (cond ((and (or (equal governed-line nxtlinumb)
                                (and (not (atom nxtlinumb))
                                          (equal governed-line (first nxtlinumb))))
                            (not (equal (get-synt-inpword nxtline)
                                        (get-synt-inpword (first remlines)))))
    ; *** before adding the traces, skip already existing traces
    ; *** the next cond in case the new trace depends on another trace or
    ;     on a word sub-component
                        (cond ((is-a-synt-trace? nxtline)
                                 (setq actgovlinumb (first governed-line))
                                 (setq temptrindex (1+ (second governed-line))))
                              ((atom governed-line)
                                 (setq actgovlinumb governed-line)
                                 (setq temptrindex 10))
                              (t (setq actgovlinumb (first governed-line))
                                 (setq temptrindex 10)))
                        (do* ((intprlines (cons nxtline prlines)
                                          (cons intnxtline intprlines))
                              (intprlabs (cons nxtlab prlabs)
                                         (cons intnxtlab intprlabs))
                              (intnxtline (first remlines) (first intremlines))
                              (intnxtlab (first remlabs) (first intremlabs))
                              (intremlines (rest remlines) (rest intremlines))
                              (intremlabs (rest remlabs) (rest intremlabs)))
                            ((not (is-a-synt-trace? intnxtline))
                  ; *** the advancement on the existing traces is completed:
                  ;     add all the new traces
                  ; *** the final result of the inner do is to add in front of
                  ;     remlines and remlabs the new traces
                               (do ((nxtrmod (first found-rmods) (first found-rmods))
                                    (found-rmods (rest found-rmods) (rest found-rmods))
                                    (traceindex temptrindex (1+ traceindex)))
                                  ((null nxtrmod)
                                    (setq intremlabs 
                                         (append newtracelabs 
                                                 (cond ((null intnxtline) nil)
                                                       (t (list intnxtlab)))
                                                 intremlabs))
                                     (setq intremlines 
                                         (append newtracelines 
                                                 (cond ((null intnxtline) nil)
                                                       (t (list intnxtline)))
                                                 intremlines)))
                                  (setq newtracelabs
                                     (append1 newtracelabs
                                          (make-link governed-line 'RMOD 'unlinked-v-raising)))
                                  (setq newtracelines
                                     (append1
                                       newtracelines
          ; *** insertion of a trace
                                       (cond ((eq *TREE-FORMAT* 'tut)
                                                `((,actgovlinumb ,traceindex)
                                                  #\t
                                                  ,(get-synt-syntinfo nxtrmod)
                                                  nil
                                                  (,(get-synt-numb nxtrmod) #\f)))
                                             (t `((posit (,actgovlinumb ,traceindex))
                                                  (form #\t)
                            ; *** the syntinfo
                                                  (syn ,(get-synt-syntinfo nxtrmod))
                            ; *** the trace reference; assumed to be a full trace
                                                  (coref ((line ,(get-synt-numb nxtrmod))
                                                                (ctype #\f)))))))))
                               (setq result
                                   (list
                                      (append (reverse intprlines) intremlines)
                                      (append (reverse intprlabs) intremlabs)))
                               (setq done t))
                            (setq temptrindex (1+ temptrindex))
                            (setq done t))))))
   ; *** no adjunct to move; return the original lines and labs
         (t (list allines allabs)))))

; *********************************************************************
; *** finds the first finite verb (checking the presence of auxiliaries)
;     among the ones in 'still-unlinked'
; *** assumes that the verbs in 'still-unlinked' are in sentence order
(defun find-first-finite (still-unlinked allines)
  (let (found)
  ; *** loop on all the lines of the sentence
   (do ((nxtline (first allines) (first allines))
        (prevlines nil (cons nxtline prevlines))
        (allines (rest allines) (rest allines)))
       ((or (null nxtline) (null still-unlinked) found)
          (cond ((and (null nxtline) (not (null still-unlinked)))
                  (exception 'parse-error "PROC/chunk-parser: find-first-finite"))
                (t found)))
  ; *** if the current line is the first element in still-unlinked
       (cond ((equal nxtline (first still-unlinked))
  ; *** get the first auxiliary (find-first-aux returns the same verb, if
  ;     it has no auxiliary)
               (let ((aux (find-first-aux nxtline prevlines)))
  ; *** check if it is in a finite mood
                   (cond ((memq (get-synt-mood aux)
                                '(IND CONG CONDIZ IMPER))
                            (setq found nxtline))
  ; *** if not, return advance on 'still-unlinked'
                         (t (setq still-unlinked (rest still-unlinked))))))))))

; *********************************************************************
; *** this checks if there is already a second conjunct for a given element
;     in a list of labels
(defun has-2nd-conj (head allabs)
 (let (found (headlinumb (get-synt-numb head)))
  (do ((nxtlab (first allabs) (first allabs))
       (allabs (rest allabs) (rest allabs)))
      ((or (null nxtlab) found) found)
      (setq found (and (equal (first nxtlab) headlinumb)
                       (eq (second nxtlab) 'COORD2ND))))))

; *********************************************************************
; *** checks if in a list of verbs or phrasal elements there is a
;     phrasal element
;     if not, it returns nil
;     if yes, it returns the phras
(defun find-phrasal-el (elemlist)
  (cond ((null elemlist) nil)
        ((eq (get-synt-categ (first elemlist)) 'PHRAS)
          (first elemlist))
        (t (find-phrasal-el (rest elemlist)))))

; *********************************************************************
; *** checks if in a list of verbs there is a declarative verb
;     if not, it returns nil
;     if yes, it returns the declverb
(defun find-d-v (verblist)
  (cond ((null verblist) nil)
        ((inh-member (get-synt-word (first verblist)) '£predic2)
          (first verblist))
        (t (find-d-v (rest verblist)))))

; *********************************************************************
; *** finds the attachment point for a verb which is not the root
(defun default-attach-verbs (lines links unlinked)
  (let (attach (allines lines) (allinks links) (link-lab 'RMOD))
  (do ((prevlines nil (cons curline prevlines))
       (prevlinks nil (cons curlink prevlinks))
       (curline (first lines) (first lines))
       (curlink (first links) (first links))
       (lines (rest lines) (rest lines))
       (links (rest links) (rest links)))
      ((or (null unlinked)
           (null curline))
        (cond ((null curline)
                 (exception 'parse-error "PROC/chunk-parser: default-attach-verbs"))
              (t (append (reverse prevlinks) (list curlink) links))))
      (cond ((equal curline (first unlinked))
  ; *** we are now positioned on an unlinked verb
               (setq unlinked (rest unlinked))
    ; *** look for a previous verb, as a possible attachment point; the last
    ;     parameter of find-a-line specifies that the found verb must not
    ;     already be a dependent of the current one
               (setq attach (first (find-a-line '(categ (phras))
                                                 prevlines prevlinks nil nil
                                                 curline allines allinks)))
               (cond ((null attach)
                       (setq attach 
                           (first (find-a-line '(categ (verb) type (main mod))
                                                 prevlines prevlinks nil nil
                                                 curline allines allinks))))
                     (t (setq link-lab 'APPOSITION)))
               (cond ((null attach)
    ; *** if not found, look for a following verb; again, with the non-looping
    ;     condition
                       (setq attach
                             (first (find-a-line '(categ (verb) type (main mod))
                                                 lines links nil nil
                                                 curline allines allinks)))))
               (cond ((null attach)
                       (setq attach (first prevlines))))
               (cond ((null attach)
                       (setq attach (first lines))))
               (setq curlink
                    (make-link (get-synt-numb attach) link-lab 'unlinked-verbs)))))))

; *********************************************************************
; *** In case no root already exists, it chooses as root of the tree the first
;     (if any) unlinked item that is not punctuation, chapter number, ...
(defun assign-root (allines allabs)
 (declare (special *PARSE-CONTEXT*))
   (let (found-unlinked foundroots prevline rootnumb temp-unlinked)
      ; *** repeat for all lines of a sentence, advancing in parallel on data and labels
	(do ((nxtline (first allines) (first nxtlocsent))
	     (nxtlocsent (rest allines) (rest nxtlocsent))
	     (nxtlab (first allabs) (first nxtlocsentlab))
	     (nxtlocsentlab (rest allabs) (rest nxtlocsentlab)))
            ((null nxtline)
               (cond ((null foundroots)
                        (cond ((null found-unlinked)
                                 (cond ((null temp-unlinked)
                                          (multiple-value-setq (rootnumb allabs)
                                                 (force-a-root allines allabs))
		                          (set-final-end rootnumb allines allabs))
                                       (t (setq rootnumb 
                                               (get-synt-numb (first temp-unlinked)))
                                          (put-root-arc allines allabs rootnumb))))
                              (t (setq rootnumb 
                                     (get-synt-numb (first found-unlinked)))
                                 (put-root-arc allines allabs rootnumb))))
                     ((eq (length foundroots) 1)
                        ; *** the root is already assigned uniquely
		        (set-final-end (get-synt-numb (first foundroots)) allines allabs))
                     (t (break "More than one root and no unlinked in assign-root")
                        allabs)))
                        ; *** otherwise, 
	   (cond ((equal 0 (first nxtlab))
                   (setq foundroots (append1 foundroots nxtline)))
		 ((and (null nxtlab)
		       (not (memq (get-synt-categ nxtline) 
                                    '(MARKER nil)))
          ; *** the next for not taking into account initial chapter numberings
          ;     the third condition, to avoid errors in case the sentence contains only 
          ;     a possible listpos (e.g. "2001 .")
                       (not (and (null prevline)
                                 (is-listpos nxtline (first nxtlocsent))
                                 (not (null (rest (rest nxtlocsent)))))))
              ; *** the next for not taking into account initial headings (MILANO - ...)
                     (cond ((and (neq (get-synt-categ nxtline) 'PUNCT)
                                 (not (and (null prevline)
                                           (eq 'NOUN (get-synt-categ nxtline))
                                           (eq 'PROPER (get-synt-type nxtline))
                                           (or (eq #\- (get-synt-word (first nxtlocsent)))
                                               (eq #\. (get-synt-word (first nxtlocsent))))
                           ; *** unless the sequence "MILANO -" is the full sentence
                                      (not (null (rest nxtlocsent))))))
                              (setq found-unlinked (append1 found-unlinked nxtline)))
                           ((and (null temp-unlinked) (null found-unlinked))
                 ; *** the item is saved anyway in temp-unlinked, so that if no other
                 ;     possibility exists it is taken as the root
                              (setq temp-unlinked (append1 temp-unlinked nxtline))))))
           (setq prevline nxtline))))

; *********************************************************************
(defun assign-root-old (allines allabs)
; *** This checks if there is just one line (except for punctuation) which has no
;     label. In such a case, it is taken as the head of the sentence
 (declare (special *PARSE-CONTEXT*))
 (let (uniqueline fail exists (prevline nil))
; *** repeat for all lines of a sentence, advancing in parallel on data and labels
	(do ((nxtline (first allines) (first nxtlocsent))
	     (nxtlocsent (rest allines) (rest nxtlocsent))
	     (nxtlab (first allabs) (first nxtlocsentlab))
	     (nxtlocsentlab (rest allabs) (rest nxtlocsentlab)))
; *** at end of sentence or in case more than one unlabelled line found, exit the
;     loop. In case of failure, the original label list is left unchanged,
;     otherwise, the found line gets the root label; moreover, inside put-root-arc,
;     the final punctuation mark, if any, gets the link to the root,with label 'end'
	   ((or (null nxtline) fail exists)
             (format t "assign root; fail: ~a; exists: ~a; uniqueline: ~a~%" 
                  fail exists uniqueline)
             (break "")
	     (cond ((null uniqueline) allabs)		; *** no unattached words
	           (fail (put-root-arc allines allabs uniqueline))
                         ; (cond ((eq (first *PARSE-CONTEXT*) 'sentence)
			;	  (put-root-arc allines allabs uniqueline))
			;       (t allabs))
                          		; *** more than one unattached words
		   (exists (set-final-end uniqueline allines allabs))
                                                        ; *** root already exists
		   (t (put-root-arc allines allabs uniqueline))))
                                                        ; *** found the unique possible root
; *** if a root already exists, then the search is interrupted (exists=t)
	   (cond ((equal 0 (first nxtlab))
		   (setq uniqueline (get-synt-numb nxtline))
		   (setq exists t))
; *** when an unlabelled non-punctuation line is found, if it is the first one,
;     its index is saved in uniqueline; if there was another before, then it is
;     not unique, and fail is set to true
		 ((and (null nxtlab)
		       (not (memq (get-synt-categ nxtline) 
                                    '(PUNCT MARKER nil)))
    ; *** the next for not taking into account initial chapter numberings
                       (not (and (null prevline)
                                 (is-listpos nxtline (first nxtlocsent))))
    ; *** the next for not taking into account initial headings (MILANO - ...)
                       (not (and (null prevline)
                                 (eq 'NOUN (get-synt-categ nxtline))
                                 (eq 'PROPER (get-synt-type nxtline))
                                 (eq #\- (get-synt-word (first nxtlocsent)))
                      ; *** unless the sequence "MILANO -" is the full sentence
                                 (not (null (rest nxtlocsent))))))
    ; *** the category is nil for unknown words
		   (cond ((null uniqueline)
           ; *** in this case uniqueline is set, but exists is false, since it is not the root
			   (setq uniqueline (get-synt-numb nxtline)))
			 (t (setq fail t)))))
           (setq prevline nxtline))))

; *********************************************************************
; *** it inserts in 'labels' the root label corresponding to the unique unlabelled
;     line (whose index is in 'index'). It advances on sent and labels in parallel.
;     Just the labels are returned.
(defun put-root-arc (allines allabs index)
 (declare (special *PARSE-CONTEXT*))
   (cond ((null allines) 
            (exception 'parse-error "PROC/chunk-parser: put-root-arc")))
   (cond ((equal (get-synt-numb (first allines)) index)
; *** the first element is the new label: (0 top-lab)
	    (cons (make-link 0 
			(cond ((eq (first *PARSE-CONTEXT*) 'sentence)
		     		(case (get-synt-categ (first allines))
		   		    (adj 'TOP-ADJ)
		   		    (adv 'TOP-ADV)
		   		    (art 'TOP-ART)
		   		    (conj 'TOP-CONJ)
		   		    (noun 'TOP-NOUN)
		   		    (num 'TOP-NUM)
		   		    (phras 'TOP-PHRAS)
		   		    (prep 'TOP-PREP)
		   		    (pron 'TOP-PRON)
		   		    (punct 
                                       (cond ((eq (get-synt-word (first allines)) #\.) 'END)
                                             (t 'SEPARATOR)))
		   		    (verb 'TOP-VERB)))
			      ((eq (first *PARSE-CONTEXT*) 'parenth)
				    'APPOSITION))
                        'root-arc)
; *** the rest are the remaining labels, possibly updated in their last element,
;     in case it is a punctuation mark
		 (set-final-end index (rest allines) (rest allabs))))
; *** recursion on label list
	(t (cons (first allabs) 
		 (put-root-arc (rest allines) (rest allabs) index)))))

; *********************************************************************
(defun set-final-end (rootnumb lines labs)
  (declare (special *LANGUAGE*))
  (cond ((null lines) nil)
        (t (let (reslines)
             (cond ((and (eq 'PUNCT (get-synt-categ (ult lines)))
                         (not (member (get-synt-word (ult lines)) '(#\" #\')))) ; "
	             (setq reslines (append1 (butlast labs) (list rootnumb 'END 'final-end))))
	           (t (setq reslines labs)))
             (cond ((and (eq *LANGUAGE* 'spanish)
                         (equal (get-synt-word (first lines)) #\¿))  ;   #\¿  UTF-8 !!!!
	             (cons (make-link rootnumb 'BEGIN 'final-end) (rest reslines)))
                   (t reslines))))))
 
; *********************************************************************
; *** finds the referent of a relative clause
; *** INPUT:
;  >>> curline: the line of the verb of the relative clause
;  >>> pronline: the line of the relative pronoun
;  >>> pronlink: the link up of the pronoun
;  >>> prlines: the lines before the relative pronoun (in reverse order)
;  >>> prlinks: the links of those lines
; *** OUTPUT:
;  >>> the line of the found referent 
(defun find-relcl-ref (curline pronline pronlink prlines prlinks allines allinks)
  (let (checkagree
        (tempresult
  	  (cond ((and (has-gramm-type (get-synt-word pronline) '&double-who)
  	              (not (has-gramm-type (get-synt-word pronline) '&base-relat)))
   ; *** this is a doubly linked relpron, so it is the referent (governor of the
   ;     verb). The second clause express preference for not taking a pronoun as
   ;     a double relpron, if it can also act as a simple relative pronoun (ex.
   ;     english "who")
	 	   (list pronline pronlink))
  	        ((has-gramm-type (get-synt-word pronline) '&art-relat)
	   	  (cond ((eq (get-synt-categ (first prlines)) 'art)
			   (find-a-line '(categ (noun pron) not-trace) 
					  (rest (rest prlines))
					  (rest (rest prlinks)))
;		   	   (cond ((memq (get-synt-categ (second prlines))
;				        '(NOUN PRON))
   ; *** ... i 'ragazzi' i $ quali $ ...
;			    	    (second prlines))
;			 	 ((eq (get-synt-categ (second prlines)) 'PREP)
;		   	   	    (cond ((memq (get-synt-categ (third prlines))
;						 '(NOUN PRON))
   ; *** ... i 'ragazzi' con i $ quali $ ...
;			    	    	     (third prlines))
   ; *** ... ??? con i $ quali $ ...
;			 	 	  (t nil)))
   ; *** ... ??? i $ quali $ ...
;			 	   (t nil))
					    )
   ; *** ... ??? $ quali $ ... 
		 	   (t nil)))
        	((has-gramm-type (get-synt-word pronline) '&mid-relat)
		   (find-a-line '(categ (noun pron) not-trace) prlines prlinks)
;	   	   (cond ((eq (get-synt-categ (first prlines)) 'PREP)
;		   	   (cond ((memq (get-synt-categ (second prlines))
;					'(NOUN PRON))
   ; *** ... i 'ragazzi' con $ cui $ ...
;			    	    (second prlines))
   ; *** ... ??? con $ cui $ ...
;		 	 	 (t nil)))
;		 	 (t (cond ((memq (get-synt-categ (first prlines))
;					 '(NOUN PRON))
   ; *** ... i 'ragazzi' $ cui $ ...
;			    	     (first prlines))
   ; *** ... ??? $ cui $ ...
;		 	 	  (t nil))))
						)
   ; *** ... i 'ragazzi' $ che $ ...
		(t (find-a-line '(categ (noun pron)) prlines prlinks)))))
; *** if nothing found, simply find the closest previous noun or pronoun line
; *** if the pronoun is the subject there should be agreement
   (cond ((null tempresult)
	    (cond ((eq 'subj (second pronlink))
                     (setq checkagree t)
	             (setq tempresult 
                        (find-a-line '(categ (noun pron) not-trace agree) prlines prlinks)))
                  (t (setq tempresult
	                (find-a-line '(categ (noun pron) not-trace) prlines prlinks))))))
; *** tempresult is a pair <line, label>
   (cond ((null tempresult) nil)
	 (t (poss-move-rel-attach curline tempresult prlines prlinks allines allinks)))))

; *******************************************************************
; *** this tries too see if the proposed attachment for the verb should
;     be modified in order to keep into account "strong prepositional links"
;     as "acquisto in deroga di cui si parla", where "acquisto in deroga" is
;     a quasi-multiword
(defun poss-move-rel-attach (verbline startdata prev prevlabs allines allabs)
   (let ((startline (first startdata))
         (startlink (second startdata)))
       (setq prevlabs
             (find-new-up-attach
                    verbline startline nil startlink prev prevlabs allines allabs 'verb))))

; *********************************************************************
; *** tries to handle doubly linked relative pronouns (such as 'chi')
;     returns two values: the line of the verb which should be followed by
;     a trace to the relpron line, and the line of the actual governing verb
; *** prev, prevlabs, nxtline, nxtlab, nxtlocsent, nxtlocsentlab are special
(defun attach-double-relpron (allines allinks)
 (declare (special nxtlocsent nxtlocsentlab))
 (let* ((status 'search)
	cluster-head+rem attachp foundline preclines)
; *** the do aims at determining the verb of the relative clause governed by
;     the doubly linked relpron
   (do ((curline (first nxtlocsent) (first remsent))
	(remsent (rest nxtlocsent) (rest remsent))
	(curlink (first nxtlocsentlab) (first remlink))
	(remlink (rest nxtlocsentlab) (rest remlink)))
       ((or (eq status 'found)
	    (and (null curline) (null remsent))))
       (cond ((eq status 'search)
		(cond ((or (test-barriers (cons curline remsent) 
					  (cons curlink remlink)
			 		  (first cluster-head+rem))
			   (and (eq (get-synt-categ curline) 'CONJ)
	      			(eq (get-synt-type curline) 'SUBORD)))
			 (setq status 'skip))
		      ((eq (get-synt-categ curline) 'VERB)
			 (cond ((eq (get-synt-type curline) 'AUX)
				  (setq status 'findmain))
			       (t (setq cluster-head+rem
					 (list (get-synt-numb curline)
					        remsent remlink))
                                  (setq foundline curline)
				  (setq status 'found))))))
	     ((eq status 'skip)
		(cond ((eq (get-synt-categ curline) 'VERB)
			 (cond ((neq (get-synt-type curline) 'AUX)
				  (setq status 'search))
			       (t (setq status 'skipmain))))))
	     ((eq status 'findmain)
		(cond ((and (eq (get-synt-categ curline) 'VERB)
			    (neq (get-synt-type curline) 'AUX))
			 (setq cluster-head+rem
				 (list (get-synt-numb curline) remsent remlink))
                         (setq foundline curline)
			 (setq status 'found))))
	     ((eq status 'skipmain)
		(cond ((and (eq (get-synt-categ curline) 'VERB)
			    (neq (get-synt-type curline) 'AUX))
			 (setq status 'search))))))
      (setq attachp (find-verb-after-cluster cluster-head+rem allines allinks))
      (cond ((null attachp)
              (setq preclines (find-prec-lines foundline allines))
              (setq attachp (skip-punctuation preclines))))
      (cond ((null attachp)
               (list 0 'VERB-SUBJ 'double-relpron))
            (t (list (get-synt-numb attachp) 'VERB-SUBJ 'double-relpron)))))

; *********************************************************************
(defun skip-punctuation (lines)
  (cond ((null lines) nil)
        ((eq (get-synt-categ (first lines)) 'PUNCT)
          (skip-punctuation (rest lines)))
        (t (first lines))))

; *********************************************************************
; *** used to find the actual governing verb for a doubly linked relative
;     pronoun (such as 'chi')
; *** it assumes that all words following the pronoun belong to a cluster, and
;     that the governing verb is the first main finite verb following that
;     cluster.
;     Ex: Chi (, tra quelli che sono qui, ha mangiato la mela che mi hai
;	  portato) deve dirlo
; *** The function is entered after the previous function has already found the
;     main verb of the chunk ('mangiato' in the example above).
; *** INPUT:
;  --> cluster-head+rem is a list of three elements:
;	first: the line number of the cluster head ('mangiato')
;	second: all lines following the chunk head ('la mela che ...')
;	third: their links
(defun find-verb-after-cluster (cluster-head+rem allines allinks)
  (let ((cluster-headlnum (first cluster-head+rem))
	(remlines (second cluster-head+rem))
	(remlinks (third cluster-head+rem))
	(context 'inchunk)
	seenchunkelem result)
      (do ((line (first remlines) (first remlines))
	   (remlines (rest remlines) (rest remlines))
	   (link (first remlinks) (first remlinks))
	   (remlinks (rest remlinks) (rest remlinks)))
	 ((or result (null line)) result)
	 (cond ((eq context 'afterchunk)
		 (cond ((and (eq (get-synt-categ line) 'VERB)
		       	     (memq (get-synt-type line) '(MAIN MOD)))
		  	  (setq result line))))
	       ((eq context 'inchunk)
		  (setq seenchunkelem
		 	(belong-to-chunk line link cluster-headlnum 
					 seenchunkelem allines allinks nil))
		 (cond ((eq seenchunkelem 'fail)
		 	  (cond ((and (eq (get-synt-categ line) 'VERB)
		       	     	      (memq (get-synt-type line) '(MAIN MOD)))
		  	  	   (setq result line)))
			  (setq context 'afterchunk))))))))

; *********************************************************************
; *** this should check if a line depends on another line
;     since this is a very expensive operation, I try to speed it up by
;     saving in seendep all already encountered nodes of the chunk. 
; *** this is used for determining the existence of loops
(defun belong-to-chunk (depline deplink cluster-head-pos seendep allines allinks loops)
  (let ((depnumb (get-synt-numb depline)))
   (cond ((equal depnumb cluster-head-pos) 
	    (list cluster-head-pos))
	 ((or (null deplink)
	      (equal (first deplink) 0))
	    'fail)
	 ((member depnumb seendep :test #'equal) seendep)
	 ((member depnumb loops :test #'equal) seendep)
	 (t (let* ((gov (find-a-line (list 'position (first deplink))
				      allines allinks))
		   (parents (belong-to-chunk (first gov) (second gov)
					cluster-head-pos seendep allines allinks
					(cons depnumb loops))))
		(cond ((eq parents 'fail) 'fail)
		      (t (cons depnumb parents))))))))
	  
; ***************************************************************
(defun force-tree-connection (data links)
 (let (final-data final-links newsent newlinks)
; *** repeat for all sentences, advancing in parallel on data and labels
    (do ((nxtsent (first data) (first locdata))
	 (locdata (rest data) (rest locdata))
	 (nxtsentlab (first links) (first loclinks))
	 (loclinks (rest links) (rest loclinks)))
        ((and (null nxtsent) (null locdata))
           (list (reverse final-data) (reverse final-links)))
       (setq newlinks (singsent-force-tree-conn nxtsent nxtsentlab 0))
       (setq final-data (cons nxtsent final-data))
       (setq final-links (cons newlinks final-links)))))

; ***************************************************************
; *** this checks and forces the connection for a single sentence
; *** "level" specifies if this is the first (standard) check on a
;     sentence or if this is a second (internal) call, to verify if
;     the changes have been successful. In the first case, level=0,
;     otherwise level=1
(defun singsent-force-tree-conn (lines links level)
   ;(format t "links: ~a~%" links)
   ;(break "")
   (let ((connected (makeli (length lines) nil))
         (savelines lines) (savelinks links) nxtlink nxtconn 
         (inside-marker 0) changed line-changed)
   ; *** makeli builds a new list of equal elements (nil in this case)
     (do ((nxtline (first lines) (first lines))
          (lines (rest lines) (rest lines))
          (nxtlink (first links) (first links))
          (links (rest links) (rest links)))
         ((null nxtline) 
             (cond ((not changed) savelinks)
                   ((< level 2) 
                      ; (break "second check")
                       (singsent-force-tree-conn savelines savelinks (1+ level)))
                   (t (exception-nothrow "Chunk parser: Failed attempt to solve a loop"))))
   ; *** the next is needed, since the info about connection
   ;     are changed inside the loop. So, nxtconn is not
   ;     the one present in the original "connected"
     ;    (cond ((null nxtlink)
     ;             (format t "Force tree conn; nxtline= ~a~%" nxtline)
     ;             (break "")))
         (cond ((eq (get-synt-categ nxtline) 'MARKER)
                  (cond ((eq (get-synt-word nxtline) #\<)
                           (setq inside-marker (1+ inside-marker)))
                        ((eq (get-synt-word nxtline) #\>)
                           (setq inside-marker (1- inside-marker))))))
         (cond ((and (null nxtlink)
                     (or (> inside-marker 0)
                         (and (eq (get-synt-categ nxtline) 'MARKER)
                              (eq (get-synt-word nxtline) #\>))))
                  nil)
               (t (setq nxtconn (search-item-by-key nxtline savelines connected))
                  (cond ((null nxtconn)
                           (setq nxtlink (search-item-by-key nxtline savelines savelinks))
      ; *** see the comment above for the assignment to nxtlink
      ; *** check-line-connected returns a new version of links and connected:
      ;       links is unchanged, unless a loop was found: it has been removed
      ;       connected is changed according a new set of lines that has been checked
      ;          this does not concern only nxtline, but the whole path connecting
      ;          nxtline to the root
      ;   (format t "Nxtline: ~a~% nxtlink: ~a~% nxtconn: ~a~% connected: ~a~%" 
      ;                  nxtline nxtlink nxtconn connected)
      ;   (break "singsent-force")
                           (multiple-value-setq (savelinks connected line-changed)
                               (check-line-connected 
                                    nxtlink savelines savelinks connected 
                                    (list (get-synt-numb nxtline))))
       ; *** If, for any line, the connections have been modified, set as true the flag
       ;     for an overall change
                           (cond (line-changed (setq changed t))))))))))
         
; *********************************************************************
; *** looks for the item in targetlist whose position is the same as the
;     one of searchitem in searchlist
(defun search-item-by-key (searchitem searchlist targetlist)
   (cond ((null searchlist)
            (break "force-connection: search-item-by-key"))
         ((equal searchitem (first searchlist))
            (first targetlist))
         (t (search-item-by-key searchitem (rest searchlist) (rest targetlist)))))
   
; *********************************************************************
; *** this checks if a line is connected to the root of the tree
;     It is connected in case:
;       1. its parent was already found to be connected (parconn = t)
;       2. its parent is the root of the tree
;     It is not connected in case its parent belongs to the search path
;       In this case, there is a loop, which is the only way to have
;       disconnected lines
;     Otherwise, the search must continue
;     If a loop is found, then the problem is solved and the new values
;      of links and connected represent a linked structure
; >>> INPUT:
;   nxtline: the line that has to be checked (if it is connected to the root)
;   nxtlink: its link to the parent
;   alllines: all lines in the sentence
;   alllinks: all links in the sentence
;   connected: a list of boolean values parallel to lines and links, specifying if a
;              given line was already found to be connected
;   path: a list of line numbers specifying the path that is currently being inspected
;         Until nxtlink has a 0 pointer, or a pointer to a line that is known to be
;         connected, all lines whose numbers are in path have an "unknown" connection
;         status. Path includes the line number of nxtline
; >>> OUTPUT:
;   three values:
;   links: the set of (possibly rearranged) set of links
;   connected: the information about what lines are connected. This has been updated
;          with the new lines that have been checked (possibly also some of them that
;          were previously unconnected, but that now have connected rearranging the links)
;   change: true or false, accroding to the fact that thye inspection of the line has
;          produced a modification of the tree
(defun check-line-connected (nxtlink alllines alllinks connected path)
  (let (parline parlink parconn top-loop-line newlinks)
    ;(format t "check-line-connected: nxtlink = ~a~% connected = ~a~% path = ~a~%"
    ;      nxtlink connected path)
    ;(break "")
    (cond ((eq (first nxtlink) 0)
             (values alllinks (set-connected path alllines connected) nil))
   ; *** It is ok: we have reached the root: the data in "connected" are updated via 
   ;     the ones in "path"
          (t (multiple-value-setq (parline parlink parconn)
                 (find-line-with-conn (first nxtlink) alllines alllinks connected))
      ; *** otherwise pick the parent
             (cond (parconn (values alllinks 
                                (set-connected path alllines connected) nil))
         ; *** the parent line is connected: store the info in "connected" to avoid repeating the search
                   ((member (first nxtlink) path)
         ; *** if the parent is not connected but already is in "path": loop! Solve the problem
                      (setq top-loop-line (get-top-loop path alllines alllinks))
                      (setq newlinks (set-path-connection top-loop-line alllines alllinks path))
           ; *** returns the triple, signalling the change made.
           ;     set-connected is called on the original path, since its composition has not
           ;     changed
                      (values newlinks (set-connected path alllines connected) t))
         ; *** otherwise go ahead, and repeat the check on the parent
                   (t (check-line-connected parlink alllines alllinks connected 
                                         (cons (first nxtlink) path))))))))

; *********************************************************************
; *** this finds the line having number linumb
(defun find-line-with-conn (linenumb lines links connected)
  (cond ((null lines)
           (break "chunk-parser: get-connected-line"))
        ((equal linenumb (get-synt-numb (first lines)))
           (values (first lines) (first links) (first connected)))
        (t (find-line-with-conn linenumb (rest lines) (rest links) (rest connected)))))

; *********************************************************************
; *** replaces with t all values in connected corresponding to the lines in "path"
;     returns an updated version of the array "connected"
(defun set-connected (path lines connected)
  (let (found connresult (finalres connected))
  ; *** carry out the substitution for all line numbers in path
    (do ((nxtpath (first path) (first path))
         (path (rest path) (rest path)))
        ((null nxtpath) finalres)
    ; ** for each item in path, look for the corresponding line
        (setq found nil)
        (setq connresult nil)
        (setq finalres
           (do ((nxtline (first lines) (first tmplines))
                (tmplines (rest lines) (rest tmplines))
                (nxtconn (first finalres) (first finalres))
                (finalres (rest finalres) (rest finalres)))
               ((or (null nxtline) found)
       ; *** the line whose number is in path (nxtpath) must exist among the lines
                  (cond ((not found)
                           (break "chunk-parser: set-connected"))
          ; *** when it is found, replace "connected" with the new version
                        ((null nxtline) (reverse connresult))
                        (t (append (reverse (cons nxtconn connresult)) finalres))))
               (cond ((equal (get-synt-numb nxtline) nxtpath)
                        (setq found t)
                        (setq connresult (cons t connresult)))
                     (t (setq connresult (cons nxtconn connresult))))))
     ;  (format t "set-connected: nxtpath ~a~%; finalres ~a~%" nxtpath finalres)
     ;  (break "")
        )))

; *********************************************************************
; *** this chooses, among the lines in "path" the one which is the most probable
;     "wrong attachment" according to the source of the attachment
;     Returns a pair of line and link of the worst attachment
(defun get-top-loop (path lines links)
   (let (worst-attach foundline foundlink)
    ; *** retrieve all lines and links corresponding to the line numbers in path
     (do ((nxtpath (first path) (first path))
          (path (rest path) (rest path)))
         ((null nxtpath) worst-attach)
         (setq foundline nil)
         (setq foundlink nil)
         (do ((nxtline (first lines) (first tmplines))
              (tmplines (rest lines) (rest tmplines))
              (nxtlink (first links) (first tmplinks))
              (tmplinks (rest links) (rest tmplinks)))
             ((or (null nxtline) foundline)
               (cond (foundline
                        (cond ((or (null worst-attach)
                                   (> (get-link-uncertainty foundlink)
                                      (get-link-uncertainty (second worst-attach))))
                                 (setq worst-attach (list foundline foundlink)))))
                     (t (break "chunk parser: get-top-loop"))))
             (cond ((equal nxtpath (get-synt-numb nxtline))
                      (setq foundline nxtline)
                      (setq foundlink nxtlink)))))))

; *********************************************************************
(defun get-link-uncertainty (link)
; *** this assign a numerical level of uncertainty to an attachment on the basis
;     of the source of the attacment. The lower the value the more certain is the attachment
; *** the existing link markers are:
;     - add-parentheses
;     - unattached-aux
;     - citation (in parse-citation)
;     - locution (in set-locution-links)
;     - chunkrules (in various functions associated with the chunking rules)
;     - preposition-1 (in check-missing-link, that try build larger prepositional chunks)
;     - conjunctions (in link-conjunctions and associated functions)
;     - conj-in-verbal (in apply-verbal-rules; looking for sequences of conjuncts while
;                       analysing verbal dependents)
;     - verbal (obtained in the application of verbal subcategorization frames)
;     - appositions (in find-appositions)
;     - traces (in ins-tut-trace and ins-avm-trace)
;     - control (for "visitor" labels added for verbal control)
;     - prep-move (while changing preposition attachment after the analysis of verbal dependents)
;     - conj-in-attach-unl (in attach-unlinked, for sequence of conjunctions replecing unattached items)
;     - attach-unlinked (in attach-unlinked and related functions)
;     - unlinked-verbs (in attach-unlinked-verbs; inside "attach-unlinked")
;     - unlinked-v-raising (for raising adjuncts of modals - inside attachment of unlinked verbs)
;     - move-adj (in move-adj-up)
;     - listpos (in check-listpos; for paragraph numbers)
;     - attach-punct (in attach-punct-and-others: final defaults for unattached items)
;     - final-punct (in solve-rem-punct-and-others: used when the final defaults did not work)
;     - root-arc (in put-root-arc)
;     - final-end (after determining the root, sets the link to it from the final punctuation mark)
  (case (third link)
      (add-parentheses 4)
      (citation 1) 
      (locution 1)
      (chunkrules 2)
      (unattached-aux 2)
      (preposition-1 3)
      (conjunctions 7)
      (conj-in-verbal 7)
      (verbal 3)
      (double-relpron 3)
      (appositions 8)
      (traces 1)
      (control 3)
      (prep-move 3)
      (conj-in-attach-unl 7)
      (attach-unlinked 5)
      (unlinked-verbs 5)
      (unlinked-v-raising 5)
      (move-adj 3)
      (listpos 1)
      (attach-punct 6)
      (final-punct 6)
      (root-arc 6)
      (final-end 6)
      (break-loop 7)
      (force-root 7)
      (otherwise (break "unknown source of link"))))

; *********************************************************************
; *** This is the function that should actually solve the problem, by
;     changing one of the links in the loop
; >>> INPUT:
;     - the pair <line link> of the line whose link should be changed to
;       break the loop
;     - the lines and links of the sentence
;     - the path (sequence of line numbers) that forms a loop
; >>> OUTPUT:
;     - the modified links
(defun set-path-connection (top-loop-line-link lines links path)
  (let ((line (first top-loop-line-link)) (link (second top-loop-line-link))
        attach-point linumb pathboundaries root newlab relcl-ref line-to-change noun-gov)
   (setq pathboundaries 
       (cond ((eq (length path) 1)
                (list (first path) (first path)))
             (t (get-max-min-line path))))
  ; *** pathboundaries is pair: the first item is the first line of the path
  ;     (in the linear order); the second item is the last line of the path
  ;     The idea is that one of the pointer that must be changed must go outside
  ;     those boundaries
   (setq linumb (get-synt-numb line))
   (cond ((eq (second link) 'VERB-RMOD+RELCL)
  ; *** the link that must be modified concerns the verb of a relative clause
            (let (prevlines parlink)
                (multiple-value-setq (parlink prevlines) 
                           (inside-parentheses line lines links))
                (cond ((null parlink)
            ; *** the line is not inside a parenthesis *********
                         (cond ((eq 1 (first pathboundaries))
                ; *** if the loop includes the first line, attach the referent of the rel clause to
                ;     the main verb
                                  (setq newlab 'RMOD)
                                  (setq relcl-ref (find-a-line `(position ,(first link)) lines links nil))
                      ; *** relcl-ref is pair line-link
                                  (setq noun-gov (find-a-line `(position ,(first (second relcl-ref))) 
                                             lines links nil))
                                  (cond ((memq (get-synt-categ (first noun-gov)) '(ADJ ART NUM))
                                           (setq line-to-change (get-synt-numb (first noun-gov))))
                                        (t (setq line-to-change (get-synt-numb (first relcl-ref)))))
                                  (setq attach-point (get-synt-numb (get-tree-single-root lines links))))
                               (t (setq newlab 'VERB-RMOD+RELCL)
                                  (setq line-to-change linumb)
                                  (setq attach-point
                                     (get-loop-relcl-attach lines (first pathboundaries))))))
            ; *** the line is inside a parenthesis *********
                      (t (setq attach-point (get-loop-relcl-attach prevlines (first pathboundaries)))
                         (setq line-to-change linumb)
                         (cond ((null attach-point)
                                  (setq attach-point (first parlink))))))
             ;   (format t "Loop found: ~a~% worst attachment: top-loop: ~a~% new-attach-point: ~a~%"
             ;          path top-loop-line-link attach-point)
             ;   (break "set-path-connection-1")
                (change-lab lines links line-to-change (make-link attach-point newlab 'break-loop))))
        (t (setq root (get-tree-single-root lines links))
          ; *** otherwise, link the line to the root of the tree, if there is a unique root
           (cond ((null root)
             ;       (format t "Loop found: ~a~% worst attachment: top-loop: ~a~%" path top-loop-line-link)
             ;       (break "set-path-connection-2")
                    links)
                 ((and (eq (get-synt-categ root) 'VERB)
                       (eq (get-synt-categ line) 'CONJ)
                       (eq (get-synt-type line) 'SUBORD))
                    (change-lab lines links linumb 
                           (make-link (get-synt-numb root) 'VERB+FIN-RMOD 'break-loop)))
                 (t ;(format t "Loop found: ~a~% worst attachment: top-loop: ~a~%" path top-loop-line-link)
                    ;(break "set-path-connection-3")
                    (change-lab lines links linumb (make-link (get-synt-numb root) 'UNKNOWN 'break-loop))
   ))))))

; *********************************************************************
; *** the function returns the (unique) root of the tree
(defun get-tree-single-root (lines links)
   (let ((num-root 0) foundroot)
      (do ((nxtline (first lines) (first lines))
           (lines (rest lines) (rest lines))
           (nxtlink (first links) (first links))
           (links (rest links) (rest links)))
          ((null nxtline) foundroot)
          (cond ((eq (first nxtlink) 0)
                   (setq num-root (1+ num-root))
                   (cond ((= num-root 1) (setq foundroot nxtline))
              ; *** if it is the first root, store it in foundroot, otherwise reset foundroot
                         (t (setq foundroot nil))))))))

; *********************************************************************
; *** the functions looks for a line (before boundary) of category NOUN. If it finds
;     it it returns it, otherwise it returns the last line before boundary
;     N.B. lines is in reverse order, so (first lines) is the closest to the stuff to
;          attach
(defun get-loop-relcl-attach (lines boundary)
   (let (found stop (firstline (first lines)))
     (do ((nxtline (first lines) (first lines))
          (lines (rest lines) (rest lines)))
         ((or (null nxtline) stop found)
            (cond (found found)
                  (t (get-synt-numb firstline))))
        (cond ((equal (get-synt-numb nxtline) boundary)
           ; *** we have reached the left boundary of the loop
                 (setq stop t))
              ((eq (get-synt-categ nxtline) 'NOUN)
           ; *** we have found a possible attachment
                 (setq found (get-synt-numb nxtline)))))))
          
; *********************************************************************
; *** given a list of line numbers, looks for the first and the last one
(defun get-max-min-line (path)
  (let (min max)
      (cond ((index-precedes (first path) (second path))
               (setq min (first path))
               (setq max (second path)))
            (t (setq min (second path))
               (setq max (first path))))
      (dolist (nxtpath (rest (rest path)) (list min max))
           (cond ((index-precedes nxtpath min) (setq min nxtpath))
                 ((index-precedes max nxtpath) (setq max nxtpath))))))
         

; *********************************************************************
; *** the check is made using the status parameter, set as follows
;     <l1 l2 l3 l4 ( l5 l6 line l7  ) l8 ... >
;      ^  ^  ^  ^  | ^  ^    |  ^   | ^  ^
;       OUTSIDE    | INSIDE  |INSIDE| OUTSIDE
;                  | BEFORE  |AFTER |
; *** it returns the lines before the parenthesis in reverse order (i.e. <l4 l3 l2 l1>)
;     nil, if the parenthesis is at the beginning. Fail if the line is not inside a parenthesis
(defun inside-parentheses (line lines links)
  (let ((linenumb (get-synt-numb line)) (status 'outside) (prevlines nil) parlink fail found)
     (do ((nxtline (first lines) (first lines))
          (lines (rest lines) (rest lines))
          (nxtlink (first links) (first links))
          (links (rest links) (rest links)))
         ((or (null nxtline) found fail)
            (cond (found (values prevlines parlink))
                  (t nil)))
         (case status
            (outside
                 (cond ((equal (get-synt-numb nxtline) linenumb)
            ; *** we are still outside a parenthesis, but we have found our line: fail
                          (setq fail t))
                       ((eq (get-synt-word nxtline) #\()	; )
                          (setq status 'inside-before)
                          (setq parlink nxtlink))
                       (t (setq prevlines (cons nxtline prevlines)))))
            (inside-before
                 (cond ((equal (get-synt-numb nxtline) linenumb)
            ; *** we are inside a parenthesis, and we have found our line: wait for the closed par
                          (setq status 'inside-after))
                       ((eq (get-synt-word nxtline) #\))	; (
                          (setq status 'outside)
                          (setq parlink nil)
            ; *** prevlines is set to nil, because the attachment point should not be found before
            ;     the parenthesis that has been now closed
                          (setq prevlines nil))))
            (inside-after
                 (cond ((eq (get-synt-word nxtline) #\))	; (
                          (setq found t))))))))

; *********************************************************************
;	GENERAL FUNCTIONS
; *********************************************************************
; *** this simply builds a link represented as a triple
(defun make-link (pointer label source)
   (list pointer label source))

; *********************************************************************
; *** this is as the standard find-a-line below, but it applies only if
;     a single line must be returned (all=nil) and returns two values
;     instead of a list (line link)
(defun find-a-line-split 
       (condition lines links &optional all nocrossv notgovern allines allinks)
   (cond (all (exception-nothrow "chunk-parser: incorrect use of find-a-line-split"))
         (t (let ((foundline
                     (find-a-line condition lines links all nocrossv notgovern allines allinks)))
                   (values (first foundline) (second foundline))))))

; *****************************************************************************
; *** returns the non-determiner head of the chunk whose head is in "line"
(defun skip-synt-determiner (line allines allinks)
  (cond ((null line) nil)
        (t (let ((categ (get-synt-categ line)))
   ;(format t "find-np-head. subjline: ~a~%" line)
               (cond ((memq categ '(ART ADJ NUM))
   ; *** if the line is a possible determiner, look among its dependents
                        (let* ((deps (find-dependents line allines allinks))
                               (deplines (first deps))
                               found)
                            (do ((nxtdep (first deplines) (first deplines))
                                 (deplines (rest deplines) (rest deplines)))
                                ((or (null nxtdep) found) found)
                               (cond ((equal nxtdep '(#\#)) 
                                        ; *** this does not work on true AVM !!!!!
                                        (break "PARSER-PROC-ALL/find-np-head"))
                                     ((memq (get-synt-categ nxtdep) '(COORD PUNCT)) nil)
                                     ((eq (get-synt-categ nxtdep) 'NOUN)
                                        (setq found nxtdep))
                                     (t (setq found (find-np-head nxtdep allines allinks)))))))
                     ((memq categ '(PUNCT MARKER)) nil)
                     (t line))))))

; *********************************************************************
; *** given a line, which is assumed to be a NOUN line, it looks for its governor,
;     skipping possible determiners above it
(defun find-noun-group-governor (line link prevlines prevlinks)
   (cond ((neq (get-synt-categ line) 'NOUN)
            (exception-nothrow 
               "chunk-parser: searching for the governor of a noun group not starting from a noun")))
   (let (upline uplink)
      (multiple-value-setq (upline uplink) 
            (find-a-line-split `(position ,(first link)) prevlines prevlinks))
      (cond ((memq (get-synt-categ upline) '(ART ADJ NUM))
               (find-a-line-split `(position ,(first uplink)) prevlines prevlinks))
            (t (values upline uplink)))))

; *****************************************************************************
; *** returns the "nominal" head of a noun group; if the syntatcic head is an
;     adj, an article or a num, looks for a depending noun
(defun find-np-head (subjline allines allinks)
  (cond ((null subjline) nil)
        (t (let ((subjcat (get-synt-categ subjline)))
   ;(format t "find-np-head. subjline: ~a~%" subjline)
               (cond ((eq subjcat 'NOUN) subjline)
                     ((memq subjcat '(ART ADJ NUM))
   ; *** if the line is a determiner, look among its dependents
                        (let* ((deps (find-dependents subjline allines allinks))
                               (deplines (first deps))
                               found)
                            (do ((nxtdep (first deplines) (first deplines))
                                 (deplines (rest deplines) (rest deplines)))
                                ((or (null nxtdep) found) found)
                               (cond ((equal nxtdep '(#\#))
                                        ; *** this does not work on true AVM !!!!!
                                        (break "PARSER-PROC-ALL/find-np-head"))
                                     ((memq (get-synt-categ nxtdep) '(COORD PUNCT)) nil)
                                     ((eq (get-synt-categ nxtdep) 'NOUN)
                                        (setq found nxtdep))
                                     (t (setq found (find-np-head nxtdep allines allinks)))))))
                     (t nil))))))

; *********************************************************************
; *** the optional parameter 'all specifies if all lines and links following
;     (and including) the line that satisfies the condition must be returned
;     (all=T), or if just that line must be returned (all=NIL)
; *** the optional parameter 'nocrossv specifies if a finite verb must stop
;     the search
; *** the optional parameter 'notgovern' specifies if the found line must not
;     depend on the line in 'notgovern'
(defun find-a-line (condition lines links 
                        &optional all nocrossv notgovern allines allinks)
  (cond ((null lines) nil)
        ((and nocrossv
              (eq 'VERB (get-synt-categ (first lines)))
              (memq (get-synt-mood (first lines)) '(IND CONG CONDIZ)))
           nil)
        ((apply-line-cond condition (first lines) (first links) lines)
  ; *** if the line satisfies the search condition
           (cond (notgovern
    ; *** if it is required that it be not governed by 'notgovern'
    ;     the first argument of 'dependent?' is the line number of the 
    ;     governor of the current candidate; this line must not depend on
    ;     'notgovern'
                   (cond ((dependent? (first (first links)) notgovern
                                      allines allinks)
                            (find-a-line
                               condition (rest lines) (rest links)
                               all nocrossv notgovern allines allinks))
                         (all (list lines links))
                         (t (list (first lines) (first links)))))
                 (all (list lines links))
                 (t (list (first lines) (first links)))))
  ; *** if the line does not satisfy the search condition
        (t (find-a-line
               condition (rest lines) (rest links)
               all nocrossv notgovern allines allinks))))

; *********************************************************************
; *** this returns the lines in "lines" that come before "line". They
;     are returned in the original order:
;     line=lx
;     lines= <l1, l2, ..., lx-1, lx, lx+1, ... ln>
;       result: <l1, l2, ... lx-1>
(defun find-prec-lines (line allines)
  (cond ((null allines) (exception 'parse-error "PROC/chunk-parser: find-prec-lines"))
	((equal line (first allines)) nil)
	(t (cons (first allines) (find-prec-lines line (rest allines))))))
 
; *********************************************************************
; *** this returns a single line and bases its search on the line index
(defun find-prec-line (line allines allinks)
  (let ((linumb (get-synt-numb line)))
       (cond ((numberp linumb)
	       (cond ((= linumb 1) nil)
		     (t (first 
			   (find-a-line 
				`(position ,(1- linumb)) allines allinks)))))
	     ((or (= 1 (second linumb))
		  (= 10 (second linumb)))
	 	(first 
		   (find-a-line 
			`(position ,(first linumb)) allines allinks)))
	     (t (first 
		   (find-a-line 
			`(position ,(list (first linumb) (1- (second linumb))))
			 allines allinks))))))
 
; *********************************************************************
; *** used in 'find-a-line' to check if a line satisfies the search conditions
(defun apply-line-cond (condition line link &optional prevlines)
  (cond ((null condition) t)
        (t (case (first condition)
     	      (categ (and (memq (get-synt-categ line) (second condition))
		 	  (apply-line-cond (rest (rest condition)) line link prevlines)))
     	      (type (and (memq (get-synt-type line) (second condition))
		 	  (apply-line-cond (rest (rest condition)) line link prevlines)))
     	      (word (equal (get-synt-word line) (second condition)))
     	      (word-typ 
	           (has-gramm-type (get-synt-word line) (second condition)))
     	      (position (equal (get-synt-numb line) (second condition)))
     	      (head (equal (first link) 0))
     	      (has-compl (and (equal (first link) (third condition))
		   	      (eq (second link) (second condition))))
	      (indef-det (is-indef-determiner line))
	      (mood (and (eq (get-synt-mood line) (second condition))
		 	  (apply-line-cond (rest (rest condition)) line link prevlines)))
      	      (actmood (and (eq (get-synt-mood 
                                    (find-first-aux line 
                                       (reverse
                                         (find-prec-lines line (reverse prevlines)))))
                                (second condition))
		 	  (apply-line-cond (rest (rest condition)) line link prevlines)))
	      (tense (and (eq (get-synt-tense line) (second condition))
		 	  (apply-line-cond (rest (rest condition)) line link prevlines)))
	      (agree (and (chunk-ch-agree line (second condition))
		 	  (apply-line-cond (rest (rest condition)) line link prevlines)))
     	      (unlinked (null (first link)))
     	      (not-unlinked (not (null (first link))))
     	      (not-word (not (equal (get-synt-word line) (second condition))))
     	      (not-trace (and (not (is-a-synt-trace? line))
		 	      (apply-line-cond (rest condition) line link prevlines)))
	      (not-compos (and (numberp (get-synt-numb line))
		 	      (apply-line-cond (rest condition) line link prevlines)))
     	      (not-categ (and (not (memq (get-synt-categ line) (second condition)))
		 	      (apply-line-cond (rest (rest condition)) line link prevlines)))
     	      (linked-to (and (equal (first link) (second condition))
		 	      (apply-line-cond (rest (rest condition)) line link prevlines)))
     	      (otherwise (exception 'parse-error 
                                   "PROC/chunk-parser: Unknown condition in find-a-line"
                                   condition))))))

; *********************************************************************
; *** simple agreement check on some features;
; *** agr-unif defined in ALLLANG/PROC-ALL/TAGGER-PROC-ALL/postagger
(defun chunk-ch-agree (line feat-vals)
  (cond ((null feat-vals) t)
	((eq 'gender (first (first feat-vals)))
	   (and (agr-unif (list (get-synt-gender line)) 
			  (list (second (first feat-vals))))
	        (chunk-ch-agree line (rest feat-vals))))
	((eq 'number (first (first feat-vals)))
	   (and (agr-unif (list (get-synt-number line)) 
			  (list (second (first feat-vals))))
	        (chunk-ch-agree line (rest feat-vals))))
	((eq 'person (first (first feat-vals)))
	   (and (agr-unif (list (get-synt-person line)) 
			  (list (second (first feat-vals))))
	        (chunk-ch-agree line (rest feat-vals))))))

; ************************************************************************
; *** changes the label in 'links' in the position corresponding to the line
;     'linumb' 
(defun change-lab (lines links linumb newlab)
  (cond ((null linumb) links)
        ((null lines) (exception 'parse-error "PROC/chunk-parser: change-lab"))
	((equal linumb (get-synt-numb (first lines))) 
	  (cons newlab (rest links)))
	(t (cons (first links)
		 (change-lab (rest lines) (rest links) linumb newlab)))))

; ************************************************************************
; *** as the previous one, but newlinks is a sequence of pairs
;     <linenumber, newlink>
; *** the optional argument force?, if true, specifies that the new links
;     must be assigned even if they were non-null
(defun mult-change-labs (lines links newlinks source &optional force?)
   (cond ((null newlinks) links)
	 ((null links) 
            (exception 'parse-error "PROC/chunk-parser: mult-change-lab"))
	 ((eq (first (first newlinks)) (get-synt-numb (first lines)))
	   (cond ((or force?
                      (null (first links))		; *** the link is not already assigned
                      (eq (second (first links)) 'TOP-CONJ))
                                    ; *** I assume that the second condition holds when we are
                                    ;     inside a parenthesis, so that the new link must
                                    ;     overcome the existing one
      ; *** now, the second of the first of newlinks is the new link
                   (let ((actnew (second (first newlinks))))
                       (cons (make-link (first actnew) (second actnew) source)
		  	     (mult-change-labs 
				(rest lines) (rest links) (rest newlinks) source force?))))
                 ((eq (second (first links)) 'DET+DEF-ARG)
    ; *** this is a TRICK!!! In a sequence "The lady, the boy, the nice girl and baby" (which is
    ;     possibly not a correct sequence of conjuncts), we get that the labels to change
    ;     are the ones of:
    ;     1. the (the one of "boy")
    ;     2. girl
    ;     This is because "girl" was chosen as first conjunct of "baby", and it triggered
    ;     the search for previous conjuncts. Since it is a noun group, "the boy" and
    ;     "the lady" were accepted as previous further conjuncts. But "girl" was already
    ;     attached to its determiner as DET+DEF-ARG. Now, the situation is:
    ;     - lines: ((32 |girl| (GIRL ...) ...)
    ;               (31 |nice| (NICE ...) ...)
    ;               (30 |the| (THE ...) ...) 
    ;               (29 |,| (#\, ...) ...) ...)
    ;     - links: ((30 DET+DEF-ARG) NIL ...)
    ;     - newlinks: ((32 (29 COORD2ND))
    ;                  (29 (27 COORD)) ...)
    ;     The solution is to leave the link of "girl" unchanged, and to modify the first
    ;     item in newlinks into (30 (29 COORD2ND))
                    (cons (first links)
		  	  (mult-change-labs 
				(rest lines) (rest links) 
                                (cons (list (first (first links)) 
                                            (second (first newlinks))
                                            source)
                                      (rest newlinks)) source force?)))
	    	 (t (cons (first links)		; *** the link is already assigned
		  	  (mult-change-labs 
				(rest lines) (rest links) (rest newlinks) source force?)))))
	 (t (cons (first links) 
		  (mult-change-labs (rest lines) (rest links) newlinks source force?)))))

; *****************************************************************************
; *** coordinations are considered as adjuncts in a verbal case frame
(defun is-an-adjunct (lab)
   (intersection '(RMOD COORDINATOR) (get-all-lab-ancestors lab)))

; *****************************************************************************
; *** checks if a line is the head of a noun complex
;     Returns the semantic head of the complex (e.g. the noun) or nil
(defun is-a-synt-noun-complex (line allines allinks)
  (let ((linecat (get-synt-categ line))
        (linetype (get-synt-type line))
        (linedeps (find-dependents line allines allinks))
        label downline (oblig nil))
     (cond ((eq linecat 'NUM)
              (setq downline (find-case linedeps 'DET+QUANTIF-ARG)))
           ((eq linecat 'ART)
              (setq label
                    (cond ((eq linetype 'DEF) 'DET+DEF-ARG)
                          ((eq linetype 'INDEF) 'DET+INDEF-ARG)))
              (setq downline (find-case linedeps label))
              (setq oblig t))
           ((and (eq linecat 'ADJ) (memq linetype '(DEITT INDEF DEMONS POSS INTERR)))
              (setq label (case linetype
                               (DEITT 'DET+INDEF-ARG)
                               (INDEF 'DET+QUANTIF-ARG)
                               (DEMONS 'DET+DEF-ARG)
                               (POSS 'DET+DEF-ARG)
                               (INTERR 'DET+INTERR-ARG)))
              (setq downline (find-case linedeps label))
              (setq oblig t)))
    (cond ((not (null downline)) downline)
          ;((and oblig (not (is-a-synt-trace? line)))
          ;   (exception 'parse-error "PROC/chunk-parser: determiner not governing"))
          (t nil))))
            
; *****************************************************************************
; *** finds all ancestors of 'label' in the hierarchy of labels
(defun get-all-lab-ancestors (label)
   (cond ((null label) nil)
	 (t (cons label (merge-flatten (mapcar #'get-all-lab-ancestors 
					 (get label 'more-gen-labels)))))))

; *****************************************************************************
; *** this function counts the number of nil elements in a list of lists
;     (which presumably represents the assigned links)
(defun count-unlinked (links)
 (let ((totelem 0) (unlinked 0))
   (do ((nxtsent (first links) (first links))
	(links (rest links) (rest links))
	(endsent nil nil))
       ((null nxtsent) (values totelem unlinked))
     (do ((nxtlk (first nxtsent) (first nxtsent))
	  (nxtsent (rest nxtsent) (rest nxtsent)))
	(endsent)
	(setq totelem (1+ totelem))
	(cond ((null nxtlk) (setq unlinked (1+ unlinked))))
	(cond ((null nxtsent) (setq endsent t)))))))
	 
; *****************************************************************************
; *** moves all links from the end of locutions to the beginning of those locutions
;     parseresult is a pair of data lines and links
; *** It does the same for name sequences
(defun move-locution-links (parseresult)
  (declare (special *LANGUAGE*))
  (let ((datalines (first parseresult))
        (links (second parseresult))
        savenxtsent savenxtlinks linkres)
  ; *** external loop on all sentences of the file
   (do ((nxtsent (first datalines) (first datalines))
        (datalines (rest datalines) (rest datalines))
        (nxtlinks (first links) (first links))
        (links (rest links) (rest links)))
      ((null nxtsent) (list (first parseresult) (reverse linkres)))
    (setq savenxtsent nxtsent)
    (setq savenxtlinks nxtlinks)
  ; *** internal loop on all lines of the sentence
    (do* ((prevlines nil (cons nxtline prevlines))
          (nxtline (first nxtsent) (first nxtsent))
          (nxtsent (rest nxtsent) (rest nxtsent))
          (prevlinks nil (cons nxtlink prevlinks))
          (nxtlink (first nxtlinks) (first nxtlinks))
          (nxtlinks (rest nxtlinks) (rest nxtlinks)))
  ; *** end of sentence: update linkres
        ((null nxtline) 
     ;      (format t "Exiting move locution links. prevlinks: ~a~% linkres ~a~%" prevlinks linkres)
     ;      (break "")
           (setq linkres (cons (reverse prevlinks) linkres)))
  ; *** if link not null (and not a continuation of a locution)
     ;      (format t "In move locution links. prevlinks: ~a~% nxtline: ~a~% nxtlink ~a~%" 
     ;                        prevlinks nxtline nxtlink)
     ;      (break "")
        (cond ((and (not (null nxtlink))
                    (neq (second nxtlink) 'CONTIN+LOCUT)
                    (neq (second nxtlink) 'CONTIN+DENOM))
                (let* ((head (find-a-line `(position ,(first nxtlink))
                                             savenxtsent savenxtlinks))
       ; *** head is the element the current word is linked to
                       (headline (first head))
                       (headlink (second head))
                       locutionbeg namebeg)
  ; *** if pointer to a locution line, move to the beginning of the locution
                    (cond ((is-a-synt-locution headline)
                            (setq locutionbeg 
                                  (find-locution-beg headline savenxtsent))
                            (setq nxtlink (cons (get-synt-numb locutionbeg)
                                                (rest nxtlink))))
  ; *** the same is done for complex names (which are not locutions), as George C. Scott,
  ;     where a dependent previously linked to Scott, is moved to George
                          ((eq 'CONTIN+DENOM (second headlink))
                            (setq namebeg 
                                  (find-name-beg headline savenxtsent savenxtlinks))
                            (setq nxtlink (cons (get-synt-numb namebeg)
                                                (rest nxtlink))))
  ; *** and similarly for appositions: "the president Lincoln, who ...", the relclause
  ;     is moved from Lincoln to President
  ; *** This also applies to "avinguda Diagonal, 33", where the comma and the street
  ;     number are moved from Diagonal to avinguda
  ; *** The second condition to avoid application to "la ferrovia Tirana-Durazzo"
                          ((and (eq 'NOUN-APPOSITION-DENOM (second headlink))
                                (not (and (eq 'proper (get-synt-type nxtline))
                                          (eq #\- (get-synt-word (first prevlines)))))
                              ;  (neq 'punct (get-synt-categ nxtline))
                              ;  (neq 'num (get-synt-categ nxtline))
                                     )
                            (setq nxtlink (cons (first headlink)
                                                (rest nxtlink))))
  ; *** for Italian NOUN-NOUN modifications, I assume the same thing happens:
  ;     "le parti ricambio dei componenti" must have "dei componenti" attached to
  ;     "parti". I do not know for other languages
                          ((and (eq *LANGUAGE* 'italian)
                                (eq 'NOUN-RMOD (second headlink)))
                            (setq nxtlink (cons (first headlink)
                                                (rest nxtlink))))))))))))

; **********************************************************************
; *** given a line containing a name, this finds the line where the name
;     starts (e.g. Mario Rossi)
(defun find-name-beg (nameline datalines datalinks)
 (let ((found nil))
  ; *** the external do* moves on all lines in search for the name line
   (do* ((prevlines nil (cons nxtline prevlines))
         (prevlinks nil (cons nxtlink prevlinks))
         (nxtline (first datalines) (first datalines))
         (nxtlink (first datalinks) (first datalinks))
         (datalines (rest datalines) (rest datalines))
         (datalinks (rest datalinks) (rest datalinks)))
        ((or found (null nxtline))
          (cond (found 
  ; *** when the name line is found, prevlines contains all the lines preceding it
  ; *** the internal do moves (backward) on the preceding lines, checking if the
  ;     line also is a CONTIN+DENOM; in such a case, it goes on; otherwise, it
  ;     returns the line
                     (do* ((precline (first prevlines) line)
                           (line (second prevlines) (first prevlines))
                           (prevlines (rest (rest prevlines)) (rest prevlines))
                           (preclink (first prevlinks) link)
                           (link (second prevlinks) (first prevlinks))
                           (prevlinks (rest (rest prevlinks)) (rest prevlinks)))
                         ((or (null line) 
                              (neq 'CONTIN+DENOM (second preclink)))
                            precline)))
                (t (exception 'parse-error "PROC/chunk-parser: find-name-beg"))))
        (setq found (equal nameline nxtline)))))

; ***************************************************************
;    Checks if the word appearing in the tree node which is the first argument
;    has, as its ontological type (i.e. according to what appears in the ontology)
;    the second argument
(defun has-ont-type (line class)
   (let ((wmean 
            (first (inlist (get-word-meaning (get-synt-word line)
                                  (get-synt-categ line)
                                  (get-synt-type line)
                                  nil)))))
          (cond ((atom wmean)
                   (is-instance-or-subclass-of wmean class))
                ((and (listp (second wmean))
                      (eq 'thematic-grid (first (second wmean))))
                   (is-instance-or-subclass-of (first wmean) class))
                (t (break "chunk-parser: has-ont-type")))))

; ***************************************************************
(defun inspect-prep-art (data)
; *** this function is a kind of pre-processing, dedicated to plural indefinite
;     articles (delle, dei, ...). Since almost all occurrences of these items are
;     prep+art, the lexicon does not include the article interpretation. In ATLAS,
;     it is useful to get it, so on the basis of an inspection of the context, some
;     prep+art interpretation are changed to ART
  (let (newdata newlines change?)
   (do ((lines (first data) (first data))
        (data (rest data) (rest data)))
      ((null lines) (reverse newdata))
    (do* ((prevline nil nxtline)
          (nxtline (first lines) (first lines))
          (lines (rest lines) (rest lines)))
       ((null nxtline) 
           (setq newlines (reverse newlines)))
       (cond (change? (setq change? nil))
     ; *** the previous branch in case a change has been made in the previous step, so
     ;     that the current line (art of the prep+art) must be skipped
             ((and (eq 'PREP (get-synt-categ nxtline))
                   (has-gramm-type (get-synt-word nxtline) '&neutral-prep)
                   (eq 'ART (get-synt-categ (first lines)))
                   (same-linumb (get-synt-numb nxtline) (get-synt-numb (first lines)))
                   (or 
     ; *** the next is the first  condition for changing
                       (eq 'ADV (get-synt-categ prevline))
     ; *** the next is the second  condition for changing
                       (and (eq 'CONJ (get-synt-categ prevline))
                            (eq 'COORD (get-synt-type prevline))
		            (not (null (find-a-line '(word del) 
					   (first-n 10 newlines) nil))))))
               (setq change? t)
               (setq newlines (cons 
                                 (list (get-synt-numb nxtline) (get-synt-inpword nxtline) 
                                       (list 'del 'ART 'INDEF 
                                             (get-synt-gender (first lines))
                                             (get-synt-number (first lines))))
                                 newlines)))
             (t (setq newlines (cons nxtline newlines)))))
       (setq newdata (cons newlines newdata))
       (setq newlines nil))))

; ***************************************************************
;     WRITING ON OUTPUT FILE
; ***************************************************************
;    writes the final result in the file X.prs
(defun write-parsed-file (data parseresult buff parsedport)
; *** it loops on 'buff' via a dolist, while advancing manually on 'data' and
;     'parseresult'  
; *** it must be recalled that 'buff' is a linear list including both data
;     lines and comment/heading lines. On the contrary, 'data' and 'parseresult'
;     are both lists of lists (supposedly parallel), where each sublist
;     corresponds to a different sentence
   (let (nxtsent nxtsentlab nxtwrd nxtlab prvwrd)
; *** MAIN LOOP ************************************
     (dolist (line buff)
   ; (format t "line: ~a~% nxtwrd; ~a~% nxtlab: ~a~%" line nxtwrd nxtlab)
   ; (break "")
	     (cond ((stringp line)
; *** comments and empty lines are simply printed
          	     (cond ((or (string= (string-trim 
                                             '(#\Space #\Tab #\Return) line)
                                          "")
                	        (same-chars? (read-from-string line) #\?))
                             (cond ((not (null nxtwrd))
    ; *** there are some traces at the end, and no final punctuation mark
                                      (print-rem-traces (cons nxtwrd nxtsent)
                                                        (cons nxtlab nxtsentlab)
                                                        parsedport)))
		   	     (format parsedport "~a~%" line))
; *** in correspondence to a heading line, a new sentence must be initialized
	      	           ((is-sentence-heading line nil)
			     (cond ((not (null nxtsent))
			  	     (exception 'parse-error 
                                               "PROC/chunk-parser: Misalignment in buff-data"
                                               line nxtsent))
		      	           (t (format parsedport "~a~%" line)
			 	     (setq nxtsent (first data))
			 	     (setq data (rest data))
			 	     (setq nxtsentlab (first parseresult))
			 	     (setq parseresult (rest parseresult))
	     			     (setq nxtwrd (first nxtsent))
	     			     (setq nxtsent (rest nxtsent))
	     			     (setq nxtlab (first nxtsentlab))
	     			     (setq nxtsentlab (rest nxtsentlab)))))))
; *** otherwise, it is a data line; print and advance on sentence
;     the advance on the result must be repeated, without advancing on 'buff', for
;     each trace (which did not occur in the original input stored in 'buff')
		   (t (do ()
			  ((not (is-a-synt-trace? nxtwrd))
			     (print-data-line nxtwrd nxtlab parsedport)
	     	     	     (setq prvwrd nxtwrd)
	     	             (setq nxtwrd (first nxtsent))
	     	             (setq nxtsent (rest nxtsent))
	     	             (setq nxtlab (first nxtsentlab))
	     	             (setq nxtsentlab (rest nxtsentlab)))
		      	  (print-data-line nxtwrd nxtlab parsedport)
	     	      	  (setq prvwrd nxtwrd)
	     	      	  (setq nxtwrd (first nxtsent))
	     	      	  (setq nxtsent (rest nxtsent))
	     	      	  (setq nxtlab (first nxtsentlab))
	     	      	  (setq nxtsentlab (rest nxtsentlab))
                      ))))))

; ***************************************************************************
; *** This prints a sequence of traces (probably at the end of a sentence)
;     Only traces are allowed in the sequence
(defun print-rem-traces (remsent remsentlab outport)
   (cond ((null remsent) nil)
         ((not (is-a-synt-trace? (first remsent)))
             (exception 'parse-error "print-rem-traces: Misalignment at end sentence"))
         (t (print-data-line (first remsent) (first remsentlab) outport)
            (print-rem-traces (rest remsent) (rest remsentlab) outport))))
            
; ***************************************************************************
; *** outport is nil when this function is called from build-tut-result
;     (see below) in order to return a set of strings. It is non-nil,
;     when it must actually write on the output .prs file
; *** The  various assignments (via concatenation) to printres should work 
;     both in case outport is nil (printres gets the data) and in case it is
;     non-nil (printres gets nil, and the data are printed)
(defun print-data-line (line label outport)
  (declare (special *CHAR-SET-ID* *LISP-CHAR-SET-ID*))
  (let (printres temp)
   (cond ((null line) 
      ; *** the next works both in case outport is nil and in case it has a value
           (format outport "~%"))
         (t
           (setq *print-pretty* nil)
           (setq *print-level* nil)
           (setq *print-length* nil)
; *** the traceinfo is displaced in the readable form (fourth position) with
;     respect to the output position (third)
   (let ((linenumb (first line))
	 (word (second line))
	 (syntinfo (third line))
	 (traceinfo (fifth line))
	 (comments (sixth line)))
; *** writing the line number. If not a number, it must be of the form (n1 n2),
;     and the output must appear as n1.n2
       (cond ((numberp linenumb)
                (setq printres (format outport "~a " linenumb)))
	     (t (setq printres (format outport "~a.~a " (first linenumb) (second linenumb)))))
; *** writing the word, which must be converted from the internal format (mixed base scheme
;     enforced scheme) to the external one (the one chosen)
       (cond ((neq *LISP-CHAR-SET-ID* *CHAR-SET-ID*)
                (setq temp (implode
                    (convert-tule-char-names-to-numcodes
                         (convert-base-codes-to-tule-char-names
                             (mapcar #'char-code (explode word)))))))
             (t (setq temp word)))
       (setq printres (concatenate 'string printres (format outport "~a " temp)))
; *** if traceinfo not null, writing traceinfo:
       (cond ((not (null traceinfo))
; *** non-coindexed trace; output []
		(cond ((eq traceinfo 'empty)
                         (setq printres (concatenate 'string printres
			                   (format outport "[] "))))
; *** trace co-indexed with line N, with type of trace x: output [Nx]
		      ((numberp (first traceinfo))
                         (setq printres (concatenate 'string printres
		                           (format outport "[~a~a] " 
					                  (first traceinfo) (second traceinfo)))))
; *** trace co-indexed with line N.M, with type of trace x: output [N.Mx]
                      (t (setq printres (concatenate 'string printres
		                           (format outport "[~a.~a~a] "
					                  (first (first traceinfo)) 
					                  (second (first traceinfo))
					                  (second traceinfo))))))))
; *** writing the syntinfo
; *** the 'cond' used to force the writing in character format of the
;     punctuations and symbols
       (cond ((listp syntinfo)
               (setq printres (concatenate 'string printres
                                    (format outport "~a" #\()))
               (cond ((characterp (first syntinfo))
                        (setq printres (concatenate 'string printres
                                         (format outport "~a~a~a" #\# #\\ (first syntinfo)))))
                     (t (setq temp (first syntinfo))
                        (cond ((neq *CHAR-SET-ID* *LISP-CHAR-SET-ID*)
                                 (setq temp (convert-base-to-currscheme-uppercase temp)))
                              (t (setq temp (base-uppercase temp))))
                        (setq printres 
                            (concatenate 'string printres
                                (format outport "~s" temp)))))
               (dolist (el (rest syntinfo))
                      (setq printres (concatenate 'string printres
                                           (format outport " ~s" el))))
               (setq printres (concatenate 'string printres
                                   (format outport "~a " #\)))))
   ; *** syntinfo is not a list in case the word is unknown ("Does not exist")
             (t (setq printres (concatenate 'string printres
                                      (format outport "~s " syntinfo)))))
; *** writing dependency info
       (setq printres (concatenate 'string printres (format outport "[")))
       (cond ((not (null label))
               (cond ((or (null (first label)) (numberp (first label)))
                       (setq printres (concatenate 'string printres
                                           (format outport "~a" (first label)))))
                     (t (setq printres (concatenate 'string printres
	                                 (format outport "~a.~a" 
			                       (first (first label)) (second (first label)))))))
               (setq printres (concatenate 'string printres
	                         (format outport ";~s" (second label))))))
       (setq printres (concatenate 'string printres (format outport "~a" #\])))
; *** writing comments
       (cond ((not (null comments))
               (setq printres (concatenate 'string printres
		                  (format outport "	??? ~a" comments)))))
       (setq printres (concatenate 'string printres (format outport "~%")))
   (setq *print-pretty* t)
   (setq *print-level* 5)
   (setq *print-length* 10))))))

; *********************************************************************************
; **** RETURNS THE RESULT IN A LIST OF STRINGS (ONE FOR EACH LINE IN THE TUT FORMAT)
; *********************************************************************************
(defun build-tut-result (data links)
; *** 'data' and 'parseresult' are both lists of lists (supposedly parallel), where
;     each sublist corresponds to a different sentence
   (let (printbuff printsent)
; *** MAIN LOOP ************************************
     (do ((nxtsent (first data) (first data))
          (nxtsentlab (first links) (first links))
          (data (rest data) (rest data))
          (links (rest links) (rest links)))
         ((null nxtsent) printbuff)
         (setq printsent nil)
         (do ((nxtword (first nxtsent) (first nxtsent))
              (nxtlink (first nxtsentlab) (first nxtsentlab))
              (nxtsent (rest nxtsent) (rest nxtsent))
              (nxtsentlab (rest nxtsentlab) (rest nxtsentlab)))
             ((null nxtword)
                (setq printbuff (cons (reverse printsent) printbuff)))
	     (setq printsent (cons (print-data-line nxtword nxtlink nil)
                                   printsent))))))



