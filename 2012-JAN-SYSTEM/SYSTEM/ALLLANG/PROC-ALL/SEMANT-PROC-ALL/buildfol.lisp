(in-package "USER")

(defvar varindex 0)

; ********************************************************
; *** this simply moves across the RESTR path of the semantic 
;     query in order to match it to the predicate definition
; *** just the restriction is used; it includes the topic, while the
;     query goal is not useful, since it involves what is looked for,
;     not what is given
(defun sem-to-fol-translation (semquery)
 (declare (special *ANAPHORIC-CONTEXT* varindex))
 (let (goal-part restr-part par-result body vars)
; *** the next cond extracts and interprets the goal part ***************
  (cond ((eq (first semquery) 'select)
  ; ** it is a standard select ... from ... where ... query
  ;     translate the select ... part
  ; ** in handling the select part, int-stp-transl takes as input also
  ;    the "from" concept"
          (break "Select clause in the sem-to-fol-translation")
          (setq goal-part 
                    (int-fol-transl (second semquery) (fourth semquery)))
  ; ** if nothing is obtained from the translation of the "select" part,
  ;    assume that the "from" part is a subclass of a basic data type,
  ;    and use it as goal
          (cond ((all-null goal-part)
                   (setq goal-part (list (list (list (fourth semquery))))))))
  ; ** the next should not occur in TOCAI, at least in this first version
        ((eq (first semquery) 'about)
          (setq goal-part 
                    (int-fol-transl (fourth semquery))))
        (t (exception 'parameter-error "PROC/buildpred: Unknown expression" (first semquery))))
; *** the next cond extracts and interprets the goal part ***************
  (cond ((eq (first semquery) 'about)
           (setq restr-part nil))
        ((null (sixth semquery))
  ; *** but if the restriction is nil, then use the 'from' concept
          (setq restr-part
               (int-fol-transl (list (fourth semquery)))))
        (t (setq restr-part (int-fol-transl (sixth semquery)))))
  (cond ((null restr-part)
           (setq par-result (elimdup goal-part)))
        (t (setq par-result (list (elimdup goal-part) (elimdup restr-part)))))
; *** now, the "instance-of" specification are replaced by the direct use of constants *****
  (setq par-result (remove-instances par-result nil nil nil))
  (setq body (first par-result))
  (setq vars (reverse (second par-result)))
  (setq varindex 0)
  (list 'exists vars body)
  ;par-result
   ))

; ********************************************************
(defun generate-var (root)
  (declare (special varindex))
   (setq varindex (1+ varindex))
   (concat root varindex))

; ********************************************************
(defun mult-generate-vars (root numvar)
  (cond ((<= numvar 0) nil)
        (t (cons (generate-var root) (mult-generate-vars root (1- numvar))))))

; ********************************************************
(defun int-fol-transl (path)
   (cond ((atom (second path))
  ; *** the next cond initializes the "expected" var, according to the start of path
            (cond ((memq (second path) '(RANGE-OF DOMAIN-OF))
                    (build-fol path 'unary-arg nil nil))
                  ((memq (second path) '(RANGE DOMAIN))
                    (build-fol path 'binary-arg nil nil))
                  (t (format t "build fol: path starting in an unexpected way 1"))))
         (t (format t "build fol: path starting in an unexpected way 2"))))

; ********************************************************
;  path:         expected
;   X		unary-arg  (a concept, e.g. ££day)
;   RANGE-OF	binary-link
;   Y		binary-arg (a relation, e.g. &has-situation-location) 
;   DOMAIN	unary-link
;   Z		unary-arg
; *** previtem and prev2item are the two elements that come before "path"
(defun build-fol (path expected var-acc const-acc &optional previtem prev2item given-var)
   (let (result newvar actitem tempres andlist newpred evlist newvars restpath act2item
         act3path)
    ;(format t "Entering build-fol~% path: ~a~% previtem: ~a~% prev2item: ~a~%"
    ;     path previtem prev2item)
    ;(break "eye")
; *** the various assignment to "result" are made mainly for debugging purposes
     (cond ((null path) (setq result nil))
; *** The "anded" part simply result in adding new predicates, since at this stage
;     all predicates are assumed to be in and
           ((and (listp (first path)) (eq (first (first path)) 'and))
              (cond ((eq expected 'unary-arg)
    ; *** in case what is expected is a unary arg, the situation comes from
    ;     ... PRED LINK (AND (UNARY-ARG1 ...) (UNARY-ARG2 ...) ...)
    ;     In this case, we have in prevpred (PRED VAR1 VAR2), and in prevlink DOMAIN or RANGE
    ;     In such a case, we must generate new fresh variables for all conjuncts
                       (setq andlist (second (first path)))
                       (setq tempres (build-fol (first andlist) expected 
                                            var-acc const-acc previtem prev2item))
                       (setq result
                           (do ((nxt-andarg (first (rest andlist)) (first andlist))
                                (andlist (rest (rest andlist)) (rest andlist)))
                               ((null nxt-andarg) tempres)
                               (setq newvar (generate-var 'x))
     ;(format t " path: ~a~%" path)
     ;(break "unary arg")
                               (setq tempres 
                                  (append tempres
                                       (cons (cond ((eq previtem 'RANGE)
                                                      (list (first prev2item) 
                                                            (second prev2item) newvar))
                                                   (t (list (first prev2item) newvar 
                                                            (third prev2item))))
                                            (build-fol nxt-andarg 
                                                 expected (cons newvar var-acc) 
                                                        const-acc previtem prev2item)))))))
                    (t (setq result
                          (dolist (nxt-andarg (second (first path)) tempres)
                              (setq tempres 
                                   (append tempres 
                                       (build-fol nxt-andarg expected 
                                              var-acc const-acc previtem prev2item))))))))
           ((and (listp (first path)) (eq (first (first path)) 'event-and))
              (setq evlist (second (first path)))
              (setq newvars (mult-generate-vars 'x (length evlist)))
              (do ((nxtev (first evlist) (first evlist))
                   (evlist (rest evlist) (rest evlist))
                   (nxtvar (first newvars) (first loopvars))
                   (loopvars (rest newvars) (rest loopvars)))
                  ((null nxtev)
                     (setq result
                         (cons (append (list 'event-and (first var-acc)) newvars)
                               tempres)))
                  (setq tempres
                      (append tempres 
                            (build-fol nxtev expected var-acc const-acc nil nil nxtvar)))))
; *** the assumption is that an "eq" specification provides a value for the last variable
           ((and (listp (first path)) (eq (first (first path)) 'eq))
              (setq result
                  (cons (list 'eq (first var-acc) (second (first path)))
                        (build-fol (rest path) expected var-acc 
                                   (cons (second (first path)) const-acc)))))
           (t (case expected
                (unary-link
                   (cond ((memq (first path) '(RANGE DOMAIN VALUE))
                            (setq result (build-fol (rest path) 'unary-arg var-acc const-acc
                                            (first path) previtem)))
                         (t (format t "In build-fol: expected unary-link; path: ~a~%" path)
                            (break ""))))
                (binary-link
                   (cond ((memq (first path) '(RANGE-OF DOMAIN-OF ARG-OF))
                            (setq result (build-fol (rest path) 'binary-arg 
                                                var-acc const-acc (first path) previtem)))
                         ((eq (first path) 'HAS-INSTANCE)
                            (setq result 
                               (cons (list 'eq (first var-acc) 
                                           (remove-synt-pointer (second path)))
                                     (build-fol (rest (rest path)) 'binary-link var-acc const-acc))))
                         ((eq (first path) 'VALUE-OF)
                            (setq result 
                               (cons (list (first (get (second path) 'relinstance) )
                                           (first var-acc) (fourth path))
                                     (build-fol (nthcdr 4 path) 'binary-link var-acc const-acc))))
                         ((eq (first path) 'ARGUMENT-OF)
                            (setq result 
                               (cons (list (get (second path) 'relinstance) 
                                           (fourth path) (first var-acc))
                                     (build-fol (nthcdr 4 path) 'binary-link var-acc const-acc))))
                         (t (format t "In build-fol: expected binary-link; path: ~a~%" path)
                            (break ""))))
                (unary-arg                              ; *** a unary arg is a concept
     ;(format t " path: ~a~%" path)
     ;(break "unary arg 2")
                   (setq actitem (remove-synt-pointer (first path)))
                   (setq newvar (cond ((and (not given-var) (null var-acc))
                                         (generate-var 'x))
                                      (given-var given-var)
                                      (t (first var-acc))))
                   (setq result 
                       (cons (list actitem newvar) 
                             (build-fol (rest path) 'binary-link (cons newvar var-acc) const-acc 
                                        (first path) previtem))))
                (binary-arg
     ; *** when the item is a binary arg (i.e. a role name), a new variable is generated
     ;     the result is (pred newvar focusedvar) if the next item is "domain", 
     ;     (pred focusedvar newvar) if the next item is "range". 
     ;     In the recursive call, the new predicate is passed as "prevpred", since it could be
     ;     used in a following "and"
     ;(format t " path: ~a~%" path)
     ;(break "binary arg")
                   (setq actitem (remove-synt-pointer (first path)))
                   (setq act2item (remove-synt-pointer prev2item))
                   (setq act3path (remove-synt-pointer (third path)))
                   (setq newvar 
                        (cond (given-var given-var)
                              (t (generate-var 'x))))
                   (setq result 
                      (cond ((eq (second path) 'DOMAIN)
        ; *** in case the sequence is [conc1 RANGE-OF] [rel DOMAIN conc2 ...]
        ;     (the standard situation), what is generated is rel(conc2,conc1)
        ;     But there are cases where the path is 
        ;     [conc1 DOMAIN-OF] [rel DOMAIN conc2 DOMAIN-OF rel2 RANGE conc3 ...] such that 
        ;     conc2 is a subclass or superclass of conc1. In this case, what has to be generated is
        ;     rel(conc1,conc3) 
   ;(format t " Domain; act2item: ~a --- previtem: ~a~% path: ~a~%" act2item previtem path)
   ;(break "")
                               (cond ((and (eq previtem 'DOMAIN-OF)
                                           (is-subclass-of act2item act3path)
                                           (eq (fourth path) 'DOMAIN-OF)
                                           (eq (sixth path) 'RANGE))
                                        (setq newpred (list actitem (first var-acc) newvar))
                                        (setq restpath (nthcdr 5 path)))
                                     ((and (eq previtem 'DOMAIN-OF)
                                           (null (nthcdr 3 path))
                                           (or (is-subclass-of act2item act3path)
                                               (is-subclass-of act3path act2item)))
                                        (setq newpred (list actitem (first var-acc) newvar))
                                        (setq restpath (rest path)))
                                     (t (setq newpred (list actitem newvar (first var-acc)))
                                        (setq restpath (rest path))))
                               (cons newpred
                                   (build-fol restpath 'unary-link
                                              (cons newvar var-acc) const-acc actitem previtem)))
                            ((eq (second path) 'RANGE)
                               (cond ((and (eq previtem 'RANGE-OF)
                                           (is-subclass-of act2item act3path)
                                           (eq (fourth path) 'RANGE-OF)
                                           (eq (sixth path) 'DOMAIN))
                                        (setq newpred (list actitem newvar (first var-acc)))
                                        (setq restpath (nthcdr 5 path)))
                                     ((and (eq previtem 'RANGE-OF)
                                           (null (nthcdr 3 path))
                                           (or (is-subclass-of act2item act3path)
                                               (is-subclass-of act3path act2item)))
                                        (setq newpred (list actitem newvar (first var-acc)))
                                        (setq restpath (rest path)))
                                     (t (setq newpred (list actitem (first var-acc) newvar))
                                        (setq restpath (rest path))))
                               (cons newpred
                                   (build-fol restpath 'unary-link 
                                              (cons newvar var-acc) const-acc 
                                              actitem previtem)))))))))
        result))

; ********************************************************
; *** this function removes all occurrences of (eq var val)
;     it has the side effect of removing the unary predicate (var-class var);
;     all occurrences of "var" are replaced by "val"
;      (££APPLIED-FUNCTION X19) (&FUNCTOR X19 X20) (££MATH-FUNCTION X20) (EQ X20 £AVERAGE)
;          --->
;      (££APPLIED-FUNCTION X19) (&FUNCTOR X19 £AVERAGE)
(defun remove-instances (body prev vars const-acc)
  ;(format t "remove-instances: ~% body: ~a~% prev: ~a~% vars: ~a~% const-acc: ~a~%"
  ;          body prev vars const-acc)
  ;(break "")
  (let ((firstpred (first body)))
   (cond ((null firstpred) (list (reverse prev) vars))
         ((eq (first firstpred) 'eq)
             (remove-instances 
                  (replace-constants (rest body) (second firstpred) (third firstpred))
                  (replace-constants prev (second firstpred) (third firstpred))
                  (remove (second firstpred) vars)
                  (cons (third firstpred) const-acc)))
         (t (remove-instances (rest body) (cons firstpred prev) 
                   (add-vars vars (rest firstpred) const-acc) const-acc)))))

; ********************************************************
(defun add-vars (vars newvars const-acc)
  (cond ((null newvars) vars)
        ((or (member (first newvars) const-acc)
             (member (first newvars) vars))
            (add-vars vars (rest newvars) const-acc))
        (t (add-vars (cons (first newvars) vars) (rest newvars) const-acc))))

; ********************************************************
; *** replaces all occurrences of var with val, and removes the class predicate of var
(defun replace-constants (preds var val)
   (cond ((null preds) nil)
         ((eq 2 (length (first preds)))
     ; *** it is a unary predicate
            (cond ((eq var (second (first preds)))
     ; *** this is the type specification (class) of the constant
                     (replace-constants (rest preds) var val))
                  (t (cons (first preds)
                           (replace-constants (rest preds) var val)))))
     ; *** not a unary predicate
         (t (cons
                (cons (first (first preds)) (subst val var (rest (first preds))))
                (replace-constants (rest preds) var val)))))

; ********************************************************
(defun fol-print-pretty (fol-repr folport)
  (let (varindentlist (fol-body (third fol-repr)) focusedvar predargs (varindent 0)
        (varlist (second fol-repr)))
     (format folport "(exists ~a~%" varlist)
     (cond ((eq (length (first fol-body)) 2)
              (setq focusedvar (second (first fol-body)))
              (setq varindentlist (list (list focusedvar 0)))
              (fol-format folport varindent (first fol-body)))
           (t (break "buildfol: fol not starting with a binary predicate")))
     (do ((nxtpred (second fol-body) (first fol-body))
          (fol-body (rest (rest fol-body)) (rest fol-body)))
       ((null nxtpred)
          (format folport ")"))
       (cond ((eq (length nxtpred) 3)                 ; *** a binary predicate ****
              ;  (format t "Binary predicate: ~a~%" nxtpred)
              ;  (break "")
                (setq predargs (rest nxtpred))
                (cond ((member (first predargs) varlist)
                         (setq varindent (first (leggi varindentlist (first predargs))))
                         (cond ((null varindent)
                                 (cond ((or (member (second predargs) varlist)
                                            (is-a-fol-instance (second predargs)))
                                          (setq varindent 
                                              (first (leggi varindentlist (second predargs))))
                                          (cond ((null varindent)
                             ; *** if not seen before, by default, increase by 1 the last value
                                                   (setq varindentlist
                                                      (cons (list (first predargs) 
                                                                  (1+ (second (first varindentlist))))
                                                            varindentlist)))
                             ; *** the second variable of nxtpred is the previous pivot
                                                (t (setq varindentlist
                                                      (cons (list (first predargs) (1+ varindent))
                                                              varindentlist))
                                                   (fol-format folport (1+ varindent) nxtpred))))
                                       (t (break "binary pred with both variables unseen 2"))))
                       ; *** the first variable of nxtpred is the previous pivot
                               (t (setq varindentlist
                                     (cons (list (second predargs) (1+ varindent)) varindentlist))
                                  (fol-format folport (1+ varindent) nxtpred))))
                  ; *** the first argument is not a var
                      ((member (second predargs) varlist)
                         (setq varindent (first (leggi varindentlist (second predargs))))
                         (cond ((null varindent)
                                  (setq varindent (first (leggi varindentlist (first predargs))))
                                  (cond ((null varindent)
                                           (break "binary pred with both variables unseen 3"))
                             ; *** the second variable of nxtpred is the previous pivot
                                        (t (setq varindentlist
                                                (cons (list (first predargs) (1+ varindent))
                                                      varindentlist))
                                           (fol-format folport (1+ varindent) nxtpred))))))))
             ((eq (length nxtpred) 2)                 ; *** a unary predicate ****
              ;  (format t "Unary predicate: ~a~%" nxtpred)
              ;  (break "")
                (cond ((member (second nxtpred) varlist)
                         (setq varindent (first (leggi varindentlist (second nxtpred))))
                         (cond ((null varindent)
                                  (break "unary pred with variable unseen 4"))
                               (t (fol-format folport (1+ varindent) nxtpred))))
                      (t (break "unary pred with variable unseen 4"))))
             ((eq (first nxtpred) 'event-and)                 ; a variable-argument predicate
                (setq predargs (rest nxtpred))
                (setq varindent (first (leggi varindentlist (first predargs))))
                (fol-format folport (1+ varindent) nxtpred)
                (setq varindentlist
                     (append varindentlist (mapcar #'(lambda (x) (list x (+ varindent 2)))
                                                   (rest predargs)))))
             (t (break "predicate differnet from event-and, but of length different from 2 or 3"))))))

; ********************************************************
(defun is-a-fol-instance (item)
  (not (null (get item 'instance))))

; ********************************************************
(defun fol-format (folport indentcount nxtpred)
   (cond ((eq 0 indentcount) (format folport "~a~%" nxtpred))
         (t (format folport "   ")
            (fol-format folport (1- indentcount) nxtpred))))

