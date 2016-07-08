
(in-package "USER")

; ********************************************************************
; *** functions for navigating in the ontology
; ********************************************************************

; ********************************************************************
; *** returns true if the argument is a basic ontological link (as range,
;     domain-of, instance, ...)
(defun is-a-structural-item (item)
   (member item
      '(range range-of domain domain-of has-subclass subclass-of restricts
        restricted-by instance relinstance argument arg-of value value-of)))

; ********************************************************************
; *** basic, untyped, movement
(defun get-link-val (link elem)
   (get elem link))

; ********************************************************************
; *** this returns the local class of an instance
(defun get-instance-class (instance-id)
   (cond ((null instance-id) nil)
         ((not (is-subclass-of instance-id '££entity)) ; *** this is a class, not an instance
            (get-link-val 'instance instance-id))))

; ********************************************************************
; *** this returns the local relation of a relation instance
(defun get-relinstance-rel (instance-id)
   (cond ((null instance-id) nil)
         ((not (is-subclass-of instance-id '££entity)) ; *** this is a class, not a relation
            (get-link-val 'relinstance instance-id))))

; ********************************************************************
; *** membership at more levels
; *** instance-id (e.g. £northern) can have more than one direct class
;     (e.g. ££cardinal-direction and ££it-area-spec)
; *** the function returns all direct classes that are subclasses of "class"
(defun is-instance-of (instance-id class)
  (let ((iclass (get-instance-class instance-id)) found)
       (do ((nxtdirectclass (first iclass) (first iclass))
            (iclass (rest iclass) (rest iclass)))
           ((null nxtdirectclass) found)
           (cond ((is-subclass-of nxtdirectclass class)
                    (setq found (cons nxtdirectclass found)))))))

; ********************************************************************
; *** relation instances
; *** relinstance-id (e.g. &moved-sea-intens-1)
;     relation (e.g. &intensification-arg)
; *** the function returns t if relinstance is an instance of relation
(defun is-relinstance-of (relinstance-id relation)
  (let ((iclass (get-relinstance-rel relinstance-id)) found)
       (do ((nxtdirectclass (first iclass) (first iclass))
            (iclass (rest iclass) (rest iclass)))
           ((null nxtdirectclass) found)
           (cond ((is-restriction-of nxtdirectclass relation)
                    (setq found (cons nxtdirectclass found)))))))

; ********************************************************************
; *** membership at more levels (unprotected)
;     As above, but this works also if the instance is unknown (returning nil)
(defun is-instance-or-subclass-of (ident class)
  (let ((down-class (inlist (cond ((is-an-instance ident)
                                     (get-instance-class ident))
                                  (t ident))))
  ; *** down-class (i.e. the direct class of the ident) can be a list, so that
  ;     I force lists in all cases
        found)
      (do ((nxtdown (first down-class) (first down-class))
           (down-class (rest down-class) (rest down-class)))
          ((or (null nxtdown) found) found)
          (cond ((is-subclass-of nxtdown class)
                   (setq found nxtdown))))))

; ********************************************************************
; ********************************************************************
; *** true if ident is an instance
(defun is-an-instance (ident)
  (not (null (get-link-val 'instance ident))))

; ********************************************************************
; *** true if ident is a class
(defun is-a-class (ident)
  (or (not (null (get-link-val 'subclass-of ident)))
      (eq ident '££entity)))

; ********************************************************************
; *** subsumption
(defun is-subclass-of (class up-class)
  (cond ((and (listp up-class) (eq (first up-class) 'union))
           (is-subclass-of-one-of class (rest up-class)))
        ((or (null class) (null up-class))
          (exception 'semantic-error 
                     "PROC/onto-reasoning: Attempt to use a NIL ontology class"
                      class up-class))
        ((eq class up-class) t)
        (t (int-is-subclass-of (get class 'subclass-of) up-class))))

(defun int-is-subclass-of (next-up-classes up-class)
  (cond ((null next-up-classes) nil)
        ((member up-class next-up-classes) t)
        (t (int-is-subclass-of
               (merge-flatten (mapcar #'(lambda (x) (get x 'subclass-of))
                                     next-up-classes))
               up-class))))

; ********************************************************************
; *** checks if any of the elements in "classes" (which could also be an atom)
;     is a subclass of up-class
(defun one-is-subclass-of (classes up-class)
  (let ((cl-list (inlist classes)) found)
      (do ((nxtcl (first cl-list) (first cl-list))
           (cl-list (rest cl-list) (rest cl-list)))
          ((or found (null nxtcl)) found)
          (setq found (is-subclass-of nxtcl up-class)))))

; ********************************************************************
; *** checks if "class" is a subclass of one of up-classes
(defun is-subclass-of-one-of (class up-classes)
 (let ((flat-up-classes (merge-union up-classes)))
  (cond ((null flat-up-classes) nil)
        ((is-subclass-of class (first flat-up-classes)) t)
        (t (is-subclass-of-one-of class (rest flat-up-classes))))))

; ********************************************************************
; *** returns all superclasses of class, including class itself
; *** level specifies how many steps must be made climbing the hierarchy
;     If level is NIL the result includes all superclasses
(defun get-superclasses (class &optional level)
  (cond ((or (null class) 
             (and (not (null level)) (<= level 0)))
            nil)
        (t (let ((classl (inlist class))
                 (newlevel (cond ((null level) nil) (t (1- level)))))
              (append classl 
                 (get-superclasses
                      (dropnil (flatten (mapcar #'(lambda (x) (get x 'subclass-of)) classl)))
                      newlevel))))))

; ********************************************************************
; *** checks if two classes have a common ancestor
;     if level is 1, the result is always nil, since the only level 1 ancestor
;     of a class is the class itself, but it is also an (improper) ancestor
;     of itself
;     if level is NIL, the result is always T, since all classes have ££entity
;     as common ancestor
(defun ont-siblings (class1 class2 &optional level)
  (and (not (is-subclass-of class1 class2))
       (not (is-subclass-of class2 class1))
       (not (null (intersection (get-superclasses class1 level) (get-superclasses class2 level))))))

; ********************************************************************
; *** a relation must have at least a domain or a range
(defun is-relation (ident)
  (or (not (null (get ident 'domain)))
      (not (null (get ident 'range)))))

; ********************************************************************
; *** relation restrictions
(defun is-restriction-of (relation up-relation)
  (cond ((or (null relation) (null up-relation))
          (exception 'semantic-error 
                  "PROC/onto-reasoning: Attempt to use a NIL ontology relation" 
                     relation up-relation))
        ((eq relation up-relation) t)
        (t (int-is-restriction-of (get relation 'restricts) up-relation))))

(defun int-is-restriction-of (next-up-relations up-relation)
  (cond ((null next-up-relations) nil)
        ((member up-relation next-up-relations) t)
        (t (int-is-restriction-of
               (merge-flatten (mapcar #'(lambda (x) (get x 'restricts))
                                     next-up-relations))
               up-relation))))

; ********************************************************************
; *** up-relation is a single one
;     relations may be one or a list
(defun one-is-restriction-of (relations up-relation)
  (cond ((or (null relations) (null up-relation))
          (exception 'semantic-error 
                  "PROC/onto-reasoning: Attempt to use a NIL ontology relation" 
                     relations up-relation))
        (t (int-is-restriction-of relations up-relation))))

; ********************************************************************
; *** up-relation is a single one
;     relations may be one or a list
(defun is-restriction-of-one-of (relation up-relations)
  (cond ((or (null relation) (null up-relations))
          (exception 'semantic-error 
                  "PROC/onto-reasoning: Attempt to use a NIL ontology relation" 
                     relation up-relations))
        ((int-is-restriction-of (inlist relation) (first up-relations)) t)
        ((null (rest up-relations)) nil)
        (t (is-restriction-of-one-of relation (rest up-relations)))))

;; ********************************************************************
; *** some operations must be blocked for basic datatypes.
;     here, I assume that a basic datatype is a direct subclass of ££datatype
(defun is-basic-datatype (class)
   (member '££datatype (get class 'subclass-of)))

;; ********************************************************************
;  *** an ontology relation is more general than another relation
(defun more-general-rel (relup reldown)
  (cond ((eq relup reldown) nil)
        (t (m-gen-r relup (get reldown 'restricts)))))

(defun m-gen-r (relup rellist)
  (cond ((null rellist) nil)
        ((member relup rellist) t)
        (t (m-gen-r relup (flatten (mapcar #'get-more-gen-rels rellist))))))

(defun get-more-gen-rels (rel)
  (get rel 'restricts))

; ********************************************************************
(defun class-inherit ()
  (int-cl-inh '££entity))

(defun int-cl-inh (class)
  (let ((subclasses (get class 'has-subclass)))
        ()))

; ********************************************************************
; *** this finds the shortest path between ontology nodes
;     Both node1 and node2 can be sets of nodes, so that is actually
;     carried out is a parallel search 
; *** In case "constraint" is non-null, it is a list of nodes through
;     which the found path must pass
; *** the top-level function just invokes the "cached" one and
;     saves the result in a file
(defun find-shortest-path (node1 node2 &optional constraint)
  (declare (special *ONTO-TIME-START*))
  (let ((cache (find-in-cache node1 node2 constraint))
        (same-node (check-same-node node1 node2)))
      (setq *ONTO-TIME-START* (get-internal-run-time))
      (cond ((not (null same-node)) 
               (setq cache (list same-node)))
            ((null cache)
              (setq cache
                (find-shortest-path-new node1 node2 constraint))
           ;(format t "Find-shortest-path; node1; ~a node2: ~a~% result: ~a~%" node1 node2 cache)
           ;
              (add-in-cache node1 node2 constraint cache)))
      cache))

; ********************************************************************
; *** this checks if node1 and node2 are the same ontology node
;     since either node1 or node2 or both can be lists, the check is less easy
(defun check-same-node (node1 node2)
   (cond ((atom node1)
           (cond ((atom node2)
                   (cond ((eq node1 node2) (list node1))
                         (t nil)))
                 ((member node1 node2) (list node1))))
         ((atom node2)
           (cond ((member node2 node1) (list node2))
                 (t nil)))
         (t (intersection node1 node2))))

; ********************************************************************
; *** an entry in the cache includes:
;     1. The first node (actually a list)
;     2. The second node (actually a list)
;     3. The constraint, i.e. a node through which the found path must
;        pass (actualy a list, possibly nil in case of absence of constraints)
;     4. The found shortest path
;     5. The number of accesses (for statistical purposes)
(defun find-in-cache (node1 node2 constraint)
  (declare (special *ONTOLOGY-CACHE*))
  (let (found)
  (do ((nxtitem (first *ONTOLOGY-CACHE*) (first tempcache))
       (tempcache (rest *ONTOLOGY-CACHE*) (rest tempcache))
       (newcache nil (cons nxtitem newcache)))
      ((or (null nxtitem) found)
   ; *** the next to update the statistics
         (cond (found (setq *ONTOLOGY-CACHE*
                        (append (reverse (rest newcache))
                                (list (list (first found) (second found) 
                                            (third found) (fourth found) 
                                            (1+ (fifth found))))
                                (cond ((null nxtitem) nil)
                                      (t (list nxtitem)))
                                tempcache))))
         (fourth found))
      (cond ((and (equal (first nxtitem) node1)
                  (equal (second nxtitem) node2)
                  (equal (third nxtitem) constraint))
               (setq found nxtitem))))))
     
; ********************************************************************
; *** this adds a new item in the cache of ontology paths
(defun add-in-cache (node1 node2 constraint result)
  (declare (special *ONTOLOGY-CACHE*))
  (setq *ONTOLOGY-CACHE*  
     (cons (list node1 node2 constraint result 1)
            *ONTOLOGY-CACHE*)))
  
; ********************************************************************
; *** a constraint can be an atom (in which case it is a concepts through
;     which the solution path must pass) or a list of the form 
;     (neg conc1 conc2 ...), in which case conc1, conc2 ... are concepts
;     through which the solution must not pass
(defun find-shortest-path-new (node1 node2 &optional constraint check)
  (cond ((or (null node1) (null node2))
          (exception 'semantic-error
                "PROC/onto-reasoning: Looking for a path to a nil node" node1 node2)))
  (let ((lnode1 (inlist node1))
        (lnode2 (inlist node2))
        (lconstr (inlist constraint)))
  ; *** inlist puts atoms in a singleton lists, and leaves lists unchanged
      (cond ((or (member nil (mapcar #'spl lnode1))
                 (member nil (mapcar #'spl lnode2)))
               (exception 'semantic-error 
                          "PROC/onto-reasoning: No property for an ontology concept" 
                             lnode1 lnode2))
            ((or (null constraint)
                 (and (listp constraint) (eq (first constraint) 'neg)))
               (int-find-sh-path (mapcar #'list lnode1) lnode2 nil constraint check 0))
            (t 
               ;(int-2-find-sh-path lnode1 lnode2
               ;           (mapcar #'list lconstr) '(0 nil))
               ;(int-2-find-sh-path lnode1 lnode2
               ;            (mapcar #'list lconstr) check nil nil 1000 1000)
               (int-2-find-sh-path lnode1 lnode2 (mapcar #'list lconstr) check)
                ))))

(defun fsp (a b &optional c check) 
  (declare (special *ONTO-TIME-START*))
  (defvar *debugv*)
   (setq *ONTO-TIME-START* (get-internal-run-time))
   (let ((result (find-shortest-path-new a b c check)))
      (format t "Result : ~s~% Time: ~s~%" result 
                 (- (get-internal-run-time) *ONTO-TIME-START*))
      result))

; ********************************************************************
(defun int-find-sh-path (prevpaths node2 backup-solution constraint &optional check numsteps)
; *** prevpaths is a list of paths, each ending in the initial node
;     (node1 of find-shortest-path).
;     The initial node is the last, since in the implementation, the paths are kept reversed
;     Each path is a sequence <nodeN linkN node[N-1] link[N-1] ... node1>
; *** node2 is the node to be reached
; *** constraint is nil or a negative constraint
; *** backup-solution is a not appealing solution already found,
;     which could be used in absence of better alternatives
(declare (special *ONTO-TIME-START* *SYSTEM-CONTEXT*))
   ;(format t "Entering int-find-sh-path; LEVEL ~a~% prevpaths ~a~%  node2 ~a~%  backup-solution ~a~%"
   ;              numsteps prevpaths node2 backup-solution)
   ; (break "")
;;   (cond ((and (not (null *ONTO-TIME-START*)) ;ALE: altrimenti in CLISP raggiunge sempre questo limite!
;;               (> (- (get-internal-run-time) *ONTO-TIME-START*) 500000))
;;            (exception 'semantic-error 
;;                  "PROC/onto-reasoning: Find shortest path timed out")))
; *** first check for redundancies in prevpaths (see comments in prune-rels)
  (setq prevpaths (prune-rels prevpaths))
  (cond ((null prevpaths) 
; *** end of search: failure or semi-failure
          (cond ((null backup-solution) 
                  (exception 'semantic-error 
                             "PROC/onto-reasoning: No path found in find-shortest-path"))
                (t backup-solution)))
; *** there are paths to work on
    (t (let (nxtsteps (numbinst 0) (prevcl nil) found)
   ; *** repeat the job for all open paths
      (do ((prevp (first prevpaths) (first prevpaths))
           (prevpaths (rest prevpaths) (rest prevpaths)))
   ; *** exit the do when the solution has been found or when all previous paths have
   ;     been suitably extended; if no solution has been found, recursion on the new
   ;     set of extended paths (nxtsteps)
          ((null prevp) 
             (cond (found found)
                   (t (int-find-sh-path nxtsteps node2 backup-solution 
                              constraint check (1+ numsteps)))))
     ; *** printing of infos for checking execution (debug)
         (cond (check
                  (cond ((and (is-an-instance (first prevp))
                              (not (is-a-class (first prevp))))
                     ; *** the next to control the number of printed instances in 
                     ;     case of controlled execution
                           (cond ((eq (third prevp) prevcl)
                                    (setq numbinst (1+ numbinst))
                                    (cond ((= numbinst 10)
                                            (format t "    ...~%"))
                                          ((< numbinst 10)
                                            (format t "Cammino: ~a~%" prevp))))
                                 (t (setq numbinst 1)
                                    (setq prevcl (third prevp))
                                    (format t "Cammino: ~a~%" prevp))))
                        (t (setq numbinst 0)
                           (setq prevcl nil)
                           (format t "Cammino: ~a~%" prevp)))))
  ;(format t "In int-find-sh-path; ~% prevp: ~a;~%" prevp)
  ;(break "fsp 2")
  ; *** check for solution found: the path under analysis ends in node2
  ;     but the node should not have been reached just as a superclass and
  ;     the path should not be unreasonable
         (cond ((and (memq (first prevp) node2)
                    ; (not (eq (second prevp) 'subclass-of))
                     (not (unreasonable-path prevp)))
  ;(format t "path found; prevp = ~a~%" prevp)
  ;(break "fsp")
                  (setq found (append1 found (reverse prevp))))
                ((memq (first prevp) node2)
      ; *** we have reached node2 (but the solution is unreasonable); save it as backup
      ;     and do nothing: this path is not extended so nxtsteps remains unchanged
                   (cond ((null backup-solution)
                             (setq backup-solution (list (reverse prevp))))))
      ; *** if node2 has not been reached, advance on the ontology graph
                (t (multiple-value-setq (nxtsteps prevpaths) 
                             (ontology-advance prevp nxtsteps prevpaths constraint)))))))))
          ; *** - prevp is the current path that can contribute to new "nxtsteps"
          ;     - nxtsteps are the new (extended) paths to be used in the next step
          ;     - prevpaths is the list of remaing old paths to possibly extend
          ; *** the assignment to prevpaths is due to the special treatment for subclasses:
          ;     the link is followed, by replacing the class by its superclass(es), without taking
          ;     this movement into account (inheritance).
          ;     It is horrible to change the loop range (prevpaths) during the
          ;     operations, but here it's the simplest solution
          ; *** actually, the movement is recorded (in order to avoid up-down
          ;     movements, but it does not count as a step)

; ********************************************************
; *** this makes a step ahead on the connections present in the ontology
;     a first part tests if advancing on a connection is reasonable. It
;     is split in two sub-parts. In the first one, we check if the whole
;     path to extend is reasonable; in case it is, every single connection
;     starting form the end of that path is checked to validate the move.
;     If both testing sub-parts are passed, then the single connection is
;     added to the path
; *** constraint is a "negative" constraint of the form (neg conc1 conc2 ...)
;     the solution must not pass through any of the nodes conc1, conc2, ...
(defun ontology-advance (prevp nxtsteps prevpaths constraint)
  (let (all-links (start-point (first prevp)) prev-link newpaths newnewpaths)
  ; *** start of the first part: check if the (existing) path to extend is reasonable
  ;     in case it is not
  ;(cond ((equal start-point '££comparison-relation)
  ;   (format t "Entering ontology-advance; ~% prevp: ~a~% prevpaths: ~a~%" prevp prevpaths)
  ;   (break "")))
     (cond ((or (unreasonable-path prevp)
               (intersection prevp (rest constraint)))
             (values nxtsteps prevpaths))
  ; ********* start of second sub-part: check the single links
       ; *** advance on prevp on all links
          (t (setq prev-link (second prevp))
             (setq all-links (cond ((symbolp start-point) (spl start-point))
       ; *** if one arrives at a datum (i.e. a string, not symbolp), then stop
                                   (t nil)))
             (do ((nxtlink (first all-links) (first all-links))
                  (nxtvalue (merge-union (second all-links)) (merge-union (second all-links)))
                  (all-links (rest (rest all-links)) (rest (rest all-links))))
                 ((null nxtlink)
                    ;(cond ((eq start-point '££has-value)
                    ;        (format t "Exiting ontology-advance;~% newpaths: ~a~% nxtsteps: ~a~% prevpaths: ~a~%" 
                    ;                    newpaths nxtsteps prevpaths)
                    ;        (break "")))
                    (values (append nxtsteps newpaths) prevpaths))
       ; *** the loops works by adding new extensions to newpaths
                    ;(format t "ontology-advance;~% nxtlink: ~a~% newpaths: ~a~%" nxtlink newpaths)
                    ;(break "")
                 (cond 
                       ;((and (are-inverses nxtlink prev-link)
                       ;      (memq (third prevp) nxtvalue))
       ; *** no movement along links already used in the inverse direction
       ;     i.e. CONC1 link CONC2 inv-link CONC1
       ;     Returning to a previous node through the same link is useless
       ;     "get-inverse" in PROC-ALL/loadfunctions
                       ;   nil)
                       ((or (and (eq nxtlink 'subclass-of)
                                 (neq prev-link 'has-subclass))
                            (and (eq nxtlink 'has-subclass)
                                 (memq (second (elim-has-subclass prevp)) '(range domain))))
                ; *** ancestors are used immediately (in this step)
                ;     also subclasses are used immediately, if the node acts as a
                ;     domain or range of a relation, since subclasses of a range or
                ;     domain are part of the range or domain
                          (dolist (singval nxtvalue)
                ; *** the dolist for multiple parents (block loops and going up to entity)
                               (cond ((and (not (member singval prevp))
                                           (neq singval '££entity))
       ; *** the link is put both in nxtsteps (in order to proceed from it) and in 
       ;     prevpaths (in order to neutralize the subclass-link).
                                        (setq nxtsteps 
                                           (append1 nxtsteps
                                              (cons singval (cons nxtlink prevp))))
                                        (setq prevpaths 
                                            (cond-add-new-path prevpaths prevp singval nxtlink))
                                        ))))
       ; *** stop paths linking two possible topics of a dialogue, since almost everything
       ;     can be such a topic, so any two nodes could be connected in this way
                       ((and (eq (first prevp) '&HAS-INFO-TOPIC)
                             (or (and (eq prev-link 'domain)
                                      (eq nxtlink 'domain-of))
                                 (and (eq prev-link 'domain-of)
                                      (eq nxtlink 'domain))))
                           nil)
          ; *** the movement is blocked for (remember that the paths are reversed):
          ;     1. funct and constraints (functionality information and constraints are not links)
          ;     2. has-subclass, followed by subclass-of (siblings) !!!!!!!!! REMOVED FOR ATLAS
          ;     3. subclass-of, followed by has-subclass (co-parents) !!!!!!!!! REMOVED FOR ATLAS
          ;     4. domain followed by domain-of (domain-of is a function)
          ;     5. range followed by range-of (range-of is a function)
                       ((or (eq nxtlink 'funct)
                            (eq nxtlink 'disjoint-from)
                            (eq nxtlink 'constraints)
                            (and (eq nxtlink 'domain)
                                 (eq prev-link 'domain-of))
                            (and (eq nxtlink 'range)
                                 (eq prev-link 'range-of))
                               )
                          nil)
      ; *** in all other cases, standard movement
                       (t (setq nxtvalue (merge-union nxtvalue))
           ; *** repeat for all single values
                          (dolist (singval nxtvalue)
           ; *** test for loops 
          ;     6. domain-of followed by domain (in case the two involved realation are restrictions
          ;        one of the other)
          ;     7. range-of followed by range (in case the two involved relations are restrictions
          ;        one of the other)
          ;     8. movement to siblings is blocked for disjoint sets
                               (cond ((member singval prevp) nil)
                                     ((or (and (eq nxtlink 'domain-of)
                                               (eq prev-link 'domain)
                                               (or (is-restriction-of singval (third prevp))
                                                   (is-restriction-of (third prevp) nxtvalue)))
                                          (and (eq nxtlink 'range-of)
                                               (eq prev-link 'range)
                                               (or (is-restriction-of singval (third prevp))
                                                   (is-restriction-of (third prevp) singval)))
                                          (and (eq nxtlink 'has-subclass)
                                               (eq prev-link 'subclass-of)
                                               (are-disjoint (third prevp) singval (first prevp)))
                                          (and (are-inverses nxtlink prev-link)
                                               (eq (third prevp) singval)))
   ;(break "domain-of")
                                        nil)
           ; *** add an extended path
           ; !!!! here we must block the expansion of the same ancestor
           ;      reached via different paths
           ;      e.g. ££time-spec subclass-of ££entity vs.
           ;           ££time-spec subclass-of ££dialogue-topic subclass-of ££entity
                                     (t (setq newnewpaths
                                            (cond-add-new-path newpaths prevp singval nxtlink)
                                            ;(cons (cons singval (cons nxtlink prevp))
                                            ;       newpaths)
                                                   )
                                       ;(cond ((equal (length newpaths) (length newnewpaths))
                                       ;         (format t "Path-not-added; newpaths: ~a~%" newpaths)
                                       ;          (break "")))
                                        (setq newpaths newnewpaths)
                                         ))))))))))

;; ********************************************************************
; *** a path (c1 has-subclass c2 has-subclass c3 rel1 c4 rel2 c5)
;     is reduced to (c3 rel1 c4 rel2 c5)
(defun elim-has-subclass (path)
  (cond ((eq (second path) 'has-subclass)
           (elim-has-subclass (rest (rest path))))
        (t path)))

;; ********************************************************************
(defun are-disjoint (firstconc secondconc upperconc)
   (member (list secondconc upperconc) 
           (get firstconc 'disjoint-from)
           :test #'equal))

;; ********************************************************************
; *** a new expansion is added just in case no shorter path to the same
;     concept already exists in the previously computed paths
(defun cond-add-new-path (oldpaths firstpart conc link)
  (let (found (paths oldpaths))
  (do ((nxtold (first paths) (first paths))
       (paths (rest paths) (rest paths)))
      ((or (null nxtold) found)
        (cond (found oldpaths)
              (t (cons (cons conc (cons link firstpart)) oldpaths))))
      (cond ((eq conc (first nxtold))
               (cond ((>= (+ 2 (length firstpart)) (length nxtold))
                        (setq found t))))))))

;; ********************************************************************
; *** this checks if a path includes any unreasonable subpath
(defun unreasonable-path (prevp)
  (declare (special *SYSTEM-CONTEXT*))
  (let ((start-point (first prevp)))
    (and (> (length prevp) 1)
  ; *** paths of length 1 are always ok
         (or 
      ; *** stop paths passing from ££entity: too general!!! (unless prevp has
      ;     length 1, so that ££entity is the starting point)
             (and (eq '££entity (first prevp))
                  (eq 'subclass-of (second prevp)))
      ; *** stop paths passing from ££dialogue-topic, unless they start with ££dialogue
      ;     we do not talk about the topic, apart from the main topic of the sentence
             (and (memq '££dialogue-topic prevp)
                  (neq '££dialogue (ult prevp)))
      ; *** stop paths passing from instances, since instances of relations are
      ;     not stored in the ontology
             (and (neq *SYSTEM-CONTEXT* 'atlas)	; In ATLAS, relation instances are in the KB
                  (is-an-instance start-point)
                  (not (is-a-class start-point)))
      ; *** The same for any type of description and ££reified-relations
             (and ;(is-subclass-of (third prevp) '££description)
                  (eq (second prevp) 'subclass-of)
                  (eq start-point '££reified-relation))
      ; *** stop paths including both a relation restriction and an inverse
      ;     relation restriction
             (check-rel-restr prevp)))))

;; ********************************************************************
; *** this check is made on a set of paths, so it is outside the ontology-advance
;     function, which works on the single path
; *** Currently, this only aims at avoiding the use of too general
;     relations. For instance, in searching for the connection between
;     ££day and ££day-descr, all ancestors of ££day-descr (as ££time-descr
;     and ££entity) are reached. But, since there is the &day-description
;     relation, it is useless to expand the paths involving the more
;     general &time-description and &has-description
(defun prune-rels (paths)
  (let (rels useless-rels rempaths)
    (dolist (path paths)
       (cond ((and (is-an-ont-relation (first path))
                   (not (member (first path) rels)))
                (setq rels (cons (first path) rels)))))
   ; *** now in rels we have all the relations reached in the last step
   ;     check if any of them is a restriction of another one
    (dolist (rel rels)
       (cond ((and (more-general-rel '&has-description rel)
                   (member rel rels :test #'more-general-rel))
               (setq useless-rels (cons rel useless-rels)))))
   ; *** now in useless-rels we have all the too general relations
   ;    prune the paths
    (dolist (path paths rempaths)
       (cond ((not (member (first path) useless-rels))
               (setq rempaths (cons path rempaths)))))))
    
;; ********************************************************************
;  *** an ontology relation is something that has a domain
(defun is-an-ont-relation (node)
   (not (null (get node 'domain))))

; ********************************************************************
; *** looks for the shortest paths connecting one of the nodes in lnode1 to one of the nodes in 
;     lnode2 and passing from one of the nodes in lconstr
;     Actually, lconstr is a list of singleton lists, each containing a constaint node,
;     representing an initial path
(defun int-2-find-sh-path (lnode1 lnode2 constraint &optional check)
  (let ((first-half (int-find-sh-path constraint lnode1 nil nil check 0))
        (second-half (int-find-sh-path constraint lnode2 nil nil check 0))
        nxtpath1 final-result)
; *** the first arg of int-find-sh-path is a list of paths, each ending in the initial node
;     The initial node is the last, since in the implementation, the paths are kept reversed
;     Each path is a sequence <nodeN linkN node[N-1] link[N-1] ... node1>
; *** the second arg is the node to be reached
; *** the third argument is a constraint that, in this case is nil, since int-find-sh-path
;     handles only negative constraints
; *** the fourth argument is a backup-solution, i.e. a not appealing solution already found,
;     which could be used in absence of better alternatives
; *** the last argument is a record of the level of nesting of the call to int-find-sh-path,
;     useful for debugging
     (dolist (revpath1 first-half (reverse final-result))
        (setq nxtpath1 (rev-links (reverse revpath1)))
        (dolist (nxtpath2 second-half)
           (cond ((eq (first revpath1) (first nxtpath2))
                   (setq final-result (cons (append nxtpath1 (rest nxtpath2)) final-result))))))))
         

; ********************************************************************
; *** this finds the shortest path from node1 to node2, passing from
;     "prevpaths", which, initially, is a list of nodes (the ones through
;     which the solution path is constrained to pass).
; *** It starts from each node in prevpaths (nodeX) and then moves one step.
;     When node1 or node2 are reached, there are three cases:
;     1. the found path is of the form <node1 node1a ... nodeX>
;        and there is, among the previously found paths (partial-sol)
;        one of the form <node2 node2a ... nodeX>. This means that a
;        complete solution has been found. However, this could not be
;        the best (shortest) one: it could be that <node1 node1a ... nodeX>
;        is of length N, <node2 node2a ... nodeX> is of length M, but a
;        shortest solution exists passing through the constraint nodeY. 
;        This cannot happen just in case B+K > N+M, where N and M are
;        as above, B is the currently best partial path, and K is the
;        current distance from the constraint nodes. So:
;        1a. If B+K >= N+M, close the search
;        1b. If B+K < N+M, proceed in the search, saving in complete-sol
;            the solution found
;     2. Neither node1 nor node2 have been reached:
;        Proceed in the search
; >>> INPUT:
;     node1: one of the nodes to connect
;     node2: the other node to connect
;     prevpaths: all the paths along which we are moving
;     partial-sol: a set of pairs, each of which includes a partial solution 
;            (half-path) currently found and its length
;     complete-sol: the complete solutions currently found
;     best-l: a pair including the length of the shortest path to node1
;             and the length of the shortest path to node2
(defun int-2-find-sh-path-old (node1 node2 prevpaths check 
                              partial-sol complete-sol best-1-l best-2-l)
; *** prevpaths is a list of paths, each starting from a central node
;     (constraint of find-shortest-path).
;     Each path is a sequence <nodeK linkK-1 nodeK-1 linkK-2 ... sodeX>,-ths wher
;     nodeX is one of the constraints
;     (N.B in the implementation, the paths are kept reversed)
  (declare (special *ONTO-TIME-START* *debugv*))
  (cond ((> (- (get-internal-run-time) *ONTO-TIME-START*) 5000)
           (exception 'semantic-error 
                 "PROC/onto-reasoning: Find-shortest-path-2 timed out")))
  ;(format t "  Prevpaths: ~a~% best-1-l: ~a~% best-2-l: ~a~%" prevpaths best-1-l best-2-l)
  ;(format t "  node 1: ~a   node 2: ~a~%  best-1-l: ~a~% best-2-l: ~a~%" node1 node2 best-1-l best-2-l)
(setq *debugv* prevpaths)
  ;(break "int-2-find-sh-path: Next Level")
  (cond ((null prevpaths) 
; *** end of search: failure
           (exception 'semantic-error 
                      "PROC/onto-reasoning: no path found in find-shortest-path-2")))
  (cond (check
            (format t "  Prevpaths: ~a~%" prevpaths)
            (break "int-2-find-sh-path: Next Level")))
  (let (nxtsteps newly-reached result
        (new-best-1-l best-1-l)
        (new-best-2-l best-2-l)
        (new-complete-sol complete-sol)
        (new-partial-sol partial-sol)
        (best-prevp-l 1000)
        (testvar1 nil)
        (testvar2 nil)
        )
; ****************** FIRST SECTION ***********************************
; *** check if any path in prevpaths satisfies the search conditions
; ********************************************************************
   ; *** repeat the job for all open paths
      (do* ((prevp (first prevpaths) (first prevpaths))
            (prevpaths (rest prevpaths) (rest prevpaths))
            (prevp-l (get-path-length prevp) (get-path-length prevp)))
      ; *** the search stops when there are no more paths to follow
          ((null prevp))
         (cond (check (format t "cammini: ~a~%" prevp)))
         (cond ((< prevp-l best-prevp-l)
                 (setq best-prevp-l prevp-l)))
  ; *** if the length of the new path is less than the previous best, replace it
         (cond ((intersection prevp node1)
            ; (format t " Node 1 reached?; prevp: ~a~% node1; ~a~%" prevp node1)
             (cond ((is-a-good-sol prevp)
            ;     (cond ((not testvar1) (break "node1 is a good sol")))
                 (setq testvar1 t)
  ; *** node1 has been reached ******************************************
  ;     member-or-subcl checks if the first item in prevp is equal, or linked only
  ;     via subclass links, to one of the nodes in node1
                 (cond (check (format t "  Node1 reached; path: ~a~%" prevp)
                              (break "int-2-find-sh-path")))
                 (let ((prev-partial
                         (find-partial-sol new-partial-sol node2 (ult prevp))))
      ; *** "ult prevp" is the constraint node from which this partial
      ;     solution has been found
  ;(format t "after find-partial-sol: ~% found path: ~a~%" prev-partial)
  ;(break "")
                     (cond ((not (null prev-partial))
             ; *** save this complete solution
                              (setq new-complete-sol
                                 (append1 new-complete-sol 
                                     (build-complete-sol prevp prevp-l prev-partial 1))))
             ; *** this is not a complete solution
                           (t (setq new-partial-sol
                                   (append1 new-partial-sol (list prevp prevp-l)))))
                     (cond ((eq new-best-1-l 1000)
  ;(format t "after assignment to new-best-1-l; prevp-l: ~a~%" prevp-l)
  ;(break "")
                               (setq new-best-1-l prevp-l))))))))
  ; *** node2 has been reached ******************************************
  ;     member-or-subcl checks if the first item in prevp is equal, or linked only
  ;     via subclass links, to one of the nodes in node2
         (cond ((intersection prevp node2)
                ;(member-or-subcl prevp node2)
            ; (format t " Node 2 reached?; prevp: ~a~% node2; ~a~%" prevp node2)
             (cond ((is-a-good-sol prevp)
            ;     (cond ((not testvar2) (break "node2 is a good sol")))
                 (setq testvar2 t)
                 (cond (check
                           (format t "  Node2 reached; path: ~a~%" prevp)
                           (break "int-2-find-sh-path")))
                 (let ((prev-partial
                         (find-partial-sol new-partial-sol node1 (ult prevp))))
      ; *** "ult prevp" is the constraint node from which a partial solution has been found
      ;     find-partial-sols simply extracts from new-partial-sol the ones that actually
      ;     lead from node1 the the intermediate node. This is needed, since new-partial-sol
      ;     was obtained by parallel advance, so that it may include also paths that do
      ;     not end in (ult prevp)
  ;(format t "after find-partial-sol: ~% found path: ~a~% prevp: ~a~%" prev-partial prevp)
  ;(break "")
                     (cond ((not (null prev-partial))
             ; *** save this complete solution
                              (setq new-complete-sol
                                 (append1 new-complete-sol
                                     (build-complete-sol prevp prevp-l prev-partial 2))))
             ; *** this is not a complete solution; save it as partial
                           (t (setq new-partial-sol
                                   (append1 new-partial-sol (list prevp prevp-l)))))
                     (cond ((eq new-best-2-l 1000)
                                 (setq new-best-2-l prevp-l)))))))))
; ****************** SECOND SECTION ***********************************
 ; *** now, we have possibly found a new complete solution; we must check if
 ;     it is the best one. Note that even if in this step no new solution has
 ;     been found, the check is useful, since a previous complete solution
 ;     could have now become the best (after advancement in the ontology)
; *********************************************************************
         (let ((best-complete-sol (get-best-sol new-complete-sol)))
    ; *** best-complete-sol is the currently best complete solution;
    ;     N.B. get-best-sol returns <nil 3000> in case no complete solution
    ;          does currently exist
; (format t "Entering the second section; best-complete-sol: ~a~%" best-complete-sol)
; (break "")
             (cond ( ;(not (null best-complete-sol))
                      (< (second best-complete-sol)
    ; *** new-best-1-l and new-best-2-l are the length of the shortest paths
    ;     leading to node1 and node2 (respectively) from any of the constraint
    ;     nodes
                        (+ (min new-best-1-l new-best-2-l) best-prevp-l))
                     (list (first best-complete-sol)))
    ; ************* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *************************
    ; *** we have not found the (certainly) best solution, so advance on the ontology
                  (t 
 ;(format t "Entering the loop for advancing; prevpaths: ~a~%" prevpaths)
 ;(break "")
                     (do* ((prevp (first prevpaths) (first prevpaths))
                           (prevpaths (rest prevpaths) (rest prevpaths)))
      ; *** the advancement stops when there are no more paths to follow
                        ((null prevp)
 ;(format t "Exiting the loop for advancing; prevpaths: ~a~% nxtsteps: ~a~%" prevpaths nxtsteps)
 ;(format t "    testvar1: ~a~% testvar2: ~a~% " testvar1 testvar2)
 ;(break "")
                           (setq newly-reached (elimdup (get-arrival-nodes nxtsteps)))
     ; *** the first two branches of the next cond are inconsistent with the rest of the
     ;     algorithm. They try to anticipate the closure of the search, to avoid timing out
                           (cond ((and testvar1 (intersection newly-reached node2))
                                    (let ((new-half-sol (get-half-solution nxtsteps node2))
                                          prev-partial new-complete-sols)
          ; *** new-half-sol is a list of paths
                                      (setq result
                                        (dolist (nxtnewhalf new-half-sol (reverse new-complete-sols))
                                          (setq prev-partial
                                            (find-partial-sol new-partial-sol node1 (ult nxtnewhalf)))
                                          (setq new-complete-sols
                                             (cons 
                                                (first (build-complete-sol nxtnewhalf
                                                   (get-path-length nxtnewhalf) prev-partial 2))
                                                new-complete-sols))))
                                      (setq *debugv* result)
                                      (break "Solution found 1")
                                      result))
                                 ((and testvar2 (intersection newly-reached node1))
                                    (let ((new-half-sol (get-half-solution nxtsteps node1))
                                          prev-partial new-complete-sols)
          ; *** new-half-sol is a list of paths
                                      (setq result
                                        (dolist (nxtnewhalf new-half-sol (reverse new-complete-sols))
                                          (setq prev-partial
                                            (find-partial-sol new-partial-sol node2 (ult nxtnewhalf)))
                                          (setq new-complete-sols
                                             (cons
                                                (first (build-complete-sol nxtnewhalf
                                                    (get-path-length nxtnewhalf) prev-partial 1))
                                                new-complete-sols))))
                                      (setq *debugv* result)
                                      (break "Solution found 2")
                                      result))
                                 ((and (intersection newly-reached node1)
                                       (intersection newly-reached node2))
                                    (let ((new-half-sol-1 (get-half-solution nxtsteps node1))
                                          (new-half-sol-2 (get-half-solution nxtsteps node2))
                                          new-complete-sols)
                                      (setq result
                                        (dolist (nxtnewhalf1 new-half-sol-1 
                                                         (reverse new-complete-sols))
                                           (dolist (nxtnewhalf2 new-half-sol-2)
                                              (setq new-complete-sols
                                                  (cons (append (rev-links nxtnewhalf1)
                                                                (rest (reverse nxtnewhalf2)))
                                                        new-complete-sols)))))
                                      (setq *debugv* result)
                                      (break "Solution found 3")
                                      result))
                                 (t (int-2-find-sh-path-old node1 node2 nxtsteps check 
                                       new-partial-sol new-complete-sol
                                       new-best-1-l new-best-2-l))))
;(cond ((eq (ult prevp) '&property-evaluated)
;         (format t "Loop for advancing; prevp: ~a~% prevpaths: ~a~% nxtsteps: ~a~%"
;              prevp prevpaths nxtsteps)
;         (break "")))
                        (multiple-value-setq (nxtsteps prevpaths) 
                             (ontology-advance prevp nxtsteps prevpaths nil))))))))

; ******************************************************************
; *** gets all nodes to which we have arrived with the new movement
(defun get-arrival-nodes (paths)
  (let (result)
     (dolist (nxtpath paths (reverse result))
        (do ((nxtconc (first nxtpath) (second nxtpath))
             (nxtpath (rest nxtpath) (rest (rest nxtpath))))
            ((or (null nxtconc)
                 (not (memq (first nxtpath) '(subclass-of has-subclass))))
               (cond ((not (null nxtconc))
                         (setq result (cons nxtconc result)))))
            (setq result (cons nxtconc result))))))

; ******************************************************************
; *** extracts from paths all paths that terminate in one of the nodes in "node"
;     or whose termination is connected to them only via subclass-of and has-subclass links
; *** if node = (c1) then (remember that the ptahs are reversed)
;     - (c1 range c2)                                    ---> ok
;     - (cx subclass-of cy has-subclass c1 range cz ...) ---> ok
;     - (cx subclass-of cy range c1 domain-of ...)	 ---> ok
;     - (cx subclass-of cy range cz domain-of c1 ...)    ---> no
(defun get-half-solution (paths node)
   (let (result fullpath found)
     (dolist (nxtpath paths (reverse result))
        (setq fullpath nxtpath)
        (do ((nxtconc (first nxtpath) (second nxtpath))
             (nxtpath (rest nxtpath) (rest (rest nxtpath))))
            ((or found
                 (null nxtconc)
                 (not (memq (first nxtpath) '(subclass-of has-subclass))))
               (cond ((or (memq nxtconc node)
                          (and (memq (first nxtpath) '(range range-of domain domain-of))
                               (memq (second nxtpath) node)))
                         (setq result (cons fullpath result)))))
            (cond ((memq nxtconc node)
                     (setq result (cons fullpath result))
                     (setq found t)))))))
  
; ******************************************************************
; *** builds a complete solution from two partial solution
;     prev-partial is a half-solution found previously
;     prevp is the path of the new half solution
;     prevp-l is the length of the new half solution
; *** A solution is a pair (path path-length)
;     If direction is 1, then the previous path connects the pivot with node2
;     otherwise it connects the pivot with node1
(defun build-complete-sol (prevp prevp-l prev-partial direction)
  (let ((newlength (1- (+ (second prev-partial) prevp-l))))
  (cond ((= direction 1)
           (list (append (rev-links prevp) (rest (reverse (first prev-partial)))) newlength))
        (t (list (append (rev-links (first prev-partial)) (rest (reverse prevp))) newlength)))))

; ******************************************************************
(defun get-path-length (path)
; *** a path is assumed to start and end with a concept, relation, or
;     instance; 
  (cond ((null path) 0)
        ((eq 1 (length path)) 1)
        ((member (second path) 
            '(subclass-of has-subclass has-instance instance-of))
           (+ .1 (get-path-length (rest (rest path)))))
        (t (1+ (get-path-length (rest (rest path)))))))
 
; ******************************************************************
(defun get-best-sol (solutions)
; *** "solutions" is a list of pairs <path path-length>
  (int-get-best-s solutions '(nil 3000)))

(defun int-get-best-s (solutions best)
  (cond ((null solutions) best)
        ((< (second (first solutions)) (second best))
           (int-get-best-s (rest solutions) (first solutions)))
        (t (int-get-best-s (rest solutions) best))))
        
; ******************************************************************
; *** partial-sols is a list of partial solutions, each of which has the
;     form <path length>
;     By "partial solution" we mean a path from one of the two end
;     nodes to the constraint (intermediate node)
;     where length is the conventional length of the path
;     (see get-path-length above)
; *** end-node is a list (as node1 and node2 in find-shortest-path),
;     while start-node is an atom (the constraint node actually found
;     in a path)
; *** it returns the first path (i.e. the pair <path length>) that
;     actually links one of the end nodes (starting point of the path)
;     to the constraint (ending point of the path)
;     it returns nil if no such path exists
(defun find-partial-sol (partial-sols end-node start-node)
 ; (format t "find-partial-sol: ~% start-node: ~a~% end-node: ~a~% partial-sols: ~a~%"
 ;       start-node end-node partial-sols)
 ; (break "")
  (cond ((null partial-sols) nil)
        ((and (member (first (first (first partial-sols))) end-node)
              (eq start-node (ult (first (first partial-sols)))))
   ; *** (first partial-sols): the first solution in the list
   ;     (first (first partial-sols)): the path of this solution
   ;     (first (first (first partial-sols))): node1 or node2
   ;     (ult (first (first partial-sols))): the constraint node
           (first partial-sols))
        (t (find-partial-sol (rest partial-sols) end-node start-node))))
        
; ******************************************************************
(defun is-a-good-sol (path)
 (and (not (are-there-inverses path))
      (not (member 'restricts path))      ; *** this and the next avoid
      (not (member 'restricted-by path))  ;     also moving among roles
      (not (member '&has-description path))))

; ******************************************************************
; *** returns true if the first element of "path" or any following element
;     reachable only via subclass links is a member of nodelist
(defun member-or-subcl (path nodelist)
  (cond ((null path) nil)
        ((memq (first path) nodelist) t)
        ((memq (second path) '(subclass-of has-subclass))
           (member-or-subcl (rest (rest path)) nodelist))
        (t nil)))
  
; ********************************************************************
; *** this handles the 'union operator
(defun merge-union (vals)
  (cond ((null vals) nil)
        ((atom (first vals))
          (cons (first vals) (merge-union (rest vals))))
        ((eq 'union (first (first vals)))
          (append (rest (first vals)) (merge-union (rest vals))))
        (t (cons (first vals) (merge-union (rest vals))))))

; ********************************************************************
; *** is there any pair of adjacent inverse links in path?
(defun are-there-inverses (path)
  (cond ((null (fourth path)) nil)
        ((are-inverses (second path) (fourth path))
           (cond ((memq (second path) 
                     '(range range-of argument arg-of domain domain-of has-subclass subclass-of))
     ; *** in case of (X range Y range-of Z), they are real inverses in case X=Z
                    (cond ((eq (first path) (fifth path)) t)
                          (t (are-there-inverses (rest (rest path))))))
                 (t t)))
        (t (are-there-inverses (rest (rest path))))))

; ********************************************************************
(defun are-inverses (rel1 rel2)
  (and (not (null rel1))
       (not (null rel2))
       (or (eq rel1 (get-inverse rel2))
           (eq rel2 (get-inverse rel1)))))

; ********************************************************************
; *** this "reverses" a path, by changing all links to their inverses.
;     path is assumed to start with a concept and to alternate concepts
;     and links
(defun rev-links (path)
  (cond ((null path) nil)
        ((null (rest path)) path)
        (t (cons (first path)
                 (cons (get-inverse (second path))
                       (rev-links (rest (rest path))))))))

; ********************************************************************
; *** this checks if a given syntactic dependent can play a given role
;     Usual matters of case-role mapping are not taken into account
;     In articular, VERB-SUBJ is mapped to "agent"
(defun check-complem-compatib (compl-mean verb-mean link)
 (let (deep-role local-role 
       (compl-lexmean (get-flatavm-feat-val compl-mean 'lexmean))
       (verb-lexmean (get-flatavm-feat-val verb-mean 'lexmean)))
  (cond ((eq link 'VERB-SUBJ)
          (cond ((eq verb-lexmean '££to-be) t)
      ; *** anything can be the subject of "to be"
      ;     I prefer this procedural solution instead of writing in the ontology
      ;     that "to be" has any entity as &agent, since it does not seem
      ;     to me that "to be" has an agent
                (t (setq deep-role '&agent)
                   (setq local-role 
                      (find-ont-relation verb-lexmean 'range deep-role))
                   (cond ((and (not (null local-role))
                           (is-subclass-of compl-lexmean 
                                 (first (get local-role 'domain)))) t)
                         (t nil)))))
        (t (exception 'semantic-error
                      "PROC/onto-reasoning: checking complement compatibility for a non-subject")))))

; ********************************************************************
; *** this finds any relation having "node" as its range or domain
;     and restricting the "upper-rel" relation
(defun find-ont-relation (sem range-or-domain upper-rel)
  (let ((relations (get sem (get-inverse range-or-domain)))
        found)
; *** if we want the agent of "££buy" (node), we must find the relation such that
;     ££buy is its range (so, find all "range-of", i.e. inverse of "range")
     (do ((nextrel (first relations) (first relations))
          (relations (rest relations) (rest relations)))
         ((or (null nextrel) found) found)
         (cond ((is-restriction-of nextrel upper-rel)
                  (setq found nextrel))))))

; ********************************************************************
; *** this looks for a sequence ... downr1 restricted-by rel restricts downr2 ...
;     Since paths are reversed, this refers to a more specific relation
;     (downr2) from which a sister (downr1) was reached. 
(defun check-rel-restr (path)
  (let ((restr (member 'restricted-by path)))
     (eq (third restr) 'restricts)))

