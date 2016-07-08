;(defvar *HOME-DIR* "/Users/mazzei/lavori/Projects/ATLAS/softExt/tup/2012-JAN-SYSTEM/SYSTEM/")
(defvar *HOME-DIR* (directory-namestring (car (directory #P""))))
(defvar *LISP-CHAR-SET-ID* 'UTF-8)
;; *** this encodes the character encoding scheme used by the Lisp machine

;; (defvar *CHAR-SET-ID* 'ISO-8859-1) ; Iso Latin 1: ISO/IEC 8859-1
(defvar *CHAR-SET-ID* 'UTF-8)

;; *** this encodes the character encoding scheme of the input data

(defvar *SYSTEM-CONTEXT* 'tule)
					;(defvar *SYSTEM-CONTEXT* 'hops-dial)
					; other possible values are hops and hops-dial
					; *** the next few functions are needed for "build-file-name", which is in
					;     turn needed for loading all files, among which "utilities"

(defun uconcat (&rest x)
  (cond (x (format nil "~{~a~}" x)) ))
(defun concat (&rest x) (identity (intern (string (apply #'uconcat x)))))
(defun build-file-name (filen) (string (concat *HOME-DIR* filen)))
(defun build-subdir-file-name (filen sub) (string (concat *HOME-DIR* sub filen)))
(defun removePrefix (prefisso-stringa) (cond ((search "italian (1) " prefisso-stringa) (subseq prefisso-stringa 11))  (t prefisso-stringa)))

;;********* General utilities ***************************************
(load (build-file-name "ALLLANG/PROC-ALL/utilities"))
(defvar loadresult) ; for testing the correct loading of knowledge bases
(defvar *LANGUAGE*)
(defvar *TEMP*)
					; *** the three next variables hold the name of the lisp properties associated
					;     with the dictionary entries
(defvar *MAIN-DICT*)
(defvar *PROPER-DICT*)
(defvar *SIGLE-DICT*)
(defvar *PRINT-LEV*)
(defvar *TREE-FORMAT*) ; may be "tut" or "avm"; set in main or in chunk-parser
(setq *TREE-FORMAT*  'avm)
					;********* The variable associated with the ontology cache *********************
(defvar *ONTO-CACHE-FILE* (build-file-name "ALLLANG/KB-ALL/SEMANT-KB-ALL/onto-cache.dat"))
(setq *LANGUAGE* 'italian)
(setq *MAIN-DICT* 'diz-gw)
(setq *PROPER-DICT* 'diz-pn)
(setq *SIGLE-DICT* 'diz-sig)



(defun server2 ()  
  (setq temp (concatenate 'string (first EXT:*ARGS*) "load-ita"))
  (load temp :extra-file-types EXT:*ARGS*)
  (format t "ho caricato il parser...~%")

  (LET ((server (SOCKET:SOCKET-SERVER 3001)));;2728
    (FORMAT t "~&Waiting for a connection on ~S:~D~%"
	    (SOCKET:SOCKET-SERVER-HOST server) (SOCKET:SOCKET-SERVER-PORT server))
    (UNWIND-PROTECT
	 ;; infinite loop, terminate with Control+C
	 (LOOP (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-ACCEPT server :EXTERNAL-FORMAT charset:UTF-8))
		 (MULTIPLE-VALUE-BIND (local-host local-port) (SOCKET:SOCKET-STREAM-LOCAL socket)
		   (MULTIPLE-VALUE-BIND (remote-host remote-port) (SOCKET:SOCKET-STREAM-PEER socket)
		     (FORMAT T "~&Connection: ~S:~D -- ~S:~D~%"
			     remote-host remote-port local-host local-port)))
		 
		 ;;beg Ale
		 (setq line-val (removePrefix (read-line socket nil nil)))
		 (format t "RICEVUTA: ~S~%" line-val)
		 (setq tagres (hops-ana-text+tag line-val))
		 (setq parseres (mergeresult (start-parse-sentences tagres))) ; è qui il parsing!!!!
					; (print parseres) 
		 (setq albero (reshuffle-tree parseres))
					;(print albero)
		 (format socket "~s~%" albero)
		 (force-output socket)
					;(format t "~%ho spedito un messaggio al client:~% ~a~%" albero) 
		 (format t "Parsato e chiudo.~%" line-val)
		 ;;end Ale
		 ;;close socket (lo stream) dovrebbe essere inutile
		 (close socket)))
      (SOCKET:SOCKET-SERVER-CLOSE server))))

(defun server ()  
  (setq temp (concatenate 'string (first EXT:*ARGS*) "load-ita"))
  (load temp :extra-file-types EXT:*ARGS*)
  (format t "ho caricato il parser...~%")

  (setq temp nil)
  (do  () (temp)		
    (setq sock (socket-server (parse-integer "3001")))
    (format t "listening...")
    (setq connection (socket-wait sock))

    (cond
      (connection 
					;(format t "~%received call...")
       (setq stream (socket-accept sock :EXTERNAL-FORMAT charset:UTF-8))
       (setq line-val (removePrefix (read-line stream nil nil)))
       (format t "~%RICEVUTA: ~A" line-val)
       (setq tagres (hops-ana-text+tag line-val))
       (setq parseres (mergeresult (start-parse-sentences tagres))) ; è qui il parsing!!!!
					; (print parseres) ;@dd
       (setq albero (reshuffle-tree parseres))
					;(print albero)
       (format stream "~s~%" albero)
       (force-output stream)
       (format t "~%ho spedito un messaggio al client:~% ~a~%" albero) ;@dd
       (close stream)
       (socket-server-close sock)
       )
      (t
       (setq temp 1)))))

(server2)
