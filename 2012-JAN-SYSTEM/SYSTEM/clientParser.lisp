(DEFUN chiama-parser-socket (frase &OPTIONAL (host "127.0.0.1") (port 3001))
  "Semplice client per interrogare il parser con i socket"
  (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-CONNECT port host :EXTERNAL-FORMAT charset:UTF-8))
    (unwind-protect
	 ;; Open connection
	 (progn
	   (format t "> Sending request to ~a:~a...~%" host port)
	   ;; Send request
					;	   (http-send-line socket (format nil "GET ~a HTTP/0.9~%~%" path))
	   (format socket "~a~%" frase)
	   (force-output socket)	   
	   ;; Read response and output it
	   (format t "> Received response:~%")
	   (loop
	      (let ((line (read-line socket nil nil)))
		(unless line (return))
		(format t "~a~%" line))))      

      ;; Close socket before exiting.
      (close socket))))

