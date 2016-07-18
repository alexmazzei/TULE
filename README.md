# TULE

* This git project contains the last release of the TUrin Linguistic
  Enviroment, i.e. TULE.  TULE is a set of common lisp programs to
  perform NLP tasks for the Italian Language. In particular, TULE
  contains:
  
  - A sentence splitter
  - A tokenizer
  - A morphological analyzer and disambiguator
  - A PoS (part of speech) tagger
  - A Dependecy rule-based syntactic parser
  - A semantic interpreter based on ontologies (experimental)

  TULE has been developed by the Prof. Leonardo Lesmo in more than
  twenty years. Leonardo died October 1th 2013. People that met him
  knows how much energy, time and passion he spent in developing
  TULE. Leonardo wanted to share TULE with everybody, so we distribute
  it with GPL license.
  
  The directory docs contains a number of documents that we 

* Very small tutorial to use TULE:

  1. If you have Allegro common lisp installed, you can use it. As alternative, Install clisp.

  2. The parser can be used in two differen ways:

     A. by calling the"tule" file from CL interpreter. In this case
     you can call "tule" lisp file from the CL prompt. In order to
     speed-up the execution you can compile the lisp code by calling
     the "compile-all" lisp file.

     B. by using a socket based server. In this case you need to call
     the server ( "serverStarter.sh" ) and you can use a graphical
     client ("java -jar viewerTULETUT-locale-3001.jar", developed by
     Livio Robaldo), or in alternative a very basical textual client
     ("clientParser.lisp").

   
* NOTE: if you did not meet Leonardo, he was "the" free spirit. One
  sentece that he liked to cite was: "Chi trova dolce la propria terra
  è solo un tenero dilettante; chi trova dolci tutte le terre è un
  uomo che si è incamminato già su una buona via, ma è perfetto solo
  chi si sente straniero in ogni luogo" (Ugo di San Vittore)
