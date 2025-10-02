(asdf:defsystem #:honami
  :description "A modular Hunchentoot-based website with add-ons"
  :depends-on (#:hunchentoot #:cl-who #:cl-json #:cl-dbi
	       #:drakma #:local-time #:cl-ppcre #:split-sequence
	       #:uuid #:sqlite #:ironclad)
  :components ((:file "src/utils")
	       (:file "src/database")
	       (:file "src/auth" :depends-on ("src/database"))
	       (:file "src/login" :depends-on ("src/auth" "src/database"))
	       (:file "src/dashboard" :depends-on ("src/auth" "src/database"))))
