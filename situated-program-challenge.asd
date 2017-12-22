#|
  This file is a part of situated-program-challenge project.
|#

(defsystem "situated-program-challenge"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:clack :ningle :mito :cl-markup :jonathan :dexador :anaphora :woo)

  
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "server" :depends-on ("package"))
                 (:file "db" :depends-on ("package"))
                 (:file "routes" :depends-on ("package" "server" "db")))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "situated-program-challenge-test"))))
