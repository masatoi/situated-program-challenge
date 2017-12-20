#|
  This file is a part of situated-program-challenge project.
|#

(defsystem "situated-program-challenge"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "situated-program-challenge"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "situated-program-challenge-test"))))
