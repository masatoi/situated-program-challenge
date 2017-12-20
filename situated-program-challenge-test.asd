#|
  This file is a part of situated-program-challenge project.
|#

(defsystem "situated-program-challenge-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("situated-program-challenge"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "situated-program-challenge"))))
  :description "Test system for situated-program-challenge"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
