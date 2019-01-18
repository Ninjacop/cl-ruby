;;;; cl-ruby-test.asd
;;;; Created by Ninjacop123
;;;; Wednesday, January 9th, 2019
(asdf:defsystem cl-ruby-test
  :description "A Common Lisp to Ruby FFI."
  :author "Ninjacop123"
  :license "GNU GPLv3"
  :version "0.0.2"
  :serial t 
  :components ((:module "t"
                :components 
                ((:file "cl-ruby-test"))))
  :defsystem-depends-on (:cl-ruby))