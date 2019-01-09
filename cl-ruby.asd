;;;; cl-ruby.asd
;;;; Created by Ninjacop123
;;;; Wednesday, January 9th, 2019
(asdf:defsystem #:cl-ruby
  :description "A Common Lisp to Ruby FFI."
  :author "Ninjacop123"
  :license  "GNU GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "cl-ruby")
                 (:file "ruby-types"))))
