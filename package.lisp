;;;; package.lisp
;;;; Created by Ninjacop123
;;;; Wednesday, January 9th, 2019
(defpackage #:cl-ruby
  (:use #:cl #:cffi)
  ;; cl-ruby.lisp
  (:export #:init-ruby
           #:end-ruby
           #:in-ruby
           #:evaluate
           #:evaluate-or-die
           #:module
           #:submodule
           #:class-with-super
           #:define-class
           #:subclass-with-super
           #:define-subclass
           #:class-method
           #:module-method
           #:global
           #:const 
           #:module-const
           #:undef
           #:const-call 
           #:module-const-call 
           #:ruby-funcall 
           #:require-script
           #:load-script 
           #:load-script-or-die))

