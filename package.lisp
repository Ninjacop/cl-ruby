;;;; package.lisp
;;;; Created by Ninjacop123
;;;; Wednesday, January 9th, 2019
(defpackage #:cl-ruby
  (:use #:cl #:cffi)
  ;; cl-ruby.lisp
  (:export #:evaulate
           #:evaulate-or-die
           #:module
           #:submodule
           #:class-with-super
           #:class
           #:nested-class
           #:define-subclass
           #:subclass
           #:nested-subclass
           #:class-method
           #:module-method
           #:global
           #:const 
           #:module-const
           #:undef
           #:const-call 
           #:module-const-call 
           #:ruby-funcall 
           #:require 
           #:load-script 
           #:load-script-or-die))
