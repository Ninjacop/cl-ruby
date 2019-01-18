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
           #:define-class
           #:define-subclass)
  ;; cffi-fun-defs.lisp
  (:export #:module
           #:submodule
           #:class-with-super
           #:subclass-with-super
           #:class-method
           #:module-method
           #:global
           #:define-int 
           #:const 
           #:module-const
           #:undef
           #:const-call 
           #:module-const-call 
           #:ruby-funcall 
           #:script
           #:require-script
           #:load-script 
           #:load-script-or-die
           #:last-exception)
  ;; ruby-types.lisp
  (:export #:var-call
           #:defrubyvar))

