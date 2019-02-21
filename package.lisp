;;;; package.lisp
;;;; Created by Ninjacop123
;;;; Wednesday, January 9th, 2019
(defpackage #:cl-ruby
  (:use #:cl #:cffi)
           ;; cl-ruby.lisp
           ;; func
  (:export #:init-ruby
           #:end-ruby
           #:stop-ruby
           #:in-ruby
           #:evaluate
          ;#:evaluate-or-die
           #:class-instance
           #:class-accessor
           #:defrubyclass
           #:defrubysubclass
           #:class-method 
           #:module-method 
           #:global-method
          ;#:load-script-or-die
           #:ruby-funcall
           #:clear-last-exception
           ;; cffi-fun-defs.lisp
           ;; Ruby/C wrapper functions
           #:ruby-nil
           #:int2num
           #:num2int
           #:dbl2num
           #:num2dbl
           #:module
           #:submodule
           #:define-class
           #:define-subclass
           #:ruby-intern
           #:script
           #:require-script
           #:load-script 
           #:last-exception
           ;; ruby-types.lisp
           ;; Variables/Constantes
           #:rbstr->clstr
           #:clstr->rbstr
           #:undef-const 
           #:undef-module-const
           #:defrubyvar
           #:defrubygconst
           #:defrubymconst))
           ;; ruby-classes.lisp
           ;; All the symbols in this file are
           ;; exported in there via `export`
