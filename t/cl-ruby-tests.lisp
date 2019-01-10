;;;; cl-ruby-test.lisp
;;;; Created by Ninjacop123
;;;; Wednesday, January 9th, 2019
(defpackage #:cl-ruby-tests
  (:use :cl))
(in-package #:cl-ruby-tests)

(defun init-and-end-ruby-test ()
  (cl-ruby:init-ruby)
  (cl-ruby:end-ruby)) ; should return NIL or nothing

(defun in-and-evaluate-ruby-test ()
  (cl-ruby:in-ruby 
    ;; should print the string + 8 or 4 depending on the OS
    (cl-ruby:evaluate "puts 'Hello Common Lisp!'"))) 

(defun evaluate-or-die-test ()
  (cl-ruby:in-ruby
    (cl-ruby:evaluate-or-die "puts 'Hello Common Lisp!" 
                             "You forgot a closing apostrophe!"))
  (cl-ruby:in-ruby
    (cl-ruby:evaluate-or-die "puts 'Hello Common Lisp!'" 
                             "This should eval fine"))

(defun module-test ()
  (cl-ruby:in-ruby
    (cl-ruby:module "Example")))

(defun submodule-test () 
  (cl-ruby:in-ruby 
    (defparameter *example-module* (cl-ruby:module "Example0"))
    (defparameter *example-module-2* 
      (cl-ruby:submodule "Example1" *example-module*)))

(defun class-with-super-test ()
  (cl-ruby:in-ruby
    (defparameter *example-class-0*
      ; Shouldn't work at the moment because ~basic-object~ is not
      ; exported
      (cl-ruby:class-with-super "Example2" ~basic-object~))) 

(defun define-class-test ()
  (cl-ruby:in-ruby
    (defparameter *example-class-2* (define-class "Example3"))))

(defun subclass-with-super-test ()
  (cl-ruby:in-ruby
    (defparameter *example-class-5* 
      (cl-ruby:define-class "Example6"))
    (defparameter *example-class-6*
      (cl-ruby:subclass-with-super "Example7" *example-class-5* ~array~))))

(defun define-subclass-test ()
  (cl-ruby:in-ruby
    (defparameter *example-class-7*
      (cl-ruby:define-class "Example8"))
    (defparameter *example-class-8*
      (cl-ruby:define-subclass "Example9" *example-class-7*))))

(defun global-const-test ()
  (cl-ruby:in-ruby
    (defparameter *my-const-0* (cl-ruby:const "MYCONST" 4))))

(defun global-const-call-test ()
  (cl-ruby:in-ruby
    (defparameter *my-const-1* (cl-ruby:const "ANOTHERCONST" 4))
    (cl-ruby:const-call *my-const-1*)))
  ; (cl-ruby:const-call "ANOTHERCONST") ?

(defun global-const-undef-test ()
  (cl-ruby:in-ruby
    (defparameter *my-const-2* (cl-ruby:const "LASTCONST" 4))
    (cl-ruby:undef *my-const-2*)
    (cl-ruby:const-call *my-const-2*))) ; should result in an error
  ; (cl-ruby:const-call "LASTCONST") ? 

(defun ruby-require-test ()
  (cl-ruby:in-ruby
    (cl-ruby:require-script "example"))) ; .rb extension not needed

(defun ruby-load-test () 
  (cl-ruby:in-ruby
    (cl-ruby:load-script "example.rb")))

(defun ruby-load-or-die-test ()
  (cl-ruby:in-ruby
    (cl-ruby:load-script-or-die "example.rb" "File not found or something")))
