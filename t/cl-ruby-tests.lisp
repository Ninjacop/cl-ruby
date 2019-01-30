;;;; cl-ruby-test.lisp
;;;; Created by Ninjacop123
;;;; Wednesday, January 9th, 2019
;;;; Tests for each major/every function in cl-ruby
;;;; that are timed to see how fast they are 
(defpackage #:cl-ruby-tests
  (:use :cl))
(in-package #:cl-ruby-tests)

(defun init-and-end-ruby-test ()
  (time (cl-ruby:init-ruby)
  (cl-ruby:end-ruby))) ; should return NIL or nothing

(defun init-and-stop-ruby-test ()
  (time (cl-ruby:init-ruby)
    (cl-ruby:stop-ruby)))

(defun %init-stop-try-init-test ()
  (cl-ruby:init-ruby)
  (cl-ruby:stop-ruby)
  (cl-ruby:init-ruby)) ; this 2nd `init-ruby` should quit out of the impl on purpose

(defun in-and-evaluate-ruby-test ()
  (time (cl-ruby:in-ruby 
    ;; should print the string + 8 or 4 depending on the OS
    (cl-ruby:evaluate "puts 'Hello Common Lisp!'"))))

(defun evaluate-or-die-test ()
  (time (cl-ruby:in-ruby
    (cl-ruby:evaluate-or-die "puts 'Hello Common Lisp!" 
                             "You forgot a closing apostrophe!")))
  (cl-ruby:in-ruby
    (cl-ruby:evaluate-or-die "puts 'Hello Common Lisp!'" 
                             "This should eval fine")))

(defun class-instance-test ()
  (time (cl-ruby:in-ruby
    (cl-ruby:load-script (cl-ruby:clstr->rbstr "./example.rb") 0)
    (class-instance ~object~ "Example"))))

(defun class-accessor-test ()
  (time (cl-ruby:in-ruby
    (cl-ruby:load-script (cl-ruby:clstr->rbstr "./example.rb") 0)
    (class-accessor ~object~ "Example"))))

(defun define-class-test ()
  (time (cl-ruby:in-ruby 
    (cl-ruby:define-class "Example" ~object~))))

(defun define-subclass-test ()
  (time (cl-ruby:in-ruby
    (let ((parent-class (cl-ruby:define-class "Example" ~object~)))
    (cl-ruby:define-subclass "Example" parent-class ~object~)))))

(defun class-method-test ()
  (time (cl-ruby:in-ruby
    (cl-ruby:class-method "Klass" "method" (x y) 
      (+ x y))
    (|Example.method| 1 1))))

(defun module-method-test ()
  (time (cl-ruby:in-ruby 
    (cl-ruby:class-method "Modul" "method" (x y)
      (- x y))
    (|Modul.method| 1 1))))

(defun global-method-test ()
  (time (cl-ruby:in-ruby
    (cl-ruby:global-method "method" (x y)
      (* x y))
    (|global.method| 2 2))))

(defun load-script-or-die-test ()
  (time (cl-ruby:in-ruby
    (cl-ruby:load-script-or-die (cl-ruby:clstr->rbstr "../example.rb") 0 "File does not exist")
    ; File example.rb exists in the same folder that this file is in, but it doesn't exist
    ; in the parent directory
    (cl-ruby:load-script-or-die (cl-ruby:clstr->rbstr "./example.rb") 0 "File exists!"))))
    ; In this case, it searches this folder, so we're good

(defun ruby-funcall-test ()
  ; this will use defrubyvar & num2int, which the test is farther down in the file
  (time (cl-ruby:in-ruby
    (let ((arr (cl-ruby:defrubyvar "arr" :array)))
      (cl-ruby:ruby-funcall arr "push" 5)
      (cl-ruby:num2int (cl-ruby:ruby-funcall arr "pop"))))))

(defun clear-last-exception-test ()
  ; uses last-exception, test is further down 
  (time (cl-ruby:in-ruby
    (cl-ruby:evaluate-or-die "puts 'a" "Missing Apostrophe")
    (cl-ruby:last-exception)
    (cl-ruby:clear-last-exception)
    (cl-ruby:last-exception)))

(defun ruby-nil-test ()
  (equal 8 (cl-ruby:ruby-nil))) ; Ruby's NIL is at the 8/9th index of the Ruby C type enum

(defun int2num-test ()
  (time (cl-ruby:in-ruby
    (let ((number 1))
      (equal 1 number) ; T
      (equal 1 (cl-ruby:int2num number)))))) ; NIL

(defun num2int-test ()
  (time (cl-ruby:in-ruby
    (let ((number (cl-ruby:int2num 1)))
      (equal 1 number) ; NIL
      (equal 1 (cl-ruby:num2int number)))))) ; T

(defun dbl2num-test ()
  (time (cl-ruby:in-ruby
    (let ((number 2.1))
      (equal 2.1 number) ; T
      (equal 2.1 (cl-ruby:dbl2num number)))))) ; NIL

(defun num2dbl-test ()
  (time (cl-ruby:in-ruby
    (let ((number (cl-ruby:dbl2num 2.1)))
      (equal 2.1 number) ; NIL
      (equal 2.1 (cl-ruby:num2dbl number)))))) ; T

(defun module-test ()
  (time (cl-ruby:in-ruby 
    (numberp (cl-ruby:module "Example"))))) ; T, because it returns a pointer 

(defun submodule-test ()
  (time (cl-ruby:in-ruby
    (numberp (cl-ruby:submodule "Subexample" 
               (cl-ruby:module "Example")))))) ; T

(defun define-class-test ()
  (time (cl-ruby:in-ruby
    (numberp (cl-ruby:define-class "Example" cl-ruby:~object~))))) ; T

(defun define-subclass-test ()
  (time (cl-ruby:in-ruby
    (numberp (cl-ruby:define-subclass "Example" 
               (define-class "ExampleParent" cl-ruby:~object~) cl-ruby:~object~)))))
    ; T

(defun ruby-intern-test () 
  (time (cl-ruby:in-ruby
    (numberp (cl-ruby:ruby-intern "Some String")))))

(defun script-test ()
  (time (cl-ruby:in-ruby 
    (cl-ruby:script "My Script"))))

(defun require-script-test ()
  (time (cl-ruby:in-ruby
    (cl-ruby:require-script "./example.rb"))))

(defun load-script-test ()
  (time (cl-ruby:in-ruby
    (cl-ruby:load-script (clstr->rbstr "./example.rb") 0))))

(defun last-exception-test () 
  (time (cl-ruby:in-ruby
    (cl-ruby:evaluate-or-die "puts 'a" "An error occurred!")
    (cl-ruby:last-exception))))

(defun clstr->rbstr-test ()
  (time (cl-ruby:in-ruby
    (numberp (cl-ruby:clstr->rbstr "a string")))))

(defun rbstr->clstr-test ()
  (time (cl-ruby:in-ruby
    (let ((str-ptr (cl-ruby:clstr->rbstr "a string")))
      (stringp (cl-ruby:rbstr->clstr str-ptr))))))

(defun defrubyvar-test ()
  (time (cl-ruby:in-ruby
    (let ((inte (cl-ruby:defrubyvar "inte" :int 1))
          (str (cl-ruby:defrubyvar "stri" :string "example"))
          (flt  (cl-ruby:defrubyvar "flt" :float 2.1))
          (arr (cl-ruby:defrubyvar "arr" :array))
          (arr-with-len (cl-ruby:defrubyvar "arrWithLen" :array :len 2))
          (hsh (cl-ruby:defrubyvar "hsh" :hash)))
          (+ (cl-ruby:num2int inte) 1) ; 2
          (concatenate 'string "an " (cl-ruby:rbstr->clstr str)) ; "an example"
          (- (cl-ruby:num2dbl flt) 1.0) ; 1.1
          (cl-ruby:ruby-funcall arr "push" 5)
          (cl-ruby:num2int (cl-ruby:ruby-funcall arr "length")) ; 1
          (cl-ruby:num2int (cl-ruby:ruby-funcall arr-with-len "length")) ; 2
          (cl-ruby:ruby-funcall hsh "store" "asdf" 6)
          (cl-ruby:num2int (cl-ruby:ruby-funcall hsh "[]" "asdf")))))) ; 6

(defun defrubygconst-test ()
  (time (cl-ruby:in-ruby 
    (let ((inte (cl-ruby:defrubygconst "INTE" :int 1))
          (str (cl-ruby:defrubygconst "STRI" :string "example"))
          (flt  (cl-ruby:defrubygconst "FLT" :float 2.1))
          (arr (cl-ruby:defrubygconst "ARR" :array))
          (arr-with-len (cl-ruby:defrubygconst "ARRWITHLEN" :array :len 2))
          (hsh (cl-ruby:defrubygconst "HSH" :hash)))
          (+ (cl-ruby:num2int inte) 1) ; 2
          (concatenate 'string "an " (cl-ruby:rbstr->clstr str)) ; "an example"
          (- (cl-ruby:num2dbl flt) 1.0) ; 1.1
          (cl-ruby:ruby-funcall arr "push" 5)
          (cl-ruby:num2int (cl-ruby:ruby-funcall arr "length")) ; 1
          (cl-ruby:num2int (cl-ruby:ruby-funcall arr-with-len "length")) ; 2
          (cl-ruby:ruby-funcall hsh "store" "asdf" 6)
          (cl-ruby:num2int (cl-ruby:ruby-funcall hsh "[]" "asdf")))))) ; 6

(defun defrubymconst-test ()
  (time (cl-ruby:in-ruby
    (let ((inte (cl-ruby:defrubymconst "Example" "INTE" :int 1))
          (str (cl-ruby:defrubymconst "Example" "STRI" :string "example"))
          (flt  (cl-ruby:defrubymconst "Example" "FLT" :float 2.1))
          (arr (cl-ruby:defrubymconst "Example " "ARR" :array))
          (arr-with-len (cl-ruby:defrubymconst "Example ""ARRWITHLEN" :array :len 2))
          (hsh (cl-ruby:defrubymconst "HSH" :hash)))
          (+ (cl-ruby:num2int inte) 1) ; 2
          (concatenate 'string "an " (cl-ruby:rbstr->clstr str)) ; "an example"
          (- (cl-ruby:num2dbl flt) 1.0) ; 1.1
          (cl-ruby:ruby-funcall arr "push" 5)
          (cl-ruby:num2int (cl-ruby:ruby-funcall arr "length")) ; 1
          (cl-ruby:num2int (cl-ruby:ruby-funcall arr-with-len "length")) ; 2
          (cl-ruby:ruby-funcall hsh "store" "asdf" 6)
          (cl-ruby:num2int (cl-ruby:ruby-funcall hsh "[]" "asdf")))))) ; 6)))
 
(defun undef-const-test ()
  (time (cl-ruby:in-ruby
    (let ((const (cl-ruby:defrubygconst "MYCONST" :int 1)))
      (+ (num2int const) 1) ; 2
      (undef-const "MYCONST")
      (+ (num2int const) 1))))) ; 9, because the enum position of Ruby NIL is 8, therefore 8 + 1 = 9

(defun undef-module-const-test ()
  (time (cl-ruby:in-ruby
    (let ((mconst (cl-ruby:defrubymconst "Example" "MYCONSTTWO" :string "Common Lisp")))
      (princ (cl-ruby:rbstr->clstr mconst)) ; "Common Lisp"
      (cl-ruby:undef-module-const "Example" "MYCONSTTWO")
      (princ mconst))))) ; 8