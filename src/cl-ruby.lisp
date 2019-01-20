;;;; cl-ruby.lisp
;;;; Created by Ninjacop123
;;;; Wednesday, January 9th, 2019
;;;; Basic functions that need to be elaborated upon
;;;; from their C counterparts
(in-package #:cl-ruby)

(cffi:define-foreign-library cl-ruby
    (t (:default "~/Desktop/more/lisp/cl-ruby/src/cl-ruby"))) ; the dylib/dll/so part is omitted
                              ; TODO update paths and make sure this can
                              ; be found in specific directories/make it
                              ; specified

(use-foreign-library cl-ruby)

;;; The uintptr_t declaration is here because if it is declared elsewhere
;;; it results in an error 
;; Ruby values are custom, but in actuality, 
;; the overarching value type in C is a uintptr_t
(cffi:defctype uintptr_t #.(ecase 
  (cffi:foreign-type-size :pointer) 
  (4 :uint32) (8 :uint64))) ; x86-64 and x64 specifications 


;; Global variable for segfault/memory protection with starting/ending the 
;; Ruby VM
(defparameter *ruby-is-running* nil)

;; Global variable for storing Ruby CFFI callbacks
(defvar *ruby-callbacks* (make-hash-table))

;;; Common Lisp functions that use the C functions and make it 
;;; safer/has more fallthroughs so segfaults are less likely to occur
(defun init-ruby ()
  "Initializes the Ruby VM, but with extra precaution."
  (if *ruby-is-running* 
      (error "Ruby is already running!")
      (progn
        (ruby-init-ruby)
        (setq *ruby-is-running* t))))

(defun end-ruby ()
  "Ends and cleans up the Ruby VM, but with extra precaution."
  (if *ruby-is-running*
      (progn 
        (ruby-end-ruby)
        (setq *ruby-is-running* nil))
      (error "There is no Ruby VM running.")))

(defmacro in-ruby (&body body)
  "Shortcut macro for starting the Ruby VM, evaluating code, 
   and then cleaning up the VM regardless of errors."
  `(progn
    (init-ruby)
    (unwind-protect 
        (progn ,@body)
      (end-ruby))))

(defun evaluate (code)
  "Evaluate Ruby code, where the Ruby code is stored in the string
   `code`"
  (if *ruby-is-running* 
      (ruby-evaluate code)
      (error "There is no Ruby VM running.")))

(defmacro evaluate-or-die (code exception)
  "Evaluate Ruby code and throw an exception/error when something 
   goes wrong/segfault."
  `(unwind-protect 
      (evaluate ,code) ; TODO fix the SEGFAULT here prints out, 
                       ; which should be supressed
    (progn (end-ruby)  ; and replaced with this
           (error ,exception)))) 

(defun define-class (name)
  "Shortcut for defining a Ruby/C compatible class."
  (cffi:foreign-funcall-pointer (cffi:callback define-class-callback)
    () :string name uintptr_t))

(defun define-subclass (name parent-name)
  "Defines a Ruby class that is inherited without a superclass."
  (subclass name parent-name ~basic-object~))

;;; Most of the code below has been added/created by zulu-inuoe
;;; Special thanks to them! 
(defun %register-callback (name signature)
  "Add a Ruby/CFFI callback into a callbacks hash table, where 
   `name` is the key and `signature` is the signature of the 
   function."
  (setf (gethash name *ruby-callbacks*) signature))

(defun %signature-equal-p (s1 s2)
  "Compare the given function signatures."
  (= s1 s2))

(defun %existing-signature-match-p (name signature)
  "Check if the given function name and signature are 
   declared/included in the callback hash table and if 
   they are the same signature."
  (multiple-value-bind (existing defined-p)
      (gethash name *ruby-callbacks*)
    (and defined-p (%signature-equal-p signature existing))))

(defun %make-signature (args)
  "Find the length of the function arguments, thereby declaring the 
   signature of a given function"
  (length args))

(defun %make-method-name (class-name method-name)
  "Make a Ruby method name by its convention -- Class.Method.
   This also works with Modules as well -- Module.Method"
  (intern (format nil "~A.~A" class-name method-name)))

(defun %make-global-func-name (global-name)
  "Global Ruby methods/functions need to be unique, so a 
   `global.` is added. "
  (intern (format nil "global.~A" global-name)))

(defmacro class-method (ruby-class name (&rest args) &body body)
  "Define a Ruby class method given the class that it's defined under, the 
   name of the target function, it's arguments and the body of that function."
  (let ((method-name (%make-method-name ruby-class name))
        (signature (%make-signature args)))
    `(progn
       (defun ,method-name ,args ,@body)
       (unless (%existing-signature-match-p ',method-name ,signature)
         (%register-callback ',method-name ,signature)
         ,(let ((ffi-args (mapcar (lambda (name) (list name 'uintptr_t)) args)))
            `(cffi:defcallback ,method-name uintptr_t ,ffi-args (,method-name ,@args)))
         (ruby-class-method (cffi:translate-to-foreign (define-class ,ruby-class) 'uintptr_t)
                            ,name (cffi:callback ,method-name) ,(length args)))
    (values ',method-name))))

(defmacro module-method (ruby-module name (&rest args) &body body)
  "Define a Ruby module method given the module that it's defined under, the 
   name of the target function, it's arguments and the body of that function."
  (let ((method-name (%make-method-name ruby-module name))
        (signature (%make-signature args)))
    `(progn
       (defun ,method-name ,args ,@body)
       (unless (%existing-signature-match-p ',method-name ,signature)
         (%register-callback ',method-name ,signature)
         ,(let ((ffi-args (mapcar (lambda (name) (list name 'uintptr_t)) args)))
            `(cffi:defcallback ,method-name uintptr_t ,ffi-args (,method-name ,@args)))
         (ruby-module-method (cffi:translate-to-foreign (module ,ruby-module) 'uintptr_t)
                            ,name (cffi:callback ,method-name) ,(length args)))
    (values ',method-name))))

(defmacro global-method (name (&rest args) &body body)
  "Define a Ruby global function that is defined under the toplevel, with 
   the given name of the function, its arguments and body."
   (let ((global-name (%make-global-func-name name))
         (signature (%make-signature args)))
    `(progn
       (defun ,global-name ,args ,@body)
       (unless (%existing-signature-match-p ',global-name ,signature)
         (%register-callback ',global-name ,signature)
         ,(let ((ffi-args (mapcar (lambda (name) (list name 'uintptr_t)) args)))
            `(cffi:defcallback ,global-name uintptr_t ,ffi-args (,global-name ,@args)))
         (ruby-global ,name (cffi:callback ,global-name) ,(length args)))
    (values ',global-name))))