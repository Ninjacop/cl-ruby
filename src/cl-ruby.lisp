;;;; cl-ruby.lisp
;;;; Created by Ninjacop123
;;;; Wednesday, January 9th, 2019
;;;; Basic functions that need to be elaborated upon
;;;; from their C counterparts
(in-package #:cl-ruby)

(cffi:define-foreign-library cl-ruby
    (t (:default "libruby"))) ; not using specific OS specifications because this is simpler

(use-foreign-library cl-ruby)

;;; The uintptr_t declaration is here because if it is declared elsewhere
;;; it results in an error 
;;; Ruby values are custom, but in actuality, 
;;; the overarching value type in C is a uintptr_t
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
  "Initializes the Ruby VM."
  (if *ruby-is-running* 
      (error "Ruby is already running!")
      (progn (ruby-init-ruby)
        (ruby-init-loadpath)
        (setq *ruby-is-running* t))))

(defun end-ruby () 
  "Ends/stops the Ruby VM, but `(init-ruby)` can be called again."
  (if *ruby-is-running*
      (progn (ruby-finalize)
        (setq *ruby-is-running* nil))
      (error "There is no Ruby VM running.")))

(defun stop-ruby ()
  "Ends and cleans up the Ruby VM, entirely. Fully shuts down the 
   Ruby VM, so `(init-ruby)` cannot be called again in the same
   session."
  (if *ruby-is-running*
      (progn (ruby-end-ruby 0)
        (setq *ruby-is-running* nil))
      (error "There is no Ruby VM running.")))

(defmacro in-ruby (&body body)
  "Shortcut macro for starting the Ruby VM, evaluating code, 
   and then cleaning up the VM regardless of errors."
  `(progn (init-ruby)
    (unwind-protect 
        (progn ,@body)
      (end-ruby))))

(defun evaluate (code)
  "Evaluate Ruby code, where the Ruby code is stored in the string
   `code`"
  (if *ruby-is-running* 
      (ruby-evaluate code)
      (error "There is no Ruby VM running.")))

(defun evaluate-or-die (code exception)
  "Evaluate Ruby code and throw an exception/error when something 
   goes wrong/segfault."
  (cffi:with-foreign-object (state :int)
    (let ((result (ruby-eval-or-die str state)))
      (when (/= 0 state)
        (format t "~A" exception)
        (finish-output))))
  (values))

(defun class-instance (module-name class-name &rest self-args)
  (if self-args ; if self.class-function needs any arguments, or if there are any
      (let ((signature (%make-signature self-args))
            (ruby-class (ruby-get-const module-name (ruby-intern class-name))))
        (cffi:with-foreign-object (ruby-args 'uintptr_t signature)
          (dotimes (i signature)
            (setf (mem-aref ruby-args 'uintptr_t i) 
                  (cond ((integerp (nth i args)) (int2num (nth i args)))
                        ((stringp (nth i args)) (clstr->rbstr (nth i args)))
                        ((floatp (nth i args)) (dbl2num (nth i args)))
                        (t (error "~S's type is not an acceptable type" (nth i args))))))
          (new-ruby-class-instance signature ruby-args ruby-class)))
      (new-ruby-class-instance 0 (cffi:null-pointer) (ruby-get-const module-name 
                                                                    (ruby-intern class-name)))))

(defun class-accessor (module-name class-name)
  (ruby-get-const module-name (ruby-intern class-name)))

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

(defmacro class-method (super ruby-class name (&rest args) &body body)
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
         (ruby-class-method (cffi:translate-to-foreign (defrubyclass ,ruby-class ,super) 'uintptr_t)
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

(defun load-script-or-die (name value exception)
    (cffi:with-foreign-object (state :int)
    (let ((result (load-script-protect name value state)))
      (when (/= 0 state)
        (format t "~A" exception)
        (finish-output))))
  (values))

(defun ruby-funcall (object func-name &rest args)
  (if args ; if there are any arguments
      (let ((signature (%make-signature args)))
        (cffi:with-foreign-object (ruby-args 'uintptr_t signature)
          (dotimes (i signature)
            (setf (mem-aref ruby-args 'uintptr_t i) 
                  (cond ((integerp (nth i args)) (int2num (nth i args)))
                        ((stringp (nth i args)) (clstr->rbstr (nth i args)))
                        ((floatp (nth i args)) (dbl2num (nth i args)))
                        (t (error "~S's type is not an acceptable type" (nth i args))))))
          (rb-funcall-v object (ruby-intern func-name) signature ruby-args)))
      (rb-funcall-v object (ruby-intern func-name) 0 (cffi:null-pointer))))

(defun clear-last-exception ()
  (set-last-exception (ruby-nil)))