;;;; cl-ruby.lisp
;;;; Created by Ninjacop123
;;;; Wednesday, January 9th, 2019

(in-package #:cl-ruby)

(cffi:define-foreign-library cl-ruby
    (t (:default "cl-ruby")))

(use-foriegn-library cl-ruby)

;; Ruby values are custom, but in actuality, 
;; the overarching value type in C is a uintptr_t
(cffi:defctype uintptr_t #.(ecase 
  (cffi:foreign-type-size :pointer) 
  (4 :uint32) (8 :uint64))) ; x86-64 and x64 specifications 

(cffi:defcfun ("ruby_eval" evaluate) uintptr_t
  "Evaluate a given Ruby expression/expressions."
  (code :string))

(cffi:defcfun ("ruby_eval_or_die" evaluate-or-die) uintptr_t
  "Evaluate a given Ruby expression and throw an error if  
   applicable."
  (code :string)
  (exception :string))

(cffi:defcfun ("ruby_define_module" module) :void
  "Define a Ruby module. Ex - `module Example`."
  (module-name :string))

(cffi:defcfun ("ruby_define_submodule" submodule) :void 
  "Define a nested Ruby module. Ex - `module Example::Submodule`."
  (submodule-name :string)
  (module-name uintptr_t))

(cffi:defcfun ("ruby_define_class" class-with-super) :void
  "Defines a Ruby class. Ex - `class Example`."
  (name :string)
  (super-name uintptr_t))

(cffi:defcallback define-class :void ((name :string)
                                    (super-name uintptr_t))
  "Since uintptr_t is only accessable in CFFI, this 
   creates a Ruby/C compatible class with no specific superclass"
  (ruby-class name ~basic-object~))

(cffi:defcallback nested-class :void ((name :string) 
                                 (parent-name uintptr_t))
  "This creates a compatible Ruby/C class with a parent.
   Ex - `class Example < Example2`, but with no specific superclass"
  (define-subclass name parent-name ~basic-object~))

(cffi:defcfun ("ruby_define_subclass" subclass) :void
  "Defines a Ruby class that is inherited. Ex - `class Example < Example2`.
   "
  (name :string)
  (parent-name uintptr_t)
  (super-name uintptr_t))

(cffi:defcallback nested-subclass :void ((name :string)
                                        (parent-name uintptr_t)
                                        (super-name uintptr_t)) 
  "This creates a compatible Ruby/C class with a parent and specific
   superclass. Ex - `class Example < Example2 < Array`."
  (define-sub-class name parent-name super-name))

(cffi:defcfun ("ruby_define_class_method" ruby-class-method) :void
  "Defines a Ruby class method."
  (class-name uintptr_t)
  (func-name :string)
  (func uintptr_t)
  (argc :int))

; todo, fix this to where it's very similar to cffi:defcfun
(defmacro class-method (ruby-class name &key (return-type :void) 
                        &rest args
                        &body body)
  "Shortcut for declaring a Ruby class method."
  `(cffi:defcallback ,name ,return-type ,args ,@body)
  (ruby-class-method ,ruby-class ,name (cffi:callback ,name) (length ,args)))

(cffi:defcfun ("ruby_define_module_method" ruby-module-method) :void
  "Defines a Ruby module method."
  (module-name uintptr_t)
  (func-name :string)
  (func uintptr_t)
  (argc :int))

; todo, fix this also to where it's similar to cffi:defcfun
(defmacro module-method (module name &key (return-type :void)
                                     &rest args
                                     &body body)
  "Shortcut for declaring a Ruby module method."
  `(cffi:defcallback ,name ,return-type ,args ,@body)
  (ruby-module-method ,module ,name (cffi:callback ,name) (length ,args)))

(cffi:defcfun ("ruby_define_global_func" ruby-global) :void
  "Define a global Ruby function."
  (name :string)
  (func uintptr_t)
  (argc :int))

; todo, make similar to cffi:defcfun
(defmacro global (name, &key (return-type :void)
                        &rest args 
                        &body body)
  "Shortcut for defining a global Ruby function."
  `(cffi:defcallback ,name ,return-type ,args ,@body)
  (ruby-global ,name (cffi:callback ,name) (length ,args)))

(cffi:defcfun ("ruby_define_constant" const) :void
  "Define a Ruby global constant."
  (name :string)
  (value uintptr_t))

(cffi:defcfun ("ruby_define_module_constant" module-const) :void
  "Define a Ruby module constant - Ex - `Example.CONSTANT"
  (module_name uintptr_t)
  (name :string)
  (value uintptr_t))

(cffi:defcfun ("ruby_undef_constant" undef) :void
  "Undefine a Ruby global constant."
  (name :string))

(cffi:defcfun ("ruby_call_constant" const-call) uintptr_t
  "Call a Ruby global constant."
  (name :string))

(cffi:defcfun ("ruby_call_module_constant" module-const-call) uintptr_t
  "Call a Ruby module constant."
  (module_name uintptr_t)
  (name :string))

(cffi:defcfun ("ruby_funcall" ruby-funcall) uintptr_t
  "Call a Ruby function with arguments."
  (object uintptr_t)
  (func :string)
  ; todo In C this is an array, find out a CFFI array type
  (args :pointer uintptr_t)) 

(cffi:defcfun ("ruby_require" require) :void 
  "Require a Ruby script -- ONLY LOADS ONCE."
  (name :string))

(cffi:defcfun ("ruby_load" load-script) :void  
  "Load a Ruby script, can be loaded more than once."
  (name :string))

(cffi:defcfun ("ruby_load_or_die" load-script-or-die) :void
  "Load a Ruby script, and if an error occurs or it is not
   found, throw an error"
  (name :string)
  (exception :string))

(cffi:defcfun ("ruby_get_last_exception" last-exception) :void
  "Get the last Ruby exception if there is one.")
  ; no arguments 


