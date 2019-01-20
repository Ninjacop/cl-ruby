;;;; cffi-fun-defs.lisp
;;;; Created by Ninjacop123
;;;; Thursday, January 17th, 2019
;;;; CFFI bindings and callbacks from C 
(in-package #:cl-ruby)

;;; Going in and out of Ruby 
(cffi:defcfun ("ruby_init_all" ruby-init-ruby) :void
  "Initializes Ruby and subsequently the FFI.")
  ; no arguments

(cffi:defcfun ("ruby_end_processes" ruby-end-ruby) :void
  "Ends/cleans up the Ruby VM. NOTE: Once this is called,
   it will disable using Ruby in the current session because 
   it fully cleans up the VM.")
  ; no arguments

;;; Ruby evaluation
(cffi:defcfun ("ruby_eval" ruby-evaluate) uintptr_t
  "Evaluate a given Ruby expression/expressions in the form 
   of a string."
  (code :string))

;; this can be uncommented to see the original output, but it is 
;; highly advised not to
;(cffi:defcfun ("ruby_eval_or_die" ruby-evaluate-or-die) uintptr_t
;  "Evaluate a given Ruby expression and throw an error if  
;   applicable."
;  (code :string)
;  (exception :string))

;;; Defining Ruby modules
(cffi:defcfun ("ruby_define_module" module) uintptr_t
  "Define a Ruby module. Ex - `module Example`."
  (module-name :string))

(cffi:defcfun ("ruby_define_submodule" submodule) uintptr_t
  "Define a nested Ruby module. Ex - `module Example::Submodule`."
  (submodule-name :string)
  (module-name uintptr_t))

;;; Defining Ruby classes
(cffi:defcfun ("ruby_define_class" class-with-super) uintptr_t
  "Defines a Ruby class. Ex - `class Example`."
  (name :string)
  (super-name uintptr_t))

(cffi:defcallback define-class-callback uintptr_t ((name :string))
  "Since uintptr_t is only accessible in CFFI, this 
   creates a Ruby/C compatible class with no spnecific superclass"
  (class-with-super name ~basic-object~))

(cffi:defcfun ("ruby_define_subclass" subclass-with-super) uintptr_t
  "Defines a Ruby class that is inherited. Ex - `class Example < Example2`."
  (name :string)
  (parent-name uintptr_t)
  (super-name uintptr_t))

(cffi:defcfun ("ruby_define_class_method" ruby-class-method) :void
  "Defines a Ruby class method."
  (class-name uintptr_t)
  (func-name :string)
  (func :pointer)
  (argc :int))

(cffi:defcfun ("ruby_define_module_method" ruby-module-method) :void
  "Defines a Ruby module method."
  (module-name uintptr_t)
  (func-name :string)
  (func :pointer)
  (argc :int))

(cffi:defcfun ("ruby_define_global_func" ruby-global) :void
  "Define a global Ruby function/method."
  (name :string)
  (func :pointer)
  (argc :int))

;;; Variables
(cffi:defcfun ("ruby_define_int_var" define-int) :void
  "Defines a Ruby VALUE (uintptr_t) integer."
  (name :string)
  (value :int))

(cffi:defcfun ("ruby_define_string_var" define-string) :void 
  "Defines a Ruby VALUE (uintptr_t) string."
  (name :string)
  (value :string))

(cffi:defcfun ("ruby_define_float_var" define-float) :void
  "Defines a Ruby VALUE (uintptr_t) floating point integer."
  (name :string)
  (value :float))

; TODO implement array functions
(cffi:defcfun ("ruby_define_array_var" define-array) :void
  "Defines a Ruby VALUE (uintptr_t) array. NOTE: this will
   return the pointer where it is stored."
  (name :string))

; TODO implement hash functions
(cffi:defcfun ("ruby_define_hash_var" define-hash) :void
  "Defines a Ruby VALUE (uintptr_t) hash table. NOTE: this 
   will return the pointer where it is stored."
  (name :string))

(cffi:defcfun ("ruby_call_int" int-call) :int
  "Ruby VALUEs are sometimes encrypted, so this converts
   the VALUE into a C int, then is returned (not printed)."
  (name :string))

(cffi:defcfun ("ruby_call_string" str-call) :string
  "Ruby VALUEs are sometimes encrypted, so this converts
   the VALUE into a C const char*, then is returned (not printed)."
  (name :string))

(cffi:defcfun ("ruby_call_float" float-call) :float 
  "Ruby VALUEs are sometimes encrypted, so this converts
   the VALUE into a C float, then is returned (not printed)."
  (name :string))

(cffi:defcfun ("ruby_call_array" array-call) uintptr_t
    "Ruby Array VALUEs aren't encrypted, so a pointer 
     is returned."
  (name :string))

(cffi:defcfun ("ruby_call_hash" hash-call) uintptr_t
  "Ruby Hash Table VALUEs aren't encrypted, so a pointer
   is returned."
  (name :string))

;;; Constants 
(cffi:defcfun ("ruby_define_int_constant" int-const) :void 
  "Define an integer Ruby global constant."
  (name :string)
  (value :int))

(cffi:defcfun ("ruby_define_int_module_constant" int-module-const) :void
  "Define an integer Ruby module constant."
  (module-name uintptr_t)
  (name :string)
  (value :int))

(cffi:defcfun ("ruby_define_str_constant" str-const) :void 
  "Define a string Ruby global constant."
  (name :string)
  (value :string))

(cffi:defcfun ("ruby_define_str_module_constant" str-module-const) :void 
  "Define a string Ruby module constant."
  (module-name uintptr_t)
  (name :string)
  (value :string))

(cffi:defcfun ("ruby_define_float_consant" float-const) :void 
  "Define a floating point integer Ruby constant."
  (name :string)
  (value :float))

(cffi:defcfun ("ruby_define_float_module_constant" float-module-const) :void 
  "Define a floating point integer Ruby module constant."
  (module-name uintptr_t)
  (name :string)
  (value :float))

(cffi:defcfun ("ruby_undef_constant" undef-const) :void
  "Undefine a Ruby global constant. The Ruby VM will say that the variable
   is being redefined, because it is being binded to NIL."
  (name :string))

(cffi:defcfun ("ruby_undef_module_constant" undef-module-const) :void
  "Undefine a Ruby module constant. The Ruby VM will say that the variable
   is being redefined, because it is being binded to NIL."
  (module-name uintptr_t)
  (name :string))

(cffi:defcfun ("ruby_call_int_constant" int-const-call) :int 
  "Define an integer Ruby global constant. See int-call for 
   more of an explanation."
  (name :string))

(cffi:defcfun ("ruby_call_int_module_constant" int-module-const-call) :int
  "Define an integer Ruby module constant. See int-call for more of
   and explanation."
  (module-name uintptr_t)
  (name :string))

(cffi:defcfun ("ruby_call_str_constant" str-const-call) :string 
  "Define a string Ruby global constant. See str-call for more of 
   an explanation."
  (name :string))

(cffi:defcfun ("ruby_call_str_module_constant" str-module-const-call) :string
  "Define a string Ruby module constant. See str-call for more of 
   an explanation."
  (module-name uintptr_t)
  (name :string))

(cffi:defcfun ("ruby_call_float_constant" float-const-call) :float 
  "Define a float Ruby constant. See float-call for more of 
   an explantion."
  (name :string))

(cffi:defcfun ("ruby_call_float_module_constant" float-module-const-call) :float
  "Define a float Ruby module constant. See float-call for more of 
   and explantion."
  (module-name uintptr_t)
  (name :string))

;; TODO make variables callable in funcalls
(cffi:defcfun ("ruby_funcall" ruby-funcall) uintptr_t
  "Call a Ruby function with arguments to be applied to an
   object."
  (object uintptr_t)
  (func :string)
  ; todo In C this is an array, find out a CFFI array type
  (args :pointer uintptr_t)) 

(cffi:defcfun ("ruby_name_script" script) :void
  "Create a named script that the Ruby VM can reference for
   errors."
  (name :string))

(cffi:defcfun ("ruby_require" require-script) :void 
  "Require a Ruby script. NOTE: Only loads once."
  (name :string))

(cffi:defcfun ("ruby_load" load-script) :void  
  "Load a Ruby script, can be loaded more than once."
  (name :string))

(cffi:defcfun ("ruby_load_or_die" load-script-or-die) :void
  "Load a Ruby script, and if an error occurs or it is not
   found, throw an error."
  (name :string)
  (exception :string))

(cffi:defcfun ("ruby_get_last_exception" last-exception) :void
  "Get the last Ruby exception if there is one.")
  ; no arguments 