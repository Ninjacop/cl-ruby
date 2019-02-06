;;;; cffi-fun-defs.lisp
;;;; Created by Ninjacop123
;;;; Thursday, January 17th, 2019
;;;; CFFI bindings and callbacks from C 
(in-package #:cl-ruby)

;;; In Ruby, it's NIL is in an enum, which is coerced into a Ruby VALUE (unintptr_t)
;;; like so: #define Qnil (VALUE)RUBY_Qnil; 
;;; TODO maybe use/replicate the Ruby enum, and use `foreign-enum-value`?
(defun ruby-nil ()
  "The current equivalent of Ruby's NIL"
  (translate-to-foreign 8 'uintptr_t))

;;; Going in and out of Ruby 
(cffi:defcfun ("ruby_init" ruby-init-ruby) :void
  "Initializes Ruby and subsequently the FFI.")
  ; no arguments

(cffi:defcfun ("ruby_init_loadpath" ruby-init-loadpath) :void
  "Initializes and makes loading files possible")
  ; no arguments

(cffi:defcfun ("ruby_finalize" ruby-finalize) :void 
  "Ends the Ruby VM, but this makes it so the VM can be restarted")
  ; no arguments

(cffi:defcfun ("ruby_cleanup" ruby-end-ruby) :void
  "Ends/cleans up the Ruby VM. NOTE: Once this is called,
   it will disable using Ruby in the current session because 
   it fully cleans up the VM."
   (code :int))

;;; Type conversions
(cffi:defcfun ("rb_int2inum" int2num) uintptr_t
  "Converts a Common Lisp integer to a Ruby numeric (integer)."
  (num :int))

(cffi:defcfun ("rb_num2int" num2int) :int
  "Converts a Ruby numeric (integer) to a Common Lisp integer."
  (num uintptr_t))

(cffi:defcfun ("rb_float_new" dbl2num) uintptr_t
  "Converts a Common Lisp floating point integer to a Ruby
   numeric (double)."
  (double :float))

(cffi:defcfun ("rb_num2dbl" num2dbl) :float 
  "Converts a Ruby numeric (double) to a Common Lisp floating
   point integer."
  (double uintptr_t))

(cffi:defcfun ("rb_str_new_cstr" clstr->rbstr) uintptr_t
  "Converts a Common Lisp string into a Ruby string pointer."
  (str :string))

(cffi:defcfun ("rb_string_value_cstr" rb-string-value-cstr) :string 
  "Converts a Ruby string pointer into a Common Lisp string."
  (str :pointer))

;;; Ruby evaluation
(cffi:defcfun ("rb_eval_string" ruby-evaluate) uintptr_t
  "Evaluate a given Ruby expression/expressions in the form 
   of a string."
  (code :string))

(cffi:defcfun ("rb_eval_string_protect" ruby-eval-or-die) uintptr_t
  "A wrapper function to evaluating a Common Lisp string containing 
   Ruby code, which is evaluated and raises a custom runtime error 
   if something arises."
  (code :string)
  (state :int))

;;; Defining Ruby modules
(cffi:defcfun ("rb_define_module" module) uintptr_t
  "Define a Ruby module. Ex - `module Example`."
  (module-name :string))

(cffi:defcfun ("rb_define_module_under" submodule) uintptr_t
  "Define a nested Ruby module. Ex - `module Example::Submodule`."
  (submodule-name :string)
  (module-name uintptr_t))

;;; Defining Ruby classes
(cffi:defcfun ("rb_class_new_instance" new-ruby-class-instance) uintptr_t
  "Creates a pointer for a specific class (case senstive) where Ruby class
   instance functions can be used."
  (argc :int)
  (ary  :pointer)
  (using-class uintptr_t))

(cffi:defcfun ("rb_define_class" defrubyclass) uintptr_t
  "Defines a Ruby class. Ex - `class Example`."
  (name :string)
  (super-name uintptr_t))

(cffi:defcfun ("rb_define_class_under" defrubysubclass) uintptr_t
  "Defines a Ruby class that is inherited. Ex - `class Example < Example2`."
  (name :string)
  (parent-name uintptr_t)
  (super-name uintptr_t))

(cffi:defcfun ("rb_define_method" ruby-class-method) :void
  "Defines a Ruby class method."
  (class-name uintptr_t)
  (func-name :string)
  (func :pointer)
  (argc :int))

(cffi:defcfun ("rb_define_module_method" ruby-module-method) :void
  "Defines a Ruby module method."
  (module-name uintptr_t)
  (func-name :string)
  (func :pointer)
  (argc :int))

(cffi:defcfun ("rb_define_global_function" ruby-global) :void
  "Define a global Ruby function/method."
  (name :string)
  (func :pointer)
  (argc :int))

;;; Variables
(cffi:defcfun ("rb_gv_set" ruby-define-global) uintptr_t
  "Set/define a Ruby global variable, which the pointer/Ruby 
   value is returned."
  (name :string)
  (value uintptr_t))

(cffi:defcfun ("rb_gv_get" ruby-get-global) uintptr_t
  "Get the value of a defined Ruby global variable."
  (name :string))

(cffi:defcfun ("rb_ary_new" ruby-new-array) uintptr_t 
  "Create a new Ruby array, returning the pointer where 
   the array is stored.")
   ; no arguments 

(cffi:defcfun ("rb_ary_new_capa" ruby-new-array-length) uintptr_t
  "Create a new Ruby array with a specific, predetermined length,
   filled with Ruby NILs as placeholders"
   (len :int))

;; TODO | keep this here, probably don't need to implement because of 
;; TODO | ruby-funcall?
(cffi:defcfun ("rb_ary_store" ruby-arr-insert) uintptr_t
  "Replace the value at the given index of a certain Ruby array."
  (arr uintptr_t)
  (index :int)
  (value uintptr_t))

(cffi:defcfun ("rb_hash_new" ruby-new-hash) uintptr_t
  "Create a new Ruby hash table, returning the pointer where
   the hash is stored.")
  ; no arguments

;;; Constants 
(cffi:defcfun ("rb_define_global_const" ruby-define-gconst) :void 
  "Define a Ruby global constant."
  (name :string)
  (value uintptr_t))

(cffi:defcfun ("rb_define_const" ruby-define-mconst) :void
  "Define a Ruby module constant."
  (module-name uintptr_t)
  (name :string)
  (value uintptr_t))

(cffi:defcfun ("rb_const_get" ruby-get-const) uintptr_t 
  "Get the value of a constant of any level (global/class/module)
   dependent if the module name is given."
  (module-name uintptr_t)
  (name uintptr_t))


(cffi:defcfun ("rb_funcallv" rb-funcall-v) uintptr_t
  "Call a Ruby function with arguments to be applied to an
   object."
  (object uintptr_t)
  (func uintptr_t)
  (argc :int)
  (args (:pointer uintptr_t)))

(cffi:defcfun ("rb_intern" ruby-intern) uintptr_t
  "Creates a Ruby symbol."
  (sym :string))

(cffi:defcfun ("ruby_script" script) :void
  "Create a named script that the Ruby VM can reference for
   errors."
  (name :string))

(cffi:defcfun ("rb_require" require-script) :void 
  "Require a Ruby script. NOTE: Only loads once."
  (name :string))

(cffi:defcfun ("rb_load" load-script) :void  
  "Load a Ruby script, can be loaded more than once."
  (name uintptr_t)
  (value :int))

(cffi:defcfun ("ruby_load_protect" load-script-protect) :void
  "Load a Ruby script, and if an error occurs or it is not
   found, throw an error."
  (name uintptr_t)
  (value :int)
  (state :int))

;;; Errors & Error info
(cffi:defcfun ("rb_errinfo" last-exception) :void
  "Get the last Ruby exception if there is one.")
  ; no arguments 

(cffi:defcfun ("rb_set_errinfo" set-last-exception) :void
  "Set the current error info of the last exception given by Ruby."
  (value uintptr_t))