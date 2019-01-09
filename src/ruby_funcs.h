// Created by Ninjacop123 
// Wednesday, January 9th, 2019
#ifndef ruby_funcs_h
#define ruby_funcs_h

// Ruby C API is available at http://silverhammermba.github.io/emberb/c/

#include <Ruby/ruby.h>

/* Evaluating Ruby code from a string */
VALUE ruby_eval(const char* code);
VALUE ruby_eval_or_die(const char* code, const char* exception);

/* Define modules and submodules/child modules */
void ruby_define_module(const char* module_name);
void ruby_define_submodule(const char* submodule_name, VALUE module_name);

/* Define classes and subclasses/child classes */
void ruby_define_class(const char* class_name, VALUE super_name);
void ruby_define_subclass
(const char* subclass_name, VALUE parent_name, VALUE super);

/* Define class methods, module methods and global methods/functions */
void ruby_define_class_method
(VALUE class_name, const char* func_name, VALUE(*func)(), int argc);
void ruby_define_module_method
(VALUE module_name, const char* func_name, VALUE(*func)(), int argc);
void ruby_define_global_func(const char* name, VALUE(*func)(), int argc);

/* Define module and global constants, undefine them  
   and call them when needed */
void ruby_define_constant(const char* name, VALUE value);
void ruby_define_module_constant
(VALUE module_name, const char* name, VALUE value);
void ruby_undef_constant(const char* name);
VALUE ruby_call_constant(const char* name);
VALUE ruby_call_module_constant(VALUE module_name, const char* name);

/* Call a Ruby function */
VALUE ruby_funcall(VALUE object, const char* func, VALUE* args[]);
 
/* Require and load a Ruby function */
void ruby_require(const char* name);
void ruby_load(const char* name);
void ruby_load_or_die(const char* name, const char* exception);

/* Get the last exception for debugging */
VALUE ruby_get_last_exception();

#endif
