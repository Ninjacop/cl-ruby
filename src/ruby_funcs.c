// Created by Ninjacop123 
// Wednesday, January 9th, 2019
#include "../include/ruby_funcs.h"

// returns an evaluated Ruby expression (which is a string) regardless
// of errors
VALUE ruby_eval(const char* code) {
    VALUE result;
    result = rb_eval_string(code);

    return result;
}

// ruby_eval, but this cares about errors
VALUE ruby_eval_or_die(const char* code, const char* exception) {
    int state;
    VALUE result;
    result = rb_eval_string_protect(code, &state);

    if (state) { 
        /* If there is an exception in the Ruby code, it will print here
           because the function returns a Ruby VALUE.
           TODO this problem could probably be solved somewhere down the
           road */
        printf("%s", exception);
        fflush(stdout);
    } 

    return result;
}

// Define a simple Ruby module 
VALUE ruby_define_module(const char* module_name) {
    return rb_define_module(module_name);
}

// Define a Ruby module that uses another Ruby module
VALUE ruby_define_submodule(const char* submodule_name, VALUE module_name) {
    return rb_define_module_under(module_name, submodule_name);
}

// Define a Ruby class with a specific super class
VALUE ruby_define_class(const char* class_name, VALUE super_name) {
    return rb_define_class(class_name, super_name);
}

// Define a Ruby class with a parent class and a super class
VALUE ruby_define_subclass
(const char* subclass_name, VALUE parent_name, VALUE super) {
    return rb_define_class_under(parent_name, subclass_name, super);
}

// Define a Ruby class method
void ruby_define_class_method
(VALUE class_name, const char* func_name, VALUE(*func)(), int argc) {
    rb_define_method(class_name, func_name, func, argc);
}

// Define a Ruby module method
void ruby_define_module_method
(VALUE module_name, const char* func_name, VALUE(*func)(), int argc) {
    rb_define_module_function(module_name, func_name, func, argc);
}

// Define a Ruby global function (outside of a module)
void ruby_define_global_func
(const char* name, VALUE(*func)(), int argc) {
    rb_define_global_function(name, func, argc);
}   

// Use an array of arguments with a given function on a specific object
VALUE ruby_funcall(VALUE object, const char* func, int argc, VALUE* args[]) {
    return rb_funcall(object, rb_intern(func), argc, args);
}

// Create a named script that the Ruby VM can reference
void ruby_name_script(const char* name) {
    ruby_script(name);
}

// Equivalent of `require('example')`, loads a file only once
void ruby_require(const char* name) {
    rb_require(name);
}

// Equivalent of `load('/path/to/example.rb')`, can load a file more than
// once
void ruby_load(const char* name) {
    rb_load(rb_str_new_cstr(name), 0);
}

// Load a ruby script, and if it doesn't exist, syntax error (etc.) throw
// an error
void ruby_load_or_die(const char* name, const char* exception) {
    int state; 
    rb_load_protect(rb_str_new_cstr(name), 0, &state);

    if (state) {
        /* If there is an exception in the Ruby code, it will print here
           because the function returns a Ruby VALUE.
           TODO this problem could probably be solved somewhere down the
           road */
        printf("%s", exception);
        fflush(stdout);
    }
}

// Get the last exception that Ruby threw, and return it
VALUE ruby_get_last_exception() {
    VALUE exception = rb_errinfo();
    return exception;
}

// Initialize the Ruby VM, load paths and enable loading/requiring Ruby scripts
void ruby_init_all() {
    ruby_init();
    ruby_init_loadpath();
}

// ruby_end_processes is declared so in Common Lisp
// it can be called with no arguments instead of 
// including the 0, which may or may not be needed in the future
// i.e. exit(num);
void ruby_end_processes() {
    ruby_cleanup(0); 
}

/* Method main is used for testing if libruby works/compiles
int main () {
    ruby_init();
    VALUE y = DBL2NUM(2);
    rb_gv_set("$y", y);
    printf("%lu\n", rb_gv_get("$y"));
    printf("%f\n", to_ruby_num_converter((double)4));
    ruby_cleanup(0);

    return 0;
}*/