// Created by Ninjacop123 
// Wednesday, January 9th, 2019
#include "ruby_funcs.h"

VALUE ruby_eval(const char* code) {
    VALUE result;
    result = rb_eval_string(code);

    return result;
}

VALUE ruby_eval_or_die(const char* code, const char* exception) {
    int state;
    VALUE result;
    result = rb_eval_string_protect(code, &state);

    if (state) { 
        // If there is an exception in the Ruby code, it will print here
        printf("%s", exception);
    } 

    return result;
}

VALUE ruby_define_module(const char* module_name) {
    return rb_define_module(module_name);
}

VALUE ruby_define_submodule(const char* submodule_name, VALUE module_name) {
    return rb_define_module_under(module_name, submodule_name);
}

VALUE ruby_define_class(const char* class_name, VALUE super_name) {
    return rb_define_class(class_name, super_name);
}

VALUE ruby_define_subclass
(const char* subclass_name, VALUE parent_name, VALUE super) {
    return rb_define_class_under(parent_name, subclass_name, super);
}

void ruby_define_class_method
(VALUE class_name, const char* func_name, VALUE(*func)(), int argc) {
    rb_define_method(class_name, func_name, func, argc);
}

void ruby_define_module_method
(VALUE module_name, const char* func_name, VALUE(*func)(), int argc) {
    rb_define_module_function(module_name, func_name, func, argc);
}

void ruby_define_global_func
(const char* name, VALUE(*func)(), int argc) {
    rb_define_global_function(name, func, argc);
}   

void ruby_define_constant(const char* name, VALUE value) {
    rb_define_global_const(name, value);
}

void ruby_define_module_constant
(VALUE module_name, const char* name, VALUE value) {
    // VALUE value needs to be coerced from uintptr_t
    // to a c number/string/something
    // documentation of this is under VALUE in the Ruby C API
    rb_define_const(module_name, name, value);
}

void ruby_undef_module_constant(const char* name) {
    // By the API, you can undefine a constant by setting it
    // to Ruby's NIL
    rb_define_global_const(name, Qnil);
}

VALUE ruby_call_constant(const char* name) {
    // A Constant with no module/context can be declared 
    // under a basic object
    return rb_const_get(rb_cBasicObject, rb_intern(name));
}

VALUE ruby_call_module_constant(VALUE module_name, const char* name) {
    return rb_const_get(module_name, rb_intern(name));
}

VALUE ruby_funcall(VALUE object, const char* func, VALUE* args[]) {
    int argc = sizeof(*args)/sizeof(args[0]);
    return rb_funcall(object, rb_intern(func), argc, args);
}

void ruby_require(const char* name) {
    rb_require(name);
}

void ruby_load(const char* name) {
    rb_load(rb_str_new_cstr(name), 0);
}

void ruby_load_or_die(const char* name, const char* exception) {
    int state; 
    rb_load_protect(rb_str_new_cstr(name), 0, &state);

    if (state) {
        printf("%s", exception);
    }
}

VALUE ruby_get_last_exception() {
    VALUE exception = rb_errinfo();
    return exception;
}

void ruby_init_all() {
    ruby_init();
    ruby_init_loadpath();
}

// ruby_end_processes is declared so in Common Lisp
// it can be called with no arguments instead of 
// including the 0
void ruby_end_processes() {
    ruby_cleanup(0); 
}

// Method main is used for testing if libruby works/compiles
int main () {
    ruby_init();
    VALUE result;
    result = rb_eval_string("puts 'example'");
    
    return 0;
}