#include "../include/ruby_types.h"

// Declares a Ruby VALUE int, which is encrypted
void ruby_define_int_var(const char* name, int value) {
    rb_gv_set(name, INT2NUM(value));
}

// Declares a Ruby VALUE string
void ruby_define_string_var(const char* name, const char* value) {
    rb_gv_set(name, rb_str_new_cstr(value));
}

// Declares a Ruby VALUE float, which is encrypted
void ruby_define_float_var(const char* name, float value) {
    rb_gv_set(name, DBL2NUM(value));
}

// Declares a Ruby VALUE array, which returns a pointer
void ruby_define_array_var(const char* name) {
    rb_gv_set(name, rb_ary_new());
}

// Declares a Ruby VALUE hash table, which returns a pointer
void ruby_define_hash_var(const char* name) {
    rb_gv_set(name, rb_hash_new());
}

// Decrypts a given Ruby integer and returns it
int ruby_call_int(const char* name) {
    VALUE num = rb_gv_get(name);
    return NUM2INT(num);
}

// Converts a Ruby string to the C equivalent and returns it
const char* ruby_call_string(const char* name) {
    VALUE str = rb_gv_get(name);
    return StringValueCStr(str);
}

// Decrypts a Ruby floating point integer and returns it
float ruby_call_float(const char* name) {
    VALUE flt = rb_gv_get(name);
    return NUM2DBL(flt);
}

// Returns a pointer to a Ruby array 
VALUE ruby_call_array(const char* name) {
    VALUE arr = rb_gv_get(name);
    return arr;
}

// Returns a pointer to a Ruby hash table 
VALUE ruby_call_hash(const char* name) {
    VALUE hsh = rb_gv_get(name);
    return hsh;
}

// Define a Ruby global constant (outside of a module)
void ruby_define_int_constant(const char* name, int value) {
    rb_define_global_const(name, INT2NUM(value));
}

void ruby_define_str_constant(const char* name, const char* value) {
    rb_define_global_const(name, rb_str_new_cstr(value));
}

void ruby_define_float_constant(const char* name, float value) {
    rb_define_global_const(name, DBL2NUM(value));
}

// Define a Ruby module constant
void ruby_define_int_module_constant
(VALUE module_name, const char* name, int value) {
    // VALUE value needs to be coerced from uintptr_t
    // to a c number/string/something
    // documentation of this is under VALUE in the Ruby C API
    rb_define_const(module_name, name, INT2NUM(value));
}

void ruby_define_str_module_constant
(VALUE module_name, const char* name, const char* value) {
    rb_define_const(module_name, name, rb_str_new_cstr(value));
}

void ruby_define_float_module_constant 
(VALUE module_name, const char* name, float value) {
    rb_define_const(module_name, name, DBL2NUM(value));
}

void ruby_undef_constant(const char* name) {
    // By the API, you can undefine a constant by setting it
    // to Ruby's NIL
    rb_define_global_const(name, Qnil);
}

// Undefine a Ruby module constant (set the constant to Ruby's NIL)
void ruby_undef_module_constant(VALUE module_name, const char* name) {
    // By the API, you can undefine a constant by setting it
    // to Ruby's NIL
    rb_define_const(module_name, name, Qnil);
}

// Call a global constant, but with specific types
int ruby_call_int_constant(const char* name) {
    VALUE integer = rb_const_get(rb_cObject, rb_intern(name));
    return NUM2INT(integer);
}

const char* ruby_call_str_constant(const char* name) {
    VALUE str = rb_const_get(rb_cObject, rb_intern(name));
    return StringValueCStr(str);
}

float ruby_call_float_constant(const char* name) {
    VALUE flt = rb_const_get(rb_cObject, rb_intern(name));
    return NUM2DBL(flt);
}

// Call a module constant, but with specific types
int ruby_call_int_module_constant(VALUE module_name, const char* name) {
    VALUE integer = rb_const_get(module_name, rb_intern(name));
    return NUM2INT(integer);
}

const char* ruby_call_str_module_constant(VALUE module_name, const char* name) {
    VALUE str = rb_const_get(module_name, rb_intern(name));
    return StringValueCStr(str);
}

float ruby_call_float_module_constant(VALUE module_name, const char* name) {
    VALUE flt = rb_const_get(module_name, rb_intern(name));
    return NUM2DBL(flt);
}


int main() {
    ruby_init();
    rb_define_global_const("NAM", INT2NUM(1));
    VALUE x = rb_const_get(rb_cObject, rb_intern("NAM"));
    printf("%d", NUM2INT(x));
    ruby_cleanup(0);
    return 0;
}