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
/*  Method main is used for testing if libruby works/compiles
int main() {
    ruby_init();
    rb_gv_set("name", INT2NUM(30));
    ruby_call_int("name");
    ruby_cleanup(0);
    return 0;
}*/