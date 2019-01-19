#ifndef ruby_types_h
#define ruby_types_h

#include <Ruby/ruby.h>

/* Declaring Ruby variables */
void ruby_define_int_var(const char* name, int value);
void ruby_define_string_var(const char* name, const char* value);
void ruby_define_float_var(const char* name, float value);
void ruby_define_array_var(const char* name);
void ruby_define_hash_var(const char* name);

/* Calling Ruby variables */
int ruby_call_int(const char* name);
const char* ruby_call_string(const char* name);
float ruby_call_float(const char* name);
VALUE ruby_call_array(const char* name);
VALUE ruby_vall_hash(const char* name);

/* Declaring Ruby constants */
void ruby_define_int_constant(const char* name, int value);
void ruby_define_int_module_constant
(VALUE module_name, const char* name, int value);
void ruby_define_str_constant(const char* name, const char* value);
void ruby_define_str_module_constant
(VALUE module_name, const char* name, const char* value);
void ruby_define_float_constant(const char* name, float value);
void ruby_define_float_module_constant
(VALUE module_name, const char* name, float value);

/* Undefining Ruby constants */
void ruby_undef_constant(const char* name);
void ruby_undef_module_constant(VALUE value, const char* name);


/* Calling Ruby constants */
int ruby_call_int_constant(const char* name);
int ruby_call_int_module_constant(VALUE module_name, const char* name);
const char* ruby_call_str_constant(const char* name);
const char* ruby_call_str_module_constant(VALUE module_name, const char* name);
float ruby_call_float_constant(const char* name);
float ruby_call_float_module_constant(VALUE module_name, const char* name);

#endif