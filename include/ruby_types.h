#ifndef ruby_types_h
#define ruby_types_h

#include <Ruby/ruby.h>

/* Declaring Ruby variables */
void ruby_define_int_var(const char* name, int value);
void ruby_define_string_var(const char* name, const char* value);
void ruby_define_float_var(const char* name, float value);
void ruby_define_array_var(const char* name);
void ruby_define_hash_var(const char* name);

/* Calling Ruby variables*/
int ruby_call_int(const char* name);
const char* ruby_call_string(const char* name);
float ruby_call_float(const char* name);
VALUE ruby_call_array(const char* name);
VALUE ruby_vall_hash(const char* name);
#endif