# Changelog

All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).  

## 0.0.2

### Added

- ruby_funcs.h, ruby_types.h, cffi-fun-defs.lisp, ruby_types.c & ruby-types.lisp for better code organization and preference

- Variables and Global & Module Constants! Supported types are Integers, Floats and Strings. -> `defrubyvar`, `defrubygconst`, `defrubymconst`, `const-call`, `module-const-call`, `undef-const` and `undef-module-const`

- Global, Module and Class methods -> `global-method`, `module-method` and `class-method`. Methods/Functions defined through these can be called as an escaped symbol, which should be printed/returned after each one is used.

- init-ruby -> Init the Ruby VM

- end-ruby -> Clean up and end the Ruby VM

- in-ruby -> Macro for doing both of the above quick

- define-subclass -> subclass-with-super but with ~basic-object~ as the superclass

- Test cases for most functions & an example Ruby script for those test cases

- More comments and function descriptions

- `include` folder for C headers

### Changed

- subclass -> subclass-with-super

- evaluate-or-die/last-exception -> Errors are `printf()` & `fflush(stdout)`ed, so that'll share that stream for printing the exception

### Removed

- nested-class -> a duplicate of define-subclass

- nested-subclass -> also a duplicate of define-subclass

## 0.0.1

### Added

- Repository

## TODO

- Maybe not redefine the one-line functions in `ruby_funcs.c` and use the original names in the CFFI bindings?

- Make it easier/simpler to create Ruby methods and functions

- Find out the type for a CFFI array

- Find out where `libruby.dll` and `libruby.so` are stored on Windows & Linux for the Makefile

- Add more functions

- Test every CL implementation

- Finish writing test cases

- Export all the symbols in ruby-types