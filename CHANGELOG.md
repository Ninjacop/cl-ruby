# Changelog

All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).  

## 0.0.4

### Added

- Finished adding in all the documentation, see the `doc` folder

### Changed

- `define-class` & `define-subclass` -> `defrubyclass` & `defrubysubclass`

- in `evaluate-or-die` and `load-script-or-die`, there was a typo -- `(fiinish-output)`

- `ruby-global` helper function was referencing a C function from when there was a need to create a shared library

- Made `example.rb` cooler

## 0.0.3

### Added

- ruby-funcall -> It works pretty well now!

- Started working on documentation, and how to actually use cl-ruby (see `doc` folder)

- Test functions/cases are fully completed

- Expanded upon the `example.rb`, which Ruby code from scripts can now be called in Common Lisp!!

- Added Ruby <-> CL type conversions

### Changed

- end-ruby -> now `stop-ruby` because you can stop the Ruby VM in more than one way

- class-with-super & subclass-with-super -> they are now `define-class` and `define-subclass`

- cffi-fun-defs.lisp -> cffi-definitions.lisp

### Removed

- var-call, const-call & module-const-call -> now merged with `defrubyvar` and the likes

- The need to create a shared library & C code/headers

## 0.0.2

### Added

- ruby_funcs.h, ruby_types.h, cffi-fun-defs.lisp, ruby_types.c & ruby-types.lisp for better code organization and preference

- Variables and Global & Module Constants! Supported types are Integers, Floats and Strings. -> `defrubyvar`, `defrubygconst`, `defrubymconst`, `const-call`, `module-const-call`, `undef-const` and `undef-module-const`

- Global, Module and Class methods -> `global-method`, `module-method` and `class-method`. Methods/Functions defined through these can be called as an escaped symbol, which should be printed/returned after each one is used.

- init-ruby -> Init the Ruby VM

- end-ruby -> Clean up and end the Ruby VM

- in-ruby -> Macro for doing both of the above quick

- subclass -> subclass-with-super but with `~basic-object~` as the superclass

- Test cases for most functions & an example Ruby script for those test cases

- More comments and function descriptions

- `include` folder for C headers

### Changed

- define-subclass -> subclass-with-super

- evaluate-or-die/last-exception -> Errors are `printf()` & `fflush(stdout)`ed, so that'll share that stream for printing the exception

### Removed

- nested-class -> a duplicate of define-subclass

- nested-subclass -> also a duplicate of define-subclass

## 0.0.1

### Added

- Repository

## TODO

- Shorten function names

- Find a way to turn a Ruby array/hash into a list and hashtable that CL can view/interpret

- Add more exception functions

- Test every CL implementation

- Change the tests