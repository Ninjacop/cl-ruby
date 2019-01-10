# Changelog

All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).  

## Unreleased

### Added

- ruby-init -> Init the Ruby VM

- ruby-end -> Clean up and end the Ruby VM

- in-ruby -> Macro for doing both of the above quick

- define-subclass -> subclass-with-super but with ~basic-object~ as the superclass

- Test cases for most functions & an example Ruby script for those test cases

## Changed

- subclass -> subclass-with-super

- Renamed `ruby-types.lisp` to `ruby-classes.lisp` because they aren't types

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