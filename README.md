# cl-ruby

A Common Lisp to Ruby FFI

## Important Note

As of the current state of the project, it isn't functional, but it will be actively maintained.

## Installation

_cl-ruby is not available on Quicklisp or ASDF yet._

1. Clone this repo into quicklisp/local-projects, or create a symlink with the `cl-ruby.asd` file and put it in that folder.

2. Navigate to the folder containing the Makefile and run `make (mac/linux/windows)` to create a library.

3. You should be ready to go, just load the package in by evaluating `(ql:quickload 'cl-ruby)`.

## Implementation Support

Implementation | Supported?
-------------- | ----------
SBCL           | :white_check_mark:
CCL            | :x:
CLISP          | :x: Hasn't been tested
ECL            | :white_check_mark:
ABCL           | :white_check_mark:
Clasp          | :x: Hasn't been tested
Allegro CL     | :x: Hasn't been tested
LispWorks      | :x: Hasn't been tested

## Caveats

- If you call `(end-ruby)` without calling `(init-ruby)` beforehand, you will get an error on the lines of a corruption warning, memory fault and the likes. You may have to restart your CL implementation if it is severe enough.

- If you call any function except `(init-ruby)`, `(in-ruby)` and `(end-ruby)` (see above) without calling `(init-ruby)` or `(in-ruby)`, you will also get a corruption warning, memory fault error, etc. You may have to restart your CL implementation if it is severe enough.

- If `(init-ruby)` has been called and `(ql:quickload 'cl-ruby)` is evaluated before `(end-ruby)` has been called, a fatal error will occur. In SBCL, you will be navigated to the LDB, or the low-level Lisp DeBugger

- Any symbol in cl-ruby (i.e. `~basic-object~`) is not exported, therefore you have to evaluate `(in-package :cl-ruby)` at the moment

## Functions That Do Not Work/Buggy

- evaluate-or-die -> Error is outputted after you quit out of your implementation

- define-class -> use `(defparameter *example* (class-with-super "Example" ~basic-object~))`

- class-method -> returns NIL/void so nothing can be captured/set

- module-method -> returns NIL/void so nothing can be captured/set

- global -> returns NIL/void so nothing can be captured/set

- call-const/module-const-call -> Values inputted aren't turned into C/Ruby values?

- ruby-funcall -> variables don't exist yet, so funcall doesn't have any arguments

- require-script -> calling this will result in a never ending loop where Ctrl-C cannot be used successfully

- load-script

- last-exception -> Error is outputted after you quit out of your implementation

## Ruby C API

The API used for cl-ruby is available [here](http://silverhammermba.github.io/emberb/c/).

## License

Copyright :copyright: Ninjacop123 2019
GNU GPLv3