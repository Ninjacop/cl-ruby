# cl-ruby

A Common Lisp to Ruby FFI

## Important Note

As of the current state of the project, it has some errors, but will be actively maintained

## Installation

_cl-ruby is not available on Quicklisp yet._

1. Clone this repo into quicklisp/local-projects, or create a symlink with the `cl-ruby.asd` file and put it in that folder.

2. Be sure that libruby.dylib/so/dll is somewhere in your filesystem, but if the library can't be found, go into `cl-ruby.lisp` and change the string `"libruby"` to the absolute path to where it is.

3. You should be ready to go, just load the package in by evaluating `(load "cl-ruby.asd") (ql:quickload 'cl-ruby)`.

## Implementation Support

Implementation | Supported?
-------------- | ----------
SBCL           | :white_check_mark:
CCL            | :x: Hasn't been tested
CLISP          | :x: Needs to be compiled with dynamic FFI support.
ECL            | :white_check_mark:
ABCL           | :white_check_mark:
Clasp          | :x: Hasn't been tested
Allegro CL     | :x: Hasn't been tested
LispWorks      | :x: Hasn't been tested

## Caveats

- If the Ruby VM has not been initialized and a function from `cl-ruby` has been called, a fatal memory error (segfault) will return, which may corrupt the current CL implementation image, in which you may have to restart your implementation.

- If `(init-ruby)` has been called and `(ql:quickload 'cl-ruby)` is evaluated before `(end-ruby)` has been called, a fatal error will occur. In SBCL, you will be navigated to the LDB, or the low-level Lisp DeBugger. To exit, just type `quit`.

## Functions That Do Not Work/Buggy

- in-ruby -> Calling this multiple times will give a bunch of nasty errors on Ruby/C's side (fixing this atm)

- end-ruby -> Gives out a segfault when called a second time

- load-script-or-die -> Gives out a segfault when called and the script can't be found

## Ruby C API

The API used for cl-ruby is available [here](http://silverhammermba.github.io/emberb/c/).

## License

Copyright :copyright: Ninjacop123 2019
GNU GPLv3

## Special Thanks

Special thanks to zulu-inuoe for helping out with how to go about creating class/module/global methods.