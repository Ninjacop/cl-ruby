# Variables, Constants and Type Conversions

## Variables

**Note**: class variables don't exist yet

With variables, there are 5 options - integer, float, string, array and hash. Hashes and arrays are a little weird though, because you can't see what they look like (at the moment, TODO), just their pointers. In addition with arrays, you can declare them with a certain length, which `nil` is a placeholder for each index. Speaking of Ruby's nil, you can call it by evaluating `(ruby-nil)` -- this returns 8 because Ruby's nil is stored in an enum, and it's in the 8th index. The format of how to declare variables goes like this:

Integers, Floats & Strings:

    (defrubyvar [string name] [type-key] [CL value])

Arrays:

    (defrubyvar [string name] :array &optional :len [integer amount])

Hashes:

    (defrubyvar [string name] :hash)

Here's an example of all these in action:

    ;;;; SBCL -- assuming that (init-ruby) has been called
    * (defrubyvar "my_int" :int 1)

    3
    * (defrubyvar "my_float" :float 2.0)

    ; some pointer to "my_float"
    * (defrubyvar "my_string" :string "Common Lisp")

    ; pointer to "my_string"
    * (defrubyvar "my_array" :array)

    ; pointer to "my_array"
    * (defrubyvar "my_array_and_length" :array :length 5)

    ; pointer to "my_array_and_length", in Ruby it looks like [nil, nil, nil, nil, nil]
    * (defrubyvar "my_hash" :hash)

    ; pointer to "my_hash"

As I've said in the previous entry, there are so many pointers because under Ruby's hood, most of all the values, classes and objects are just pointers being passed around. To combat this, there are functions to convert these types to and from pointers.

## Constants

Also in Ruby, you can define constants at the toplevel and module level. The naming conventions are UPPERCASE or starting with a capital letter, and even if they are declared with MiXeD CaSe or lowercase, the Ruby VM will print out a warning saying that the constant is invalid. Constants also work like `defrubyvar`, except the names should be uppercased. Note that constants are defined under `~object~` class.

Argument options: see `defrubyvar`

- Global Constants -> `defrubygconst` (Don't forget the "g")

        ;;;; SBCL
        * (defrubygconst "MYCONST" :int 1)

        3

- Module Constants -> `defrubymconst` (Don't forget the "m")

        ;;;; SBCL
        * (defrubymconst "Module" "MYCONST" :string "asdf")

        ; pointer

- For all the curious people wondering what'll happen for mixed/lower cased names

        ;;;; SBCL
        * (defrubygconst "myConst" :array :len 3)
        ruby: warning: rb_define_const: invalid name `myConst' for constant

        ; pointer

In addition to defining constants, you can undefine them, which is the equivalent of setting the value to Ruby's NIL.

- For global constants:

        ;;;; SBCL - assuming that `MYCONST` has been defined
        * (undef-const "MYCONST")
        ruby: warning: already initialized constant MYCONST

        ; nothing

- For module constants:

        ;;;; SBCL - assuming that `YOURCONST` is under module "MyModule" and is defined
        * (undef-module-const "MyModule" "YOURCONST")
        ruby: warning: already initialized constant MyModule::YOURCONST

        ; nothing

## Type Conversions

- int2num and num2int -> `num` is a Ruby Numeric, and `int` is a CL integer

        ;;;; SBCL
        * (int2num 1)

        3
        * (num2int 3)

        1

- dbl2num and num2dbl (I know I said floats, but in the Ruby C API the macros are called these)

        ;;;; SBCL
        * (defvar *pointer* (dbl2num 2.0)) ; this is set to a variable

        ; pointer
        * (num2dbl *pointer*) ; so it can be used here

        2.0

- clstr->rbstr and rbstr->clstr

        ;;;; SBCL
        * (defvar *pointer* (clstr->rbstr "Common Lisp"))

        ; pointer
        * (rbstr->clstr *pointer*)

        "Common Lisp"