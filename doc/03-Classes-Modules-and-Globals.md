# Classes, Modules and Globals

A core part of Ruby is creating Classes and Modules. Like variables and other values, they return pointers to the class or module.

## Classes

For defining classes, a super class is always needed, and for toplevel/global classes, use `~basic-object~` or `~object~`, but I advise to use the latter, because constants are defined under that class.

    ;;;; SBCL
    * (defrubyclass "Example" ~object~)

    ; pointer

To create a function/method in that class, use `class-method`. This creates a callback, where you can redefine it, but it doesn't work with variadic functions. In addition, to make this function callable in CL, it is interned, so don't forget to add the `|`s around the function name.

Arguments:

    (class-method [superclass] [string class] [string function] [arg list] [body])

Here's it in action:

    ;;;; SBCL
    * (class-method ~object~ "Class" "func" (x y)
        (+ x y))

    |Class.func|
    * (|Class.func| 1 1)

    2

In addition to having regular classes, you can create subclasses via `defrubysubclass`. It works the same as `defrubyclass` as well.

Arguments:

    (defrubysubclass [string name] [pointer to parent] [pointer to super])

Example:

    ;;;; SBCL
    * (defvar *example* (defrubyclass "Example" ~object~)) ; parent class

    *EXAMPLE*
    * (defvar *example2* (defrubysubclass "ExampleTwo" *example* ~object))

    *EXAMPLE2*

It also works the same for `class-method`, except the superclass is the parent class.

## Modules

Unlike classes, modules are much easier to create. With that you can also create submodules and module methods/functions, too.

Arguments for declaring modules:

    (module [string name])

Example:

    ;;;; SBCL
    * (module "Example")

    ; pointer

Arguments for declaring submodules:

    (submodule [string name] [pointer to parent module])

Example:

    ;;;; SBCL
    * (defvar *example* (module "Example"))

    *EXAMPLE*
    * (defvar *example2* (submodule "ExampleTwo" *example*))

    *EXAMPLETWO*

Arguments for declaring module methods/functions:

    (class-method [string module-name] [string function-name] [arg list] [body])

Example:

    ;;;; SBCL
    * (defvar *example* (module "Example"))

    *EXAMPLE*

    * (class-method "Example" "func" (x y)
        (- x y))

    |Example.func|
    * (|Example.func| 2 1)

    1

## Globals

Now back to globals. There's no need for defining globals, only global methods with `global-method`. A global method is called by adding a `global.` to the beginning of the name.

Arguments:

    (global-method [string func-name] [arg list] [body])

Example:

     ;;;; SBCL
     * (global-method "func" (x y)
         (* x y))

    |global.func|
    * (|global.func| 2 2)

    4