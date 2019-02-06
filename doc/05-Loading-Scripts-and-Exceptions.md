# Loading Scripts and Exceptions

## Loading Scripts

The easy way to load Ruby scripts is to use `load-script`

    (load-script [pointer to string] [number value])

Passing a non-zero value to the second argument "can wrap the loaded code in an anonymous module to protect the global namespace" (taken from the Ruby C API).

Example:

    ;;;; SBCL
    * (load-script (clstr->rbstr "./example.rb) 0)

    ; nothing is returned, but the script is loaded

Kind of like `evaluate-or-die`, you can also use `load-script-or-die`, where it takes the exact same arguments as `load-script`, except another string is added to the function call. **As of current release (Feb 2019), DO NOT USE THIS YET**

Example (of what's supposed to happen):

    ;;;; SBCL
    * (load-script-or-die (clstr->rbstr "~/example.rb) 0 "Error!")

    Error!

## Requiring scripts

`require` in Ruby loads a script once instead of using it multiple times, which the CL equivalent is `require-script`.

Arguments:

    (require-script [string path-to-script])

Example:

    ;;;; SBCL
    * (require-script "racc")

    ; nothing is returned or a SEGFAULT is produced

## Exceptions

`last-exception` -> get the last exception thrown
`clear-last-exception` -> clear the last exception

I'm not sure that these have any impact on anything, because if an error arises, usually it's a SEGFAULT and the likes.

Examples:

    ;;;; SBCL
    ; do something bad
    * (last-exception)

    ; if there's exception, print it, else nothing
    * (clear-exception)

    ; nothing is returned no matter what