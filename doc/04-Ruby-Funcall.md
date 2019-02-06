# Ruby Funcall

Instead of porting over every single function from Ruby into CL, `ruby-funcall` does that while keeping cl-ruby small. In addition, this returns a pointer or Ruby value, so `ruby-funcall` will have to be encased with an `num2int` and the likes.

Arguments:

    (ruby-funcall [pointer to object/variable/value] [string func-name] &rest [arguments])

In Ruby, you can call a function like this:

    1 + 2

...and that's totally fine, but it can be simplified down to this:

    1.+(2)

Now in CL, `ruby-funcall` is the same as doing the latter example in Ruby.

For example, in Ruby, you can add a key/value pair by doing this:

    $hashTbl = {"one" => 1, "two" => 2} # global variable

    $hashTbl["three"] = 3

    puts $hashTbl # {"one" => 1, "two" => 2, "three" => 3}

The equivalent of doing it in CL would be:

    ;;;; SBCL
    * (defparameter *hashTbl* (defrubyvar "hashTbl" :hash))

    *HASHTABLE*
    * (ruby-funcall *hashTbl* "store" "one" 1)
    ; equivalent to $hashTbl.store("one", 1)

    ; pointer

So that's the only drawback with `ruby-funcall`, but otherwise it's super useful, and can be used with mostly everything.

Another example:

    ;;;; SBCL
    * (num2int (ruby-funcall (int2num 4) "*" 2))
    ; only the first object has to be converted into a Ruby value

    8