# Using Ruby Scripts

The final part of the tutorial. You made it!

The cool thing that loading/requiring scripts does is allows Ruby code to be called straight into CL, without a lot of trouble. With that being said, there is a special thing that Ruby does with its classes. Some class functions defined as `self.funcName` are class functions, while functions in a class that are defined without the `self` are instance functions. To get those functions to be callable in CL, `class-instance` needs to be invoked for instance functions, and `class-accessor` needs to be invoked for class functions. Using this Ruby example script below, here's how to convert Ruby functions into Common Lisp. (This Ruby code is also in the `t` folder)

Arguments:

    (class-instance [pointer to module] [string class])

    (class-accessor [pointer to module] [string class])

```ruby
# Define a global function called hello
def hello
    puts 'Hello Common Lisp!'
end

# Define a function called hello with x and y as arguments
def helloWithArgs(x, y)
    puts "Hello, #{x}! I'm #{y}!"
end

# Define a class `Example` with the class function `hello`
# with x as arguments and an instance function `inst` with arguments
# x and y
class Example
    def self.amazing(x)
        puts  "#{x} is amazing!"
    end

    def inst(x, y)
        puts "#{x} is better than #{y}, change my mind."
    end
end
```

Now a full example in CL:

    ;;;; SBCL
    * (init-ruby)

    T
    * (load-script (clstr->rbstr "./t/example.rb") 0)
    ; assuming we're in the top folder of cl-ruby

    * (ruby-funcall ~object~ "hello")
    ; ~object~ is toplevel, no arguments too
    Hello Common Lisp!

    8
    * (ruby-funcall ~object~ "helloWithArgs" "Ruby" "Common Lisp")
    Hello Ruby! I'm Common Lisp!

    8
    * (defvar *example-class* (defrubyclass "Example" ~object~)

    *EXAMPLE-CLASS*
    * (defvar *example-instance* (class-instance *example-class* "Example"))

    *EXAMPLE-INSTANCE*
    * (defvar *example-accessor* (class-accessor *example-class* "Example"))

    *EXAMPLE-ACCESSOR*
    * (ruby-funcall *example-instance* "inst" "Common Lisp" "Ruby")
    Common Lisp is better than Ruby, change my mind.

    8
    * (ruby-funcall *example-accessor* "amazing" "Common Lisp")
    Common Lisp is amazing!

    8

## The End

You've reached the end! Once again, thank you for using cl-ruby, and I hope you can create cool things using it!