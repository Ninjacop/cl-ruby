## example.rb
## Created by Ninjacop123
## Thursday, January 17th, 2019
## An example function to be tested & called in C and/or CL

# Define a function called hello
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
    def self.hello(x)
        puts "Hello, #{x}!"
    end
    
    def inst(x, y)
        puts "#{x} is better than #{y}, change my mind."
    end
end