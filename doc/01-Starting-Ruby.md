# Starting Ruby

## Starting the VM

Since we're calling and using Ruby through C, we have to manually start and stop the VM, even though this step is automated when you execute `ruby` or `irb` on the command line.

So boot up your CL implementation and do the following (assuming that `cl-ruby` has been loaded):

    ;;;; SBCL
    * (init-ruby)

    T

Alright! So the Ruby VM has been started and you're ready to get started.

Just a strong reminder: if you call any function in cl-ruby before calling `init-ruby`, you will get a segfault (or a memory fault in SBCL), in which the implementation needs to be restarted.

## Stopping the VM

Once your done with using Ruby for your code, you can call `stop-ruby` to stop the VM, so you can take a sigh of relief that you won't have to worry about any more segfaults. But, once you call `stop-ruby`, you cannot call `init-ruby` any more in that session, because the Ruby VM has been fully cleaned up.

    ;;;; SBCL
    * (init-ruby)

    T

    ; do stuff

    * (stop-ruby)

    NIL

If it gets annoying to keep restarting your CL implementation, fear not! There's another function that stops the Ruby VM, but not completely -- `end-ruby`. WARNING: Still under works, as calling this twice will result in a segfault.

    ;;;; SBCL
    * (init-ruby)

    T
    * (end-ruby)

    NIL
    * (init-ruby)
    ruby: warning: already initialized constant TMP_RUBY_PREFIX
    ; ^^^ This is the Ruby VM re-initializing, where ^^^^ constant is being re-initialized

    T
    * (end-ruby)

    ; segfault at the moment

## First Steps

So there's the absolute basics. Next, to evaluate Ruby code, you can use `evaluate`, which takes the code as a string. Depending on your OS type (x86-64 v. x64) the value returned will be the length of a C `uintptr_t`, which is either a 4 or an 8 if no other value is returned.

    ;;;; SBCL
    * (init-ruby)

    T
    * (evaluate "puts 'Hello Common Lisp!!!!'")
    Hello Common Lisp!!!!

    8 
    * (evaluate "1 + 2")

    7

Wait what?? `1 + 2` returns 7?? A lot of Ruby's VM is just a bunch of pointers being passed around, which is the case for this. In the next chapter or in "02-Variables-and-Types.md", we will go over how to turn those pointers into Common Lisp values.