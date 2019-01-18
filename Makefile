# Created by Ninjacop123 
# Wednesday, January 9th, 2019

# Change the path according to your computer here
RUBYDYLIB:=/usr/local/Cellar/ruby/2.6.0/lib/
#RUBYDLL:=/path/to/libruby.dll
#RUBYSO:=/path/to/libruby.so

SRC:=$(wildcard src/*.c)

mac:
	@-gcc -L${RUBYDYLIB} -lruby -dynamiclib -o cl-ruby.dylib ${SRC}

#windows:

#linux:

test:
	@-gcc -L${RUBYDYLIB} -lruby ${SRC} -o main