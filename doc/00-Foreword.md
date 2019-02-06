# Foreword

First off, thank you for using cl-ruby! This library is pretty cool, but also "dangerous". Don't be alarmed though! cl-ruby is just prone to SEGFAULTs because it's a low-level FFI, so a typo or calling a function in the wrong place just means you probably have to reload your CL implementation.

## Variables, Constants, and Everything that is Defined

With that being said, when a most of things are defined through cl-ruby functions, a C pointer will be returned, and unless you can copy that value over and over again, it would be easiest to set it to something. Yes, you can call the same function with the same parameter to get the same pointer, but I advise that it be set to a variable.

### Without further ado, let's jump right in!